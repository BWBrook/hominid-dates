# R/leadlag.R ---------------------------------------------------------------
# Lead–lag analysis of FAD/LAD by country relative to continent

#' Map country names to continent
#' @keywords internal
country_to_continent <- function(x) {
  import::from("countrycode", countrycode)
  # Maps common English country names to continent names
  countrycode(x, origin = "country.name", destination = "continent")
}

#' Compute ΔFAD/ΔLAD by country vs continent with bootstrapping
#'
#' For each species–country pair with >= 2 records, compute country FAD/LAD and
#' subtract the corresponding continent-level FAD/LAD computed across all
#' occurrences on the same continent for that species. Dates are in Ma.
#'
#' @param tbl Occurrence tibble (after collapse; may include indeterminates).
#' @param B Number of bootstrap draws over date intervals (default 1000).
#' @param dist Sampling distribution for `mc_dates()` (default "uniform").
#' @param tol Tolerance around 0 for declaring a tie (default 1e-3).
#' @param lead_is_negative Logical: if TRUE, label lead when Δ < -tol; if FALSE,
#'        label lead when Δ > tol. Because dates are in Ma (older = larger), the
#'        intuitive choice is `FALSE` (default).
#' @return Tibble with columns per spec: species, country, delta_fad_median,
#'         delta_fad_lower, delta_fad_upper, delta_lad_median, delta_lad_lower,
#'         delta_lad_upper, leadlag_status, n_records, continent.
#' @export
fad_lad_leadlag <- function(tbl, B = 1000, dist = "uniform", tol = 1e-3,
                            lead_is_negative = FALSE) {

  import::from("dplyr", filter, mutate, select, group_by, summarise, ungroup,
               left_join, inner_join, semi_join, arrange, rename, n, distinct)
  import::from("purrr", imap_dfr)
  import::from("tibble", tibble)

  # Guard: required columns
  req <- c("species", "country", "lower", "upper", "date")
  miss <- setdiff(req, names(tbl))
  if (length(miss) > 0) stop(sprintf("fad_lad_leadlag: missing columns: %s", paste(miss, collapse = ", ")))

  # Add continent
  df_all <- tbl |>
    filter(!is.na(species), !is.na(country)) |>
    mutate(continent = country_to_continent(country))

  # Species–country counts; retain pairs with n >= 2
  pair_counts <- df_all |>
    group_by(species, country) |>
    summarise(n_records = dplyr::n(), .groups = "drop") |>
    filter(n_records >= 2)

  # List of draws across ALL rows (to compute continent-wide stats consistently)
  draws <- mc_dates(df_all, B = B, dist = dist)

  # Per-draw deltas for eligible species–country pairs
  per_draw <- imap_dfr(
    draws,
    ~ {
      df <- .x
      b  <- .y

      # attach continent
      df <- df |>
        mutate(continent = country_to_continent(country))

      # Country-level FAD/LAD per species–country (computed on this draw)
      country_stats <- df |>
        group_by(species, country) |>
        summarise(
          fad = max(date_mc, na.rm = TRUE),
          lad = min(date_mc, na.rm = TRUE),
          .groups = "drop"
        ) |>
        inner_join(pair_counts, by = c("species", "country"))

      # Continent-level FAD/LAD per species–continent
      cont_stats <- df |>
        filter(!is.na(continent)) |>
        group_by(species, continent) |>
        summarise(
          fad_cont = max(date_mc, na.rm = TRUE),
          lad_cont = min(date_mc, na.rm = TRUE),
          .groups = "drop"
        )

      # Join continent to each country row (via species + continent of that country)
      country_with_cont <- df |>
        distinct(species, country, continent) |>
        inner_join(country_stats, by = c("species", "country")) |>
        inner_join(cont_stats, by = c("species", "continent")) |>
        mutate(
          delta_fad = fad - fad_cont,
          delta_lad = lad - lad_cont,
          draw = as.integer(b)
        ) |>
        select(species, country, continent, n_records, delta_fad, delta_lad, draw)

      country_with_cont
    }
  )

  # Summarise across draws
  out <- per_draw |>
    group_by(species, country, continent, n_records) |>
    summarise(
      delta_fad_median = stats::median(delta_fad, na.rm = TRUE),
      delta_fad_lower  = stats::quantile(delta_fad, probs = 0.025, na.rm = TRUE, names = FALSE),
      delta_fad_upper  = stats::quantile(delta_fad, probs = 0.975, na.rm = TRUE, names = FALSE),
      delta_lad_median = stats::median(delta_lad, na.rm = TRUE),
      delta_lad_lower  = stats::quantile(delta_lad, probs = 0.025, na.rm = TRUE, names = FALSE),
      delta_lad_upper  = stats::quantile(delta_lad, probs = 0.975, na.rm = TRUE, names = FALSE),
      .groups = "drop"
    ) |>
    mutate(
      leadlag_status = dplyr::case_when(
        # Use FAD as primary indicator; fall back to LAD if near zero
        abs(delta_fad_median) <= tol & abs(delta_lad_median) <= tol ~ "tie",
        abs(delta_fad_median) <= tol ~ ifelse(
          if (lead_is_negative) delta_lad_median < -tol else delta_lad_median > tol,
          "lead", "lag"
        ),
        TRUE ~ ifelse(
          if (lead_is_negative) delta_fad_median < -tol else delta_fad_median > tol,
          "lead", "lag"
        )
      )
    ) |>
    arrange(species, delta_fad_median, country)

  out
}

#' Rank countries by ΔFAD and ΔLAD per species
#' @param leadlag_tbl Output of fad_lad_leadlag().
#' @return Tibble with ranking columns per spec.
#' @export
fad_lad_rankings <- function(leadlag_tbl) {
  import::from("dplyr", group_by, mutate, ungroup, arrange, select)

  leadlag_tbl |>
    group_by(species) |>
    mutate(
      rank_fad = dplyr::min_rank(delta_fad_median),
      rank_lad = dplyr::min_rank(delta_lad_median)
    ) |>
    ungroup() |>
    arrange(species, rank_fad, rank_lad, country) |>
    select(species, country, continent, delta_fad_median, delta_lad_median,
           leadlag_status, rank_fad, rank_lad)
}

#' Plot a choropleth of ΔFAD/ΔLAD by country for one species
#' @param leadlag_tbl Output of fad_lad_leadlag().
#' @param species Species name to plot.
#' @param metric One of "delta_fad_median" or "delta_lad_median".
#' @return ggplot object.
#' @export
plot_leadlag_choropleth <- function(leadlag_tbl, species, metric = c("delta_fad_median", "delta_lad_median")) {
  import::from("rnaturalearth", ne_countries)
  import::from("countrycode", countrycode)
  import::from("dplyr", filter, mutate, select, left_join)
  import::from("ggplot2", ggplot, aes, geom_sf, scale_fill_gradient2, labs, theme_minimal, guides, guide_colorbar)

  metric <- match.arg(metric)

  world <- ne_countries(scale = "medium", returnclass = "sf") |>
    dplyr::select(iso_a3, name)

  dat <- leadlag_tbl |>
    filter(species == !!species) |>
    mutate(iso_a3 = countrycode(country, origin = "country.name", destination = "iso3c"))

  world2 <- world |>
    left_join(dat |> select(iso_a3, !!metric), by = "iso_a3")

  # Robinson projection
  rob_crs <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"

  ggplot() +
    geom_sf(data = sf::st_transform(world2, crs = rob_crs), aes(fill = .data[[metric]]), color = "grey40", size = 0.1, inherit.aes = FALSE) +
    scale_fill_gradient2(
      name = paste0(ifelse(metric == "delta_fad_median", "Delta FAD", "Delta LAD"), " (Ma, country − continent)"),
      low = "#2166ac", mid = "#f7f7f7", high = "#b2182b", midpoint = 0,
      na.value = "#d9d9d9"
    ) +
    labs(
      title = paste0("Lead–Lag (", ifelse(metric == "delta_fad_median", "Delta FAD", "Delta LAD"), ") for ", species),
      subtitle = "Blue = earlier than continent; Red = later; Grey = missing",
      x = NULL, y = NULL
    ) +
    theme_minimal() +
    guides(fill = guide_colorbar(barwidth = 12))
}

#' Write one PNG per species for ΔFAD (and optionally ΔLAD)
#' @param leadlag_tbl Output of fad_lad_leadlag().
#' @param metric default "delta_fad_median".
#' @return Tibble with species and written file paths.
#' @export
write_leadlag_maps <- function(leadlag_tbl, metric = "delta_fad_median") {
  import::from("purrr", map)
  import::from("dplyr", distinct, pull, tibble)

  species_vec <- leadlag_tbl |>
    distinct(species) |>
    pull(species)

  slug <- function(s) {
    s <- gsub("[^A-Za-z0-9]+", "_", s)
    s <- gsub("_+", "_", s)
    s <- gsub("^_|_$", "", s)
    tolower(s)
  }

  out <- map(species_vec, function(sp) {
    p <- plot_leadlag_choropleth(leadlag_tbl, sp, metric = metric)
    fname <- paste0("leadlag_", metric, "_", slug(sp), ".png")
    path <- write_plot(p, fname, width = 12, height = 7)
    tibble(species = sp, path = path)
  })

  dplyr::bind_rows(out)
}
