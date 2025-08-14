# R/bin_counts_mc.R ----------------------------------------------------------
#' Species richness per time bin across MC draws
#'
#' For each draw, compute species spans and mark presence in bins defined on
#' the Ma axis. Summarise across draws to median and 90% CI per bin.
#'
#' @param mc_draws List of tibbles output by `mc_dates()`.
#' @param bin_width Width of time bins in Ma.
#' @return Tibble with bin_lo, bin_hi, bin_mid, n_med, n_lo, n_hi.
#' @export
bin_counts_from_draws <- function(mc_draws, bin_width = 0.1) {

  import::from("dplyr", group_by, summarise, ungroup, mutate, arrange, bind_rows, transmute, n_distinct)
  import::from("purrr", map_dfr)
  import::from("tibble", tibble)
  import::from("tidyr", crossing)

  # Derive overall binning range from the first draw's bounds
  bounds <- mc_draws[[1]] |>
    summarise(min_l = min(lower, na.rm = TRUE), max_u = max(upper, na.rm = TRUE))
  start <- floor(bounds$min_l)
  end   <- ceiling(bounds$max_u)
  breaks <- seq(start, end, by = bin_width)
  if (tail(breaks, 1) < end) breaks <- c(breaks, end)

  bins <- tibble(bin_lo = head(breaks, -1), bin_hi = tail(breaks, -1)) |>
    mutate(bin_mid = (bin_lo + bin_hi) / 2)

  per_draw <- map_dfr(seq_along(mc_draws), function(i) {
    df <- mc_draws[[i]]
    spans <- df |>
      group_by(species) |>
      summarise(first_occ = max(date_mc, na.rm = TRUE),
                last_occ  = min(date_mc, na.rm = TRUE), .groups = "drop")

    # Presence if bin interval intersects [last_occ, first_occ]
    crossing(spans, bins) |>
      mutate(present = (bin_hi > last_occ) & (bin_lo < first_occ)) |>
      group_by(bin_lo, bin_hi, bin_mid) |>
      summarise(n_species = n_distinct(species[present]), .groups = "drop") |>
      mutate(draw = i)
  })

  per_draw |>
    group_by(bin_lo, bin_hi, bin_mid) |>
    summarise(
      n_med = stats::median(n_species, na.rm = TRUE),
      n_lo  = stats::quantile(n_species, probs = 0.05, na.rm = TRUE, names = FALSE),
      n_hi  = stats::quantile(n_species, probs = 0.95, na.rm = TRUE, names = FALSE),
      .groups = "drop"
    ) |>
    dplyr::arrange(dplyr::desc(bin_mid))
}

#' Sensitivity of bin counts to sampling distribution
#' @param tbl Occurrence tibble
#' @param B   Number of draws per distribution
#' @param bin_width Width of time bins in Ma
#' @return Long tibble with dist, bin_mid and summary stats
#' @export
bin_sensitivity <- function(tbl, B = 1000, bin_width = 0.1) {
  import::from("dplyr", bind_rows, mutate)
  import::from("tibble", tibble)

  dists <- c("uniform", "triangular", "beta")
  res <- lapply(dists, function(d) {
    mc <- mc_dates(tbl, B = B, dist = d)
    out <- bin_counts_from_draws(mc, bin_width = bin_width)
    mutate(out, dist = d)
  })
  bind_rows(res)
}
