# R/reoccupancy.R ---------------------------------------------------------------
#' Estimate site-specific inhomogeneous Poisson rates and gap p-values
#'
#' Functions to:
#'  - define site IDs from lat/lon
#'  - estimate country-level sampling intensity over time bins
#'  - scale to site-specific detection rates lambda_site(t)
#'  - compute gap-specific P(no finds) under inhomogeneous Poisson
#'  - adjust p-values (FDR) and flag likely re-occupancy events
#'
#' Conventions:
#'  - Time is in Ma. Larger values are older.
#'  - Binning is left-closed, right-open: [start, end).
#'

#' Create a stable site identifier by rounded lat/lon (fallback when no clusters)
#' @keywords internal
make_site_id <- function(lat, lon, digits = 3) {
  paste0(
    sprintf("%0.*f", digits, round(lat, digits = digits)), "_",
    sprintf("%0.*f", digits, round(lon, digits = digits))
  )
}

#' Construct contiguous time bins from [t_min, t_max]
#' @keywords internal
time_bins <- function(t_min, t_max, width = 0.1) {

  import::from("tibble", tibble)

  if (any(is.na(c(t_min, t_max))) || t_max <= t_min) {
    return(tibble(time_bin_start = numeric(0), time_bin_end = numeric(0)))
  }
  n <- ceiling((t_max - t_min) / width)
  starts <- t_min + width * (0:(n - 1))
  tibble(time_bin_start = starts, time_bin_end = starts + width)
}

#' Country-level sampling intensity over time bins
#'
#' Intensity proxy S_c(t): counts per bin of occurrences (weighted by
#' `sampling_intensity`) within each country; bins at resolution `width`.
#'
#' @param occ_tbl Occurrence tibble without indeterminates.
#' @param width Bin width in Ma.
#' @return Tibble: country, time_bin_start, time_bin_end, intensity.
#' @export
country_intensity <- function(occ_tbl, width = 0.1) {

  import::from("dplyr", mutate, group_by, summarise, ungroup, filter, across, select)
  import::from("tibble", tibble)
  import::from("tidyr", complete)

  # define global bin grid over observed dates
  t_min <- suppressWarnings(min(occ_tbl$date, na.rm = TRUE))
  t_max <- suppressWarnings(max(occ_tbl$date, na.rm = TRUE))
  bins <- time_bins(t_min, t_max, width)
  if (nrow(bins) == 0) {
    return(tibble(country = character(), time_bin_start = numeric(),
                  time_bin_end = numeric(), intensity = numeric()))
  }

  # assign bin index by start time
  occ_tbl |>
    filter(!is.na(country), !is.na(date), !is.na(sampling_intensity)) |>
    mutate(
      bin_start = floor((date - t_min) / width) * width + t_min,
      bin_start = pmin(bin_start, max(bins$time_bin_start))
    ) |>
    group_by(country, bin_start) |>
    summarise(intensity = sum(sampling_intensity, na.rm = TRUE), .groups = "drop") |>
    # complete grid to include empty bins per country
    group_by(country) |>
    complete(bin_start = bins$time_bin_start, fill = list(intensity = 0)) |>
    ungroup() |>
    mutate(time_bin_start = bin_start, time_bin_end = bin_start + width) |>
    select(country, time_bin_start, time_bin_end, intensity)
}

#' Site-specific lambda(t) scaled from country intensity
#'
#' For each site (rounded lat/lon in its country), compute a scaling
#' factor alpha_site so that sum_t alpha_site * S_c(t) * dt over the site's
#' observed window equals its observed occurrence count. Lambda(t) is then
#' alpha_site * S_c(t) over the site's time window. Where S_c is zero or
#' the site has insufficient data, lambda is set to NA with a comment.
#'
#' @param occ_tbl Occurrence tibble without indeterminates.
#' @param width Bin width in Ma.
#' @return Tibble with columns: site_id, time_bin_start, time_bin_end, lambda, comment.
#' @export
estimate_site_lambda <- function(occ_tbl, width = 0.1) {

  import::from("dplyr", mutate, group_by, summarise, ungroup, left_join,
               select, filter, arrange, n, distinct, inner_join)
  import::from("tibble", tibble)
  import::from("tidyr", crossing)

  if (nrow(occ_tbl) == 0) {
    return(tibble(site_id = character(), time_bin_start = numeric(),
                  time_bin_end = numeric(), lambda = numeric(), comment = character()))
  }

  occ_sites <- occ_tbl |>
    filter(!is.na(lat), !is.na(lon), !is.na(country), !is.na(date))

  # Prefer cluster_id if present; otherwise fall back to rounded lat/lon
  if ("cluster_id" %in% names(occ_sites)) {
    # Use clusters as sites; drop rows with NA cluster_id
    n_na <- sum(is.na(occ_sites$cluster_id))
    if (n_na > 0) {
      warning(sprintf("estimate_site_lambda: dropping %d rows with NA cluster_id", n_na))
    }
    occ_sites <- occ_sites |>
      filter(!is.na(cluster_id)) |>
      mutate(site_id = paste0("c", cluster_id))
  } else {
    occ_sites <- occ_sites |>
      mutate(site_id = make_site_id(lat, lon))
  }

  # country intensity grid
  cint <- country_intensity(occ_sites, width)

  # derive site-specific windows and counts
  site_windows <- occ_sites |>
    group_by(site_id, country) |>
    summarise(t_min = min(date, na.rm = TRUE),
              t_max = max(date, na.rm = TRUE),
              n_occ = dplyr::n(), .groups = "drop")

  # Join site windows to country intensity and restrict to window
  site_lambda <- site_windows |>
    inner_join(cint, by = "country") |>
    filter(time_bin_start >= t_min, time_bin_end <= t_max) |>
    group_by(site_id, country, t_min, t_max, n_occ) |>
    summarise(
      denom = sum(intensity * (time_bin_end - time_bin_start), na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(alpha = ifelse(is.na(denom) | denom <= 0, NA_real_, n_occ / denom))

  # Expand per-bin lambdas
  out <- site_windows |>
    left_join(site_lambda |> select(site_id, alpha), by = "site_id") |>
    inner_join(cint, by = "country") |>
    filter(time_bin_start >= t_min, time_bin_end <= t_max) |>
    mutate(lambda = alpha * intensity,
           comment = ifelse(is.na(alpha), "intensity NA: insufficient data", NA_character_)) |>
    arrange(site_id, time_bin_start) |>
    select(site_id, time_bin_start, time_bin_end, lambda, comment)

  out
}

#' Compute gap-wise P(no finds) per site
#'
#' For consecutive occurrences at a site, compute the gap [t1, t2], integrate
#' lambda(t) over the gap by overlapping bins, and compute p = exp(-expected).
#' Applies BH FDR across gaps with non-NA p-values.
#'
#' @param occ_tbl Occurrence tibble without indeterminates.
#' @param site_lambda_tbl Output of estimate_site_lambda().
#' @return Tibble with columns: site_id, gap_start, gap_end, delta, p_value, p_value_fdr, comment.
#' @export
compute_reocc_pvals <- function(occ_tbl, site_lambda_tbl) {

  import::from("dplyr", mutate, group_by, summarise, ungroup, arrange, select,
               filter, left_join, row_number, bind_rows)
  import::from("tibble", tibble)
  import::from("purrr", map_dfr)

  if (nrow(occ_tbl) == 0) {
    return(tibble(site_id = character(), gap_start = numeric(), gap_end = numeric(),
                  delta = numeric(), p_value = numeric(), p_value_fdr = numeric(),
                  comment = character()))
  }

  occ_sites <- occ_tbl |>
    filter(!is.na(lat), !is.na(lon), !is.na(date))

  if ("cluster_id" %in% names(occ_sites)) {
    n_na <- sum(is.na(occ_sites$cluster_id))
    if (n_na > 0) {
      warning(sprintf("compute_reocc_pvals: dropping %d rows with NA cluster_id", n_na))
    }
    occ_sites <- occ_sites |>
      filter(!is.na(cluster_id)) |>
      mutate(site_id = paste0("c", cluster_id))
  } else {
    occ_sites <- occ_sites |>
      mutate(site_id = make_site_id(lat, lon))
  }

  occ_sites <- occ_sites |>
    select(site_id, date)

  # gaps per site
  gaps <- occ_sites |>
    arrange(site_id, date) |>
    group_by(site_id) |>
    summarise(
      dates = list(sort(unique(date))),
      .groups = "drop"
    ) |>
    mutate(n = lengths(dates)) |>
    mutate(gaps = purrr::map(dates, ~{
      d <- .x
      if (length(d) < 2) return(tibble(gap_start = NA_real_, gap_end = NA_real_,
                                       delta = NA_real_, comment = "insufficient data: <2 occurrences"))
      tibble(
        gap_start = head(d, -1),
        gap_end   = tail(d, -1),
        delta     = pmax(gap_end, gap_start) - pmin(gap_end, gap_start),
        comment   = NA_character_
      )
    })) |>
    select(site_id, gaps) |>
    tidyr::unnest(gaps)

  if (nrow(gaps) == 0) {
    return(tibble(site_id = character(), gap_start = numeric(), gap_end = numeric(),
                  delta = numeric(), p_value = numeric(), p_value_fdr = numeric(),
                  comment = character()))
  }

  # function to integrate lambda over [a,b]
  integrate_lambda <- function(site_id, a, b, lam_tbl) {
    sl <- lam_tbl |> filter(site_id == !!site_id)
    if (nrow(sl) == 0) return(list(mu = NA_real_, note = "no lambda for site"))
    # overlap length per bin
    ovl <- pmax(0, pmin(sl$time_bin_end, max(a, b)) - pmax(sl$time_bin_start, min(a, b)))
    mu <- sum(sl$lambda * ovl, na.rm = TRUE)
    if (!is.finite(mu) || mu <= 0) return(list(mu = NA_real_, note = "lambda NA or zero over gap"))
    list(mu = mu, note = NA_character_)
  }

  res <- gaps |>
    mutate(tmp = purrr::pmap(list(site_id, gap_start, gap_end), integrate_lambda, lam_tbl = site_lambda_tbl)) |>
    mutate(mu = purrr::map_dbl(tmp, "mu"), note = purrr::map_chr(tmp, "note")) |>
    select(-tmp) |>
    mutate(p_value = ifelse(is.na(mu), NA_real_, exp(-mu))) |>
    mutate(comment = dplyr::coalesce(comment, note)) |>
    arrange(p_value)

  # FDR across non-NA p-values
  valid <- which(!is.na(res$p_value))
  p_adj <- rep(NA_real_, nrow(res))
  if (length(valid) > 0) {
    p_adj[valid] <- p.adjust(res$p_value[valid], method = "BH")
  }
  res$p_value_fdr <- p_adj

  res |> select(site_id, gap_start, gap_end, delta, p_value, p_value_fdr, comment)
}

#' Subset of likely re-occupancy events (FDR < 0.05)
#' @export
likely_reoccupancy <- function(reocc_tbl) {
  import::from("dplyr", filter, arrange)
  reocc_tbl |>
    filter(!is.na(p_value_fdr), p_value_fdr < 0.05) |>
    arrange(p_value_fdr, p_value)
}

#' Plot p-values vs gap duration, highlighting FDR-significant gaps
#' @export
plot_reocc_pvals <- function(reocc_tbl) {
  import::from("ggplot2", ggplot, aes, geom_point, scale_y_log10, labs, theme_minimal)
  import::from("dplyr", mutate)

  reocc_tbl |>
    mutate(sig = !is.na(p_value_fdr) & p_value_fdr < 0.05) |>
    ggplot(aes(x = delta, y = p_value, color = sig)) +
    geom_point(alpha = 0.7) +
    scale_y_log10() +
    labs(x = "Gap duration (Ma)", y = "P(no finds)", color = "FDR < 0.05",
         title = "Gap-wise P(no finds) vs duration") +
    theme_minimal()
}
