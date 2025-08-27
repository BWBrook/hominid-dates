# R/bin_sensitivity.R ----------------------------------------------------------
# Bin-size sensitivity audit for time-sliced metrics

#' Compute per-bin metrics for a given width
#'
#' Metrics:
#'  - N: per-bin occurrence count (rows in occ_tbl)
#'  - S: per-bin species richness (unique species)
#'  - lambda: effort proxy = sum(sampling_intensity)
#'
#' @param occ_tbl Occurrence tibble (preferably after `filter_indet()`).
#' @param width Bin width in Ma.
#' @return Tibble: width, time_bin_lo, time_bin_hi, N, S, lambda, lambda_z.
#' @export
binned_metrics <- function(occ_tbl, width) {

  import::from("dplyr", filter, mutate, group_by, summarise, ungroup, n_distinct, select)
  import::from("tibble", tibble)
  import::from("tidyr", complete)

  if (nrow(occ_tbl) == 0) {
    return(tibble(width = numeric(), time_bin_lo = numeric(), time_bin_hi = numeric(),
                  N = numeric(), S = numeric(), lambda = numeric(), lambda_z = numeric()))
  }

  # Guard required columns
  req <- c("date", "species", "sampling_intensity")
  miss <- setdiff(req, names(occ_tbl))
  if (length(miss) > 0) {
    rlang::abort(paste0("binned_metrics: missing columns: ", paste(miss, collapse = ", ")),
                 class = "bin_sense_missing_cols")
  }

  # Global grid over observed dates
  t_min <- suppressWarnings(min(occ_tbl$date, na.rm = TRUE))
  t_max <- suppressWarnings(max(occ_tbl$date, na.rm = TRUE))
  if (!is.finite(t_min) || !is.finite(t_max) || t_max <= t_min) {
    return(tibble(width = numeric(), time_bin_lo = numeric(), time_bin_hi = numeric(),
                  N = numeric(), S = numeric(), lambda = numeric(), lambda_z = numeric()))
  }
  n <- ceiling((t_max - t_min) / width)
  starts <- t_min + width * (0:(n - 1))
  bins <- tibble(time_bin_lo = starts, time_bin_hi = starts + width)

  # Assign bin by start time
  out <- occ_tbl |>
    filter(!is.na(date)) |>
    mutate(
      bin_start = floor((date - t_min) / width) * width + t_min,
      bin_start = pmin(bin_start, max(bins$time_bin_lo))
    ) |>
    group_by(bin_start) |>
    summarise(
      N = dplyr::n(),
      S = n_distinct(species),
      lambda = sum(sampling_intensity, na.rm = TRUE),
      .groups = "drop"
    ) |>
    # complete to include empty bins
    complete(bin_start = bins$time_bin_lo, fill = list(N = 0, S = 0, lambda = 0)) |>
    mutate(
      width = width,
      time_bin_lo = bin_start,
      time_bin_hi = bin_start + width
    ) |>
    select(width, time_bin_lo, time_bin_hi, N, S, lambda)

  # Z-score for lambda within this width
  z <- function(x) {
    mu <- mean(x, na.rm = TRUE)
    sdv <- stats::sd(x, na.rm = TRUE)
    if (!is.finite(sdv) || sdv == 0) return(rep(0, length(x)))
    (x - mu) / sdv
  }
  out$lambda_z <- z(out$lambda)

  out
}

#' Bin-size sensitivity across widths
#'
#' Computes per-bin metrics for widths and pairwise stability vs 0.1 Ma.
#'
#' @param occ_tbl Occurrence tibble (preferably after `filter_indet()`).
#' @param widths Numeric vector of widths (Ma). Defaults to c(0.1, 0.25, 0.5).
#' @param ... Ignored; absorbs legacy args to maintain compatibility.
#' @return List with:
#'   - bin_sense_tbl: concatenated per-bin metrics by width
#'   - bin_sense_summary: pairwise Spearman rho and RMSD of z-scores
#' @export
bin_sensitivity <- function(occ_tbl, widths = c(0.1, 0.25, 0.5), ...) {

  import::from("dplyr", bind_rows, filter, mutate, arrange, left_join, select)
  import::from("purrr", map, map_dfr)
  import::from("tibble", tibble)

  widths <- sort(unique(widths))

  bins_list <- map(widths, ~ binned_metrics(occ_tbl, .x))
  names(bins_list) <- as.character(widths)
  bins_all <- bind_rows(bins_list)

  # Helper: resample a (const-per-bin) time series to target bins via overlap weighting
  resample_to <- function(src, to_bins, value_col) {
    v <- src[[value_col]]
    a1 <- src$time_bin_lo; b1 <- src$time_bin_hi
    a2 <- to_bins$time_bin_lo; b2 <- to_bins$time_bin_hi
    out <- numeric(length(a2))
    for (i in seq_along(a2)) {
      ovl <- pmax(0, pmin(b1, b2[i]) - pmax(a1, a2[i]))
      if (sum(ovl) <= 0) {
        out[i] <- NA_real_
      } else {
        out[i] <- sum(v * (ovl / (b2[i] - a2[i])), na.rm = TRUE)
      }
    }
    out
  }

  # Pairwise vs 0.1 Ma on the 0.1 grid (if present)
  w_ref <- 0.1
  if (!(as.character(w_ref) %in% names(bins_list))) {
    # pick the finest available as reference
    w_ref <- min(widths)
  }
  ref <- bins_list[[as.character(w_ref)]]

  # Function to compute summary stats for one comparison width
  one_comp <- function(wc) {
    comp <- bins_list[[as.character(wc)]]
    # overlapping window
    lo <- max(min(ref$time_bin_lo, na.rm = TRUE), min(comp$time_bin_lo, na.rm = TRUE))
    hi <- min(max(ref$time_bin_hi, na.rm = TRUE), max(comp$time_bin_hi, na.rm = TRUE))
    ref2 <- filter(ref, time_bin_lo >= lo, time_bin_hi <= hi)
    comp_resamp <- comp
    # Metrics to compare
    metrics <- c("N", "S", "lambda_z")
    out <- lapply(metrics, function(met) {
      # resample comparator to ref bins
      x_ref <- ref2[[met]]
      x_cmp <- resample_to(comp, ref2[, c("time_bin_lo", "time_bin_hi")], value_col = met)
      ok <- which(is.finite(x_ref) & is.finite(x_cmp))
      rho <- if (length(ok) >= 3) suppressWarnings(stats::cor(x_ref[ok], x_cmp[ok], method = "spearman")) else NA_real_
      # RMSD of z-scores per width
      z <- function(x) {
        mu <- mean(x, na.rm = TRUE)
        sdv <- stats::sd(x, na.rm = TRUE)
        if (!is.finite(sdv) || sdv == 0) return(rep(0, length(x)))
        (x - mu) / sdv
      }
      rmsd <- if (length(ok) >= 1) {
        zx <- z(x_ref[ok]); zy <- z(x_cmp[ok])
        sqrt(mean((zx - zy)^2))
      } else NA_real_
      tibble(width_ref = w_ref, width_comp = wc, metric = met, spearman_rho = as.numeric(rho), rmsd_z = as.numeric(rmsd))
    })
    bind_rows(out)
  }

  comps <- setdiff(widths, w_ref)
  summary_tbl <- map_dfr(comps, one_comp)

  list(bin_sense_tbl = bins_all, bin_sense_summary = summary_tbl)
}

