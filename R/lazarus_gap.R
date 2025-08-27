# R/lazarus_gap.R --------------------------------------------------------------
# Lazarus gaps inside species ranges relative to time-varying effort

#' Construct effort time series lambda(t)
#'
#' Piecewise-constant intensity per bin using summed `sampling_intensity`.
#' Stratify by one of: "global" (single series), "country", or "cluster".
#'
#' @param occ_tbl Occurrence tibble (preferably after `filter_indet()`).
#' @param bin_width Bin width in Ma (default 0.25).
#' @param by One of "global", "country", or "cluster".
#' @return Tibble with columns: effort_stratum, time_bin_start, time_bin_end, lambda.
#' @export
effort_time_series <- function(occ_tbl, bin_width = 0.25, by = c("global", "country", "cluster")) {

  import::from("dplyr", mutate, group_by, summarise, ungroup, filter, select)
  import::from("tibble", tibble)
  import::from("tidyr", complete)
  import::from("rlang", abort)

  by <- match.arg(by)

  if (nrow(occ_tbl) == 0) {
    return(tibble(effort_stratum = character(), time_bin_start = numeric(),
                  time_bin_end = numeric(), lambda = numeric()))
  }

  # guard required columns
  req <- c("date", "sampling_intensity")
  miss <- setdiff(req, names(occ_tbl))
  if (length(miss) > 0) {
    abort(paste0("effort_time_series: missing columns: ", paste(miss, collapse = ", ")),
          class = "lazarus_missing_cols")
  }

  # define global bin grid
  t_min <- suppressWarnings(min(occ_tbl$date, na.rm = TRUE))
  t_max <- suppressWarnings(max(occ_tbl$date, na.rm = TRUE))
  if (!is.finite(t_min) || !is.finite(t_max) || t_max <= t_min) {
    return(tibble(effort_stratum = character(), time_bin_start = numeric(),
                  time_bin_end = numeric(), lambda = numeric()))
  }

  n <- ceiling((t_max - t_min) / bin_width)
  starts <- t_min + bin_width * (0:(n - 1))
  bins <- tibble(time_bin_start = starts, time_bin_end = starts + bin_width)

  # choose stratum column name and values
  if (identical(by, "global")) {
    occ2 <- occ_tbl |>
      mutate(effort_stratum = "global")
  } else if (identical(by, "country")) {
    if (!("country" %in% names(occ_tbl))) {
      abort("effort_time_series(by='country'): 'country' column missing", class = "lazarus_missing_cols")
    }
    occ2 <- occ_tbl |>
      filter(!is.na(country)) |>
      mutate(effort_stratum = as.character(country))
  } else { # cluster
    if (!("cluster_id" %in% names(occ_tbl))) {
      abort("effort_time_series(by='cluster'): 'cluster_id' column missing", class = "lazarus_missing_cols")
    }
    occ2 <- occ_tbl |>
      filter(!is.na(cluster_id)) |>
      mutate(effort_stratum = paste0("c", as.character(cluster_id)))
  }

  occ2 |>
    filter(!is.na(date), !is.na(sampling_intensity)) |>
    mutate(
      bin_start = floor((date - t_min) / bin_width) * bin_width + t_min,
      bin_start = pmin(bin_start, max(bins$time_bin_start))
    ) |>
    group_by(effort_stratum, bin_start) |>
    summarise(lambda = sum(sampling_intensity, na.rm = TRUE), .groups = "drop") |>
    group_by(effort_stratum) |>
    complete(bin_start = bins$time_bin_start, fill = list(lambda = 0)) |>
    ungroup() |>
    mutate(time_bin_start = bin_start, time_bin_end = bin_start + bin_width) |>
    select(effort_stratum, time_bin_start, time_bin_end, lambda)
}

#' Calibrate species-level alpha-hat by method of moments
#'
#' Choose alpha so that expected detections across [min(date), max(date)] equal
#' the observed count for the (species×stratum) table.
#'
#' @param spec_tbl Occurrence tibble for one species (optionally filtered to one stratum).
#' @param effort_ts Effort time series (single stratum) as returned by `effort_time_series()`.
#' @return Numeric alpha_hat (non-negative); NA if denominator <= 0.
#' @export
calibrate_alpha <- function(spec_tbl, effort_ts) {

  import::from("dplyr", filter)

  if (nrow(spec_tbl) == 0 || nrow(effort_ts) == 0) return(NA_real_)

  a <- suppressWarnings(min(spec_tbl$date, na.rm = TRUE))
  b <- suppressWarnings(max(spec_tbl$date, na.rm = TRUE))
  if (!is.finite(a) || !is.finite(b) || b <= a) return(NA_real_)

  # overlap length with [a,b] per bin
  ovl <- pmax(0, pmin(effort_ts$time_bin_end, b) - pmax(effort_ts$time_bin_start, a))
  denom <- sum(effort_ts$lambda * ovl, na.rm = TRUE)
  n_obs <- length(unique(spec_tbl$date[!is.na(spec_tbl$date)]))

  if (!is.finite(denom) || denom <= 0) return(NA_real_)
  max(0, n_obs / denom)
}

#' Compute Lazarus gaps for one species against an effort series
#'
#' For consecutive events within the species range, compute the interior gaps
#' and the probability of zero detections given the integrated effort and alpha.
#'
#' @param spec_tbl Occurrence tibble for one species (optionally filtered to one stratum).
#' @param effort_ts Effort time series (single stratum) as returned by `effort_time_series()`.
#' @return Tibble with required fields per spec (one row per interior gap),
#'         or a single NA row if no interior gaps.
#' @export
lazarus_gaps <- function(spec_tbl, effort_ts) {

  import::from("dplyr", arrange, mutate, tibble, select)
  import::from("rlang", abort)

  species <- unique(spec_tbl$species)
  species <- if (length(species) == 1) species else as.character(species)[1]
  eff_str <- unique(effort_ts$effort_stratum)
  eff_str <- if (length(eff_str) == 1) eff_str else as.character(eff_str)[1]

  # Sort unique event dates (older to younger = decreasing Ma)
  dates <- sort(unique(spec_tbl$date[!is.na(spec_tbl$date)]), decreasing = TRUE)

  low_power <- length(dates) < 3
  if (length(dates) < 2) {
    return(dplyr::tibble(
      species = species,
      gap_start_Ma = NA_real_, gap_end_Ma = NA_real_, gap_Ma = NA_real_,
      effort_stratum = eff_str, lambda_sum = NA_real_, alpha_hat = NA_real_,
      Lambda_gap = NA_real_, p_no_detect = NA_real_, p_adj = NA_real_,
      flag_improbable = NA, low_power = TRUE
    ))
  }

  # helper: integrate lambda over [x,y]
  integrate_gap <- function(x, y, ts) {
    ovl <- pmax(0, pmin(ts$time_bin_end, max(x, y)) - pmax(ts$time_bin_start, min(x, y)))
    sum(ts$lambda * ovl, na.rm = TRUE)
  }

  # alpha-hat for this species×stratum
  alpha <- calibrate_alpha(spec_tbl, effort_ts)

  # build gaps between consecutive dates
  gaps <- dplyr::tibble(
    gap_start_Ma = dates[-length(dates)],
    gap_end_Ma   = dates[-1]
  )
  gaps$gap_Ma <- gaps$gap_start_Ma - gaps$gap_end_Ma

  # integrate effort over each gap
  lam_sum <- vapply(seq_len(nrow(gaps)), function(i) integrate_gap(gaps$gap_start_Ma[i], gaps$gap_end_Ma[i], effort_ts), numeric(1))

  if (any(!is.finite(lam_sum))) {
    abort("Non-finite lambda_sum encountered", class = "effort_integral_invalid")
  }

  # If any gap has zero effort, abort with classed error
  if (any(lam_sum <= 0)) {
    abort(sprintf("effort zero over at least one gap for species %s (stratum %s)", species, eff_str),
          class = "effort_zero_near_gap")
  }

  Lambda <- alpha * lam_sum
  p0     <- exp(-Lambda)

  out <- dplyr::tibble(
    species = species,
    gap_start_Ma = gaps$gap_start_Ma,
    gap_end_Ma   = gaps$gap_end_Ma,
    gap_Ma       = gaps$gap_Ma,
    effort_stratum = eff_str,
    lambda_sum   = lam_sum,
    alpha_hat    = alpha,
    Lambda_gap   = Lambda,
    p_no_detect  = p0
  )

  # BH within species
  valid <- which(!is.na(out$p_no_detect))
  p_adj <- rep(NA_real_, nrow(out))
  if (length(valid) > 0) p_adj[valid] <- stats::p.adjust(out$p_no_detect[valid], method = "BH")
  out$p_adj <- p_adj
  out$flag_improbable <- !is.na(out$p_adj) & out$p_adj < 0.05
  out$low_power <- low_power

  out
}

#' Compute Lazarus gaps for all species (and optional strata)
#' @param occ_tbl Occurrence tibble after `filter_indet()`.
#' @param bin_width Bin width for the effort time series (Ma).
#' @param by One of "global", "country", or "cluster".
#' @return Tibble concatenating `lazarus_gaps()` over species×stratum.
#' @export
lazarus_all <- function(occ_tbl, bin_width = 0.25, by = c("global", "country", "cluster")) {

  import::from("dplyr", distinct, filter, group_by, summarise, ungroup, arrange, mutate)
  import::from("purrr", map_dfr)

  by <- match.arg(by)

  if (nrow(occ_tbl) == 0) return(dplyr::tibble())

  # Build effort series
  eff <- effort_time_series(occ_tbl, bin_width = bin_width, by = by)
  if (nrow(eff) == 0) return(dplyr::tibble())

  # Determine strata to iterate
  strata <- unique(eff$effort_stratum)

  # Prepare grouping of occurrences by species (and stratum when applicable)
  occ2 <- occ_tbl
  if (identical(by, "country")) {
    occ2 <- occ_tbl |>
      filter(!is.na(country)) |>
      mutate(effort_stratum = as.character(country))
  } else if (identical(by, "cluster") && ("cluster_id" %in% names(occ_tbl))) {
    occ2 <- occ_tbl |>
      filter(!is.na(cluster_id)) |>
      mutate(effort_stratum = paste0("c", as.character(cluster_id)))
  } else {
    occ2 <- occ_tbl |>
      mutate(effort_stratum = "global")
  }

  # Iterate species × stratum present in the data
  combos <- occ2 |>
    distinct(species, effort_stratum)

  map_dfr(seq_len(nrow(combos)), function(i) {
    sp <- combos$species[i]
    es <- combos$effort_stratum[i]
    sp_tbl <- occ2 |>
      filter(species == !!sp, effort_stratum == !!es)
    ts_es <- eff |>
      filter(effort_stratum == !!es)
    lazarus_gaps(sp_tbl, ts_es)
  }) |>
    arrange(species, dplyr::desc(gap_Ma))
}
