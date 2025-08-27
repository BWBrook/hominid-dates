# R/indet_glm.R ---------------------------------------------------------------
# Identifiability bias: model Pr(indet) over time, region, and effort

#' Build analysis frame by time bin × country
#'
#' For each bin (width in Ma) × country compute:
#'  - n_indet: count where species == "indet"
#'  - n_total: total count (including indet)
#'  - effort:  sum(sampling_intensity)
#'  - time_mid: midpoint of bin
#'
#' @param occ_tbl Occurrence tibble (after collapse; includes indeterminates).
#' @param width Bin width in Ma (default 0.25).
#' @return Tibble with columns: country, time_bin_lo, time_bin_hi, time_mid, n_indet, n_total, effort.
#' @export
build_indet_frame <- function(occ_tbl, width = 0.25) {

  import::from("dplyr", filter, mutate, group_by, summarise, ungroup, select)
  import::from("tibble", tibble)
  import::from("tidyr", complete, crossing)

  if (nrow(occ_tbl) == 0) {
    return(tibble(country = character(), time_bin_lo = numeric(), time_bin_hi = numeric(),
                  time_mid = numeric(), n_indet = integer(), n_total = integer(), effort = numeric()))
  }

  # Guard required columns
  req <- c("date", "country", "species", "sampling_intensity")
  miss <- setdiff(req, names(occ_tbl))
  if (length(miss) > 0) {
    rlang::abort(paste0("build_indet_frame: missing columns: ", paste(miss, collapse = ", ")),
                 class = "indet_missing_cols")
  }

  # Time grid
  t_min <- suppressWarnings(min(occ_tbl$date, na.rm = TRUE))
  t_max <- suppressWarnings(max(occ_tbl$date, na.rm = TRUE))
  if (!is.finite(t_min) || !is.finite(t_max) || t_max <= t_min) {
    return(tibble(country = character(), time_bin_lo = numeric(), time_bin_hi = numeric(),
                  time_mid = numeric(), n_indet = integer(), n_total = integer(), effort = numeric()))
  }

  n <- ceiling((t_max - t_min) / width)
  starts <- t_min + width * (0:(n - 1))
  bins <- tibble(time_bin_lo = starts, time_bin_hi = starts + width, time_mid = starts + width/2)

  # Countries present in data
  countries <- sort(unique(occ_tbl$country[!is.na(occ_tbl$country)]))
  if (length(countries) == 0) {
    return(tibble(country = character(), time_bin_lo = numeric(), time_bin_hi = numeric(),
                  time_mid = numeric(), n_indet = integer(), n_total = integer(), effort = numeric()))
  }

  # Assign bin by start time and aggregate
  agg <- occ_tbl |>
    filter(!is.na(country), !is.na(date)) |>
    mutate(
      bin_start = floor((date - t_min) / width) * width + t_min,
      bin_start = pmin(bin_start, max(bins$time_bin_lo)),
      is_indet = species == "indet"
    ) |>
    group_by(country, bin_start) |>
    summarise(
      n_indet = sum(is_indet, na.rm = TRUE),
      n_total = dplyr::n(),
      effort  = sum(sampling_intensity, na.rm = TRUE),
      .groups = "drop"
    ) |>
    group_by(country) |>
    complete(bin_start = bins$time_bin_lo, fill = list(n_indet = 0L, n_total = 0L, effort = 0)) |>
    ungroup() |>
    mutate(
      time_bin_lo = bin_start,
      time_bin_hi = bin_start + width
    ) |>
    select(country, time_bin_lo, time_bin_hi, n_indet, n_total, effort) |>
    dplyr::left_join(bins, by = c("time_bin_lo", "time_bin_hi")) |>
    select(country, time_bin_lo, time_bin_hi, time_mid, n_indet, n_total, effort)

  agg
}

#' Fit binomial GLM: Pr(indet) ~ ns(time_mid, df=3) + country + log1p(effort)
#'
#' Drops rows with n_total == 0 (emits a warning); ensures predictions in [0,1].
#'
#' @param indet_data Output of build_indet_frame().
#' @return A list with elements: model (glm object), tidy (broom::tidy tibble).
#' @export
fit_indet_glm <- function(indet_data) {

  import::from("splines", ns)
  import::from("stats", glm, binomial)
  import::from("broom", tidy)

  dat <- indet_data
  if (nrow(dat) == 0) {
    rlang::abort("fit_indet_glm: empty data", class = "indet_no_data")
  }

  # Drop empty bins with a warning
  idx_empty <- which(dat$n_total <= 0 | is.na(dat$n_total))
  if (length(idx_empty) > 0) {
    cli::cli_warn(sprintf("fit_indet_glm: dropping %d bins with n_total == 0", length(idx_empty)))
    dat <- dat[-idx_empty, , drop = FALSE]
  }
  if (nrow(dat) == 0) {
    rlang::abort("fit_indet_glm: no non-empty bins remain", class = "indet_no_data")
  }

  # Ensure factors
  dat$country <- factor(dat$country)

  # Handle single-level country by dropping the term to avoid contrasts error
  has_country_effect <- nlevels(dat$country) >= 2
  form <- if (has_country_effect) {
    stats::as.formula(cbind(n_indet, pmax(0, n_total - n_indet)) ~ splines::ns(time_mid, df = 3) + country + log1p(effort))
  } else {
    stats::as.formula(cbind(n_indet, pmax(0, n_total - n_indet)) ~ splines::ns(time_mid, df = 3) + log1p(effort))
  }

  fit <- glm(
    form,
    data = dat,
    family = binomial()
  )

  coefs <- tidy(fit)
  list(model = fit, tidy = coefs)
}

#' Predict per-country time path with 90% CI
#'
#' Uses median effort per country for partial dependence on time.
#'
#' @param indet_data Output of build_indet_frame().
#' @param fit Fit object from fit_indet_glm() (list with $model or glm).
#' @return Tibble: country, time_mid, p_hat, p_lo, p_hi.
#' @export
predict_indet <- function(indet_data, fit) {

  import::from("dplyr", group_by, summarise, ungroup, arrange, mutate, distinct)
  import::from("tibble", tibble)

  if (is.list(fit) && !inherits(fit, "glm")) {
    if (!is.null(fit$model)) fit <- fit$model
  }
  stopifnot(inherits(fit, "glm"))

  if (nrow(indet_data) == 0) return(tibble(country = character(), time_mid = numeric(), p_hat = numeric(), p_lo = numeric(), p_hi = numeric()))

  # Time grid per country over observed span; step at median bin width
  widths <- with(indet_data, time_bin_hi - time_bin_lo)
  step <- stats::median(widths[is.finite(widths)], na.rm = TRUE)
  cntry_eff <- indet_data |>
    group_by(country) |>
    summarise(effort_med = stats::median(effort, na.rm = TRUE),
              tmin = min(time_bin_lo, na.rm = TRUE), tmax = max(time_bin_hi, na.rm = TRUE), .groups = "drop")

  make_grid <- function(cn, ef, tmin, tmax) {
    if (!is.finite(tmin) || !is.finite(tmax) || tmax <= tmin) return(tibble(country = character(), time_mid = numeric(), effort = numeric()))
    mids <- seq(tmin + step/2, tmax - step/2, by = step)
    tibble(country = cn, time_mid = mids, effort = ef)
  }
  grids <- purrr::pmap_dfr(list(cntry_eff$country, cntry_eff$effort_med, cntry_eff$tmin, cntry_eff$tmax), make_grid)

  # Predict on link scale
  pr <- stats::predict(fit, newdata = grids, type = "link", se.fit = TRUE)
  eta  <- as.numeric(pr$fit)
  se   <- as.numeric(pr$se.fit)
  z    <- 1.64485362695147  # ~90% CI
  lo   <- eta - z * se
  hi   <- eta + z * se
  tibble(
    country = grids$country,
    time_mid = grids$time_mid,
    p_hat = stats::plogis(eta),
    p_lo  = stats::plogis(lo),
    p_hi  = stats::plogis(hi)
  ) |>
    arrange(country, dplyr::desc(time_mid))
}
