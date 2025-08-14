# R/spans_from_draws.R -------------------------------------------------------
#' Summarise species spans across MC draws
#'
#' @param mc_draws List of tibbles output by `mc_dates()`.
#' @return Tibble with per-species medians and 90% CIs for first/last/duration.
#' @export
spans_from_draws <- function(mc_draws) {

  import::from("dplyr", bind_rows, group_by, summarise, mutate, ungroup, across, everything)
  import::from("purrr", imap_dfr)
  import::from("tibble", tibble)

  # Compute spans per draw ---------------------------------------------------
  per_draw <- imap_dfr(
    mc_draws,
    ~ {
      df <- .x
      b  <- .y
      df |>
        group_by(species) |>
        summarise(
          first_occ = max(date_mc, na.rm = TRUE),
          last_occ  = min(date_mc, na.rm = TRUE),
          .groups = "drop"
        ) |>
        mutate(duration = pmax(0, first_occ - last_occ), draw = as.integer(b))
    }
  )

  # Summarise across draws ---------------------------------------------------
  per_draw |>
    group_by(species) |>
    summarise(
      first_med = stats::median(first_occ, na.rm = TRUE),
      first_lo  = stats::quantile(first_occ, probs = 0.05, na.rm = TRUE, names = FALSE),
      first_hi  = stats::quantile(first_occ, probs = 0.95, na.rm = TRUE, names = FALSE),
      last_med  = stats::median(last_occ,  na.rm = TRUE),
      last_lo   = stats::quantile(last_occ,  probs = 0.05, na.rm = TRUE, names = FALSE),
      last_hi   = stats::quantile(last_occ,  probs = 0.95, na.rm = TRUE, names = FALSE),
      dur_med   = stats::median(duration, na.rm = TRUE),
      dur_lo    = stats::quantile(duration, probs = 0.05, na.rm = TRUE, names = FALSE),
      dur_hi    = stats::quantile(duration, probs = 0.95, na.rm = TRUE, names = FALSE),
      .groups = "drop"
    ) |>
    dplyr::arrange(dplyr::desc(first_med))
}
