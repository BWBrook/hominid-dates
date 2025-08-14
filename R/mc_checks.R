# R/mc_checks.R -------------------------------------------------------------
#' CI width convergence with number of draws B
#' @param tbl Occurrence tibble
#' @param B_grid Integer vector of draw counts to evaluate
#' @param dist Sampling distribution (passed to mc_dates)
#' @return Tibble: species, B, dur_ci_width
#' @export
mc_ci_convergence <- function(tbl, B_grid = c(200, 500, 1000, 2000), dist = "uniform") {

  import::from("dplyr", bind_rows, mutate, transmute)

  res <- lapply(B_grid, function(B) {
    mc   <- mc_dates(tbl, B = B, dist = dist)
    span <- spans_from_draws(mc)
    span |>
      transmute(species, B = !!B, dur_ci_width = dur_hi - dur_lo)
  })
  bind_rows(res)
}

