testthat::test_that("binning metrics shape and width effects", {
  import::from("tibble", tibble)

  # Synthetic series: increasing occurrences per 0.1 Ma bin (avoids constant series degeneracy)
  dates <- seq(0.05, 0.95, by = 0.1)
  counts <- seq_along(dates)  # 1,2,...
  occ <- tibble(
    species = "s1",
    date = rep(dates, times = counts),
    sampling_intensity = 1
  )

  m025 <- binned_metrics(occ, width = 0.25)
  m050 <- binned_metrics(occ, width = 0.5)
  # Doubling width halves number of bins for this aligned grid
  testthat::expect_equal(nrow(m050) * 2, nrow(m025))

  # Full sensitivity results
  res <- bin_sensitivity(occ, widths = c(0.1, 0.5))
  testthat::expect_true(all(c("bin_sense_tbl", "bin_sense_summary") %in% names(res)))
  # Expect rho ~ 1 for N under monotone increasing series
  summ <- res$bin_sense_summary
  rho <- summ$spearman_rho[summ$width_comp == 0.5 & summ$metric == "N"]
  testthat::expect_true(is.finite(rho))
  testthat::expect_gt(rho, 0.85)
})
