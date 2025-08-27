testthat::test_that("binning metrics shape and width effects", {
  import::from("tibble", tibble)

  # Synthetic series: one occurrence every 0.1 Ma from 0.05 to 0.95
  occ <- tibble(
    species = "s1",
    date = seq(0.05, 0.95, by = 0.1),
    sampling_intensity = 1
  )

  m025 <- binned_metrics(occ, width = 0.25)
  m050 <- binned_metrics(occ, width = 0.5)
  # Doubling width roughly halves number of bins (exact for 0..1 example)
  testthat::expect_equal(nrow(m050) * 2, nrow(m025))

  # Full sensitivity results
  res <- bin_sensitivity(occ, widths = c(0.1, 0.5))
  testthat::expect_true(all(c("bin_sense_tbl", "bin_sense_summary") %in% names(res)))
  # Expect perfect correlation for 0.5 vs 0.1 in this aligned construction
  summ <- res$bin_sense_summary
  rho <- summ$spearman_rho[summ$width_comp == 0.5 & summ$metric == "N"]
  testthat::expect_true(is.finite(rho))
  testthat::expect_gt(rho, 0.99)
})

