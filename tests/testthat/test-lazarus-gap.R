testthat::test_that("Lazarus gaps basic properties and edge cases", {
  import::from("tibble", tibble)

  # Constant lambda over [2.0, 3.0] Ma, bin width 0.25
  ts <- tibble(
    effort_stratum = "global",
    time_bin_start = seq(2.0, 2.75, by = 0.25),
    time_bin_end   = seq(2.25, 3.00, by = 0.25),
    lambda = 1
  )

  # Species with three events -> two interior gaps
  spec <- tibble(
    species = "s1",
    date = c(3.0, 2.7, 2.1),
    sampling_intensity = 1
  )

  lz <- lazarus_gaps(spec, ts)
  testthat::expect_true(nrow(lz) == 2)
  # Longer gap should have smaller P0 than shorter gap under constant lambda
  pvals <- lz$p_no_detect[order(lz$gap_Ma, decreasing = TRUE)]
  testthat::expect_true(pvals[1] < pvals[2])

  # Identical dates => no interior gaps (NA row)
  spec2 <- tibble(species = "s2", date = c(2.0, 2.0), sampling_intensity = 1)
  lz2 <- lazarus_gaps(spec2, ts)
  testthat::expect_true(all(is.na(lz2$gap_Ma)))

  # Zero-effort over the gap triggers classed error
  ts_zero <- tibble(
    effort_stratum = "global",
    time_bin_start = 2.0,
    time_bin_end   = 3.0,
    lambda = 0
  )
  spec3 <- tibble(species = "s3", date = c(3.0, 2.0), sampling_intensity = 1)
  testthat::expect_error(lazarus_gaps(spec3, ts_zero), class = "effort_zero_near_gap")

  # BH adjustment reproducible within species
  # For s1 above, compute BH on raw p-values and compare
  padj <- stats::p.adjust(lz$p_no_detect, method = "BH")
  testthat::expect_equal(padj, lz$p_adj)
})

