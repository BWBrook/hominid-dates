testthat::test_that("mc_dates draws within bounds", {
  raw <- read_hominid(here::here("inst","extdata","hominid_sample.csv"))
  occ <- collapse_fragments(raw) |> filter_indet()
  draws <- mc_dates(occ, B = 5, dist = "uniform")
  testthat::expect_type(draws, "list")
  testthat::expect_true(all(vapply(draws, nrow, integer(1)) > 0))
  # Check bounds on first draw
  d1 <- draws[[1]]
  testthat::expect_true(all(d1$date_mc <= d1$upper + 1e-9))
  testthat::expect_true(all(d1$date_mc >= d1$lower - 1e-9))
})
