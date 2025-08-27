testthat::test_that("temporal outliers flagged by MC envelope", {
  import::from("tibble", tibble)

  # spans_mc envelope: first_hi = 2.0, last_lo = 1.0
  spans <- tibble(
    species = "s1",
    first_hi = 2.0,
    last_lo  = 1.0
  )

  # Occurrences: one very old (2.2) and one very young (0.8) beyond delta=0.05
  occ <- tibble(
    species = c("s1","s1","s1"),
    id = c(1L, 2L, 3L),
    date = c(2.2, 1.5, 0.8),
    lat = 0, lon = 0
  )

  tout <- temporal_outliers(occ, spans, delta = 0.05)
  testthat::expect_true(any(tout$type == "FAD_ext" & tout$id == 1L))
  testthat::expect_true(any(tout$type == "LAD_ext" & tout$id == 3L))
})

testthat::test_that("spatial outlier flagged by MAD rule", {
  import::from("tibble", tibble)

  # Ring of points near 0,0 and one far away
  ring <- tibble(
    species = "s2",
    id = 1:20,
    date = 1.0,
    lat = c(rep(0, 19), 0),
    lon = c(seq(-0.5, 0.5, length.out = 19), 30) # last one far east
  )
  sout <- spatial_outliers(ring)
  testthat::expect_true(any(sout$id == 20L & sout$type == "spatial"))
})

