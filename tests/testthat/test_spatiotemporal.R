testthat::test_that("st_cluster returns cluster_id and sorted rows", {
  raw <- read_hominid(here::here("inst","extdata","hominid_sample.csv"))
  occ <- collapse_fragments(raw)
  st  <- st_cluster(occ, minPts = 2, date_weight = 0.5)
  testthat::expect_true("cluster_id" %in% names(st))
  testthat::expect_equal(nrow(st), nrow(occ))
})

testthat::test_that("range_dynamics returns expected columns", {
  raw <- read_hominid(here::here("inst","extdata","hominid_sample.csv"))
  occ <- collapse_fragments(raw) |> filter_indet()
  rng <- range_dynamics(occ, bin_width = 0.5)
  cols <- c("species","bin","centroid_lat","centroid_long","lat_span","long_span","hull_area_km2","velocity_km_per_Ma")
  testthat::expect_true(all(cols %in% names(rng)))
})
