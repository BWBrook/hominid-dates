sample_path <- here::here("inst", "extdata", "hominid_sample.csv")

testthat::test_that("read_hominid reads and types columns", {
  dat <- read_hominid(sample_path)
  testthat::expect_s3_class(dat, "tbl_df")
  testthat::expect_true(all(c("lat","lon","date","upper","lower") %in% names(dat)))
})

testthat::test_that("collapse_fragments adds sampling_intensity and groups", {
  raw <- read_hominid(sample_path)
  occ <- collapse_fragments(raw)
  testthat::expect_true("sampling_intensity" %in% names(occ))
  testthat::expect_true(all(occ$sampling_intensity >= 1))
})

testthat::test_that("filter_indet removes indeterminate species", {
  raw <- read_hominid(sample_path)
  occ <- collapse_fragments(raw)
  out <- filter_indet(occ)
  testthat::expect_true(all(out$species != "indet"))
})

testthat::test_that("summaries and overlap produce expected shapes", {
  raw <- read_hominid(sample_path)
  occ <- collapse_fragments(raw) |> filter_indet()
  freq <- summarise_freq(occ)
  spans <- summarise_spans(occ)
  ov   <- calc_overlap(spans)
  testthat::expect_true(all(c("genus_freq","species_freq") %in% names(freq)))
  testthat::expect_true(all(c("species","first_occ","last_occ","duration") %in% names(spans)))
  testthat::expect_true("species_i" %in% names(ov))
})
