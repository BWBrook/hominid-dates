testthat::test_that("indet GLM: higher Pr(indet) at older bins for increasing trend", {
  import::from("tibble", tibble)

  set.seed(1)
  # Build synthetic occ_tbl: one country, dates from 0.1 to 2.0 by 0.1
  dates <- seq(0.1, 2.0, by = 0.1)
  # True increasing probability with time
  p_true <- plogis(-4 + 2 * scales::rescale(dates, to = c(0, 1)))
  n_tot <- 30L
  n_ind <- round(p_true * n_tot)

  # Expand to occurrence rows (n_tot per date), mark 'indet' for first n_ind rows
  occ <- tibble(
    country = "X",
    date = rep(dates, each = n_tot),
    species = unlist(lapply(n_ind, function(k) c(rep("indet", k), rep("spp", n_tot - k)))),
    sampling_intensity = 1
  )

  ind <- build_indet_frame(occ, width = 0.25)
  fit <- fit_indet_glm(ind)
  eff <- predict_indet(ind, fit)
  testthat::expect_true(nrow(eff) > 0)
  # Older = larger Ma (first row after arrange); Younger = smaller Ma (last row)
  by_country <- split(eff, eff$country)
  eX <- by_country[[1]]
  testthat::expect_gt(eX$p_hat[1], eX$p_hat[nrow(eX)])
  testthat::expect_true(all(eX$p_hat >= 0 & eX$p_hat <= 1))
})

testthat::test_that("bins with zero total are dropped with a warning", {
  import::from("tibble", tibble)

  # occ with gaps: span covers 3 bins but middle bin empty
  occ <- tibble(
    country = rep("Y", 10),
    date = c(rep(1.0, 5), rep(1.75, 5)),
    species = c(rep("indet", 5), rep("spp", 5)),
    sampling_intensity = 1
  )
  ind <- build_indet_frame(occ, width = 0.25)
  # Expect a warning about dropping zero-total bins
  testthat::expect_warning(fit_indet_glm(ind), regexp = "n_total == 0")
})
