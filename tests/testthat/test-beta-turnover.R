testthat::test_that("beta turnover basic properties and edge cases", {
  # Tiny synthetic cluster with known compositions across consecutive bins
  df <- data.frame(
    cluster_id = c(1,1,1,1,1,1,1,1),
    species    = c("a","b","a","b","c","a","c","d"),
    date       = c(2.10,2.05, 1.85,1.80, 1.60,1.55, 1.35,1.30),
    sampling_intensity = 1,
    lat = 0, lon = 0, country = "X"
  )
  # Bin width 0.25 → bins: [2.0,2.25), [1.75,2.0), [1.5,1.75), [1.25,1.5)
  inc <- build_incidence_by_cluster(df, width = 0.25, top_k = 6)
  beta <- beta_turnover_by_cluster(inc$incidence)

  # Beta values in [0,1]
  rng_ok <- all(beta$beta_total >= 0 & beta$beta_total <= 1)
  testthat::expect_true(rng_ok)

  # If two bins are identical, beta_total = 0
  df2 <- data.frame(
    cluster_id = c(9,9,9,9),
    species    = c("x","y","x","y"),
    date       = c(2.04,2.01, 1.79,1.76),
    sampling_intensity = 1, lat = 0, lon = 0, country = "X"
  )
  inc2 <- build_incidence_by_cluster(df2, width = 0.25)
  beta2 <- beta_turnover_by_cluster(inc2$incidence)
  testthat::expect_true(all(abs(beta2$beta_total - 0) < 1e-8))

  # Disjoint sets → beta_total = 1
  df3 <- data.frame(
    cluster_id = c(8,8,8,8),
    species    = c("x","x","y","y"),
    date       = c(2.04,2.01, 1.54,1.51),
    sampling_intensity = 1, lat = 0, lon = 0, country = "X"
  )
  inc3 <- build_incidence_by_cluster(df3, width = 0.25)
  beta3 <- beta_turnover_by_cluster(inc3$incidence)
  testthat::expect_true(all(abs(beta3$beta_total - 1) < 1e-8))

  # Equal richness with some replacement → beta_total approx equals turnover component
  # Bin1 {a,b}, Bin2 {a,c} share 1/2; Sorensen ~ 0.333; Simpson turnover ~ 0.5? Check relation
  df4 <- data.frame(
    cluster_id = c(7,7,7,7),
    species    = c("a","b","a","c"),
    date       = c(2.04,2.01, 1.79,1.76),
    sampling_intensity = 1, lat = 0, lon = 0, country = "X"
  )
  inc4 <- build_incidence_by_cluster(df4, width = 0.25)
  beta4 <- beta_turnover_by_cluster(inc4$incidence)
  testthat::expect_true(all(beta4$beta_total >= beta4$beta_turnover - 1e-8))

  # Error when cluster_id missing
  df_bad <- subset(df, select = -cluster_id)
  testthat::expect_error(build_incidence_by_cluster(df_bad), class = "beta_turnover_missing_cols")
})
