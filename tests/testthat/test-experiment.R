test_that("randomised estimator reproduces published Hillstrom figures", {
  skip_if_not(file.exists(cfg_test$data$path), "raw data not present")

  cd  <- load_campaign_data(cfg_test)
  res <- run_experiment(cd, cfg_test, outcome = "visit")$estimates
  mens   <- res$estimate[res$arm == "Mens E-Mail"]
  womens <- res$estimate[res$arm == "Womens E-Mail"]

  # Stochastic Solutions (2008): 7.66pp and 4.52pp visit-rate uplift.
  expect_equal(mens,   0.0766, tolerance = 1e-3)
  expect_equal(womens, 0.0452, tolerance = 1e-3)
})

test_that("diagnose refuses difference-in-differences without a period column", {
  skip_if_not(file.exists(cfg_test$data$path), "raw data not present")

  cd     <- load_campaign_data(cfg_test)
  checks <- run_validity_checks(cd, cfg_test)
  dx     <- diagnose_design(cd, cfg_test, checks)

  expect_false(dx$designs$did$supported)
  expect_identical(dx$recommended, "randomised")
  expect_match(dx$designs$did$reason, "period", ignore.case = TRUE)
})