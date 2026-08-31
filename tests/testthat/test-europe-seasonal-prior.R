test_that("the four-harmonic Europe seasonal prior is converted from one-year PSD", {
  source(testthat::test_path(
    "..", "..", "code", "regions", "europe", "model_functions.R"
  ), local = TRUE)

  raw_prior <- list(u = 0.1, a = 0.01)
  converted_prior <- prior_conversion_sGP_m(
    d = 1,
    prior = raw_prior,
    a = 2 * pi,
    m = 4
  )

  expect_equal(converted_prior$u, 0.426516762063, tolerance = 1e-10)
  expect_equal(converted_prior$a, 0.01)
  expect_gt(converted_prior$u / raw_prior$u, 4)
})

test_that("the custom Europe fitter consumes an already converted SD prior", {
  source(testthat::test_path(
    "..", "..", "code", "regions", "europe", "model_functions.R"
  ), local = TRUE)

  fitter_body <- paste(deparse(body(fit_mod_IWP_sGP)), collapse = "\n")

  expect_match(fitter_body, "u2 = prior_sGP\\$u", fixed = FALSE)
  expect_match(fitter_body, "alpha2 = prior_sGP\\$a", fixed = FALSE)
  expect_false(grepl("prior_conversion_sGP", fitter_body, fixed = TRUE))
})

test_that("the historical IWP conversion remains available under its explicit name", {
  raw_prior <- list(u = 0.1, a = 0.01)
  converted_prior <- OSplines:::prior_conversion_IWP(
    d = 5,
    prior = raw_prior,
    p = 2
  )

  expect_equal(converted_prior$u, 0.0154919333848, tolerance = 1e-10)
  expect_equal(converted_prior$a, 0.01)
})
