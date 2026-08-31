test_that("the Europe BayesGP validation freezes the corrected prior contract", {
  script_path <- testthat::test_path(
    "..",
    "..",
    "validation",
    "compare_figure_01_europe_bayesgp.R"
  )

  expect_true(file.exists(script_path))
  script <- paste(readLines(script_path, warn = FALSE), collapse = "\n")

  expect_match(script, "required_bayesgp_version <- \\\"0\\.1\\.3\\\"")
  expect_match(script, "param = list\\(u = 0\\.1, alpha = 0\\.01\\)")
  expect_match(script, "h = 5")
  expect_match(script, "h = 1")
  expect_match(script, "BayesGP::prior_conversion_iwp", fixed = TRUE)
  expect_false(grepl("OSplines:::prior_conversion_IWP", script, fixed = TRUE))
  expect_false(grepl("param = list(u = 0.1, a = 0.01)", script, fixed = TRUE))
})

test_that("the Europe BayesGP validation preserves the historical model geometry", {
  script_path <- testthat::test_path(
    "..",
    "..",
    "validation",
    "compare_figure_01_europe_bayesgp.R"
  )
  script <- paste(readLines(script_path, warn = FALSE), collapse = "\n")

  expect_match(script, "model = \\\"IWP\\\"")
  expect_match(script, "order = 2")
  expect_match(script, "k = 100")
  expect_match(script, "initial_location = \\\"left\\\"")
  expect_match(script, "model = \\\"sGP\\\"")
  expect_match(script, "m = 4")
  expect_match(script, "k = 40")
  expect_match(script, "accuracy = 0\\.001")
  expect_gte(length(gregexpr("region = full_region", script, fixed = TRUE)[[1]]), 2L)
  expect_match(script, "aghq_k = 5")
  expect_match(script, "M = 3000")
  expect_match(script, "reconstruct_locked_model_from_csv")
  expect_match(script, "geo == \\\"NL\\\"")
  expect_match(script, "age == \\\"Y_GE80\\\"")
  expect_match(script, "sex == \\\"T\\\"")
  expect_match(script, "ISOweek2date")
})

test_that("the equivalence decision uses the frozen pre-fit thresholds", {
  script_path <- testthat::test_path(
    "..",
    "..",
    "validation",
    "compare_figure_01_europe_bayesgp.R"
  )
  script <- paste(readLines(script_path, warn = FALSE), collapse = "\n")

  expected_thresholds <- c(
    "matrix_max_absolute_difference = 1e-8",
    "theta_mode_max_absolute_difference = 0.02",
    "overall_post_2020_mean_absolute_relative_difference = 0.005",
    "overall_post_2020_max_absolute_relative_difference = 0.02",
    "trend_post_2020_mean_absolute_relative_difference = 0.005",
    "seasonal_post_2020_mean_absolute_difference = 0.002",
    "wave_median_pscore_max_absolute_difference = 0.005",
    "wave_interval_endpoint_max_absolute_difference = 0.01"
  )

  for (threshold in expected_thresholds) {
    expect_match(script, threshold, fixed = TRUE)
  }
  expect_match(script, "utils::write.csv", fixed = TRUE)
  expect_false(grepl("readr::write_csv", script, fixed = TRUE))
  expect_match(script, "equivalence_pass.flag", fixed = TRUE)
  expect_match(script, "complete.flag", fixed = TRUE)
})
