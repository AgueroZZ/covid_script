source(here::here("R", "config.R"))
source(here::here("R", "waves.R"))
source(here::here("R", "us_results.R"))

synthetic_prediction <- function(sex = "total", observed = c(20, 20)) {
  list(
    analysis_id = paste0("synthetic__", sex),
    analysis_path = "synthetic",
    geography = "Example",
    age_group = "20-39",
    sex = sex,
    dates = as.Date(c("2020-03-31", "2020-04-30")),
    observed_deaths = observed,
    samples = matrix(c(10, 20, 10, 20), nrow = 2, ncol = 2)
  )
}

test_that("wave P-score is a posterior ratio of totals", {
  config <- read_analysis_config(here::here("config", "analysis.yml"))
  result <- summarize_wave_ratio(synthetic_prediction(), config)
  initial <- result[result$wave == "initial", ]

  expected_draw <- (40 - 30) / 30
  expect_equal(initial$p_median, expected_draw)
  expect_false(isTRUE(all.equal(initial$p_median, mean(c(1, 0)))))
  expect_equal(initial$observed_months, 2L)
})

test_that("all canonical waves are retained when observations are absent", {
  config <- read_analysis_config(here::here("config", "analysis.yml"))
  result <- summarize_wave_ratio(synthetic_prediction(), config)

  expect_identical(result$wave, c("initial", "alpha", "delta", "omicron"))
  expect_identical(
    result$status,
    c("success", rep("no_observed_months", 3))
  )
})

test_that("pointwise summaries retain variance for reporting aggregation", {
  config <- read_analysis_config(here::here("config", "analysis.yml"))
  result <- summarize_pointwise_pscore(synthetic_prediction(), config)

  expect_true("p_variance" %in% names(result))
  expect_true(all(is.finite(result$p_variance)))
  expect_true(all(result$p_variance >= 0))
})

test_that("sex contrast is female minus male", {
  female <- synthetic_prediction("female", observed = c(30, 30))
  male <- synthetic_prediction("male", observed = c(20, 20))
  contrast <- compute_us_sex_contrast(female, male)

  expect_true(all(contrast$summary$mean > 0))
  expect_identical(unique(contrast$summary$contrast), "female_minus_male")
})

test_that("combined sex contrast explicitly covers ages 0-84", {
  predictions <- lapply(c("0-44", "45-64", "65-84"), function(age_group) {
    prediction <- synthetic_prediction("female")
    prediction$age_group <- age_group
    prediction
  })
  combined <- combine_us_age_predictions(predictions)

  expect_identical(combined$age_group, "0-84")
  expect_equal(combined$observed_deaths, c(60, 60))
  expect_equal(combined$samples, predictions[[1]]$samples * 3)
})

test_that("jurisdiction trajectories use explicit inverse-variance weights", {
  first <- list(
    geography = "A",
    age_group = "20-39",
    dates = as.Date(c("2020-03-31", "2020-04-30")),
    samples = matrix(c(0, 2, 2, 4), nrow = 2)
  )
  second <- list(
    geography = "B",
    age_group = "20-39",
    dates = first$dates,
    samples = matrix(c(1, 5, 3, 9), nrow = 2)
  )
  result <- aggregate_inverse_variance_trajectory(
    list(first, second),
    group_label = "example",
    estimand = "pointwise_pscore"
  )

  expect_equal(result$jurisdictions, c(2L, 2L))
  expect_identical(
    unique(result$interval_method),
    "fixed_effect_normal_approximation"
  )
  expect_true(all(result$lower < result$upper))
})
