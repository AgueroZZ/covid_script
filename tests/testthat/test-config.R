source(here::here("R", "config.R"))

test_that("canonical configuration validates", {
  config <- read_analysis_config(here::here("config", "analysis.yml"))

  expect_identical(config$model$trend, "IWP2")
  expect_false(config$model$population_offset)
  expect_equal(
    as.numeric(unlist(config$model$seasonal_period_months, use.names = FALSE)),
    c(12, 6, 4, 3)
  )
  expect_identical(config$regions$ireland$frequency, "quarterly")
  expect_identical(
    config$regions$england_wales$geography_label,
    "England and Wales"
  )
})
