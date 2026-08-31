source(here::here("R", "config.R"))
source(here::here("R", "validation.R"))
source(here::here("R", "vaccination.R"))

test_that("fixed US thresholds classify disputed states as neither", {
  config <- read_analysis_config(here::here("config", "analysis.yml"))
  vaccination <- prepare_us_vaccination(
    here::here("data", "raw", "owid", "us_state_vaccination_snapshot.rda"),
    config
  )
  disputed <- vaccination[
    vaccination$geography %in% c("Texas", "Arkansas", "California", "New York"),
  ]

  expect_equal(nrow(vaccination), 51L)
  expect_true(all(disputed$vaccination_group == "neither"))
  expect_setequal(
    vaccination$geography[vaccination$vaccination_group == "low"],
    c("Alabama", "Idaho", "Louisiana", "Mississippi", "Tennessee", "Wyoming")
  )
  expect_equal(sum(vaccination$vaccination_group == "high"), 11L)
})

test_that("historical positional cohorts are isolated as validation data", {
  config <- read_analysis_config(here::here("config", "analysis.yml"))
  vaccination <- prepare_us_vaccination(
    here::here("data", "raw", "owid", "us_state_vaccination_snapshot.rda"),
    config
  )
  historical <- historical_us_reporting_cohorts(vaccination$geography)

  expect_equal(sum(historical$analysis_path == "us_sex"), 38L)
  expect_equal(sum(historical$analysis_path == "us_non_sex"), 42L)
  expect_true(all(historical$cohort_role == "historical_positional_subset"))
})
