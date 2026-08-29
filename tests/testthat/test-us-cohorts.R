source(here::here("R", "config.R"))
source(here::here("R", "waves.R"))
source(here::here("R", "validation.R"))
source(here::here("R", "us_data.R"))
source(here::here("R", "us_cohorts.R"))

test_that("US cohort inventory makes suppression explicit", {
  config <- read_analysis_config(here::here("config", "analysis.yml"))
  sex_data <- us_model_input(
    standardize_us_wonder(
      here::here(us_sex_source_paths()),
      stratified_by_sex = TRUE
    ),
    config$regions$us_sex$analysis_end
  )
  non_sex_data <- us_model_input(
    standardize_us_wonder(
      here::here(us_non_sex_source_paths()),
      stratified_by_sex = FALSE
    ),
    config$regions$us_non_sex$analysis_end
  )
  sex_inventory <- build_us_cohort_inventory(sex_data, "us_sex", config)
  non_sex_inventory <- build_us_cohort_inventory(
    non_sex_data,
    "us_non_sex",
    config
  )

  expect_equal(nrow(sex_inventory), 408L)
  expect_equal(sum(sex_inventory$complete_296), 373L)
  expect_equal(range(sex_inventory$n_observations), c(4L, 296L))
  expect_true(all(sex_inventory$legacy_attempted))

  expect_equal(nrow(non_sex_inventory), 204L)
  expect_equal(sum(non_sex_inventory$complete_296), 190L)
  expect_equal(range(non_sex_inventory$n_observations), c(25L, 296L))
  expect_true(all(non_sex_inventory$legacy_attempted))
})

test_that("committed US cohort registry matches generated keys", {
  registry <- readr::read_csv(
    here::here("config", "cohorts.csv"),
    show_col_types = FALSE
  )
  expect_equal(nrow(registry[registry$region == "us_sex", ]), 408L)
  expect_equal(nrow(registry[registry$region == "us_non_sex", ]), 204L)
  expect_equal(anyDuplicated(registry$analysis_id), 0L)
})
