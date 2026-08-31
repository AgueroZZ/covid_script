source(here::here("R", "validation.R"))
source(here::here("R", "us_data.R"))

canonicalize_historical_us <- function(path, stratified_by_sex) {
  environment <- new.env(parent = emptyenv())
  load(path, envir = environment)
  historical <- environment$USA_monthly
  historical <- historical[!is.na(historical$date) & nzchar(historical$state), ]
  historical$age_group <- if (stratified_by_sex) {
    dplyr::recode(historical$age, `Over 85` = "GE85")
  } else {
    dplyr::recode(historical$age, `Over 80` = "GE80")
  }
  allowed_ages <- if (stratified_by_sex) {
    c("0-44", "45-64", "65-84", "GE85")
  } else {
    c("20-39", "40-59", "60-79", "GE80")
  }
  historical <- historical[historical$age_group %in% allowed_ages, ]
  historical$sex_canonical <- if (stratified_by_sex) {
    dplyr::recode(historical$sex, F = "female", M = "male")
  } else {
    "total"
  }

  historical |>
    dplyr::transmute(
      date,
      geography = state,
      age_group,
      sex = sex_canonical,
      observed_deaths = Deaths
    ) |>
    dplyr::arrange(date, geography, age_group, sex)
}

test_that("sex-stratified CDC sources reproduce the historical analytic data", {
  historical_path <- here::here(
    "output",
    "legacy",
    "united_states",
    "sex_stratified_monthly_data.rda"
  )
  skip_if_not(
    file.exists(historical_path),
    "The optional local legacy comparison object is not available."
  )
  observed <- standardize_us_wonder(
    here::here(us_sex_source_paths()),
    stratified_by_sex = TRUE
  )
  expected <- canonicalize_historical_us(
    historical_path,
    stratified_by_sex = TRUE
  )

  expect_equal(nrow(observed), 118190L)
  expect_equal(length(unique(observed$geography)), 51L)
  expect_setequal(
    unique(observed$age_group),
    c("0-44", "45-64", "65-84", "GE85")
  )
  expect_setequal(unique(observed$sex), c("female", "male"))
  expect_equal(
    observed[c("date", "geography", "age_group", "sex", "observed_deaths")],
    expected
  )

  model_input <- us_model_input(observed, "2023-08-31")
  expect_equal(nrow(model_input), 117079L)
  expect_equal(max(model_input$date), as.Date("2023-08-31"))
})

test_that("non-sex CDC sources reproduce the historical analytic data", {
  historical_path <- here::here(
    "output",
    "legacy",
    "united_states",
    "non_sex_stratified_monthly_data.rda"
  )
  skip_if_not(
    file.exists(historical_path),
    "The optional local legacy comparison object is not available."
  )
  observed <- standardize_us_wonder(
    here::here(us_non_sex_source_paths()),
    stratified_by_sex = FALSE
  )
  expected <- canonicalize_historical_us(
    historical_path,
    stratified_by_sex = FALSE
  )

  expect_equal(nrow(observed), 58787L)
  expect_equal(length(unique(observed$geography)), 51L)
  expect_setequal(
    unique(observed$age_group),
    c("20-39", "40-59", "60-79", "GE80")
  )
  expect_identical(unique(observed$sex), "total")
  expect_equal(
    observed[c("date", "geography", "age_group", "sex", "observed_deaths")],
    expected
  )

  model_input <- us_model_input(observed, "2023-08-31")
  expect_equal(nrow(model_input), 58459L)
  expect_equal(max(model_input$date), as.Date("2023-08-31"))
})
