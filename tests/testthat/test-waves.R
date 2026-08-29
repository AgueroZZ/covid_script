source(here::here("R", "config.R"))
source(here::here("R", "waves.R"))

test_that("wave boundaries are start-inclusive and end-exclusive", {
  config <- read_analysis_config(here::here("config", "analysis.yml"))
  dates <- as.Date(c(
    "2020-03-01",
    "2020-10-31",
    "2020-11-01",
    "2021-06-30",
    "2021-07-01",
    "2021-12-31",
    "2022-01-01",
    "2024-03-31",
    "2024-04-01"
  ))

  expect_identical(
    assign_wave(dates, config),
    c(
      "initial",
      "initial",
      "alpha",
      "alpha",
      "delta",
      "delta",
      "omicron",
      "omicron",
      NA_character_
    )
  )
})

test_that("Omicron never reuses the Delta interval", {
  config <- read_analysis_config(here::here("config", "analysis.yml"))

  expect_identical(assign_wave(as.Date("2021-12-31"), config), "delta")
  expect_identical(assign_wave(as.Date("2022-01-01"), config), "omicron")
})
