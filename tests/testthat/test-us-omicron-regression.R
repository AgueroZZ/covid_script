suppressPackageStartupMessages(library(dplyr))
source(here::here(
  "code", "regions", "united_states", "sex_stratified",
  "model_functions.R"
))

test_that("US sex-stratified Omicron aggregation uses post-2022 data", {
  dates <- seq.Date(
    from = as.Date("2020-03-01"),
    to = as.Date("2023-08-01"),
    by = "month"
  )
  excess_by_date <- ifelse(
    dates < as.Date("2020-11-01"),
    10,
    ifelse(
      dates < as.Date("2021-07-01"),
      20,
      ifelse(dates < as.Date("2022-01-01"), 30, 40)
    )
  )

  observed <- data.frame(
    year = as.integer(format(dates, "%Y")),
    date = dates,
    state = "Synthetic State",
    age = "65-84",
    sex = "F",
    Deaths = 100 + excess_by_date
  )
  predicted <- list(
    samples = matrix(100, nrow = length(dates), ncol = 4L),
    summary = data.frame(time = dates)
  )

  result <- excess_mortality_aggregate(
    State = "Synthetic State",
    Age = "65-84",
    model_pred = predicted,
    monthly_death = observed
  )

  expect_identical(result$wave, c("initial", "alpha", "delta", "omicron"))
  expect_equal(result$p_med, c(0.1, 0.2, 0.3, 0.4), tolerance = 1e-12)
})
