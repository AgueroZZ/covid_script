source(here::here("R", "config.R"))
source(here::here("R", "us_cohorts.R"))
source(here::here("R", "us_model.R"))

test_that("US analysis seeds are stable and stage-specific", {
  first <- stable_analysis_seed(20260829, "us__example", "fit")
  second <- stable_analysis_seed(20260829, "us__example", "fit")
  predictive <- stable_analysis_seed(
    20260829,
    "us__example",
    "posterior-predictive"
  )

  expect_identical(first, second)
  expect_false(identical(first, predictive))
  expect_true(first > 0L)
})

test_that("US model frame uses pre-2020 training and calendar-day offsets", {
  branch <- tibble::tibble(
    date = as.Date(c("2019-02-28", "2019-03-31", "2020-01-31")),
    geography = "Example",
    age_group = "20-39",
    sex = "total",
    observed_deaths = c(10, 11, 12),
    log_days = log(c(28, 31, 31))
  )
  attr(branch, "analysis_id") <- "us__example"
  frame <- prepare_us_model_frame(branch)

  expect_equal(nrow(frame$training), 2L)
  expect_equal(exp(frame$training$log_days), c(28, 31))
  expect_equal(frame$training$x1, frame$training$x2)
  expect_equal(frame$training$x2, frame$training$x3)
})

test_that("US model formula encodes IWP2 seasonality and month offset", {
  formula_text <- paste(deparse(us_model_formula()), collapse = " ")

  expect_match(formula_text, 'model = "IWP"', fixed = TRUE)
  expect_match(formula_text, "order = 2", fixed = TRUE)
  expect_match(formula_text, 'model = "sGP"', fixed = TRUE)
  expect_match(formula_text, "m = 4", fixed = TRUE)
  expect_match(formula_text, "offset(log_days)", fixed = TRUE)
})

test_that("US knot counts preserve the historical span rule", {
  long <- tibble::tibble(date = as.Date(c("1999-01-31", "2020-01-31")))
  short <- tibble::tibble(date = as.Date(c("2015-01-31", "2020-01-31")))

  expect_identical(us_knot_counts(long), list(iwp = 100L, seasonal = 40L))
  expect_identical(us_knot_counts(short), list(iwp = 50L, seasonal = 20L))
})
