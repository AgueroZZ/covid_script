source(here::here("R", "europe_reporting.R"))

test_that("the corrected refit changes exactly five adopted outputs", {
  expect_identical(
    corrected_europe_affected_outputs(),
    c("figure_01", "figure_02", "figure_04", "figure_05", "table_01")
  )
  expect_false("figure_03" %in% corrected_europe_affected_outputs())
})

test_that("the corrected Europe reporting cohort is explicit and complete", {
  cohort <- readr::read_csv(
    here::here("config", "europe_reporting_cohort.csv"),
    show_col_types = FALSE
  )

  expect_equal(sum(cohort$corrected_model_available), 33L)
  expect_equal(sum(cohort$figure_02), 33L)
  expect_equal(sum(cohort$figure_04), 15L)
  expect_equal(sum(cohort$figure_05), 15L)
  expect_equal(sum(cohort$table_01), 15L)
  expect_setequal(
    cohort$geography[cohort$figure_04],
    c(
      "AM", "AT", "BE", "BG", "DK", "ES", "FI", "HR",
      "HU", "IT", "LV", "PT", "RO", "RS", "SK"
    )
  )
  pending <- cohort[cohort$geography %in% c("IE", "UK"), ]
  expect_true(all(pending$scientific_status == "pending_separate_refit"))
  expect_false(any(pending$figure_02 | pending$figure_04 |
    pending$figure_05 | pending$table_01))
})

test_that("age aggregation preserves paired posterior draws", {
  dates <- as.Date("2020-01-06") + 7 * 0:1
  first <- list(
    geography = "AA",
    age_group = "Y40-59",
    sex = "T",
    dates = dates,
    observed_deaths = c(12, 15),
    samples = matrix(c(10L, 11L, 12L, 13L), nrow = 2)
  )
  second <- list(
    geography = "AA",
    age_group = "Y60-79",
    sex = "T",
    dates = dates,
    observed_deaths = c(20, 21),
    samples = matrix(c(18L, 19L, 20L, 21L), nrow = 2)
  )

  combined <- combine_corrected_europe_predictions(
    list(first, second),
    "40-79"
  )

  expect_equal(combined$observed_deaths, c(32, 36))
  expect_equal(combined$samples, first$samples + second$samples)
  expect_identical(combined$age_group, "40-79")
})

test_that("wave summaries use half-open canonical intervals", {
  prediction <- list(
    geography = "AA",
    age_group = "Y60-79",
    sex = "F",
    dates = as.Date(c("2020-10-31", "2020-11-01", "2021-06-30")),
    observed_deaths = c(12, 22, 33),
    samples = matrix(
      c(10L, 20L, 30L, 10L, 20L, 30L),
      nrow = 3,
      ncol = 2
    )
  )
  waves <- data.frame(
    wave = c("initial", "alpha"),
    start = as.Date(c("2020-03-01", "2020-11-01")),
    end_exclusive = as.Date(c("2020-11-01", "2021-07-01"))
  )

  result <- summarize_corrected_europe_waves(list(prediction), waves)

  expect_equal(result$p_median[result$wave == "initial"], 0.2)
  expect_equal(result$p_median[result$wave == "alpha"], 0.1)
  expect_identical(unique(result$analysis_path), "europe_sex")
  expect_identical(unique(result$sex), "female")
})

test_that("corrected integration contract matches local verified artifacts", {
  refit_root <- here::here(
    "artifacts",
    "results",
    "europe_corrected_psd_prior_20260830"
  )
  testthat::skip_if_not(file.exists(file.path(refit_root, "verified_complete.flag")))

  contract <- read_corrected_europe_contract(
    refit_root = refit_root,
    cohort_path = here::here("config", "europe_reporting_cohort.csv"),
    data_path = here::here("EU_analysis", "demo_r_mwk_20_linear.csv")
  )

  expect_equal(nrow(contract$manifest), 388L)
  expect_equal(length(unique(contract$manifest$geo)), 33L)
  expect_setequal(
    unique(contract$manifest$geo),
    contract$cohort$geography[contract$cohort$corrected_model_available]
  )
  required <- subset(
    contract$manifest,
    geo %in% contract$cohort$geography[contract$cohort$figure_04] &
      age %in% c("Y40-59", "Y60-79") & sex %in% c("T", "F", "M")
  )
  expect_equal(nrow(required), 90L)
  expect_true(all(file.exists(file.path(
    refit_root,
    "fitted_model",
    paste0(required$model_id, ".rda")
  ))))

  observed <- read_corrected_europe_observed(contract$data_path)
  prediction <- load_corrected_europe_prediction(
    contract$refit_root,
    observed,
    "NL",
    "Y_GE80",
    "T"
  )
  expect_equal(dim(prediction$samples), c(1269L, 3000L))
  expect_equal(length(prediction$observed_deaths), 1269L)
  expect_identical(prediction$dates, as.Date(prediction$dates))
})
