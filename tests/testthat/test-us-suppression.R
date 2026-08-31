source(here::here("R", "config.R"))
source(here::here("R", "waves.R"))
source(here::here("R", "us_cohorts.R"))
source(here::here("R", "us_results.R"))
source(here::here("R", "us_suppression.R"))

test_that("US suppression inventory reproduces the audited counts", {
  inventory <- read_us_completeness_inventory(
    here::here("config", "cohorts.csv")
  )
  expect_equal(nrow(inventory), 612L)
  expect_equal(
    sum(inventory$region == "us_sex" & inventory$complete),
    373L
  )
  expect_equal(
    sum(inventory$region == "us_non_sex" & inventory$complete),
    190L
  )
  expect_equal(sum(!inventory$complete), 49L)
})

test_that("adopted US outputs use explicit named cohorts", {
  cohort <- read_us_reporting_cohort(
    here::here("config", "us_reporting_cohort.csv")
  )
  expect_equal(nrow(cohort), 51L)
  expect_equal(anyDuplicated(cohort$geography), 0L)
  expect_equal(sum(cohort$figure_03), 51L)
  expect_equal(sum(cohort$figure_04), 13L)
  expect_equal(sum(cohort$figure_05), 11L)
  expect_equal(sum(cohort$table_01), 11L)
  expect_setequal(
    cohort$geography[cohort$table_01],
    readr::read_csv(
      here::here("config", "us_table_01_cohort.csv"),
      show_col_types = FALSE
    )$geography
  )
})

test_that("adopted-output dependency registry identifies exact incomplete strata", {
  dependencies <- build_us_adopted_dependency_registry(
    read_us_completeness_inventory(here::here("config", "cohorts.csv")),
    read_us_reporting_cohort(here::here("config", "us_reporting_cohort.csv"))
  )
  expect_equal(nrow(dependencies), 216L)
  expect_equal(
    as.list(table(dependencies$output_id)),
    as.list(c(figure_03 = 102L, figure_04 = 26L, figure_05 = 66L, table_01 = 22L))
  )
  incomplete <- dependencies[!dependencies$complete, ]
  expect_equal(nrow(incomplete), 3L)
  expect_true(all(incomplete$output_id == "figure_05"))
  expect_setequal(incomplete$analysis_id, c(
    "us__us-sex__idaho__0-44__female",
    "us__us-sex__idaho__0-44__male",
    "us__us-sex__new-mexico__0-44__female"
  ))
})

test_that("strict complete-case membership removes all incomplete Figure 5 strata", {
  cohort <- read_us_reporting_cohort(
    here::here("config", "us_reporting_cohort.csv")
  )
  dependencies <- build_us_adopted_dependency_registry(
    read_us_completeness_inventory(here::here("config", "cohorts.csv")),
    cohort
  )
  complete <- us_complete_figure05_geographies(dependencies)
  expect_setequal(setdiff(cohort$geography[cohort$figure_05], complete), c(
    "Idaho",
    "New Mexico"
  ))
  selected <- dependencies |>
    dplyr::filter(output_id == "figure_05", jurisdiction %in% complete)
  expect_true(all(selected$complete))
})

test_that("requested Figure 5 comparison cohorts use explicit exclusions", {
  cohort <- read_us_reporting_cohort(
    here::here("config", "us_reporting_cohort.csv")
  )
  dependencies <- build_us_adopted_dependency_registry(
    read_us_completeness_inventory(here::here("config", "cohorts.csv")),
    cohort
  )
  component_ages <- c("0-44", "45-64", "65-84")
  exclude_both <- us_figure05_variant_membership(
    cohort,
    dependencies,
    "exclude_idaho_new_mexico",
    component_ages
  )
  exclude_idaho <- us_figure05_variant_membership(
    cohort,
    dependencies,
    "exclude_idaho",
    component_ages
  )
  historical <- us_figure05_variant_membership(
    cohort,
    dependencies,
    "historical",
    component_ages
  )
  expect_equal(nrow(exclude_both), 9L)
  expect_equal(nrow(exclude_idaho), 10L)
  expect_equal(nrow(historical), 11L)
  expect_false(any(c("Idaho", "New Mexico") %in% exclude_both$geography))
  expect_false("Idaho" %in% exclude_idaho$geography)
  expect_true("New Mexico" %in% exclude_idaho$geography)
  expect_true(all(c("Idaho", "New Mexico") %in% historical$geography))
})

test_that("suppression decision summaries use the manuscript display period", {
  comparison <- tibble::tibble(
    variant = "fixture",
    age_group = "0-44",
    vaccination_group = "high",
    date = as.Date(c("2019-12-01", "2020-01-01", "2023-08-31", "2023-09-30")),
    abs_mean_difference = c(0.50, 0.01, 0.02, 0.40),
    sign_reversal = FALSE,
    jurisdictions_historical = 6L,
    jurisdictions_candidate = 5L
  )
  summary <- summarize_us_figure05_sensitivity(
    comparison,
    analysis_start = as.Date("2020-01-01"),
    analysis_end = as.Date("2023-08-31")
  )
  expect_equal(summary$historical_rows, 2L)
  expect_equal(summary$maximum_absolute_mean_change, 0.02)
})

test_that("reconstructed historical US Figure 5 input matches installed input", {
  bundle_root <- here::here("artifacts", "results", "zenodo_bundle")
  testthat::skip_if_not(dir.exists(bundle_root))
  cohort <- read_us_reporting_cohort(
    here::here("config", "us_reporting_cohort.csv")
  )
  dependencies <- build_us_adopted_dependency_registry(
    read_us_completeness_inventory(here::here("config", "cohorts.csv")),
    cohort
  )
  predictions <- load_us_figure05_bundle_predictions(bundle_root, cohort)
  reconstructed <- build_us_figure05_variant(
    predictions,
    cohort,
    dependencies,
    "historical"
  )
  installed <- readr::read_csv(
    here::here("artifacts", "reporting", "inputs", "figure_05_sex_difference.csv"),
    show_col_types = FALSE
  )
  comparison <- compare_us_figure05_baseline(reconstructed, installed)
  expect_true(comparison$pass)
  expect_lte(comparison$max_abs_numeric_difference, 1e-12)
})
