source(here::here("R", "reporting.R"))
source(here::here("R", "tables.R"))
source(here::here("R", "us_omicron_outputs.R"))

test_that("US state and Omicron panel registries are explicit and complete", {
  states <- utils::read.csv(
    here::here("config", "us_state_codes.csv"),
    stringsAsFactors = FALSE
  )
  panels <- utils::read.csv(
    here::here("config", "us_omicron_bar_panels.csv"),
    stringsAsFactors = FALSE
  )
  table_cohort <- utils::read.csv(
    here::here("config", "us_table_01_cohort.csv"),
    stringsAsFactors = FALSE
  )

  expect_equal(nrow(states), 51L)
  expect_equal(length(unique(states$state_name)), 51L)
  expect_equal(length(unique(states$state_code)), 51L)
  expect_true(all(nchar(states$state_code) == 2L))
  expect_equal(nrow(panels), 8L)
  expect_equal(length(unique(panels$panel_id)), 8L)
  expect_setequal(panels$age_group, c("0-44", "45-64", "65-84", "Over 85"))
  expect_setequal(panels$sex, c("F", "M"))
  expect_true(all(vapply(
    strsplit(panels$state_codes, ";", fixed = TRUE),
    length,
    integer(1)
  ) == 10L))
  expect_identical(names(table_cohort), "geography")
  expect_equal(nrow(table_cohort), 11L)
  expect_equal(length(unique(table_cohort$geography)), 11L)
  expect_true(all(table_cohort$geography %in% states$state_name))
})

synthetic_us_wave_result <- function(omicron_value, omit_omicron = FALSE) {
  wave <- c("initial", "alpha", "delta", "omicron")
  if (omit_omicron) wave <- wave[wave != "omicron"]
  p_median <- c(initial = 0.1, alpha = 0.2, delta = 0.3, omicron = omicron_value)
  data.frame(
    wave = wave,
    delta_upper = 10,
    delta_med = 5,
    delta_lower = 0,
    p_upper = unname(p_median[wave]) + 0.1,
    p_med = unname(p_median[wave]),
    p_lower = unname(p_median[wave]) - 0.1,
    state = "California",
    age = "65-84",
    sex = "F",
    stringsAsFactors = FALSE
  )
}

test_that("standardization uses named columns and canonical state codes", {
  mapping <- utils::read.csv(
    here::here("config", "us_state_codes.csv"),
    stringsAsFactors = FALSE
  )
  standardized <- standardize_us_wave_result(
    synthetic_us_wave_result(0.4),
    mapping
  )

  expect_identical(names(standardized), c(
    "state_name", "state_code", "age_group", "sex", "wave",
    "delta_lower", "delta_median", "delta_upper",
    "p_lower", "p_median", "p_upper"
  ))
  expect_identical(unique(standardized$state_code), "CA")
  expect_identical(standardized$wave, c("initial", "alpha", "delta", "omicron"))
})

test_that("controlled comparison isolates the Omicron correction", {
  mapping <- utils::read.csv(
    here::here("config", "us_state_codes.csv"),
    stringsAsFactors = FALSE
  )
  historical <- synthetic_us_wave_result(0.3)
  corrected <- synthetic_us_wave_result(0.4)
  comparison <- compare_us_omicron_results(
    standardize_us_wave_result(historical, mapping),
    standardize_us_wave_result(corrected, mapping)
  )

  expect_true(comparison$summary$non_omicron_exact)
  expect_true(comparison$summary$historical_omicron_duplicates_delta)
  expect_equal(comparison$rowwise$p_median_change, 0.1, tolerance = 1e-12)
  expect_equal(comparison$summary$changed_rows, 1L)
})

test_that("reporting conversion matches the Table 1 contract", {
  mapping <- utils::read.csv(
    here::here("config", "us_state_codes.csv"),
    stringsAsFactors = FALSE
  )
  standardized <- standardize_us_wave_result(
    synthetic_us_wave_result(0.4),
    mapping
  )
  reporting <- as_reporting_us_wave_summary(standardized)

  expect_identical(unique(reporting$analysis_path), "us_sex")
  expect_identical(unique(reporting$sex), "female")
  expect_true(all(reporting$status == "success"))
  expect_true(all(c("p_lower", "p_median", "p_upper") %in% names(reporting)))
})

test_that("expected output inventory covers every historical consumer", {
  inventory <- expected_us_omicron_output_inventory()

  expect_equal(sum(inventory$output_family == "pscore_bar"), 32L)
  expect_equal(sum(inventory$output_family == "state_map"), 128L)
  expect_equal(sum(inventory$output_family == "table_01"), 4L)
  expect_setequal(inventory$result_version, c("historical", "corrected"))
  expect_false(any(grepl("initial1|initial2|omicron1|omicron2", inventory$path)))
})

test_that("bar rendering writes nonempty PDF and PNG files", {
  mapping <- utils::read.csv(
    here::here("config", "us_state_codes.csv"),
    stringsAsFactors = FALSE
  )
  panel_data <- standardize_us_wave_result(
    synthetic_us_wave_result(0.4),
    mapping
  )
  output <- tempfile(fileext = ".pdf")
  rendered <- render_us_wave_bar(
    panel_data,
    age_group = "65-84",
    sex = "F",
    state_codes = "CA",
    output_pdf = output
  )

  expect_true(all(file.exists(rendered)))
  expect_true(all(file.info(rendered)$size > 0L))
})

test_that("the all-output runner freezes canonical inputs and no model refit", {
  runner <- here::here("scripts", "reporting", "rebuild_us_omicron_outputs.R")
  expect_true(file.exists(runner))
  code <- paste(readLines(runner, warn = FALSE), collapse = "\n")
  expect_match(code, "install-canonical", fixed = TRUE)
  expect_match(code, "rerender-manuscript", fixed = TRUE)
  expect_false(grepl("BayesGP::model_fit", code, fixed = TRUE))

  bundle_code <- paste(readLines(
    here::here("scripts", "reporting", "prepare_manuscript_bundle.R"),
    warn = FALSE
  ), collapse = "\n")
  expect_match(
    bundle_code,
    "analysis_end = config$regions$us_sex$analysis_end",
    fixed = TRUE
  )
})
