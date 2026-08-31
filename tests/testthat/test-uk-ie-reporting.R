reporting_path <- testthat::test_path("..", "..", "R", "uk_ie_reporting.R")
registry_path <- testthat::test_path(
  "..", "..", "config", "uk_ie_reporting_cohort.csv"
)

test_that("UK and Ireland approximate age mappings are explicit", {
  source(reporting_path, local = TRUE)
  registry <- read_uk_ie_age_mapping(registry_path)

  expect_equal(nrow(registry), 8L)
  expect_true(all(registry$mapping_type == "approximate"))
  expect_true(all(nzchar(registry$disclosure)))
})

test_that("England-and-Wales Under 65 duplication is declared", {
  source(reporting_path, local = TRUE)
  registry <- read_uk_ie_age_mapping(registry_path)
  selected <- registry[
    registry$geography == "England and Wales" &
      registry$source_age_group == "Under 65",
    ,
    drop = FALSE
  ]

  expect_equal(nrow(selected), 2L)
  expect_setequal(selected$display_age_group, c("Y20-39", "Y40-59"))
})

test_that("a mapping join retains source and display age groups", {
  source(reporting_path, local = TRUE)
  registry <- read_uk_ie_age_mapping(registry_path)
  summary <- data.frame(
    geography = "England and Wales",
    source_age_group = "Under 65",
    wave = "initial",
    p_med = 0.1,
    stringsAsFactors = FALSE
  )
  mapped <- apply_uk_ie_age_mapping(summary, registry)

  expect_equal(nrow(mapped), 2L)
  expect_true(all(mapped$source_age_group == "Under 65"))
  expect_setequal(mapped$display_age_group, c("Y20-39", "Y40-59"))
  expect_true(all(mapped$mapping_type == "approximate"))
})

test_that("undeclared source bands are rejected", {
  source(reporting_path, local = TRUE)
  registry <- read_uk_ie_age_mapping(registry_path)
  summary <- data.frame(
    geography = "England and Wales",
    source_age_group = "20-39",
    stringsAsFactors = FALSE
  )

  expect_error(
    apply_uk_ie_age_mapping(summary, registry),
    "undeclared source age group"
  )
})

test_that("UK and Ireland wave summaries retain mapping provenance", {
  source(reporting_path, local = TRUE)
  registry <- read_uk_ie_age_mapping(registry_path)
  summary <- data.frame(
    geography = "Republic of Ireland",
    source_age_group = "45-64",
    wave = "initial",
    wave_start = as.Date("2020-03-01"),
    wave_end_exclusive = as.Date("2020-11-01"),
    p_lower = 0.01,
    p_med = 0.02,
    p_upper = 0.03,
    observed_deaths = 100L,
    posterior_draws = 3000L,
    stringsAsFactors = FALSE
  )
  standardized <- standardize_uk_ie_wave_summary(summary, registry)

  expect_identical(standardized$analysis_path, "ireland_quarterly")
  expect_identical(standardized$geography, "IE")
  expect_identical(standardized$source_age_group, "45-64")
  expect_identical(standardized$age_group, "40-59")
  expect_identical(standardized$mapping_type, "approximate")
})
