source(here::here("R", "analysis_contract.R"))

test_that("final manuscript analysis contract has the required schema", {
  contract <- read_manuscript_analysis_contract()
  expect_identical(contract$contract_version, "1.0.0")
  expect_identical(contract$status, "frozen_for_submission")
  expect_setequal(
    names(contract$scientific_contract),
    c(
      "training",
      "endpoints",
      "waves",
      "vaccination",
      "age_bands",
      "reporting_estimands",
      "cohorts"
    )
  )
  expect_true(length(contract$registries) >= 9L)
})

test_that("final manuscript analysis contract validates across registries", {
  contract <- read_manuscript_analysis_contract()
  result <- validate_manuscript_analysis_contract(contract)
  expect_true(result$valid)
  expect_true(all(result$registry_hashes$hash_matches))
  expect_equal(result$summary$value[result$summary$check == "us_endpoint"], "2023-08-31")
  expect_equal(result$summary$value[result$summary$check == "adopted_outputs"], "6")
})

test_that("contract rejects endpoint drift", {
  contract <- read_manuscript_analysis_contract()
  contract$scientific_contract$endpoints$us_sex <- "2023-09-30"
  expect_error(
    validate_manuscript_analysis_contract(contract),
    "US sex endpoint"
  )
})

test_that("contract rejects registry hash drift", {
  contract <- read_manuscript_analysis_contract()
  contract$registries[[1]]$sha256 <- paste(rep("0", 64L), collapse = "")
  expect_error(
    validate_manuscript_analysis_contract(contract),
    "registry hash"
  )
})

test_that("vaccination reporting groups freeze every adopted high or low cohort", {
  groups <- readr::read_csv(
    here::here("config", "reporting_vaccination_groups.csv"),
    show_col_types = FALSE
  )
  expect_equal(nrow(groups), 28L)
  expect_equal(anyDuplicated(groups[c("region", "geography")]), 0L)
  expect_setequal(unique(groups$vaccination_group), c("high", "low"))
  expect_equal(sum(groups$region == "europe"), 15L)
  expect_equal(sum(groups$region == "us"), 13L)
})

test_that("Figure 5 missingness decision remains available-month reporting", {
  contract <- read_manuscript_analysis_contract()
  decision <- contract$scientific_contract$cohorts$us_figure_05_missingness
  expect_identical(
    decision$rule,
    "common_available_months_within_vaccination_group"
  )
  expect_identical(decision$production_exclusions, list())
  expect_setequal(decision$accepted_incomplete_geographies, c("Idaho", "New Mexico"))
})
