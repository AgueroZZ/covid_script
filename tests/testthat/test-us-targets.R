test_that("US target graph declares every pipeline stage", {
  manifest <- targets::tar_manifest(
    script = here::here("_targets.R"),
    callr_function = NULL
  )
  required <- c(
    "us_sex_raw_files",
    "us_non_sex_raw_files",
    "us_sex_standardized",
    "us_non_sex_standardized",
    "us_sex_cohorts",
    "us_non_sex_cohorts",
    "us_vaccination_membership",
    "us_sex_model_run",
    "us_non_sex_model_run",
    "us_sex_prediction_file",
    "us_non_sex_prediction_file",
    "us_wave_summary_file",
    "us_pointwise_summary_file",
    "us_sex_contrast_summary_file",
    "us_model_status_file",
    "us_model_smoke"
  )

  expect_true(all(required %in% manifest$name))
})
