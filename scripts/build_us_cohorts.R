#!/usr/bin/env Rscript

source("R/config.R")
source("R/waves.R")
source("R/validation.R")
source("R/us_data.R")
source("R/us_cohorts.R")

config <- read_analysis_config()
sex_data <- us_model_input(
  standardize_us_wonder(us_sex_source_paths(), stratified_by_sex = TRUE),
  config$regions$us_sex$analysis_end
)
non_sex_data <- us_model_input(
  standardize_us_wonder(
    us_non_sex_source_paths(),
    stratified_by_sex = FALSE
  ),
  config$regions$us_non_sex$analysis_end
)

registry <- dplyr::bind_rows(
  cohort_registry_rows(build_us_cohort_inventory(
    sex_data,
    analysis_path = "us_sex",
    config = config
  )),
  cohort_registry_rows(build_us_cohort_inventory(
    non_sex_data,
    analysis_path = "us_non_sex",
    config = config
  ))
) |>
  dplyr::arrange(region, jurisdiction, age_group, sex)

readr::write_csv(registry, "config/cohorts.csv", na = "")
cat("Registered ", nrow(registry), " US model cohorts.\n", sep = "")
