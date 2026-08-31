#!/usr/bin/env Rscript

arguments <- commandArgs(trailingOnly = TRUE)
if (length(arguments) < 1L || length(arguments) > 2L) {
  stop(
    "Usage: validate_us_historical.R ARCHIVE_NORTH_AMERICA_ROOT [OUTPUT_DIR]"
  )
}

archive_root <- normalizePath(arguments[[1]], mustWork = TRUE)
output_dir <- if (length(arguments) == 2L) {
  arguments[[2]]
} else {
  file.path("output", "results", "us", "validation")
}
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

source("R/validation.R")
source("R/us_data.R")
source("R/us_historical.R")

new_sex <- standardize_us_wonder(
  us_sex_source_paths(),
  stratified_by_sex = TRUE
)
new_non_sex <- standardize_us_wonder(
  us_non_sex_source_paths(),
  stratified_by_sex = FALSE
)
historical_sex <- canonicalize_historical_us_data(
  file.path(archive_root, "sex-stratified", "USA", "USA_monthly.rda"),
  stratified_by_sex = TRUE
)
historical_non_sex <- canonicalize_historical_us_data(
  file.path(archive_root, "non-stratified", "USA", "USA_monthly.rda"),
  stratified_by_sex = FALSE
)

data_comparison <- tibble::tibble(
  analysis_path = c("us_sex", "us_non_sex"),
  new_rows = c(nrow(new_sex), nrow(new_non_sex)),
  historical_rows = c(nrow(historical_sex), nrow(historical_non_sex)),
  exact_match = c(
    us_data_values_match(
      canonical_us_comparison_columns(new_sex),
      historical_sex
    ),
    us_data_values_match(
      canonical_us_comparison_columns(new_non_sex),
      historical_non_sex
    )
  )
)
archive_inventory <- inventory_us_historical_archive(archive_root)

readr::write_csv(
  data_comparison,
  file.path(output_dir, "standardized_data_comparison.csv")
)
readr::write_csv(
  archive_inventory,
  file.path(output_dir, "historical_artifact_inventory.csv")
)

if (!all(data_comparison$exact_match) ||
    !all(archive_inventory$matches_expected)) {
  stop("One or more US historical validation checks failed.")
}
cat("US historical data and artifact inventory validation passed.\n")
