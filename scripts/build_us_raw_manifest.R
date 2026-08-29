#!/usr/bin/env Rscript

source("R/validation.R")
source("R/raw_manifest.R")
source("R/us_data.R")

paths <- c(
  us_sex_source_paths(),
  us_non_sex_source_paths(),
  file.path("USA_analysis", "us_state_vaccinations_select.rda"),
  file.path("USA_analysis", "cb_2018_us_state_500k.zip")
)
source_ids <- c(
  rep("cdc_wonder_sex", length(us_sex_source_paths())),
  rep("cdc_wonder_non_sex", length(us_non_sex_source_paths())),
  "owid_vaccination",
  "us_census_boundary"
)

us_manifest <- build_file_manifest(
  paths = paths,
  source_ids = source_ids,
  snapshot_dates = rep(as.Date("2025-11-18"), length(paths)),
  tracked_in_git = TRUE
)

manifest_path <- file.path("data", "raw", "manifest.csv")
existing <- readr::read_csv(manifest_path, show_col_types = FALSE)
existing <- existing[!existing$source_id %in% unique(source_ids), ]
manifest <- if (nrow(existing) == 0L) {
  us_manifest
} else {
  dplyr::bind_rows(existing, us_manifest)
}
manifest <- manifest |>
  dplyr::arrange(source_id, relative_path)

readr::write_csv(manifest, manifest_path)
validate_file_manifest(us_manifest)
cat("Registered ", nrow(us_manifest), " US raw source files.\n", sep = "")
