#!/usr/bin/env Rscript

if (!requireNamespace("digest", quietly = TRUE)) {
  stop("Package 'digest' is required to build the snapshot manifest.")
}

source("R/validation.R")
source("R/raw_manifest.R")
source("R/us_data.R")

sources <- utils::read.csv(
  file.path("config", "data_sources.csv"),
  stringsAsFactors = FALSE,
  check.names = FALSE
)

snapshot_row <- function(
  dataset_id,
  snapshot_path,
  snapshot_date,
  time_coverage,
  access_method
) {
  source_row <- sources[sources$dataset_id == dataset_id, , drop = FALSE]
  if (nrow(source_row) != 1L) {
    stop("Expected one data-source registry row for ", dataset_id, ".")
  }
  data.frame(
    dataset_id = dataset_id,
    provider = source_row$provider,
    snapshot_path = snapshot_path,
    snapshot_date = snapshot_date,
    bytes = as.numeric(file.info(snapshot_path)$size),
    sha256 = digest::digest(
      snapshot_path,
      algo = "sha256",
      file = TRUE,
      serialize = FALSE
    ),
    source_url = source_row$source_url,
    provider_identifier = source_row$provider_identifier,
    access_method = access_method,
    geography = source_row$geography,
    time_coverage = time_coverage,
    license_note = source_row$license_or_terms,
    reproduction_role = source_row$reproduction_role,
    tracked_in_git = TRUE,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

rows <- list(
  snapshot_row(
    "eurostat_weekly",
    "data/raw/eurostat/demo_r_mwk_20_linear.csv",
    "2024-05-22",
    "2000-W01 to 2024-W19",
    "sdmx_csv_export"
  ),
  snapshot_row(
    "statcan_weekly",
    "data/raw/statcan/13100768.csv",
    "2025-10-09",
    "2010-01-09 to 2023-08-05",
    "full_table_csv_export"
  ),
  snapshot_row(
    "ons_occurrence_1981_2020",
    "data/raw/ons/daily_deaths_occurrences_1981_2020.xlsx",
    "2022-07-06",
    "1981-01-01 to 2020-12-31",
    "provider_xlsx_download"
  ),
  snapshot_row(
    "ons_registration_2021",
    "data/raw/ons/weekly_deaths_2021_week52.xlsx",
    "2022-01-04",
    "2021 week 01 to week 52",
    "provider_xlsx_download"
  ),
  snapshot_row(
    "ons_registration_2022",
    "data/raw/ons/weekly_deaths_2022_week52.xlsx",
    "2023-01-10",
    "2022 week 01 to week 52",
    "provider_xlsx_download"
  ),
  snapshot_row(
    "ons_registration_2023",
    "data/raw/ons/weekly_deaths_2023_week35.xlsx",
    "2023-09-19",
    "2023 week 01 to week 35",
    "provider_xlsx_download"
  ),
  snapshot_row(
    "cso_quarterly",
    "data/raw/cso/ireland_quarterly_deaths.csv",
    "2023-10-18",
    "2010Q1 to 2023Q1",
    "pxstat_csv_export"
  ),
  snapshot_row(
    "owid_europe_vaccination",
    "data/raw/owid/europe_vaccination_snapshot.rda",
    "2025-10-09",
    "reference date 2021-07-01",
    "archived_csv_filtered_to_reference_date"
  ),
  snapshot_row(
    "owid_us_state_vaccination",
    "data/raw/owid/us_state_vaccination_snapshot.rda",
    "2025-11-18",
    "reference date 2021-07-01",
    "archived_csv_filtered_to_reference_date"
  ),
  snapshot_row(
    "us_census_boundary",
    "data/raw/us_census/cb_2018_us_state_500k.zip",
    "2025-11-18",
    "2018 cartographic boundary vintage",
    "direct_zip_download"
  )
)

for (path in us_sex_source_paths()) {
  years <- sub("^.*([0-9]{4}-[0-9]{4}).*$", "\\1", basename(path))
  rows[[length(rows) + 1L]] <- snapshot_row(
    "cdc_wonder_sex",
    path,
    "2025-11-18",
    years,
    "manual_saved_query_export"
  )
}
for (path in us_non_sex_source_paths()) {
  years <- sub("^.*([0-9]{4}_[0-9]{4}).*$", "\\1", basename(path))
  years <- gsub("_", "-", years, fixed = TRUE)
  rows[[length(rows) + 1L]] <- snapshot_row(
    "cdc_wonder_non_sex",
    path,
    "2025-11-18",
    years,
    "manual_saved_query_export"
  )
}

manifest <- do.call(rbind, rows)
manifest <- manifest[order(manifest$dataset_id, manifest$snapshot_path), , drop = FALSE]
row.names(manifest) <- NULL

manifest_path <- file.path("data", "raw", "manifest.csv")
utils::write.csv(manifest, manifest_path, row.names = FALSE, na = "")
validate_file_manifest(manifest, root = ".")

message("Registered and verified ", nrow(manifest), " raw snapshot files.")
