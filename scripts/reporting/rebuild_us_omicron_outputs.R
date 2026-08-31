#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(digest)
  library(dplyr)
  library(ggplot2)
  library(here)
  library(readr)
  library(sf)
  library(tidyr)
})

source(here::here("R", "reporting.R"))
source(here::here("R", "tables.R"))
source(here::here("R", "us_omicron_outputs.R"))

parse_arguments <- function(defaults, arguments = commandArgs(trailingOnly = TRUE)) {
  if (length(arguments) == 0L) return(defaults)
  valid <- grepl("^--[A-Za-z0-9_-]+=.+$", arguments)
  if (!all(valid)) stop("Arguments must use --name=value syntax.")
  parsed <- sub("^--", "", arguments)
  names_parsed <- gsub("-", "_", sub("=.*$", "", parsed), fixed = TRUE)
  values <- sub("^[^=]*=", "", parsed)
  unknown <- setdiff(names_parsed, names(defaults))
  if (length(unknown) > 0L) {
    stop("Unknown arguments: ", paste(unknown, collapse = ", "), ".")
  }
  defaults[names_parsed] <- as.list(values)
  defaults
}

parse_boolean <- function(value, name) {
  normalized <- tolower(value)
  if (!normalized %in% c("true", "false")) {
    stop("--", name, " must be true or false.")
  }
  identical(normalized, "true")
}

sha256_file <- function(path) {
  digest::digest(path, algo = "sha256", file = TRUE, serialize = FALSE)
}

file_record <- function(path, role, root = NULL) {
  if (!file.exists(path)) stop("Cannot inventory missing file: ", path, ".")
  normalized <- normalizePath(path, winslash = "/", mustWork = TRUE)
  relative <- normalized
  if (!is.null(root)) {
    normalized_root <- paste0(normalizePath(root, winslash = "/", mustWork = TRUE), "/")
    relative <- sub(paste0("^", normalized_root), "", normalized)
  }
  data.frame(
    role = role,
    path = relative,
    bytes = unname(file.info(path)$size),
    sha256 = sha256_file(path),
    stringsAsFactors = FALSE
  )
}

copy_once <- function(source, destination, verify_existing = TRUE) {
  if (!file.exists(source)) stop("Baseline source does not exist: ", source, ".")
  dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)
  if (file.exists(destination)) {
    if (verify_existing &&
        !identical(sha256_file(source), sha256_file(destination))) {
      stop("Existing immutable baseline differs from source: ", destination, ".")
    }
    return(destination)
  }
  copied <- file.copy(source, destination, overwrite = FALSE, copy.date = TRUE)
  if (!isTRUE(copied)) stop("Could not preserve baseline: ", source, ".")
  destination
}

copy_directory_files_once <- function(source_directory, destination_directory) {
  sources <- list.files(source_directory, full.names = TRUE, recursive = TRUE)
  sources <- sources[file.info(sources)$isdir %in% FALSE]
  vapply(sources, function(source) {
    relative <- substring(
      normalizePath(source, winslash = "/", mustWork = TRUE),
      nchar(normalizePath(source_directory, winslash = "/", mustWork = TRUE)) + 2L
    )
    copy_once(
      source,
      file.path(destination_directory, relative),
      verify_existing = FALSE
    )
  }, character(1))
}

write_standardized_result <- function(data, directory) {
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  csv <- file.path(directory, "us_wave_summary_full.csv")
  rds <- file.path(directory, "us_wave_summary_full.rds")
  readr::write_csv(data, csv)
  saveRDS(data, rds, version = 3)
  c(csv, rds)
}

compare_reporting_summary <- function(reference, candidate) {
  keys <- c("analysis_path", "geography", "age_group", "sex", "wave", "status")
  values <- c("p_lower", "p_median", "p_upper")
  reference <- reference[do.call(order, reference[keys]), c(keys, values)]
  candidate <- candidate[do.call(order, candidate[keys]), c(keys, values)]
  if (!isTRUE(all.equal(
    reference[keys],
    candidate[keys],
    tolerance = 0,
    check.attributes = FALSE
  ))) {
    stop("Corrected Table 1 cohort keys differ from the installed canonical summary.")
  }
  comparison <- dplyr::inner_join(
    reference,
    candidate,
    by = keys,
    suffix = c("_installed_before", "_corrected")
  )
  for (value in values) {
    comparison[[paste0(value, "_change")]] <-
      comparison[[paste0(value, "_corrected")]] -
      comparison[[paste0(value, "_installed_before")]]
  }
  non_omicron <- comparison$wave != "omicron"
  changed_non_omicron <- vapply(values, function(value) {
    any(abs(comparison[[paste0(value, "_change")]][non_omicron]) > 1e-15)
  }, logical(1))
  if (any(changed_non_omicron)) {
    stop("The installed and corrected Table 1 summaries differ outside Omicron.")
  }
  comparison
}

arguments <- parse_arguments(list(
  historical_result = file.path(
    dirname(here::here()), "covid_excess", "North_America", "sex-stratified",
    "USA", "USA_monthly_result.rda"
  ),
  corrected_result = file.path(
    dirname(here::here()), "covid_agents", "validation", "results",
    "issue_01_us_omicron", "USA_monthly_result_omicron_corrected.rda"
  ),
  output_root = here::here(
    "output", "validation", "us_omicron_all_outputs_20260830"
  ),
  install_canonical = "true",
  rerender_manuscript = "true"
))

install_canonical <- parse_boolean(arguments$install_canonical, "install-canonical")
rerender_manuscript <- parse_boolean(
  arguments$rerender_manuscript,
  "rerender-manuscript"
)
output_root <- arguments$output_root
dir.create(output_root, recursive = TRUE, showWarnings = FALSE)

state_codes_path <- here::here("config", "us_state_codes.csv")
bar_panels_path <- here::here("config", "us_omicron_bar_panels.csv")
table_cohort_path <- here::here("config", "us_table_01_cohort.csv")
geometry_path <- here::here("USA_analysis", "cb_2018_us_state_500k.zip")
canonical_us_path <- here::here("output", "results", "us", "wave_summary.csv")
baseline_canonical_path <- file.path(
  output_root,
  "baseline",
  "canonical",
  "us_wave_summary.csv"
)
europe_wave_path <- here::here("output", "results", "europe", "wave_summary.csv")
us_vaccination_path <- here::here(
  "output", "data", "us", "vaccination_membership.csv"
)
europe_vaccination_path <- here::here(
  "output", "data", "europe", "vaccination_membership.csv"
)
reporting_require_files(
  c(
    arguments$historical_result, arguments$corrected_result,
    state_codes_path, bar_panels_path, table_cohort_path, geometry_path,
    canonical_us_path, europe_wave_path,
    us_vaccination_path, europe_vaccination_path
  ),
  "US Omicron rebuild input"
)

message("Preserving source summaries and manuscript baselines.")
source_copies <- c(
  copy_once(
    arguments$historical_result,
    file.path(output_root, "sources", "USA_monthly_result_historical.rda")
  ),
  copy_once(
    arguments$corrected_result,
    file.path(output_root, "sources", "USA_monthly_result_corrected.rda")
  )
)
baseline_files <- c(
  copy_directory_files_once(
    here::here("output", "figures"),
    file.path(output_root, "baseline", "manuscript", "figures")
  ),
  copy_directory_files_once(
    here::here("output", "tables"),
    file.path(output_root, "baseline", "manuscript", "tables")
  ),
  copy_once(
    canonical_us_path,
    baseline_canonical_path,
    verify_existing = FALSE
  )
)

message("Loading and validating historical and corrected US wave summaries.")
historical <- standardize_us_wave_result(
  load_us_wave_result(arguments$historical_result),
  state_codes_path
)
corrected <- standardize_us_wave_result(
  load_us_wave_result(arguments$corrected_result),
  state_codes_path
)
validate_us_wave_result(historical, "historical")
validate_us_wave_result(corrected, "corrected")
comparison <- compare_us_omicron_results(historical, corrected)
if (!isTRUE(comparison$summary$non_omicron_exact)) {
  stop("Historical and corrected non-Omicron rows are not exactly identical.")
}
if (!isTRUE(comparison$summary$historical_omicron_duplicates_delta)) {
  stop("Historical Omicron rows do not reproduce the audited Delta duplication.")
}

standardized_files <- c(
  write_standardized_result(historical, file.path(output_root, "historical", "data")),
  write_standardized_result(corrected, file.path(output_root, "corrected", "data"))
)
comparison_directory <- file.path(output_root, "comparison")
dir.create(comparison_directory, recursive = TRUE, showWarnings = FALSE)
rowwise_comparison_path <- file.path(
  comparison_directory,
  "omicron_rowwise_comparison.csv"
)
summary_comparison_path <- file.path(comparison_directory, "omicron_summary.csv")
readr::write_csv(comparison$rowwise, rowwise_comparison_path)
readr::write_csv(comparison$summary, summary_comparison_path)

message("Rendering eight bar panels for each result version.")
panels <- readr::read_csv(bar_panels_path, show_col_types = FALSE)
render_bars <- function(data, version) {
  unlist(lapply(seq_len(nrow(panels)), function(index) {
    state_codes <- strsplit(panels$state_codes[[index]], ";", fixed = TRUE)[[1]]
    render_us_wave_bar(
      data,
      age_group = panels$age_group[[index]],
      sex = panels$sex[[index]],
      state_codes = state_codes,
      output_pdf = file.path(
        output_root,
        version,
        "pscore_bars",
        paste0(
          "pscore_bar_age_", us_age_slug(panels$age_group[[index]]),
          "_sex_", panels$sex[[index]], ".pdf"
        )
      )
    )
  }), use.names = FALSE)
}
bar_files <- c(
  render_bars(historical, "historical"),
  render_bars(corrected, "corrected")
)

message("Loading state geometry and rendering 64 historical/corrected map panels.")
geometry <- load_us_map_geometry(geometry_path)
render_maps <- function(data, version) {
  paths <- character()
  for (age_group in us_omicron_age_groups()) {
    for (sex in c("F", "M")) {
      for (wave in us_canonical_waves()) {
        paths <- c(paths, render_us_wave_map(
          data,
          geometry,
          age_group = age_group,
          sex = sex,
          wave = wave,
          output_pdf = file.path(
            output_root,
            version,
            "maps",
            paste0(
              "state_map_age_", us_age_slug(age_group),
              "_sex_", sex, "_wave_", wave, ".pdf"
            )
          )
        ))
      }
    }
  }
  unname(paths)
}
map_files <- c(
  render_maps(historical, "historical"),
  render_maps(corrected, "corrected")
)

message("Building controlled historical and corrected Table 1 outputs.")
canonical_before <- readr::read_csv(
  baseline_canonical_path,
  show_col_types = FALSE
)
table_cohort <- readr::read_csv(table_cohort_path, show_col_types = FALSE)$geography
if (!setequal(table_cohort, unique(canonical_before$geography))) {
  stop("The explicit US Table 1 cohort differs from the installed manuscript cohort.")
}
reporting_for_table <- function(data) {
  output <- as_reporting_us_wave_summary(data)
  output[
    output$age_group == "65-84" & output$geography %in% table_cohort,
  ]
}
historical_reporting <- reporting_for_table(historical)
corrected_reporting <- reporting_for_table(corrected)
canonical_comparison <- compare_reporting_summary(
  canonical_before,
  corrected_reporting
)
canonical_comparison_path <- file.path(
  comparison_directory,
  "installed_canonical_vs_corrected.csv"
)
readr::write_csv(canonical_comparison, canonical_comparison_path)

europe_wave <- readr::read_csv(europe_wave_path, show_col_types = FALSE)
europe_vaccination <- readr::read_csv(
  europe_vaccination_path,
  show_col_types = FALSE
)
us_vaccination <- readr::read_csv(us_vaccination_path, show_col_types = FALSE)
historical_table <- build_table_01(
  europe_wave, historical_reporting, europe_vaccination, us_vaccination
)
corrected_table <- build_table_01(
  europe_wave, corrected_reporting, europe_vaccination, us_vaccination
)
historical_table_files <- write_table_01(
  historical_table,
  file.path(output_root, "historical", "tables", "table_01_wave_pscores.csv"),
  file.path(output_root, "historical", "tables", "table_01_wave_pscores.html")
)
corrected_table_files <- write_table_01(
  corrected_table,
  file.path(output_root, "corrected", "tables", "table_01_wave_pscores.csv"),
  file.path(output_root, "corrected", "tables", "table_01_wave_pscores.html")
)
table_comparison <- dplyr::full_join(
  historical_table,
  corrected_table,
  by = c(
    "region_set", "geography", "people_vaccinated_per_hundred",
    "vaccination_group", "estimand_age_group"
  ),
  suffix = c("_historical", "_corrected")
) |>
  dplyr::mutate(
    omicron_changed = omicron_historical != omicron_corrected
  )
table_comparison_path <- file.path(comparison_directory, "table_01_comparison.csv")
readr::write_csv(table_comparison, table_comparison_path)

if (install_canonical) {
  message("Installing the corrected 11-state Table 1 summary.")
  dir.create(dirname(canonical_us_path), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(corrected_reporting, canonical_us_path)
}

if (rerender_manuscript) {
  if (!install_canonical) {
    stop("Manuscript rerendering requires --install-canonical=true.")
  }
  message("Rerendering all manuscript figures and Table 1.")
  status <- system2(
    file.path(R.home("bin"), "Rscript"),
    c("scripts/reporting/run_all.R", "--include_caption_review=true")
  )
  if (!identical(status, 0L)) {
    stop("Manuscript reporting runner failed with status ", status, ".")
  }
}

message("Verifying output inventory and writing provenance manifests.")
inventory <- expected_us_omicron_output_inventory()
inventory$absolute_path <- file.path(output_root, inventory$path)
missing_outputs <- inventory$absolute_path[!file.exists(inventory$absolute_path)]
if (length(missing_outputs) > 0L) {
  stop("Expected outputs are missing: ", paste(missing_outputs, collapse = ", "), ".")
}
inventory$bytes <- file.info(inventory$absolute_path)$size
inventory$sha256 <- vapply(inventory$absolute_path, sha256_file, character(1))
inventory$absolute_path <- NULL

manuscript_paths <- c(
  list.files(here::here("output", "figures"), full.names = TRUE),
  list.files(here::here("output", "tables"), full.names = TRUE)
)
manuscript_paths <- manuscript_paths[file.info(manuscript_paths)$isdir %in% FALSE]
manuscript_manifest <- dplyr::bind_rows(lapply(manuscript_paths, function(path) {
  record <- file_record(path, "corrected_canonical_manuscript")
  data.frame(
    result_version = "corrected_canonical",
    output_family = if (grepl("/figures/", path, fixed = TRUE)) "manuscript_figure" else "manuscript_table",
    age_group = NA_character_,
    sex = NA_character_,
    wave = NA_character_,
    format = tools::file_ext(path),
    path = record$path,
    bytes = record$bytes,
    sha256 = record$sha256,
    stringsAsFactors = FALSE
  )
}))
output_manifest <- dplyr::bind_rows(
  inventory[, c(
    "result_version", "output_family", "age_group", "sex", "wave",
    "format", "path", "bytes", "sha256"
  )],
  manuscript_manifest
)

source_paths <- c(
  historical_result = arguments$historical_result,
  corrected_result = arguments$corrected_result,
  state_codes = state_codes_path,
  bar_panels = bar_panels_path,
  table_cohort = table_cohort_path,
  state_geometry = geometry_path,
  canonical_us_before = baseline_canonical_path,
  europe_wave = europe_wave_path,
  us_vaccination = us_vaccination_path,
  europe_vaccination = europe_vaccination_path
)
source_manifest <- dplyr::bind_rows(lapply(names(source_paths), function(role) {
  file_record(source_paths[[role]], role)
}))

manifest_directory <- file.path(output_root, "manifests")
dir.create(manifest_directory, recursive = TRUE, showWarnings = FALSE)
source_manifest_path <- file.path(manifest_directory, "source_manifest.csv")
output_manifest_path <- file.path(manifest_directory, "output_manifest.csv")
readr::write_csv(source_manifest, source_manifest_path)
readr::write_csv(output_manifest, output_manifest_path)

all_generated <- list.files(output_root, recursive = TRUE, full.names = TRUE)
all_generated <- all_generated[file.info(all_generated)$isdir %in% FALSE]
all_generated <- all_generated[basename(all_generated) != "sha256.csv"]
sha256_manifest <- dplyr::bind_rows(lapply(all_generated, function(path) {
  file_record(path, "validation_artifact", root = output_root)
}))
sha256_path <- file.path(manifest_directory, "sha256.csv")
readr::write_csv(sha256_manifest, sha256_path)

required_controlled <- c(
  source_copies, baseline_files, standardized_files,
  rowwise_comparison_path, summary_comparison_path, table_comparison_path,
  canonical_comparison_path,
  bar_files, map_files, historical_table_files, corrected_table_files,
  source_manifest_path, output_manifest_path, sha256_path
)
if (any(!file.exists(required_controlled)) || any(file.info(required_controlled)$size <= 0L)) {
  stop("One or more controlled rebuild artifacts are missing or empty.")
}
writeLines(
  c(
    "US Omicron downstream-output rebuild completed successfully.",
    paste0("completed_at=", format(Sys.time(), tz = "UTC", usetz = TRUE)),
    paste0("core_output_count=", nrow(inventory)),
    paste0("manuscript_output_count=", nrow(manuscript_manifest))
  ),
  file.path(output_root, "complete.flag"),
  useBytes = TRUE
)

message("Completed US Omicron downstream-output rebuild: ", output_root)
