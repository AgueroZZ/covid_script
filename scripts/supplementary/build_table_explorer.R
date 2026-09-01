#!/usr/bin/env Rscript

script_argument <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(script_argument) != 1L) {
  stop("Unable to determine the supplementary table builder path.")
}
script_path <- normalizePath(sub("^--file=", "", script_argument))
project_root <- normalizePath(file.path(dirname(script_path), "..", ".."))

suppressPackageStartupMessages({
  library(digest)
  library(jsonlite)
  library(yaml)
})

source(file.path(project_root, "R", "reporting.R"))
source(file.path(project_root, "R", "tables.R"))
source(file.path(project_root, "R", "supplementary_table_explorer.R"))

parse_arguments <- function(arguments) {
  defaults <- list(
    output_root = file.path(
      project_root,
      "output",
      "supplementary",
      "table_explorer_20260901_v2"
    ),
    core_root = file.path(
      project_root,
      "output",
      "supplementary",
      "frozen_20260831"
    ),
    force = "false"
  )
  if (length(arguments) == 0L) return(defaults)
  if (!all(grepl("^--[A-Za-z0-9_-]=?.+$", arguments))) {
    stop("Arguments must use --name=value syntax.")
  }
  parsed <- sub("^--", "", arguments)
  argument_names <- gsub("-", "_", sub("=.*$", "", parsed))
  values <- sub("^[^=]*=", "", parsed)
  unknown <- setdiff(argument_names, names(defaults))
  if (length(unknown) > 0L) {
    stop("Unknown arguments: ", paste(unknown, collapse = ", "), ".")
  }
  defaults[argument_names] <- as.list(values)
  defaults
}

parse_boolean <- function(value, label) {
  normalized <- tolower(value)
  if (!normalized %in% c("true", "false")) {
    stop(label, " must be true or false.")
  }
  identical(normalized, "true")
}

write_csv_atomic <- function(data, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  temporary <- tempfile(pattern = ".csv-", tmpdir = dirname(path))
  on.exit(unlink(temporary), add = TRUE)
  utils::write.csv(
    data,
    temporary,
    row.names = FALSE,
    na = "NA",
    fileEncoding = "UTF-8"
  )
  if (!file.rename(temporary, path)) {
    stop("Failed to atomically install CSV: ", path, ".")
  }
  invisible(path)
}

write_json <- function(object, path, pretty = FALSE) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(
    object,
    path,
    auto_unbox = TRUE,
    dataframe = "rows",
    na = "null",
    null = "null",
    digits = 10,
    pretty = pretty
  )
  invisible(path)
}

build_manifest <- function(root) {
  paths <- list.files(
    root,
    recursive = TRUE,
    full.names = TRUE,
    all.files = FALSE
  )
  paths <- paths[file.info(paths)$isdir %in% FALSE]
  relative <- substring(paths, nchar(root) + 2L)
  keep <- !relative %in% c("manifest.csv", "complete.flag")
  paths <- paths[keep]
  relative <- relative[keep]
  data.frame(
    path = relative,
    bytes = as.numeric(file.info(paths)$size),
    sha256 = vapply(
      paths,
      digest::digest,
      character(1L),
      algo = "sha256",
      file = TRUE,
      serialize = FALSE
    ),
    stringsAsFactors = FALSE
  )
}

column_contract <- function() {
  data.frame(
    key = c(
      "region_set",
      "geography_display",
      "people_vaccinated_per_hundred",
      "vaccination_group",
      "vaccination_measurement_date",
      "estimand_age_group",
      "frequency",
      supplementary_table_wave_levels()
    ),
    label = c(
      "Region",
      "Geography",
      "Vaccinated per hundred",
      "Vaccination group",
      "Vaccination date",
      "Age group",
      "Frequency",
      "Initial",
      "Alpha",
      "Delta",
      "Omicron"
    ),
    type = c(
      rep("text", 2L),
      "number",
      rep("text", 8L)
    ),
    stringsAsFactors = FALSE
  )
}

download_column_contract <- function() {
  data.frame(
    key = c(
      "region_set",
      "analysis_family",
      "population_view",
      "geography",
      "geography_label",
      "geography_display",
      "people_vaccinated_per_hundred",
      "vaccination_group",
      "vaccination_measurement_date",
      "estimand_age_group",
      "estimand_sex_group",
      "frequency",
      supplementary_table_wave_levels()
    ),
    label = c(
      "Region",
      "Analysis family",
      "Population view",
      "Geography key",
      "Geography label",
      "Geography",
      "Vaccinated per hundred",
      "Vaccination group",
      "Vaccination date",
      "Age group",
      "Estimand sex group",
      "Frequency",
      "Initial",
      "Alpha",
      "Delta",
      "Omicron"
    ),
    type = c(
      rep("text", 6L),
      "number",
      rep("text", 9L)
    ),
    stringsAsFactors = FALSE
  )
}

arguments <- parse_arguments(commandArgs(trailingOnly = TRUE))
force <- parse_boolean(arguments$force, "--force")
output_root <- normalizePath(
  arguments$output_root,
  winslash = "/",
  mustWork = FALSE
)
core_root <- normalizePath(arguments$core_root, winslash = "/", mustWork = TRUE)
dir.create(dirname(output_root), recursive = TRUE, showWarnings = FALSE)

input_paths <- c(
  frozen_wave_summary = file.path(core_root, "wave_summary.csv"),
  europe_vaccination = file.path(
    project_root,
    "output",
    "data",
    "europe",
    "vaccination_membership.csv"
  ),
  us_vaccination = file.path(
    project_root,
    "output",
    "data",
    "us",
    "vaccination_membership.csv"
  ),
  manuscript_table_1 = file.path(
    project_root,
    "tables",
    "manuscript",
    "table_01_wave_pscores.csv"
  ),
  table_helper = file.path(
    project_root,
    "R",
    "supplementary_table_explorer.R"
  ),
  builder = script_path
)
reporting_require_files(input_paths, "supplementary table input")
reporting_require_files(
  file.path(core_root, c("complete.flag", "manifest.csv")),
  "completed core supplementary freeze"
)

if (file.exists(file.path(output_root, "complete.flag")) && !force) {
  stop(
    "A completed supplementary table freeze already exists at ",
    output_root,
    ". Use --force=true only for an intentional replacement."
  )
}
if (dir.exists(output_root)) {
  if (!force) {
    stop("An incomplete output directory already exists: ", output_root, ".")
  }
  backup <- paste0(
    output_root,
    ".superseded_",
    format(Sys.time(), "%Y%m%dT%H%M%S")
  )
  if (!file.rename(output_root, backup)) {
    stop("Failed to preserve the previous supplementary table freeze.")
  }
  message("Preserved the previous supplementary table freeze at ", backup, ".")
}

staging_root <- tempfile(
  pattern = paste0(basename(output_root), ".building-"),
  tmpdir = dirname(output_root)
)
dir.create(staging_root, recursive = TRUE, showWarnings = FALSE)
completed <- FALSE
on.exit({
  if (!completed && dir.exists(staging_root)) {
    unlink(staging_root, recursive = TRUE)
  }
}, add = TRUE)

wave_summary <- utils::read.csv(
  input_paths[["frozen_wave_summary"]],
  stringsAsFactors = FALSE,
  check.names = FALSE
)
europe_vaccination <- utils::read.csv(
  input_paths[["europe_vaccination"]],
  stringsAsFactors = FALSE,
  check.names = FALSE
)
us_vaccination <- utils::read.csv(
  input_paths[["us_vaccination"]],
  stringsAsFactors = FALSE,
  check.names = FALSE
)
manuscript <- utils::read.csv(
  input_paths[["manuscript_table_1"]],
  stringsAsFactors = FALSE,
  check.names = FALSE
)

expanded <- build_supplementary_table_explorer(
  wave_summary,
  europe_vaccination,
  us_vaccination,
  manuscript_table = manuscript
)
equivalence <- validate_supplementary_table_against_manuscript(
  expanded,
  manuscript
)

expanded_path <- file.path(staging_root, "expanded_table_01.csv")
equivalence_path <- file.path(staging_root, "manuscript_equivalence.csv")
write_csv_atomic(expanded, expanded_path)
write_csv_atomic(equivalence, equivalence_path)

browser_root <- file.path(staging_root, "browser", "table_explorer")
downloads_root <- file.path(browser_root, "downloads")
dir.create(downloads_root, recursive = TRUE, showWarnings = FALSE)
visible_columns <- column_contract()
download_columns <- download_column_contract()
if (anyDuplicated(visible_columns$key) || anyDuplicated(download_columns$key)) {
  stop("The supplementary table column contracts contain duplicated keys.")
}
if (!all(visible_columns$key %in% download_columns$key) ||
    !all(download_columns$key %in% names(expanded))) {
  stop("The supplementary table column contracts do not match the frozen data.")
}
public_rows <- expanded[download_columns$key]
public_expanded_path <- file.path(downloads_root, "expanded_table_01.csv")
write_csv_atomic(public_rows, public_expanded_path)

region_counts <- as.data.frame(
  table(factor(
    expanded$region_set,
    levels = unique(supplementary_table_region_contract()$region_set)
  )),
  stringsAsFactors = FALSE
)
names(region_counts) <- c("region_set", "rows")
view_counts <- as.data.frame(
  table(factor(
    expanded$population_view,
    levels = c("Sex-stratified (M/F)", "Total population")
  )),
  stringsAsFactors = FALSE
)
names(view_counts) <- c("population_view", "rows")

created_at_utc <- format(Sys.time(), tz = "UTC", usetz = TRUE)
input_hashes <- as.list(vapply(
  input_paths,
  digest::digest,
  character(1L),
  algo = "sha256",
  file = TRUE,
  serialize = FALSE
))
metadata <- list(
  schema_version = "1.1.0",
  frozen_on = "2026-09-01",
  created_at_utc = created_at_utc,
  model_refitting_performed = FALSE,
  public_bundle_contains_posterior_draws = FALSE,
  public_bundle_contains_fitted_models = FALSE,
  expanded_rows = nrow(expanded),
  manuscript_rows = nrow(manuscript),
  manuscript_rows_exact = sum(equivalence$exact_match),
  partial_rows = sum(expanded$result_status == "partial"),
  regions = region_counts,
  input_hashes = input_hashes,
  session_info = paste(capture.output(utils::sessionInfo()), collapse = "\n")
)
metadata_path <- file.path(staging_root, "bundle_metadata.yml")
yaml::write_yaml(metadata, metadata_path)

public_metadata <- list(
  schema_version = "1.1.0",
  frozen_on = "2026-09-01",
  created_at_utc = created_at_utc,
  model_refitting_performed = FALSE,
  public_bundle_contains_posterior_draws = FALSE,
  public_bundle_contains_fitted_models = FALSE,
  expanded_rows = nrow(public_rows),
  population_views = view_counts,
  partial_rows = sum(expanded$result_status == "partial"),
  regions = region_counts
)
yaml::write_yaml(
  public_metadata,
  file.path(downloads_root, "bundle_metadata.yml")
)

index <- list(
  schema_version = "1.1.0",
  frozen_on = "2026-09-01",
  created_at_utc = created_at_utc,
  row_count = nrow(public_rows),
  population_views = view_counts,
  regions = region_counts,
  columns = visible_columns,
  download_columns = download_columns,
  rows = public_rows,
  downloads = list(
    expanded_table = "downloads/expanded_table_01.csv",
    metadata = "downloads/bundle_metadata.yml"
  )
)
write_json(index, file.path(browser_root, "index.json"))

validation_summary <- list(
  status = "passed",
  expanded_rows = nrow(expanded),
  unique_row_keys = length(unique(supplementary_table_key(expanded))),
  manuscript_rows = nrow(manuscript),
  manuscript_rows_exact = sum(equivalence$exact_match),
  vaccination_na_rows = sum(is.na(expanded$people_vaccinated_per_hundred)),
  paired_sex_rows = sum(expanded$estimand_sex_group == "Male and female"),
  total_rows = sum(expanded$estimand_sex_group == "Total"),
  partial_rows = sum(expanded$result_status == "partial"),
  posterior_draws_published = FALSE,
  fitted_models_published = FALSE,
  completed_at_utc = created_at_utc
)
saveRDS(validation_summary, file.path(staging_root, "validation_summary.rds"))
manifest <- build_manifest(staging_root)
write_csv_atomic(manifest, file.path(staging_root, "manifest.csv"))
writeLines("complete", file.path(staging_root, "complete.flag"))

if (!file.rename(staging_root, output_root)) {
  stop("Failed to install the completed supplementary table freeze.")
}
completed <- TRUE
message(
  "Supplementary table freeze completed: ", output_root, "\n",
  "Expanded rows: ", validation_summary$expanded_rows, "\n",
  "Exact manuscript rows: ", validation_summary$manuscript_rows_exact, "\n",
  "Rows with vaccination NA: ", validation_summary$vaccination_na_rows, "\n",
  "Partial result rows: ", validation_summary$partial_rows
)
