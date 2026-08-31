#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(digest)
  library(dplyr)
  library(here)
  library(readr)
  library(sf)
  library(tidyr)
})

source(here::here("R", "config.R"))
source(here::here("R", "waves.R"))
source(here::here("R", "reporting.R"))
source(here::here("R", "figures.R"))
source(here::here("R", "tables.R"))
source(here::here("R", "europe_reporting.R"))

script_argument <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(script_argument) != 1L) {
  stop("Unable to determine the corrected Europe reporting script path.")
}
script_path <- normalizePath(sub("^--file=", "", script_argument))

parse_corrected_europe_arguments <- function(arguments) {
  defaults <- list(
    refit_root = here::here(
      "output",
      "results",
      "europe_corrected_psd_prior_20260830"
    ),
    comparison_root = here::here(
      "output",
      "reporting",
      "validation",
      "europe_corrected_psd_prior_20260830"
    ),
    render = "true"
  )
  if (length(arguments) == 0L) {
    return(defaults)
  }
  if (!all(grepl("^--[A-Za-z0-9_-]+=.+$", arguments))) {
    stop("Arguments must use --name=value syntax.")
  }
  parsed <- sub("^--", "", arguments)
  argument_names <- gsub("-", "_", sub("=.*$", "", parsed), fixed = TRUE)
  values <- sub("^[^=]*=", "", parsed)
  unknown <- setdiff(argument_names, names(defaults))
  if (length(unknown) > 0L) {
    stop("Unknown arguments: ", paste(unknown, collapse = ", "), ".")
  }
  defaults[argument_names] <- as.list(values)
  defaults
}

parse_corrected_europe_boolean <- function(value, label) {
  normalized <- tolower(value)
  if (!normalized %in% c("true", "false")) {
    stop(label, " must be true or false.")
  }
  identical(normalized, "true")
}

sha256_file <- function(path) {
  if (!file.exists(path)) {
    return(NA_character_)
  }
  unname(digest::digest(file = path, algo = "sha256"))
}

copy_immutable <- function(source, destination) {
  if (!file.exists(source)) {
    stop("Historical artifact is unavailable: ", source, ".")
  }
  dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)
  if (file.exists(destination)) {
    return(invisible(destination))
  }
  if (!file.copy(source, destination, copy.mode = TRUE)) {
    stop("Failed to archive historical artifact: ", source, ".")
  }
  invisible(destination)
}

copy_corrected <- function(source, destination) {
  if (!file.exists(source)) {
    stop("Corrected artifact is unavailable: ", source, ".")
  }
  dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)
  if (!file.copy(source, destination, overwrite = TRUE, copy.mode = TRUE)) {
    stop("Failed to copy corrected artifact: ", source, ".")
  }
  invisible(destination)
}

atomic_save_rds <- function(object, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  temporary <- tempfile(pattern = ".corrected-rds-", tmpdir = dirname(path))
  on.exit(unlink(temporary), add = TRUE)
  saveRDS(object, temporary, compress = "gzip")
  if (!file.rename(temporary, path)) {
    stop("Failed to atomically install corrected RDS: ", path, ".")
  }
  invisible(path)
}

atomic_write_csv <- function(data, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  temporary <- tempfile(pattern = ".corrected-csv-", tmpdir = dirname(path))
  on.exit(unlink(temporary), add = TRUE)
  readr::write_csv(data, temporary)
  if (!file.rename(temporary, path)) {
    stop("Failed to atomically install corrected CSV: ", path, ".")
  }
  invisible(path)
}

archive_installed_artifacts <- function(comparison_root) {
  historical_root <- file.path(comparison_root, "historical")
  artifacts <- c(
    "inputs/figure_01_model_illustration.rds" = here::here(
      "output", "reporting", "inputs", "figure_01_model_illustration.rds"
    ),
    "inputs/figure_02_europe_maps.rds" = here::here(
      "output", "reporting", "inputs", "figure_02_europe_maps.rds"
    ),
    "inputs/figure_04_vaccination_pscore.csv" = here::here(
      "output", "reporting", "inputs", "figure_04_vaccination_pscore.csv"
    ),
    "inputs/figure_05_sex_difference.csv" = here::here(
      "output", "reporting", "inputs", "figure_05_sex_difference.csv"
    ),
    "inputs/europe_wave_summary.csv" = here::here(
      "output", "results", "europe", "wave_summary.csv"
    ),
    "outputs/figure_01_model_illustration.pdf" = here::here(
      "output", "figures", "figure_01_model_illustration.pdf"
    ),
    "outputs/figure_01_model_illustration.png" = here::here(
      "output", "figures", "figure_01_model_illustration.png"
    ),
    "outputs/figure_02_europe_maps.pdf" = here::here(
      "output", "figures", "figure_02_europe_maps.pdf"
    ),
    "outputs/figure_02_europe_maps.png" = here::here(
      "output", "figures", "figure_02_europe_maps.png"
    ),
    "outputs/figure_04_vaccination_pscore.pdf" = here::here(
      "output", "figures", "figure_04_vaccination_pscore.pdf"
    ),
    "outputs/figure_04_vaccination_pscore.png" = here::here(
      "output", "figures", "figure_04_vaccination_pscore.png"
    ),
    "outputs/figure_05_sex_difference.pdf" = here::here(
      "output", "figures", "figure_05_sex_difference.pdf"
    ),
    "outputs/figure_05_sex_difference.png" = here::here(
      "output", "figures", "figure_05_sex_difference.png"
    ),
    "outputs/table_01_wave_pscores.csv" = here::here(
      "output", "tables", "table_01_wave_pscores.csv"
    ),
    "outputs/table_01_wave_pscores.html" = here::here(
      "output", "tables", "table_01_wave_pscores.html"
    )
  )
  for (relative_path in names(artifacts)) {
    copy_immutable(
      artifacts[[relative_path]],
      file.path(historical_root, relative_path)
    )
  }
  data.frame(
    relative_path = names(artifacts),
    installed_path = unname(artifacts),
    historical_path = file.path(historical_root, names(artifacts)),
    historical_sha256 = vapply(
      file.path(historical_root, names(artifacts)),
      sha256_file,
      character(1)
    ),
    stringsAsFactors = FALSE
  )
}

compare_component_input <- function(historical, corrected) {
  components <- c("overall", "trend", "seasonal")
  dplyr::bind_rows(lapply(components, function(component) {
    old <- historical[[component]] |>
      dplyr::transmute(
        date = as.Date(date),
        historical_mean = mean,
        historical_lower = lower,
        historical_upper = upper
      )
    new <- corrected[[component]] |>
      dplyr::transmute(
        date = as.Date(date),
        corrected_mean = mean,
        corrected_lower = lower,
        corrected_upper = upper
      )
    dplyr::full_join(old, new, by = "date") |>
      dplyr::mutate(
        component = component,
        key_status = dplyr::case_when(
          is.na(historical_mean) ~ "added",
          is.na(corrected_mean) ~ "removed",
          TRUE ~ "common"
        ),
        mean_difference = corrected_mean - historical_mean,
        lower_difference = corrected_lower - historical_lower,
        upper_difference = corrected_upper - historical_upper,
        .before = 1
      )
  }))
}

compare_map_input <- function(historical, corrected) {
  old <- sf::st_drop_geometry(historical$map_data) |>
    dplyr::transmute(
      label = dplyr::recode(label, EL = "GR"),
      age_group,
      wave,
      historical_p_median = p_median
    )
  new <- sf::st_drop_geometry(corrected$map_data) |>
    dplyr::transmute(
      label,
      age_group,
      wave,
      corrected_p_median = p_median
    )
  dplyr::full_join(old, new, by = c("label", "age_group", "wave")) |>
    dplyr::mutate(
      key_status = dplyr::case_when(
        is.na(historical_p_median) ~ "added",
        is.na(corrected_p_median) ~ "removed",
        TRUE ~ "common"
      ),
      p_median_difference = corrected_p_median - historical_p_median
    )
}

compare_trajectory_input <- function(historical, corrected) {
  keys <- c("region", "age_group", "vaccination_group", "date")
  old <- historical |>
    dplyr::filter(region == "Europe") |>
    dplyr::transmute(
      region,
      age_group,
      vaccination_group,
      date = as.Date(date),
      historical_mean = mean,
      historical_lower = lower,
      historical_upper = upper,
      historical_variance = variance
    )
  new <- corrected |>
    dplyr::filter(region == "Europe") |>
    dplyr::transmute(
      region,
      age_group,
      vaccination_group,
      date = as.Date(date),
      corrected_mean = mean,
      corrected_lower = lower,
      corrected_upper = upper,
      corrected_variance = variance
    )
  dplyr::full_join(old, new, by = keys) |>
    dplyr::mutate(
      key_status = dplyr::case_when(
        is.na(historical_mean) ~ "added",
        is.na(corrected_mean) ~ "removed",
        TRUE ~ "common"
      ),
      mean_difference = corrected_mean - historical_mean,
      historical_interval_width = historical_upper - historical_lower,
      corrected_interval_width = corrected_upper - corrected_lower,
      interval_width_difference = corrected_interval_width -
        historical_interval_width
    )
}

compare_wave_summary <- function(historical, corrected) {
  keys <- c("analysis_path", "geography", "age_group", "sex", "wave")
  old <- historical |>
    dplyr::select(dplyr::all_of(keys), historical_p_median = p_median)
  new <- corrected |>
    dplyr::select(dplyr::all_of(keys), corrected_p_median = p_median)
  dplyr::full_join(old, new, by = keys) |>
    dplyr::mutate(
      key_status = dplyr::case_when(
        is.na(historical_p_median) ~ "added",
        is.na(corrected_p_median) ~ "removed",
        TRUE ~ "common"
      ),
      p_median_difference = corrected_p_median - historical_p_median
    )
}

comparison_summary_row <- function(
  output_id,
  comparison,
  difference_column,
  notes
) {
  common <- comparison$key_status == "common"
  differences <- abs(comparison[[difference_column]][common])
  data.frame(
    output_id = output_id,
    changed = any(differences > 0, na.rm = TRUE) ||
      any(comparison$key_status != "common"),
    common_keys = sum(common),
    added_keys = sum(comparison$key_status == "added"),
    removed_keys = sum(comparison$key_status == "removed"),
    median_absolute_primary_difference = stats::median(
      differences,
      na.rm = TRUE
    ),
    maximum_absolute_primary_difference = max(
      differences,
      na.rm = TRUE
    ),
    notes = notes,
    stringsAsFactors = FALSE
  )
}

reporting_rows_are_value_identical <- function(historical, rebuilt) {
  if (nrow(historical) != nrow(rebuilt) ||
      !all(names(historical) %in% names(rebuilt))) {
    return(FALSE)
  }
  all(vapply(names(historical), function(column) {
    identical(historical[[column]], rebuilt[[column]])
  }, logical(1)))
}

render_affected_outputs <- function() {
  scripts <- c(
    "scripts/reporting/figure_01_model_illustration.R",
    "scripts/reporting/figure_02_europe_maps.R",
    "scripts/reporting/figure_04_vaccination_pscore.R",
    "scripts/reporting/figure_05_sex_difference.R",
    "scripts/reporting/table_01_wave_pscores.R"
  )
  for (script in scripts) {
    status <- system2(
      file.path(R.home("bin"), "Rscript"),
      script,
      stdout = "",
      stderr = ""
    )
    if (!identical(status, 0L)) {
      stop("Affected reporting renderer failed: ", script, ".")
    }
  }
  invisible(scripts)
}

arguments <- parse_corrected_europe_arguments(commandArgs(trailingOnly = TRUE))
render <- parse_corrected_europe_boolean(arguments$render, "--render")
comparison_root <- normalizePath(arguments$comparison_root, mustWork = FALSE)
dir.create(comparison_root, recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(comparison_root, "comparisons"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(comparison_root, "corrected"), recursive = TRUE, showWarnings = FALSE)

contract <- read_corrected_europe_contract(
  refit_root = arguments$refit_root,
  cohort_path = here::here("config", "europe_reporting_cohort.csv"),
  data_path = here::here("data", "raw", "eurostat", "demo_r_mwk_20_linear.csv")
)
historical_manifest <- archive_installed_artifacts(comparison_root)
figure_03_paths <- c(
  here::here("output", "figures", "figure_03_north_america_maps.pdf"),
  here::here("output", "figures", "figure_03_north_america_maps.png")
)
figure_03_hash_before <- vapply(figure_03_paths, sha256_file, character(1))

historical_root <- file.path(comparison_root, "historical")
historical_figure_01 <- readRDS(file.path(
  historical_root,
  "inputs",
  "figure_01_model_illustration.rds"
))
historical_figure_02 <- readRDS(file.path(
  historical_root,
  "inputs",
  "figure_02_europe_maps.rds"
))
historical_figure_04 <- readr::read_csv(
  file.path(historical_root, "inputs", "figure_04_vaccination_pscore.csv"),
  show_col_types = FALSE
)
historical_figure_05 <- readr::read_csv(
  file.path(historical_root, "inputs", "figure_05_sex_difference.csv"),
  show_col_types = FALSE
)
historical_wave_summary <- readr::read_csv(
  file.path(historical_root, "inputs", "europe_wave_summary.csv"),
  show_col_types = FALSE
)

config <- read_analysis_config(here::here("config", "analysis.yml"))
wave_definitions <- wave_table(config)
observed <- read_corrected_europe_observed(contract$data_path)
vaccination <- readr::read_csv(
  here::here("output", "data", "europe", "vaccination_membership.csv"),
  show_col_types = FALSE
)

message("Building corrected Europe wave summaries.")
corrected_wave_summary <- build_corrected_europe_wave_summary(
  contract,
  observed,
  wave_definitions
)
corrected_figure_01 <- readRDS(file.path(
  contract$refit_root,
  "figure_01",
  "figure_01_model_illustration.rds"
))
validate_figure_01_input(corrected_figure_01)
corrected_figure_02 <- build_corrected_europe_map_input(
  corrected_wave_summary,
  here::here(
    "output",
    "results",
    "zenodo_bundle",
    "source_artifacts",
    "europe",
    "geometry",
    "ne_10m_admin_0_countries_lakes.shp"
  )
)

reporting_geographies <- contract$cohort$geography[contract$cohort$figure_04]
message("Building corrected Europe total-sex trajectories.")
total_predictions <- build_corrected_europe_prediction_set(
  contract,
  observed,
  reporting_geographies,
  sexes = "T"
)
corrected_figure_04_europe <- aggregate_corrected_europe_by_vaccination(
  total_predictions,
  vaccination,
  contract$cohort,
  list(
    `40-79` = c("Y40-59", "Y60-79"),
    `40-59` = "Y40-59",
    `60-79` = "Y60-79"
  ),
  contrast = FALSE
)
rm(total_predictions)
gc(verbose = FALSE)

message("Building corrected Europe sex-contrast trajectories.")
sex_predictions <- build_corrected_europe_prediction_set(
  contract,
  observed,
  reporting_geographies,
  sexes = c("F", "M")
)
corrected_figure_05_europe <- aggregate_corrected_europe_by_vaccination(
  sex_predictions,
  vaccination,
  contract$cohort,
  list(
    `40-79` = c("Y40-59", "Y60-79"),
    `40-59` = "Y40-59",
    `60-79` = "Y60-79"
  ),
  contrast = TRUE
)
rm(sex_predictions)
gc(verbose = FALSE)

historical_figure_04_us <- historical_figure_04 |>
  dplyr::filter(region == "United States")
historical_figure_05_us <- historical_figure_05 |>
  dplyr::filter(region == "United States")
corrected_figure_04 <- dplyr::bind_rows(
  corrected_figure_04_europe,
  historical_figure_04_us
)
corrected_figure_05 <- dplyr::bind_rows(
  corrected_figure_05_europe,
  historical_figure_05_us
)
if (!reporting_rows_are_value_identical(
  historical_figure_04_us,
  corrected_figure_04 |> dplyr::filter(region == "United States")
) || !reporting_rows_are_value_identical(
  historical_figure_05_us,
  corrected_figure_05 |> dplyr::filter(region == "United States")
)) {
  stop("Unchanged United States trajectory rows were altered during Europe rebuild.")
}

figure_01_input_path <- here::here(
  "output", "reporting", "inputs", "figure_01_model_illustration.rds"
)
figure_02_input_path <- here::here(
  "output", "reporting", "inputs", "figure_02_europe_maps.rds"
)
figure_04_input_path <- here::here(
  "output", "reporting", "inputs", "figure_04_vaccination_pscore.csv"
)
figure_05_input_path <- here::here(
  "output", "reporting", "inputs", "figure_05_sex_difference.csv"
)
wave_summary_path <- here::here(
  "output", "results", "europe", "wave_summary.csv"
)
atomic_save_rds(corrected_figure_01, figure_01_input_path)
atomic_save_rds(corrected_figure_02, figure_02_input_path)
atomic_write_csv(corrected_figure_04, figure_04_input_path)
atomic_write_csv(corrected_figure_05, figure_05_input_path)
atomic_write_csv(corrected_wave_summary, wave_summary_path)

figure_01_comparison <- compare_component_input(
  historical_figure_01,
  corrected_figure_01
)
figure_02_comparison <- compare_map_input(
  historical_figure_02,
  corrected_figure_02
)
figure_04_comparison <- compare_trajectory_input(
  historical_figure_04,
  corrected_figure_04
)
figure_05_comparison <- compare_trajectory_input(
  historical_figure_05,
  corrected_figure_05
)
wave_comparison <- compare_wave_summary(
  historical_wave_summary,
  corrected_wave_summary
)
comparison_files <- list(
  figure_01_pointwise.csv = figure_01_comparison,
  figure_02_wave_medians.csv = figure_02_comparison,
  figure_04_europe_pointwise.csv = figure_04_comparison,
  figure_05_europe_pointwise.csv = figure_05_comparison,
  table_01_europe_wave_medians.csv = wave_comparison
)
for (name in names(comparison_files)) {
  readr::write_csv(
    comparison_files[[name]],
    file.path(comparison_root, "comparisons", name)
  )
}

if (render) {
  message("Rendering all five affected manuscript outputs.")
  render_affected_outputs()
}

figure_03_hash_after <- vapply(figure_03_paths, sha256_file, character(1))
if (!identical(figure_03_hash_before, figure_03_hash_after)) {
  stop("Figure 3 changed even though it is outside the corrected Europe dependency set.")
}

corrected_output_paths <- c(
  "figure_01_model_illustration.pdf" = here::here(
    "output", "figures", "figure_01_model_illustration.pdf"
  ),
  "figure_01_model_illustration.png" = here::here(
    "output", "figures", "figure_01_model_illustration.png"
  ),
  "figure_02_europe_maps.pdf" = here::here(
    "output", "figures", "figure_02_europe_maps.pdf"
  ),
  "figure_02_europe_maps.png" = here::here(
    "output", "figures", "figure_02_europe_maps.png"
  ),
  "figure_04_vaccination_pscore.pdf" = here::here(
    "output", "figures", "figure_04_vaccination_pscore.pdf"
  ),
  "figure_04_vaccination_pscore.png" = here::here(
    "output", "figures", "figure_04_vaccination_pscore.png"
  ),
  "figure_05_sex_difference.pdf" = here::here(
    "output", "figures", "figure_05_sex_difference.pdf"
  ),
  "figure_05_sex_difference.png" = here::here(
    "output", "figures", "figure_05_sex_difference.png"
  ),
  "table_01_wave_pscores.csv" = here::here(
    "output", "tables", "table_01_wave_pscores.csv"
  ),
  "table_01_wave_pscores.html" = here::here(
    "output", "tables", "table_01_wave_pscores.html"
  )
)
for (name in names(corrected_output_paths)) {
  copy_corrected(
    corrected_output_paths[[name]],
    file.path(comparison_root, "corrected", name)
  )
}

historical_table <- readr::read_csv(
  file.path(historical_root, "outputs", "table_01_wave_pscores.csv"),
  show_col_types = FALSE
)
corrected_table <- readr::read_csv(
  here::here("output", "tables", "table_01_wave_pscores.csv"),
  show_col_types = FALSE
)
table_keys <- c("region_set", "geography")
table_comparison <- dplyr::full_join(
  historical_table |>
    dplyr::filter(region_set == "Europe") |>
    dplyr::rename_with(
      ~ paste0("historical_", .x),
      -dplyr::all_of(table_keys)
    ),
  corrected_table |>
    dplyr::filter(region_set == "Europe") |>
    dplyr::rename_with(
      ~ paste0("corrected_", .x),
      -dplyr::all_of(table_keys)
    ),
  by = table_keys
)
for (wave in c("initial", "alpha", "delta", "omicron")) {
  table_comparison[[paste0(wave, "_changed")]] <-
    table_comparison[[paste0("historical_", wave)]] !=
    table_comparison[[paste0("corrected_", wave)]]
}
readr::write_csv(
  table_comparison,
  file.path(comparison_root, "comparisons", "table_01_rendered_cells.csv")
)

output_summary <- dplyr::bind_rows(
  comparison_summary_row(
    "figure_01",
    figure_01_comparison,
    "mean_difference",
    "Netherlands GE80 total-sex corrected prior"
  ),
  comparison_summary_row(
    "figure_02",
    figure_02_comparison,
    "p_median_difference",
    "Corrected 33-geography Eurostat scope; England and Wales and Ireland removed pending refits"
  ),
  comparison_summary_row(
    "figure_04",
    figure_04_comparison,
    "mean_difference",
    "Europe panels rebuilt; United States panels unchanged"
  ),
  comparison_summary_row(
    "figure_05",
    figure_05_comparison,
    "mean_difference",
    "Europe panels rebuilt; United States panels unchanged"
  ),
  comparison_summary_row(
    "table_01",
    wave_comparison |>
      dplyr::filter(
        geography %in% contract$cohort$geography[contract$cohort$table_01],
        age_group == "60-79",
        sex %in% c("female", "male")
      ),
    "p_median_difference",
    "Europe rows rebuilt for the frozen 15-country cohort; United States rows unchanged"
  )
)
readr::write_csv(
  output_summary,
  file.path(comparison_root, "output_change_summary.csv")
)

artifact_manifest <- dplyr::bind_rows(
  historical_manifest |>
    dplyr::transmute(
      artifact_version = "historical",
      relative_path,
      path = historical_path,
      sha256 = historical_sha256
    ),
  data.frame(
    artifact_version = "corrected",
    relative_path = names(corrected_output_paths),
    path = unname(corrected_output_paths),
    sha256 = vapply(corrected_output_paths, sha256_file, character(1)),
    stringsAsFactors = FALSE
  )
)
readr::write_csv(
  artifact_manifest,
  file.path(comparison_root, "artifact_manifest.csv")
)
saveRDS(
  list(
    created_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
    affected_outputs = corrected_europe_affected_outputs(),
    unaffected_output = "figure_03",
    figure_03_sha256 = figure_03_hash_after,
    refit_verification = contract$verification,
    refit_root = contract$refit_root,
    refit_manifest_sha256 = sha256_file(file.path(
      contract$refit_root,
      "manifests",
      "model_manifest.csv"
    )),
    reporting_cohort_sha256 = sha256_file(contract$cohort_path),
    script_sha256 = sha256_file(script_path),
    session_info = utils::sessionInfo()
  ),
  file.path(comparison_root, "provenance.rds"),
  compress = "gzip"
)
writeLines("complete", file.path(comparison_root, "complete.flag"))

message("Rebuilt corrected Europe manuscript outputs: ", comparison_root)
message("Affected outputs: ", paste(corrected_europe_affected_outputs(), collapse = ", "))
message("Unaffected output verified byte-identical: figure_03")
