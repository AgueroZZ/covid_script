#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(here)
  library(RColorBrewer)
  library(sf)
})

source(here::here("R", "reporting.R"))
source(here::here("R", "figures.R"))
source(here::here("R", "uk_ie_reporting.R"))

arguments <- reporting_arguments(list(
  england_wales_root = here::here(
    "output", "results", "england_wales_corrected_20260830"
  ),
  ireland_root = here::here(
    "output", "results", "ireland_corrected_20260830"
  ),
  eurostat_wave_summary = here::here(
    "output", "results", "europe", "wave_summary.csv"
  ),
  mapping = here::here("config", "uk_ie_reporting_cohort.csv"),
  geometry = here::here(
    "output", "results", "zenodo_bundle", "source_artifacts",
    "europe", "geometry", "ne_10m_admin_0_countries_lakes.shp"
  ),
  figure_input = here::here(
    "output", "reporting", "inputs", "figure_02_europe_maps.rds"
  ),
  figure_output = here::here(
    "output", "figures", "figure_02_europe_maps.pdf"
  ),
  validation_root = here::here(
    "output", "validation", "uk_ie_corrected_20260830"
  )
))

required <- c(
  file.path(arguments$england_wales_root, "batch_complete.flag"),
  file.path(arguments$ireland_root, "batch_complete.flag"),
  arguments$eurostat_wave_summary,
  arguments$mapping,
  arguments$geometry,
  arguments$figure_input
)
reporting_require_files(required, "UK/Ireland corrected reporting")

dir.create(arguments$validation_root, recursive = TRUE, showWarnings = FALSE)
baseline_root <- file.path(arguments$validation_root, "baseline")
dir.create(baseline_root, recursive = TRUE, showWarnings = FALSE)
baseline_files <- c(
  figure_input = arguments$figure_input,
  figure_pdf = arguments$figure_output,
  figure_png = sub("\\.pdf$", ".png", arguments$figure_output)
)
for (name in names(baseline_files)) {
  path <- baseline_files[[name]]
  baseline_path <- file.path(baseline_root, basename(path))
  if (file.exists(path) && !file.exists(baseline_path)) {
    file.copy(path, baseline_path, overwrite = FALSE)
  }
}

read_region_summaries <- function(root, geography) {
  files <- sort(list.files(
    file.path(root, "wave_summary"),
    pattern = "\\.csv$",
    full.names = TRUE
  ))
  if (length(files) == 0L) {
    stop("No corrected wave summaries were found under ", root, ".")
  }
  rows <- lapply(files, function(path) {
    value <- utils::read.csv(path, stringsAsFactors = FALSE)
    value$source_age_group <- value$age_group
    value$geography <- geography
    value
  })
  dplyr::bind_rows(rows)
}

source_summary <- dplyr::bind_rows(
  read_region_summaries(arguments$england_wales_root, "England and Wales"),
  read_region_summaries(arguments$ireland_root, "Republic of Ireland")
)

load_named_object <- function(path, object_name) {
  environment <- new.env(parent = emptyenv())
  loaded <- load(path, envir = environment)
  if (!object_name %in% loaded) {
    stop("Expected object ", object_name, " in ", path, ".")
  }
  environment[[object_name]]
}

historical_specification <- data.frame(
  geography = c(
    rep("England and Wales", 3L),
    rep("Republic of Ireland", 4L)
  ),
  source_age_group = c(
    "Under 65", "65-84", "85+",
    "25-44", "45-64", "65-84", "85+"
  ),
  path = c(
    here::here(
      "output", "legacy", "england_wales", "results",
      "UK_result_under_65.rda"
    ),
    here::here(
      "output", "legacy", "england_wales", "results",
      "UK_result_65_85.rda"
    ),
    here::here(
      "output", "legacy", "england_wales", "results",
      "UK_result_85.rda"
    ),
    here::here(
      "output", "legacy", "ireland", "results",
      "IE_result_age_25_45.rda"
    ),
    here::here(
      "output", "legacy", "ireland", "results",
      "IE_result_age_45_65.rda"
    ),
    here::here(
      "output", "legacy", "ireland", "results",
      "IE_result_age_65_85.rda"
    ),
    here::here(
      "output", "legacy", "ireland", "results",
      "IE_result_age_85.rda"
    )
  ),
  object_name = c(
    rep("model_result", 3L),
    "model_result_25_45",
    "model_result_45_65",
    "model_result_65_85",
    "model_result_85"
  ),
  stringsAsFactors = FALSE
)
reporting_require_files(
  historical_specification$path,
  "historical UK/Ireland summaries"
)
historical_source_summary <- dplyr::bind_rows(lapply(
  seq_len(nrow(historical_specification)),
  function(index) {
    value <- load_named_object(
      historical_specification$path[[index]],
      historical_specification$object_name[[index]]
    )
    value$geography <- historical_specification$geography[[index]]
    value$source_age_group <- historical_specification$source_age_group[[index]]
    value
  }
))
regional_comparison <- dplyr::inner_join(
  historical_source_summary |>
    dplyr::select(
      "geography", "source_age_group", "wave",
      p_med_historical = "p_med",
      p_lower_historical = "p_lower",
      p_upper_historical = "p_upper",
      delta_med_historical = "delta_med"
    ),
  source_summary |>
    dplyr::select(
      "geography", "source_age_group", "wave",
      p_med_corrected = "p_med",
      p_lower_corrected = "p_lower",
      p_upper_corrected = "p_upper",
      delta_med_corrected = "delta_med"
    ),
  by = c("geography", "source_age_group", "wave")
)
if (nrow(regional_comparison) != 28L) {
  stop("Historical/corrected UK-Ireland comparison must contain 28 rows.")
}
regional_comparison$p_med_change <-
  regional_comparison$p_med_corrected - regional_comparison$p_med_historical
regional_comparison$delta_med_change <-
  regional_comparison$delta_med_corrected -
  regional_comparison$delta_med_historical
regional_comparison$sign_changed <-
  sign(regional_comparison$p_med_corrected) !=
  sign(regional_comparison$p_med_historical)
regional_comparison_summary <- regional_comparison |>
  dplyr::group_by(.data$geography, .data$source_age_group) |>
  dplyr::summarise(
    max_absolute_p_change = max(abs(.data$p_med_change)),
    sign_changes = sum(.data$sign_changed),
    .groups = "drop"
  )
registry <- read_uk_ie_age_mapping(arguments$mapping)
mapped_summary <- standardize_uk_ie_wave_summary(source_summary, registry)
eurostat_summary <- utils::read.csv(
  arguments$eurostat_wave_summary,
  stringsAsFactors = FALSE
)

historical_input <- readRDS(file.path(
  baseline_root,
  basename(arguments$figure_input)
))
extended_input <- build_extended_europe_map_input(
  eurostat_summary,
  mapped_summary,
  arguments$geometry
)
historical_values <- sf::st_drop_geometry(historical_input$map_data) |>
  dplyr::select("label", "age_group", "wave", "p_median")
extended_values <- sf::st_drop_geometry(extended_input$map_data) |>
  dplyr::filter(.data$label %in% historical_values$label) |>
  dplyr::select("label", "age_group", "wave", "p_median")
comparison <- dplyr::inner_join(
  historical_values,
  extended_values,
  by = c("label", "age_group", "wave"),
  suffix = c("_before", "_after")
)
comparison$absolute_difference <- abs(
  comparison$p_median_after - comparison$p_median_before
)
if (nrow(comparison) != 132L || any(comparison$absolute_difference > 1e-12)) {
  stop("Existing 33-geography Figure 2 values changed during UK/Ireland extension.")
}

utils::write.csv(
  source_summary,
  file.path(arguments$validation_root, "uk_ie_source_wave_summary.csv"),
  row.names = FALSE
)
utils::write.csv(
  mapped_summary,
  file.path(arguments$validation_root, "uk_ie_mapped_wave_summary.csv"),
  row.names = FALSE
)
utils::write.csv(
  comparison,
  file.path(arguments$validation_root, "existing_map_value_comparison.csv"),
  row.names = FALSE
)
utils::write.csv(
  regional_comparison,
  file.path(
    arguments$validation_root,
    "historical_vs_corrected_source_wave_summary.csv"
  ),
  row.names = FALSE
)
utils::write.csv(
  regional_comparison_summary,
  file.path(
    arguments$validation_root,
    "historical_vs_corrected_source_wave_summary_overview.csv"
  ),
  row.names = FALSE
)
saveRDS(extended_input, arguments$figure_input, compress = "gzip")
rendered <- render_four_panel_map(
  extended_input,
  arguments$figure_output,
  region = "europe"
)

output_paths <- c(
  figure_input = arguments$figure_input,
  figure_pdf = rendered[[1]],
  figure_png = rendered[[2]],
  source_summary = file.path(
    arguments$validation_root,
    "uk_ie_source_wave_summary.csv"
  ),
  mapped_summary = file.path(
    arguments$validation_root,
    "uk_ie_mapped_wave_summary.csv"
  ),
  comparison = file.path(
    arguments$validation_root,
    "existing_map_value_comparison.csv"
  ),
  regional_comparison = file.path(
    arguments$validation_root,
    "historical_vs_corrected_source_wave_summary.csv"
  ),
  regional_comparison_overview = file.path(
    arguments$validation_root,
    "historical_vs_corrected_source_wave_summary_overview.csv"
  )
)
manifest <- data.frame(
  artifact = names(output_paths),
  path = unname(output_paths),
  bytes = unname(file.info(output_paths)$size),
  sha256 = vapply(
    output_paths,
    digest::digest,
    character(1),
    file = TRUE,
    algo = "sha256"
  ),
  stringsAsFactors = FALSE
)
utils::write.csv(
  manifest,
  file.path(arguments$validation_root, "output_manifest.csv"),
  row.names = FALSE
)
writeLines("complete", file.path(arguments$validation_root, "complete.flag"))
message("Rebuilt Figure 2 with 35 geographies and explicit UK/Ireland age mappings.")
