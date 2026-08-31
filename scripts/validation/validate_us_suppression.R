#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(here)
  library(readr)
})

source(here::here("R", "us_cohorts.R"))
source(here::here("R", "us_results.R"))
source(here::here("R", "us_suppression.R"))
source(here::here("R", "validation.R"))
source(here::here("R", "config.R"))

parse_arguments <- function(arguments) {
  defaults <- list(
    output_root = here::here(
      "artifacts",
      "validation",
      "us_suppression_20260830"
    ),
    bundle_root = here::here("artifacts", "results", "zenodo_bundle"),
    cohort_path = here::here("config", "us_reporting_cohort.csv"),
    inventory_path = here::here("config", "cohorts.csv"),
    installed_input = here::here(
      "artifacts",
      "reporting",
      "inputs",
      "figure_05_sex_difference.csv"
    )
  )
  for (argument in arguments) {
    if (!grepl("^--[^=]+=", argument)) {
      stop("Arguments must use --name=value syntax: ", argument)
    }
    key <- sub("^--([^=]+)=.*$", "\\1", argument)
    value <- sub("^[^=]+=", "", argument)
    if (!key %in% names(defaults)) {
      stop("Unknown argument: ", key)
    }
    defaults[[key]] <- value
  }
  defaults
}

arguments <- parse_arguments(commandArgs(trailingOnly = TRUE))
required <- c(
  arguments$bundle_root,
  arguments$cohort_path,
  arguments$inventory_path,
  arguments$installed_input
)
if (!all(file.exists(required))) {
  stop(
    "US suppression validation inputs are missing: ",
    paste(required[!file.exists(required)], collapse = ", ")
  )
}

dir.create(arguments$output_root, recursive = TRUE, showWarnings = FALSE)
baseline_root <- file.path(arguments$output_root, "baseline")
dir.create(baseline_root, recursive = TRUE, showWarnings = FALSE)
baseline_path <- file.path(baseline_root, "figure_05_sex_difference.csv")
if (!file.exists(baseline_path)) {
  copied <- file.copy(arguments$installed_input, baseline_path)
  if (!copied) {
    stop("Failed to preserve the installed Figure 5 input baseline.")
  }
}

cohort <- read_us_reporting_cohort(arguments$cohort_path)
inventory <- read_us_completeness_inventory(arguments$inventory_path)
dependencies <- build_us_adopted_dependency_registry(inventory, cohort)
dependency_summary <- summarize_us_adopted_dependencies(dependencies)
incomplete <- dependencies[!dependencies$complete, ]

readr::write_csv(
  dependencies,
  file.path(arguments$output_root, "adopted_dependency_registry.csv")
)
readr::write_csv(
  dependency_summary,
  file.path(arguments$output_root, "adopted_dependency_summary.csv")
)
readr::write_csv(
  incomplete,
  file.path(arguments$output_root, "incomplete_adopted_dependencies.csv")
)

predictions <- load_us_figure05_bundle_predictions(arguments$bundle_root, cohort)
historical <- build_us_figure05_variant(
  predictions,
  cohort,
  dependencies,
  "historical"
)
panel_specific <- build_us_figure05_variant(
  predictions,
  cohort,
  dependencies,
  "panel_specific_complete_case"
)
strict <- build_us_figure05_variant(
  predictions,
  cohort,
  dependencies,
  "strict_complete_case"
)
installed <- readr::read_csv(arguments$installed_input, show_col_types = FALSE)
baseline_comparison <- compare_us_figure05_baseline(historical, installed)

panel_comparison <- compare_us_figure05_sensitivity(
  historical,
  panel_specific,
  "panel_specific_complete_case"
)
strict_comparison <- compare_us_figure05_sensitivity(
  historical,
  strict,
  "strict_complete_case"
)
comparison <- dplyr::bind_rows(panel_comparison, strict_comparison)
config <- read_analysis_config(here::here("config", "analysis.yml"))
decision_start <- as.Date("2020-01-01")
decision_end <- as.Date(config$regions$us_sex$analysis_end)
summary <- summarize_us_figure05_sensitivity(
  comparison,
  analysis_start = decision_start,
  analysis_end = decision_end
)
ordering <- dplyr::bind_rows(
  compare_us_figure05_ordering(
    historical,
    panel_specific,
    "panel_specific_complete_case",
    analysis_start = decision_start,
    analysis_end = decision_end
  ),
  compare_us_figure05_ordering(
    historical,
    strict,
    "strict_complete_case",
    analysis_start = decision_start,
    analysis_end = decision_end
  )
)
decision <- decide_us_figure05_sensitivity(summary, ordering)

outputs <- list(
  figure_05_historical_reconstructed.csv = historical,
  figure_05_panel_specific_complete_case.csv = panel_specific,
  figure_05_strict_complete_case.csv = strict,
  figure_05_baseline_equivalence.csv = baseline_comparison,
  figure_05_sensitivity_comparison.csv = comparison,
  figure_05_sensitivity_summary.csv = summary,
  figure_05_vaccination_ordering_summary.csv = ordering,
  decision_summary.csv = decision
)
for (name in names(outputs)) {
  readr::write_csv(outputs[[name]], file.path(arguments$output_root, name))
}

if (!all(decision$pass)) {
  stop(
    "One or more US suppression sensitivities failed the frozen decision criteria."
  )
}

manifest_inputs <- list.files(
  arguments$output_root,
  recursive = TRUE,
  full.names = TRUE
)
manifest_inputs <- manifest_inputs[
  !grepl("/(output_manifest[.]csv|complete[.]flag)$", manifest_inputs)
]
write_artifact_manifest(
  manifest_inputs,
  file.path(arguments$output_root, "output_manifest.csv")
)
writeLines(
  c(
      "US suppression validation completed successfully.",
      paste0("completed_at=", format(Sys.time(), tz = "UTC", usetz = TRUE)),
      paste0("decision_period=", decision_start, " through ", decision_end),
      paste0("incomplete_adopted_dependencies=", nrow(incomplete)),
    paste0("all_sensitivities_pass=", all(decision$pass))
  ),
  file.path(arguments$output_root, "complete.flag")
)

message(
  "US suppression validation complete: ",
  nrow(incomplete),
  " incomplete adopted dependencies; ",
  sum(decision$pass),
  "/",
  nrow(decision),
  " sensitivities passed."
)
