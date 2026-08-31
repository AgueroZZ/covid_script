#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(here)
  library(readr)
})

source(here::here("R", "config.R"))
source(here::here("R", "waves.R"))
source(here::here("R", "reporting.R"))
source(here::here("R", "figures.R"))
source(here::here("R", "us_cohorts.R"))
source(here::here("R", "us_results.R"))
source(here::here("R", "us_suppression.R"))

parse_arguments <- function(arguments) {
  defaults <- list(
    output_root = here::here(
      "output",
      "validation",
      "figure_05_us_cohort_comparison_20260831"
    ),
    bundle_root = here::here("output", "results", "zenodo_bundle"),
    cohort_path = here::here("config", "us_reporting_cohort.csv"),
    inventory_path = here::here("config", "cohorts.csv"),
    installed_input = here::here(
      "output",
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

combine_with_europe <- function(us_data, installed) {
  dplyr::bind_rows(
    installed |> dplyr::filter(region == "Europe"),
    us_data
  ) |>
    dplyr::arrange(region, age_group, vaccination_group, date)
}

summarize_pairwise_change <- function(reference, candidate, method) {
  keys <- c("region", "age_group", "vaccination_group", "date")
  comparison <- dplyr::inner_join(
    reference,
    candidate,
    by = keys,
    suffix = c("_reference", "_candidate")
  ) |>
    dplyr::filter(region == "United States") |>
    dplyr::mutate(absolute_change = abs(mean_candidate - mean_reference))
  periods <- dplyr::bind_rows(
    comparison |> dplyr::mutate(period = "full-overlap"),
    comparison |>
      dplyr::filter(
        as.Date(date) >= as.Date("2020-01-01"),
        as.Date(date) <= as.Date("2023-08-31")
      ) |>
      dplyr::mutate(period = "2020-2023-08-31")
  )
  periods |>
    dplyr::mutate(method = method) |>
    dplyr::group_by(method, period, age_group, vaccination_group) |>
    dplyr::summarise(
      common_months = dplyr::n(),
      maximum_absolute_change = max(absolute_change, na.rm = TRUE),
      median_absolute_change = stats::median(absolute_change, na.rm = TRUE),
      reference_jurisdictions = max(jurisdictions_reference),
      candidate_jurisdictions = max(jurisdictions_candidate),
      .groups = "drop"
    )
}

render_complete_figure <- function(data, output, config, panels) {
  plotted <- smooth_reporting_trajectory(
    data,
    region = "Europe",
    bandwidth_days = 14
  )
  render_six_panel_trajectory(
    plotted,
    output,
    config,
    panels,
    y_limits = c(-0.2, 0.5),
    y_label = "P-score difference (F-M)",
    interval = "dashed",
    x_limits = list(
      Europe = as.Date(c("2020-01-01", "2024-05-01")),
      `United States` = as.Date(c("2020-01-01", "2023-09-01"))
    )
  )
}

render_us_comparison_grid <- function(variants, output, config) {
  display_ages <- c("0-84", "0-44", "65-84")
  method_labels <- c(
    exclude_idaho_new_mexico = "Exclude Idaho and New Mexico",
    exclude_idaho = "Exclude Idaho only",
    historical = "Retain both; common available months"
  )
  draw <- function() {
    old <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(old), add = TRUE)
    graphics::par(mfrow = c(3, 3), mar = c(4, 4, 2.1, 0.8), las = 1)
    panel_index <- 0L
    for (method in names(method_labels)) {
      for (age in display_ages) {
        panel_index <- panel_index + 1L
        draw_trajectory_panel(
          variants[[method]],
          "United States",
          age,
          paste0(method_labels[[method]], "; ages ", age),
          config,
          y_limits = c(-0.2, 0.5),
          y_label = "P-score difference (F-M)",
          interval = "dashed",
          include_legend = panel_index == 1L,
          x_limits = as.Date(c("2020-01-01", "2023-09-01"))
        )
      }
    }
  }
  render_submission_figure(draw, output, width = 15.3, height = 13.2)
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
    "Figure 5 comparison inputs are missing: ",
    paste(required[!file.exists(required)], collapse = ", ")
  )
}

dir.create(arguments$output_root, recursive = TRUE, showWarnings = FALSE)
cohort <- read_us_reporting_cohort(arguments$cohort_path)
inventory <- read_us_completeness_inventory(arguments$inventory_path)
dependencies <- build_us_adopted_dependency_registry(inventory, cohort)
predictions <- load_us_figure05_bundle_predictions(arguments$bundle_root, cohort)
installed <- readr::read_csv(arguments$installed_input, show_col_types = FALSE) |>
  dplyr::mutate(date = as.Date(date))

variants <- list(
  exclude_idaho_new_mexico = build_us_figure05_variant(
    predictions,
    cohort,
    dependencies,
    "exclude_idaho_new_mexico"
  ),
  exclude_idaho = build_us_figure05_variant(
    predictions,
    cohort,
    dependencies,
    "exclude_idaho"
  ),
  historical = build_us_figure05_variant(
    predictions,
    cohort,
    dependencies,
    "historical"
  )
)

config <- read_analysis_config(here::here("config", "analysis.yml"))
registry <- read_reporting_registry()
panels <- registry$panels[registry$panels$output_id == "figure_05", ]
panels <- panels[order(panels$position), ]

complete_inputs <- lapply(variants, combine_with_europe, installed = installed)
for (method in names(complete_inputs)) {
  input_path <- file.path(arguments$output_root, paste0(method, "_input.csv"))
  figure_path <- file.path(arguments$output_root, paste0("figure_05_", method, ".pdf"))
  readr::write_csv(complete_inputs[[method]], input_path)
  render_complete_figure(complete_inputs[[method]], figure_path, config, panels)
}

render_us_comparison_grid(
  variants,
  file.path(arguments$output_root, "figure_05_us_three_method_grid.pdf"),
  config
)

numerical_summary <- dplyr::bind_rows(
  summarize_pairwise_change(
    variants$historical,
    variants$exclude_idaho,
    "exclude_idaho_vs_historical"
  ),
  summarize_pairwise_change(
    variants$historical,
    variants$exclude_idaho_new_mexico,
    "exclude_idaho_new_mexico_vs_historical"
  ),
  summarize_pairwise_change(
    variants$exclude_idaho,
    variants$exclude_idaho_new_mexico,
    "exclude_idaho_new_mexico_vs_exclude_idaho"
  )
)
readr::write_csv(
  numerical_summary,
  file.path(arguments$output_root, "numerical_comparison.csv")
)

date_coverage <- dplyr::bind_rows(lapply(names(variants), function(method) {
  variants[[method]] |>
    dplyr::group_by(age_group, vaccination_group) |>
    dplyr::summarise(
      method = method,
      first_month = min(date),
      last_month = max(date),
      available_months = dplyr::n(),
      available_months_2020_to_2023_08 = sum(
        as.Date(date) >= as.Date("2020-01-01") &
          as.Date(date) <= as.Date("2023-08-31")
      ),
      jurisdictions = max(jurisdictions),
      .groups = "drop"
    )
}))
readr::write_csv(
  date_coverage,
  file.path(arguments$output_root, "date_coverage.csv")
)

message("Rendered Figure 5 cohort comparison under: ", arguments$output_root)
