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

arguments <- reporting_arguments(list(
  input = here::here(
    "artifacts",
    "reporting",
    "inputs",
    "figure_05_sex_difference.csv"
  ),
  output_dir = here::here(
    "artifacts",
    "validation",
    "figure_05_us_panel_f_comparison"
  )
))

reporting_require_files(arguments$input, "Figure 5 comparison input")
dir.create(arguments$output_dir, recursive = TRUE, showWarnings = FALSE)

config <- read_analysis_config(here::here("config", "analysis.yml"))
registry <- read_reporting_registry()
panels <- registry$panels[registry$panels$output_id == "figure_05", ]
panels <- panels[order(panels$position), ]
data <- readr::read_csv(arguments$input, show_col_types = FALSE)
data <- smooth_reporting_trajectory(data, region = "Europe", bandwidth_days = 14)

required_us_ages <- c("45-64", "65-84")
available_us_ages <- unique(data$age_group[data$region == "United States"])
missing_us_ages <- setdiff(required_us_ages, available_us_ages)
if (length(missing_us_ages) > 0L) {
  stop(
    "Figure 5 comparison input is missing US age groups: ",
    paste(missing_us_ages, collapse = ", "),
    ". Rebuild the manuscript bundle first."
  )
}

render_version <- function(panel_f_age, output_stem) {
  version_panels <- panels
  panel_f <- version_panels$panel_id == "f"
  version_panels$data_age_group[panel_f] <- panel_f_age
  version_panels$estimand_age_groups[panel_f] <- panel_f_age
  version_panels$display_label[panel_f] <- paste0(panel_f_age, ", US")
  version_panels$notes[panel_f] <- paste0(
    "Diagnostic Figure 5f version using ages ",
    panel_f_age
  )
  render_six_panel_trajectory(
    data,
    file.path(arguments$output_dir, paste0(output_stem, ".pdf")),
    config,
    version_panels,
    y_limits = c(-0.2, 0.5),
    y_label = "P-score difference (F-M)",
    interval = "dashed",
    x_limits = list(
      Europe = as.Date(c("2020-01-01", "2024-05-01")),
      `United States` = as.Date(c("2020-01-01", "2023-09-01"))
    )
  )
}

current_outputs <- render_version(
  "45-64",
  "figure_05_panel_f_45_64"
)
candidate_outputs <- render_version(
  "65-84",
  "figure_05_panel_f_65_84"
)

comparison_summary <- data |>
  dplyr::filter(
    region == "United States",
    age_group %in% required_us_ages,
    date >= as.Date("2020-03-01")
  ) |>
  dplyr::mutate(wave = assign_wave(date, config)) |>
  dplyr::filter(!is.na(wave)) |>
  dplyr::group_by(age_group, vaccination_group, wave) |>
  dplyr::summarise(
    time_points = dplyr::n(),
    trajectory_mean = mean(mean),
    trajectory_minimum = min(mean),
    trajectory_maximum = max(mean),
    mean_interval_width = mean(upper - lower),
    .groups = "drop"
  ) |>
  dplyr::arrange(age_group, vaccination_group, wave)

readr::write_csv(
  comparison_summary,
  file.path(arguments$output_dir, "panel_f_wave_summary.csv")
)

message(
  "Rendered current Figure 5f comparison: ",
  paste(current_outputs, collapse = ", ")
)
message(
  "Rendered candidate Figure 5f comparison: ",
  paste(candidate_outputs, collapse = ", ")
)
