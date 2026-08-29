#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(here)
  library(readr)
  library(yaml)
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
  output = here::here(
    "artifacts",
    "figures",
    "figure_05_sex_difference.pdf"
  )
))

reporting_require_files(arguments$input, "Figure 5 input")
registry <- read_reporting_registry()
panels <- registry$panels[registry$panels$output_id == "figure_05", ]
panels <- panels[order(panels$position), ]
config <- read_analysis_config(here::here("config", "analysis.yml"))
data <- readr::read_csv(arguments$input, show_col_types = FALSE)
data <- smooth_reporting_trajectory(data, region = "Europe", bandwidth_days = 14)
rendered <- render_six_panel_trajectory(
  data,
  arguments$output,
  config,
  panels,
  y_limits = c(-0.2, 0.5),
  y_label = "P-score difference (F-M)",
  interval = "dashed"
)
message("Rendered Figure 5: ", paste(rendered, collapse = ", "))
