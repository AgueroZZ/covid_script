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
    "figure_04_vaccination_pscore.csv"
  ),
  output = here::here(
    "artifacts",
    "figures",
    "figure_04_vaccination_pscore.pdf"
  )
))

reporting_require_files(arguments$input, "Figure 4 input")
registry <- read_reporting_registry()
panels <- registry$panels[registry$panels$output_id == "figure_04", ]
panels <- panels[order(panels$position), ]
config <- read_analysis_config(here::here("config", "analysis.yml"))
data <- readr::read_csv(arguments$input, show_col_types = FALSE)
rendered <- render_six_panel_trajectory(
  data,
  arguments$output,
  config,
  panels,
  y_limits = c(-0.3, 1),
  y_label = "P-score",
  interval = "ribbon"
)
message("Rendered Figure 4: ", paste(rendered, collapse = ", "))
