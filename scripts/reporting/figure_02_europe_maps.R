#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(here)
  library(RColorBrewer)
  library(readr)
  library(sf)
})

source(here::here("R", "reporting.R"))
source(here::here("R", "figures.R"))

arguments <- reporting_arguments(list(
  input = here::here(
    "artifacts",
    "reporting",
    "inputs",
    "figure_02_europe_maps.rds"
  ),
  output = here::here("artifacts", "figures", "figure_02_europe_maps.pdf")
))

reporting_require_files(arguments$input, "Figure 2 input")
rendered <- render_four_panel_map(
  readRDS(arguments$input),
  arguments$output,
  region = "europe"
)
message("Rendered Figure 2: ", paste(rendered, collapse = ", "))
