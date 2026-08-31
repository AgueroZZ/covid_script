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
    "output",
    "reporting",
    "inputs",
    "figure_03_north_america_maps.rds"
  ),
  output = here::here(
    "output",
    "figures",
    "figure_03_north_america_maps.pdf"
  )
))

reporting_require_files(arguments$input, "Figure 3 input")
rendered <- render_four_panel_map(
  readRDS(arguments$input),
  arguments$output,
  region = "north_america"
)
message("Rendered Figure 3: ", paste(rendered, collapse = ", "))
