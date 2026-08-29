#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(here)
  library(readr)
})

source(here::here("R", "reporting.R"))
source(here::here("R", "figures.R"))

arguments <- reporting_arguments(list(
  input = here::here(
    "artifacts",
    "reporting",
    "inputs",
    "figure_01_model_illustration.rds"
  ),
  output = here::here(
    "artifacts",
    "figures",
    "figure_01_model_illustration.pdf"
  )
))

reporting_require_files(arguments$input, "Figure 1 input")
rendered <- render_figure_01(readRDS(arguments$input), arguments$output)
message("Rendered Figure 1: ", paste(rendered, collapse = ", "))
