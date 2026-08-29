#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(here)
  library(readr)
  library(tidyr)
})

source(here::here("R", "reporting.R"))
source(here::here("R", "tables.R"))

arguments <- reporting_arguments(list(
  europe_wave = here::here("artifacts", "results", "europe", "wave_summary.csv"),
  us_wave = here::here("artifacts", "results", "us", "wave_summary.csv"),
  europe_vaccination = here::here(
    "artifacts",
    "data",
    "europe",
    "vaccination_membership.csv"
  ),
  us_vaccination = here::here(
    "artifacts",
    "data",
    "us",
    "vaccination_membership.csv"
  ),
  output = here::here("artifacts", "tables", "table_01_wave_pscores.csv")
))

inputs <- unlist(arguments[c(
  "europe_wave",
  "us_wave",
  "europe_vaccination",
  "us_vaccination"
)])
reporting_require_files(inputs, "Table 1 input")

table <- build_table_01(
  readr::read_csv(arguments$europe_wave, show_col_types = FALSE),
  readr::read_csv(arguments$us_wave, show_col_types = FALSE),
  readr::read_csv(arguments$europe_vaccination, show_col_types = FALSE),
  readr::read_csv(arguments$us_vaccination, show_col_types = FALSE)
)
html_output <- sub("\\.csv$", ".html", arguments$output, ignore.case = TRUE)
written <- write_table_01(table, arguments$output, html_output)
message("Rendered Table 1: ", paste(written, collapse = ", "))
