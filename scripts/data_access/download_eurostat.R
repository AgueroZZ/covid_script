#!/usr/bin/env Rscript

source("scripts/data_access/common.R")

arguments <- parse_download_arguments(commandArgs(trailingOnly = TRUE), "eurostat")
output_dir <- prepare_download_directory(arguments$output_dir)
url <- paste0(
  "https://ec.europa.eu/eurostat/api/dissemination/sdmx/2.1/data/",
  "demo_r_mwk_20?format=SDMX-CSV&compressed=true"
)
destination <- file.path(output_dir, "estat_demo_r_mwk_20_en.csv.gz")

download_provider_file(url, destination)
write_download_record(output_dir, "Eurostat", url, destination)
message("Downloaded the current DEMO_R_MWK_20 dataset to ", destination, ".")
