#!/usr/bin/env Rscript

source("scripts/data_access/common.R")

arguments <- parse_download_arguments(commandArgs(trailingOnly = TRUE), "us_census")
output_dir <- prepare_download_directory(arguments$output_dir)
url <- paste0(
  "https://www2.census.gov/geo/tiger/GENZ2018/shp/",
  "cb_2018_us_state_500k.zip"
)
destination <- file.path(output_dir, "cb_2018_us_state_500k.zip")

download_provider_file(url, destination)
write_download_record(output_dir, "United States Census Bureau", url, destination)
message("Downloaded the 2018 state cartographic boundary file to ", destination, ".")
