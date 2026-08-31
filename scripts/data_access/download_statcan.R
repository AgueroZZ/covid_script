#!/usr/bin/env Rscript

source("scripts/data_access/common.R")

arguments <- parse_download_arguments(commandArgs(trailingOnly = TRUE), "statcan")
output_dir <- prepare_download_directory(arguments$output_dir)
url <- "https://www150.statcan.gc.ca/n1/en/tbl/csv/13100768-eng.zip"
destination <- file.path(output_dir, "13100768-eng.zip")

download_provider_file(url, destination)
utils::unzip(destination, exdir = file.path(output_dir, "13100768-eng"))
write_download_record(output_dir, "Statistics Canada", url, destination)
message("Downloaded the current Table 13-10-0768-01 archive to ", destination, ".")
