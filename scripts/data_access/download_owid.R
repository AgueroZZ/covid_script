#!/usr/bin/env Rscript

source("scripts/data_access/common.R")

arguments <- parse_download_arguments(commandArgs(trailingOnly = TRUE), "owid")
output_dir <- prepare_download_directory(arguments$output_dir)
base <- paste0(
  "https://raw.githubusercontent.com/owid/covid-19-data/master/",
  "public/data/vaccinations/"
)
urls <- paste0(base, c("vaccinations.csv", "us_state_vaccinations.csv"))
destinations <- file.path(
  output_dir,
  c("vaccinations.csv", "us_state_vaccinations.csv")
)

for (i in seq_along(urls)) {
  download_provider_file(urls[[i]], destinations[[i]])
}
write_download_record(output_dir, "Our World in Data", urls, destinations)
message(
  "Downloaded the archived OWID vaccination source tables to ",
  output_dir,
  ". Use the repository snapshots for the adopted 2021-07-01 classifications."
)
