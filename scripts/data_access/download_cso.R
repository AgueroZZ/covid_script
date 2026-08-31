#!/usr/bin/env Rscript

source("scripts/data_access/common.R")

arguments <- parse_download_arguments(commandArgs(trailingOnly = TRUE), "cso")
output_dir <- prepare_download_directory(arguments$output_dir)
url <- paste0(
  "https://ws.cso.ie/public/api.restful/",
  "PxStat.Data.Cube_API.ReadDataset/VSQ20/CSV/1.0/en"
)
raw_destination <- file.path(output_dir, "VSQ20_current.csv")
analysis_destination <- file.path(
  output_dir,
  "ireland_quarterly_deaths_current.csv"
)

download_provider_file(url, raw_destination)
if (file.exists(analysis_destination)) {
  stop("Refusing to overwrite an existing provider extract: ", analysis_destination)
}

current <- utils::read.csv(
  raw_destination,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  fileEncoding = "UTF-8-BOM"
)
required <- c(
  "STATISTIC Label",
  "Quarter",
  "Age Group",
  "ICD 10 Diagnostic Group",
  "UNIT",
  "VALUE"
)
if (!all(required %in% names(current))) {
  stop("The current VSQ20 schema no longer contains the expected columns.")
}
analysis <- current[
  current[["ICD 10 Diagnostic Group"]] == "Total deaths (A00-Y89)",
  required,
  drop = FALSE
]
utils::write.csv(analysis, analysis_destination, row.names = FALSE, na = "")
write_download_record(
  output_dir,
  "Central Statistics Office Ireland",
  url,
  c(raw_destination, analysis_destination)
)
message("Downloaded and filtered the current VSQ20 table to ", output_dir, ".")
