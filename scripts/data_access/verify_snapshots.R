#!/usr/bin/env Rscript

source("R/validation.R")
source("R/raw_manifest.R")

manifest <- utils::read.csv(
  file.path("data", "raw", "manifest.csv"),
  stringsAsFactors = FALSE,
  check.names = FALSE
)
validate_file_manifest(manifest, root = ".")
message("Verified ", nrow(manifest), " canonical raw-data snapshots.")
