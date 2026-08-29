#!/usr/bin/env Rscript

artifact_paths <- list.files(
  "artifacts",
  recursive = TRUE,
  full.names = TRUE,
  all.files = FALSE
)
artifact_paths <- artifact_paths[file.info(artifact_paths)$isdir %in% FALSE]
artifact_paths <- artifact_paths[
  !grepl("^artifacts/manifests/", artifact_paths)
]

if (length(artifact_paths) == 0L) {
  stop("No non-manifest artifacts were found. Run the pipeline before verification.")
}

source("R/validation.R")
manifest_path <- "artifacts/manifests/artifacts.csv"
write_artifact_manifest(artifact_paths, manifest_path)
cat("Verified ", length(artifact_paths), " artifacts.\n", sep = "")
