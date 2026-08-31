#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(here)
  library(readr)
})

source(here::here("R", "submission_freeze.R"))

arguments <- commandArgs(trailingOnly = TRUE)
if (length(arguments) != 2L ||
    !grepl("^--output-root=.+$", arguments[[1]]) ||
    !identical(arguments[[2]], "--visual-qa=pass")) {
  stop("Use --output-root=PATH --visual-qa=pass after inspecting every final PDF.")
}
output_root <- normalizePath(sub("^--output-root=", "", arguments[[1]]))
required <- file.path(
  output_root,
  c(
    "automatic_checks.flag",
    "manifests/artifact_manifest.csv",
    "manifests/command_status.csv",
    "manifests/reporting_status.csv",
    "manifests/figure_metadata.csv"
  )
)
submission_freeze_assert_files(required, "Freeze finalization")
manifest <- readr::read_csv(required[[2]], show_col_types = FALSE)
validate_submission_freeze_manifest(manifest, output_root)
commands <- readr::read_csv(required[[3]], show_col_types = FALSE)
if (any(commands$exit_status != 0L)) stop("A freeze command did not pass.")
reporting <- readr::read_csv(required[[4]], show_col_types = FALSE)
assert_strict_reporting_status(reporting)

visual_qa_path <- file.path(output_root, "visual_qa.txt")
writeLines(
  c(
    "Final PDF visual QA passed.",
    paste0("reviewed_at_utc=", format(Sys.time(), tz = "UTC", usetz = TRUE)),
    "review_scope=Figures 1-5 rendered from frozen PDFs",
    "criteria=no clipping, overlap, missing panels, malformed maps, black boxes, or unreadable labels"
  ),
  visual_qa_path
)
writeLines(
  c(
    "Local submission freeze complete.",
    paste0("completed_at_utc=", format(Sys.time(), tz = "UTC", usetz = TRUE)),
    paste0(
      "artifact_manifest_sha256=",
      submission_sha256(file.path(output_root, "manifests", "artifact_manifest.csv"))
    ),
    "visual_qa=pass",
    "zenodo_bundle=not_built"
  ),
  file.path(output_root, "complete.flag")
)
message("Finalized local submission freeze: ", output_root)
