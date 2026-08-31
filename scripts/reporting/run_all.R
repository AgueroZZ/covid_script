#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(here)
  library(readr)
})

source(here::here("R", "reporting.R"))
source(here::here("R", "submission_freeze.R"))

arguments <- reporting_arguments(list(
  include_caption_review = "false",
  strict = "false",
  status_output = "none"
))
include_caption_review <- identical(tolower(arguments$include_caption_review), "true")
strict <- identical(tolower(arguments$strict), "true")
registry <- read_reporting_registry()
outputs <- registry$outputs
status_rows <- vector("list", nrow(outputs))

for (index in seq_len(nrow(outputs))) {
  output <- outputs[index, ]
  output_status <- "rendered"
  detail <- ""
  if (output$scientific_status == "blocked_estimand_definition") {
    message(output$manuscript_label, ": blocked by unresolved estimand definition.")
    output_status <- "blocked"
    detail <- output$scientific_status
    status_rows[[index]] <- data.frame(
      output_id = output$output_id,
      manuscript_label = output$manuscript_label,
      status = output_status,
      detail = detail,
      stringsAsFactors = FALSE
    )
    next
  }
  if (output$scientific_status == "caption_review_required" &&
      !include_caption_review) {
    message(
      output$manuscript_label,
      ": caption review required; rerun with --include_caption_review=true to include it."
    )
    output_status <- "skipped_caption_review"
    detail <- output$scientific_status
    status_rows[[index]] <- data.frame(
      output_id = output$output_id,
      manuscript_label = output$manuscript_label,
      status = output_status,
      detail = detail,
      stringsAsFactors = FALSE
    )
    next
  }
  input_paths <- strsplit(output$input_contract, "; ", fixed = TRUE)[[1]]
  if (!all(file.exists(input_paths))) {
    message(
      output$manuscript_label,
      ": awaiting upstream artifacts: ",
      paste(input_paths[!file.exists(input_paths)], collapse = ", ")
    )
    output_status <- "missing_input"
    detail <- paste(input_paths[!file.exists(input_paths)], collapse = ";")
    status_rows[[index]] <- data.frame(
      output_id = output$output_id,
      manuscript_label = output$manuscript_label,
      status = output_status,
      detail = detail,
      stringsAsFactors = FALSE
    )
    next
  }
  command_status <- system2(
    file.path(R.home("bin"), "Rscript"),
    output$script_path,
    stdout = "",
    stderr = ""
  )
  if (!identical(command_status, 0L)) {
    output_status <- "failed"
    detail <- paste0("exit_status=", command_status)
  } else if (!file.exists(output$primary_artifact) ||
             file.info(output$primary_artifact)$size <= 0L) {
    output_status <- "failed"
    detail <- paste0("missing_primary_artifact=", output$primary_artifact)
  }
  status_rows[[index]] <- data.frame(
    output_id = output$output_id,
    manuscript_label = output$manuscript_label,
    status = output_status,
    detail = detail,
    stringsAsFactors = FALSE
  )
}

reporting_status <- dplyr::bind_rows(status_rows)
if (!identical(arguments$status_output, "none")) {
  dir.create(dirname(arguments$status_output), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(reporting_status, arguments$status_output)
}
if (strict) assert_strict_reporting_status(reporting_status)
