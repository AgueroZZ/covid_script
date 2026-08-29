#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(here)
  library(readr)
})

source(here::here("R", "reporting.R"))

arguments <- reporting_arguments(list(include_caption_review = "false"))
include_caption_review <- identical(tolower(arguments$include_caption_review), "true")
registry <- read_reporting_registry()
outputs <- registry$outputs

for (index in seq_len(nrow(outputs))) {
  output <- outputs[index, ]
  if (output$scientific_status == "blocked_estimand_definition") {
    message(output$manuscript_label, ": blocked by unresolved estimand definition.")
    next
  }
  if (output$scientific_status == "caption_review_required" &&
      !include_caption_review) {
    message(
      output$manuscript_label,
      ": caption review required; rerun with --include_caption_review=true to include it."
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
    next
  }
  status <- system2(
    file.path(R.home("bin"), "Rscript"),
    output$script_path,
    stdout = "",
    stderr = ""
  )
  if (!identical(status, 0L)) {
    stop(output$manuscript_label, " reporting script failed with status ", status, ".")
  }
}
