#!/usr/bin/env Rscript

arguments <- commandArgs(trailingOnly = TRUE)
target_names <- if (length(arguments) == 0L) NULL else arguments
in_process <- identical(
  tolower(Sys.getenv("COVID_PIPELINE_IN_PROCESS")),
  "true"
)
callr_function <- if (in_process) NULL else callr::r

targets::tar_make(
  names = target_names,
  callr_function = callr_function
)
