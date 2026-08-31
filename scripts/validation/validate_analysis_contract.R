#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(here)
  library(readr)
  library(yaml)
})

source(here::here("R", "analysis_contract.R"))

parse_arguments <- function(arguments) {
  defaults <- list(
    contract = here::here("config", "manuscript_analysis_contract.yml"),
    output_root = here::here(
      "output",
      "validation",
      "final_analysis_contract_20260831"
    )
  )
  for (argument in arguments) {
    if (!grepl("^--[^=]+=", argument)) {
      stop("Arguments must use --name=value syntax: ", argument)
    }
    key <- sub("^--([^=]+)=.*$", "\\1", argument)
    value <- sub("^[^=]+=", "", argument)
    if (!key %in% names(defaults)) {
      stop("Unknown argument: ", key)
    }
    defaults[[key]] <- value
  }
  defaults
}

arguments <- parse_arguments(commandArgs(trailingOnly = TRUE))
contract <- read_manuscript_analysis_contract(arguments$contract)
validation <- validate_manuscript_analysis_contract(contract)

dir.create(arguments$output_root, recursive = TRUE, showWarnings = FALSE)
readr::write_csv(
  flatten_manuscript_analysis_contract(contract),
  file.path(arguments$output_root, "contract_snapshot.csv")
)
readr::write_csv(
  validation$registry_hashes,
  file.path(arguments$output_root, "registry_hashes.csv")
)
readr::write_csv(
  validation$summary,
  file.path(arguments$output_root, "validation_summary.csv")
)
readr::write_csv(
  validation$incomplete_figure_05,
  file.path(arguments$output_root, "accepted_figure_05_incomplete_strata.csv")
)
writeLines(
  capture.output(utils::sessionInfo()),
  file.path(arguments$output_root, "session_info.txt")
)

required_outputs <- file.path(
  arguments$output_root,
  c(
    "contract_snapshot.csv",
    "registry_hashes.csv",
    "validation_summary.csv",
    "accepted_figure_05_incomplete_strata.csv",
    "session_info.txt"
  )
)
if (any(!file.exists(required_outputs)) || any(file.info(required_outputs)$size <= 0L)) {
  stop("Final analysis contract validation outputs are incomplete.")
}

writeLines(
  c(
    "Final manuscript analysis contract validation complete.",
    paste0("contract_version=", contract$contract_version),
    paste0("contract_sha256=", analysis_contract_sha256(arguments$contract)),
    paste0("validated_at_utc=", format(Sys.time(), tz = "UTC", usetz = TRUE))
  ),
  file.path(arguments$output_root, "complete.flag")
)
message("Final analysis contract validated: ", arguments$output_root)
