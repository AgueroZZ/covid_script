#!/usr/bin/env Rscript

source("scripts/data_access/common.R")

arguments <- parse_download_arguments(commandArgs(trailingOnly = TRUE), "cdc_wonder")
output_dir <- prepare_download_directory(arguments$output_dir)
instruction_path <- file.path(output_dir, "wonder_query_instructions.txt")
if (file.exists(instruction_path)) {
  stop("Refusing to overwrite existing CDC WONDER instructions: ", instruction_path)
}

instructions <- c(
  "CDC WONDER all-cause mortality export instructions",
  "",
  "Exact reproduction uses the versioned exports under data/raw/cdc_wonder.",
  "CDC final and provisional mortality databases are revised over time.",
  "Review and accept the CDC WONDER data-use restrictions before submitting a query.",
  "",
  "Final mortality request: https://wonder.cdc.gov/mcd-icd10.html",
  "Provisional mortality request: https://wonder.cdc.gov/mcd-icd10-provisional.html",
  "API documentation: https://wonder.cdc.gov/wonder/help/wonder-api.html",
  "",
  "Sex-stratified export",
  "Group by Residence State, Ten-Year Age Groups, Gender, Year, and Month.",
  "Select all causes and residence geography; export death counts as tab-delimited text.",
  "Use the year blocks shown in data/raw/cdc_wonder/sex_stratified.",
  "",
  "Non-sex-stratified export",
  "Group by Residence State, Year, Month, and Five-Year Age Groups.",
  "Select all causes and residence geography; export death counts as tab-delimited text.",
  "Use the year blocks shown in data/raw/cdc_wonder/non_sex_stratified.",
  "",
  "Retain the Notes column and footer lines because the import contract validates them.",
  "Run scripts/data_access/verify_snapshots.R to verify repository snapshots; do not replace them with a current export for exact reproduction."
)
writeLines(instructions, instruction_path)
write_download_record(
  output_dir,
  "CDC WONDER",
  c(
    "https://wonder.cdc.gov/mcd-icd10.html",
    "https://wonder.cdc.gov/mcd-icd10-provisional.html"
  ),
  instruction_path
)
message("Wrote CDC WONDER query instructions to ", instruction_path, ".")
