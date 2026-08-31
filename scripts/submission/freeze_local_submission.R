#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(digest)
  library(dplyr)
  library(here)
  library(readr)
})

source(here::here("R", "submission_freeze.R"))

parse_arguments <- function(arguments) {
  defaults <- list(
    output_root = here::here(
      "output", "submission_freeze", "local_20260831"
    )
  )
  for (argument in arguments) {
    if (!grepl("^--[^=]+=.+$", argument)) {
      stop("Arguments must use --name=value syntax: ", argument)
    }
    key <- gsub("-", "_", sub("^--([^=]+)=.*$", "\\1", argument), fixed = TRUE)
    value <- sub("^[^=]+=", "", argument)
    if (!key %in% names(defaults)) stop("Unknown argument: ", key)
    defaults[[key]] <- value
  }
  defaults
}

run_logged_command <- function(id, script, arguments, log_root) {
  log_path <- file.path(log_root, paste0(id, ".log"))
  started <- Sys.time()
  status <- system2(
    file.path(R.home("bin"), "Rscript"),
    c(script, arguments),
    stdout = log_path,
    stderr = log_path
  )
  ended <- Sys.time()
  row <- tibble::tibble(
    command_id = id,
    script = script,
    arguments = paste(arguments, collapse = " "),
    started_at_utc = format(started, tz = "UTC", usetz = TRUE),
    ended_at_utc = format(ended, tz = "UTC", usetz = TRUE),
    elapsed_seconds = as.numeric(difftime(ended, started, units = "secs")),
    exit_status = as.integer(status),
    log_path = file.path("logs", basename(log_path))
  )
  if (!identical(status, 0L)) {
    stop("Submission freeze command failed: ", id, ". See ", log_path, ".")
  }
  row
}

relative_to_project <- function(paths) {
  project <- paste0(normalizePath(here::here(), winslash = "/"), "/")
  sub(paste0("^", project), "", normalizePath(paths, winslash = "/"))
}

arguments <- parse_arguments(commandArgs(trailingOnly = TRUE))
output_root <- normalizePath(arguments$output_root, mustWork = FALSE)
assert_new_submission_freeze_root(output_root)
if (Sys.which("pdfinfo") == "" || Sys.which("pdftoppm") == "") {
  stop("pdfinfo and pdftoppm are required for the local submission freeze.")
}

dir.create(output_root, recursive = TRUE, showWarnings = FALSE)
log_root <- file.path(output_root, "logs")
manifest_root <- file.path(output_root, "manifests")
dir.create(log_root, recursive = TRUE, showWarnings = FALSE)
dir.create(manifest_root, recursive = TRUE, showWarnings = FALSE)

contract <- submission_freeze_contract()
submission_freeze_assert_files(
  c(contract$upstream_completion_flags, contract$configuration),
  "Submission freeze preflight"
)

us_validation_root <- here::here(
  "output", "validation", "us_omicron_all_outputs_20260830"
)
historical_us_result <- file.path(
  us_validation_root, "sources", "USA_monthly_result_historical.rda"
)
corrected_us_result <- file.path(
  us_validation_root, "sources", "USA_monthly_result_corrected.rda"
)
submission_freeze_assert_files(
  c(historical_us_result, corrected_us_result),
  "Locally preserved US Omicron source"
)

commands <- list(
  list(
    id = "01_validate_analysis_contract",
    script = "scripts/validation/validate_analysis_contract.R",
    arguments = character()
  ),
  list(
    id = "02_rebuild_corrected_europe",
    script = "scripts/reporting/rebuild_corrected_europe_outputs.R",
    arguments = "--render=true"
  ),
  list(
    id = "03_rebuild_uk_ie_reporting",
    script = "scripts/europe/rebuild_uk_ie_reporting.R",
    arguments = character()
  ),
  list(
    id = "04_rebuild_us_omicron_outputs",
    script = "scripts/reporting/rebuild_us_omicron_outputs.R",
    arguments = c(
      paste0("--historical-result=", historical_us_result),
      paste0("--corrected-result=", corrected_us_result),
      "--install-canonical=true",
      "--rerender-manuscript=false"
    )
  ),
  list(
    id = "05_validate_us_suppression",
    script = "scripts/validation/validate_us_suppression.R",
    arguments = character()
  ),
  list(
    id = "06_compare_figure_05_cohorts",
    script = "scripts/validation/compare_figure_05_us_cohort_rules.R",
    arguments = character()
  ),
  list(
    id = "07_render_all_reporting_outputs",
    script = "scripts/reporting/run_all.R",
    arguments = c(
      "--include_caption_review=true",
      "--strict=true",
      paste0(
        "--status_output=",
        file.path(manifest_root, "reporting_status.csv")
      )
    )
  ),
  list(
    id = "08_full_test_suite",
    script = "tests/testthat.R",
    arguments = character()
  )
)

command_status <- vector("list", length(commands))
for (i in seq_along(commands)) {
  command <- commands[[i]]
  message("Running ", command$id, "...")
  command_status[[i]] <- run_logged_command(
    command$id,
    command$script,
    command$arguments,
    log_root
  )
}
command_status <- dplyr::bind_rows(command_status)
readr::write_csv(command_status, file.path(manifest_root, "command_status.csv"))

contract <- submission_freeze_contract()
validate_submission_freeze_contract(contract)
reporting_status <- readr::read_csv(
  file.path(manifest_root, "reporting_status.csv"),
  show_col_types = FALSE
)
assert_strict_reporting_status(reporting_status)

copied <- list()
copied[["reporting_inputs"]] <- copy_submission_freeze_files(
  contract$reporting_inputs,
  output_root,
  file.path("reporting_inputs", basename(contract$reporting_inputs)),
  "reporting_input"
)
copied[["table_inputs"]] <- copy_submission_freeze_files(
  contract$table_inputs,
  output_root,
  file.path("table_inputs", relative_to_project(contract$table_inputs)),
  "table_input"
)
copied[["figures"]] <- copy_submission_freeze_files(
  contract$final_figures,
  output_root,
  file.path("figures", basename(contract$final_figures)),
  "final_figure"
)
copied[["tables"]] <- copy_submission_freeze_files(
  contract$final_tables,
  output_root,
  file.path("tables", basename(contract$final_tables)),
  "final_table"
)
copied[["config"]] <- copy_submission_freeze_files(
  contract$configuration,
  output_root,
  file.path("configuration", basename(contract$configuration)),
  "configuration"
)
copied[["completion"]] <- copy_submission_freeze_files(
  contract$upstream_completion_flags,
  output_root,
  file.path(
    "upstream_completion",
    gsub("/", "__", relative_to_project(contract$upstream_completion_flags), fixed = TRUE)
  ),
  "upstream_completion"
)

validation_roots <- c(
  here::here(
    "output", "reporting", "validation",
    "europe_corrected_psd_prior_20260830"
  ),
  here::here("output", "validation", "uk_ie_corrected_20260830"),
  here::here("output", "validation", "us_omicron_all_outputs_20260830"),
  here::here("output", "validation", "us_suppression_20260830"),
  here::here(
    "output", "validation", "figure_05_us_cohort_comparison_20260831"
  ),
  here::here("output", "validation", "figure_05_us_panel_f_comparison")
)
submission_freeze_assert_files(validation_roots, "Downstream validation root")
validation_files <- unlist(lapply(validation_roots, submission_recursive_files))
validation_relative <- unlist(lapply(seq_along(validation_roots), function(i) {
  root <- validation_roots[[i]]
  files <- submission_recursive_files(root)
  file.path(
    "validation",
    basename(root),
    substring(
      normalizePath(files, winslash = "/"),
      nchar(normalizePath(root, winslash = "/")) + 2L
    )
  )
}))
copied[["validation"]] <- copy_submission_freeze_files(
  validation_files,
  output_root,
  validation_relative,
  "downstream_validation"
)

code_paths <- c(
  list.files(here::here("R"), pattern = "[.]R$", recursive = TRUE, full.names = TRUE),
  list.files(here::here("scripts"), pattern = "[.]R$", recursive = TRUE, full.names = TRUE),
  list.files(here::here("tests"), pattern = "[.]R$", recursive = TRUE, full.names = TRUE),
  list.files(here::here("config"), recursive = TRUE, full.names = TRUE),
  file.path(here::here(), c("README.md", "DESCRIPTION", "renv.lock"))
)
code_paths <- sort(unique(code_paths[file.exists(code_paths) & !file.info(code_paths)$isdir]))
repository_manifest <- tibble::tibble(
  relative_path = relative_to_project(code_paths),
  bytes = as.numeric(file.info(code_paths)$size),
  sha256 = unname(vapply(code_paths, submission_sha256, character(1)))
)
readr::write_csv(
  repository_manifest,
  file.path(manifest_root, "repository_manifest.csv")
)

git_head <- system2("git", c("rev-parse", "HEAD"), stdout = TRUE, stderr = TRUE)
git_status <- system2("git", c("status", "--short"), stdout = TRUE, stderr = TRUE)
writeLines(git_head, file.path(output_root, "git_head.txt"))
writeLines(git_status, file.path(output_root, "git_status.txt"))
writeLines(capture.output(utils::sessionInfo()), file.path(output_root, "session_info.txt"))

figure_metadata <- collect_submission_figure_metadata(
  file.path(output_root, "figures", basename(contract$final_figures[grepl("[.]pdf$", contract$final_figures)])),
  file.path(output_root, "figures", basename(contract$final_figures[grepl("[.]png$", contract$final_figures)]))
)
figure_metadata$path <- sub(
  paste0("^", normalizePath(output_root, winslash = "/"), "/"),
  "",
  normalizePath(figure_metadata$path, winslash = "/")
)
readr::write_csv(
  figure_metadata,
  file.path(manifest_root, "figure_metadata.csv")
)

copied_table <- dplyr::bind_rows(copied)
artifact_manifest <- build_submission_freeze_manifest(
  copied_table$path,
  output_root,
  copied_table$role
)
readr::write_csv(
  artifact_manifest,
  file.path(manifest_root, "artifact_manifest.csv")
)
validate_submission_freeze_manifest(artifact_manifest, output_root)

readme <- c(
  "# Local submission freeze",
  "",
  "This directory freezes the corrected reporting inputs, manuscript figures and",
  "table, selected downstream validation figures, command logs, source-code/config",
  "hashes, and runtime provenance for the local submission build.",
  "",
  "It does not contain manuscript authoring files, does not refit models, and is not",
  "a Zenodo upload bundle.",
  "",
  "`automatic_checks.flag` indicates that all scripted rebuilds, the strict six-output",
  "reporting build, the full test suite, hash validation, and figure metadata checks",
  "passed. `complete.flag` is added only after rendered-PDF visual review."
)
writeLines(readme, file.path(output_root, "README.md"))

root_manifest_hash <- submission_sha256(
  file.path(manifest_root, "artifact_manifest.csv")
)
writeLines(
  c(
    "Local submission freeze automatic checks passed.",
    paste0("completed_at_utc=", format(Sys.time(), tz = "UTC", usetz = TRUE)),
    paste0("artifact_manifest_sha256=", root_manifest_hash),
    paste0("command_count=", nrow(command_status)),
    paste0("artifact_count=", nrow(artifact_manifest)),
    "visual_qa=pending"
  ),
  file.path(output_root, "automatic_checks.flag")
)
message("Automatic local submission freeze complete; visual QA remains: ", output_root)
