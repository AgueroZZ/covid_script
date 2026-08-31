#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(digest)
  library(here)
  library(yaml)
})

source(here::here("R", "zenodo_deposit.R"))

parse_arguments <- function(arguments) {
  defaults <- list(
    deposit_root = here::here(
      "output", "zenodo", "covid_excess_mortality_results_20260831"
    ),
    archive_path = here::here(
      "output", "zenodo", "covid_excess_mortality_results_20260831.zip"
    ),
    archive = "true",
    dry_run = "false"
  )
  if (length(arguments) == 0L) {
    return(defaults)
  }
  if (!all(grepl("^--[A-Za-z0-9_-]+=.+$", arguments))) {
    stop("Arguments must use --name=value syntax.")
  }
  parsed <- sub("^--", "", arguments)
  names_parsed <- gsub("-", "_", sub("=.*$", "", parsed), fixed = TRUE)
  values <- sub("^[^=]*=", "", parsed)
  unknown <- setdiff(names_parsed, names(defaults))
  if (length(unknown) > 0L) {
    stop("Unknown arguments: ", paste(unknown, collapse = ", "), ".")
  }
  defaults[names_parsed] <- as.list(values)
  defaults
}

parse_boolean <- function(value, label) {
  normalized <- tolower(value)
  if (!normalized %in% c("true", "false")) {
    stop(label, " must be true or false.")
  }
  identical(normalized, "true")
}

arguments <- parse_arguments(commandArgs(trailingOnly = TRUE))
archive <- parse_boolean(arguments$archive, "--archive")
dry_run <- parse_boolean(arguments$dry_run, "--dry-run")
project_root <- normalizePath(here::here(), winslash = "/", mustWork = TRUE)
selection <- zenodo_deposit_selection(project_root)
validate_zenodo_deposit_selection(selection, project_root)

role_summary <- aggregate(
  rep(1L, nrow(selection)),
  by = list(role = selection$role),
  FUN = sum
)
names(role_summary)[[2L]] <- "files"
print(role_summary, row.names = FALSE)
message("Selected deposit source files: ", nrow(selection))
message(
  "Selected source size (GiB): ",
  round(sum(file.info(file.path(project_root, selection$source_path))$size) / 1024^3, 3)
)

if (dry_run) {
  message("Dry run complete; no deposit files were written.")
  quit(save = "no", status = 0L)
}

deposit_root <- normalizePath(arguments$deposit_root, winslash = "/", mustWork = FALSE)
archive_path <- normalizePath(arguments$archive_path, winslash = "/", mustWork = FALSE)
manifest <- build_zenodo_deposit(
  selection,
  deposit_root = deposit_root,
  project_root = project_root
)
validation <- validate_zenodo_deposit(deposit_root)
message(
  "Validated deposit files: ", validation$files,
  "; size (GiB): ", round(validation$bytes / 1024^3, 3)
)

if (archive) {
  if (file.exists(archive_path)) {
    stop("Deposit archive already exists: ", archive_path, ".")
  }
  zip_executable <- Sys.which("zip")
  if (!nzchar(zip_executable)) {
    stop("The 'zip' executable is required to package the Zenodo deposit.")
  }
  dir.create(dirname(archive_path), recursive = TRUE, showWarnings = FALSE)
  original_working_directory <- getwd()
  setwd(dirname(deposit_root))
  status <- tryCatch(
    system2(
      zip_executable,
      c(
        "-0", "-q", "-X", "-r",
        shQuote(archive_path),
        shQuote(basename(deposit_root))
      )
    ),
    finally = setwd(original_working_directory)
  )
  if (!identical(status, 0L)) {
    stop("Failed to create the Zenodo ZIP archive.")
  }
  message("Created Zenodo archive: ", archive_path)
  message("Archive size (GiB): ", round(file.info(archive_path)$size / 1024^3, 3))
}

message("Prepared Zenodo deposit: ", deposit_root)
message("Payload manifest rows: ", nrow(manifest))
