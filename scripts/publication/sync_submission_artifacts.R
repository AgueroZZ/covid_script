#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
freeze_arg <- if (length(args) >= 1L) args[[1L]] else {
  file.path("output", "submission_freeze", "local_20260831")
}

if (!requireNamespace("digest", quietly = TRUE)) {
  stop("Package 'digest' is required to verify SHA-256 hashes.")
}

project_root <- normalizePath(".", winslash = "/", mustWork = TRUE)
freeze_root <- if (grepl("^/", freeze_arg)) {
  normalizePath(freeze_arg, winslash = "/", mustWork = TRUE)
} else {
  normalizePath(file.path(project_root, freeze_arg), winslash = "/", mustWork = TRUE)
}

relative_to_project <- function(path) {
  normalized <- normalizePath(path, winslash = "/", mustWork = FALSE)
  prefix <- paste0(project_root, "/")
  if (startsWith(normalized, prefix)) substring(normalized, nchar(prefix) + 1L) else normalized
}

sha256_file <- function(path) {
  digest::digest(path, algo = "sha256", file = TRUE, serialize = FALSE)
}

complete_path <- file.path(freeze_root, "complete.flag")
manifest_path <- file.path(freeze_root, "manifests", "artifact_manifest.csv")
metadata_path <- file.path(freeze_root, "manifests", "figure_metadata.csv")

required_control_files <- c(complete_path, manifest_path, metadata_path)
if (any(!file.exists(required_control_files))) {
  stop(
    "The freeze is incomplete; missing control files: ",
    paste(relative_to_project(required_control_files[!file.exists(required_control_files)]), collapse = ", ")
  )
}

complete_lines <- readLines(complete_path, warn = FALSE)
if (!any(grepl("^visual_qa=pass$", complete_lines))) {
  stop("The freeze has not passed visual quality assurance.")
}

declared_manifest_hash <- sub(
  "^artifact_manifest_sha256=",
  "",
  complete_lines[grepl("^artifact_manifest_sha256=", complete_lines)][[1L]]
)
observed_manifest_hash <- sha256_file(manifest_path)
if (!identical(declared_manifest_hash, observed_manifest_hash)) {
  stop("The freeze artifact manifest hash does not match complete.flag.")
}

freeze_manifest <- utils::read.csv(
  manifest_path,
  stringsAsFactors = FALSE,
  check.names = FALSE
)
selected <- freeze_manifest[
  freeze_manifest$role %in% c("final_figure", "final_table"),
  ,
  drop = FALSE
]

expected_roles <- c(final_figure = 10L, final_table = 2L)
observed_roles <- table(factor(selected$role, levels = names(expected_roles)))
if (!identical(unname(as.integer(observed_roles)), unname(expected_roles))) {
  stop("The freeze must contain ten final figure files and two final table files.")
}

source_paths <- file.path(freeze_root, selected$relative_path)
if (any(!file.exists(source_paths))) {
  stop(
    "The freeze is missing publication files: ",
    paste(selected$relative_path[!file.exists(source_paths)], collapse = ", ")
  )
}

observed_hashes <- vapply(source_paths, sha256_file, character(1L))
if (any(observed_hashes != selected$sha256)) {
  stop(
    "SHA-256 verification failed for: ",
    paste(selected$relative_path[observed_hashes != selected$sha256], collapse = ", ")
  )
}

reporting_registry <- utils::read.csv(
  file.path(project_root, "config", "reporting_outputs.csv"),
  stringsAsFactors = FALSE,
  check.names = FALSE
)
figure_metadata <- utils::read.csv(
  metadata_path,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

public_root <- c(
  final_figure = file.path(project_root, "figures", "manuscript"),
  final_table = file.path(project_root, "tables", "manuscript")
)
invisible(lapply(public_root, dir.create, recursive = TRUE, showWarnings = FALSE))

destination_paths <- file.path(
  unname(public_root[selected$role]),
  basename(selected$relative_path)
)
copied <- file.copy(source_paths, destination_paths, overwrite = TRUE, copy.mode = TRUE)
if (any(!copied)) {
  stop("Failed to copy one or more validated publication files.")
}

copied_hashes <- vapply(destination_paths, sha256_file, character(1L))
if (!identical(unname(copied_hashes), unname(observed_hashes))) {
  stop("A publication file changed while it was copied.")
}

artifact_id <- sub("_(model|europe|north|vaccination|sex|wave).*", "", basename(selected$relative_path))
artifact_id <- sub("\\.(pdf|png|csv|html)$", "", artifact_id)
artifact_id <- ifelse(
  grepl("^(figure|table)_[0-9]{2}", basename(selected$relative_path)),
  sub("^((figure|table)_[0-9]{2}).*$", "\\1", basename(selected$relative_path)),
  artifact_id
)
registry_index <- match(artifact_id, reporting_registry$output_id)
if (anyNA(registry_index)) {
  stop("A publication artifact is not registered in config/reporting_outputs.csv.")
}

format <- tolower(tools::file_ext(destination_paths))
metadata_value <- rep(NA_character_, length(destination_paths))
for (i in seq_along(destination_paths)) {
  if (format[[i]] %in% c("pdf", "png")) {
    meta_row <- figure_metadata[
      figure_metadata$path == selected$relative_path[[i]],
      ,
      drop = FALSE
    ]
    if (nrow(meta_row) == 1L) {
      metadata_value[[i]] <- paste0(
        "pages=", meta_row$pages[[1L]], "; dimensions=", meta_row$dimensions[[1L]]
      )
    }
  } else if (format[[i]] == "csv") {
    table_data <- utils::read.csv(destination_paths[[i]], check.names = FALSE)
    metadata_value[[i]] <- paste0("rows=", nrow(table_data), "; columns=", ncol(table_data))
  }
}

public_manifest <- data.frame(
  artifact_id = artifact_id,
  manuscript_label = reporting_registry$manuscript_label[registry_index],
  format = format,
  public_path = vapply(destination_paths, relative_to_project, character(1L)),
  source_freeze = relative_to_project(freeze_root),
  source_relative_path = selected$relative_path,
  bytes = as.numeric(file.info(destination_paths)$size),
  sha256 = copied_hashes,
  metadata = metadata_value,
  generation_script = reporting_registry$script_path[registry_index],
  stringsAsFactors = FALSE,
  check.names = FALSE
)

figure_manifest <- public_manifest[public_manifest$format %in% c("pdf", "png"), , drop = FALSE]
table_manifest <- public_manifest[public_manifest$format %in% c("csv", "html"), , drop = FALSE]

utils::write.csv(
  figure_manifest,
  file.path(public_root[["final_figure"]], "manifest.csv"),
  row.names = FALSE,
  na = ""
)
utils::write.csv(
  table_manifest,
  file.path(public_root[["final_table"]], "manifest.csv"),
  row.names = FALSE,
  na = ""
)

message(
  "Synchronized ", nrow(public_manifest),
  " validated files from ", relative_to_project(freeze_root), "."
)
