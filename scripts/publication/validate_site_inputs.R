#!/usr/bin/env Rscript

validate_site_inputs <- function(project_root = ".") {
  if (!requireNamespace("digest", quietly = TRUE)) {
    stop("Package 'digest' is required to validate publication files.")
  }

  project_root <- normalizePath(project_root, winslash = "/", mustWork = TRUE)
  manifests <- c(
    file.path(project_root, "figures", "manuscript", "manifest.csv"),
    file.path(project_root, "tables", "manuscript", "manifest.csv")
  )
  if (any(!file.exists(manifests))) {
    stop("Publication manifests are missing.")
  }

  records <- do.call(
    rbind,
    lapply(manifests, utils::read.csv, stringsAsFactors = FALSE, check.names = FALSE)
  )
  expected_formats <- c(pdf = 5L, png = 5L, csv = 1L, html = 1L)
  observed_formats <- table(factor(records$format, levels = names(expected_formats)))
  if (!identical(unname(as.integer(observed_formats)), unname(expected_formats))) {
    stop("Publication manifests must contain five PDFs, five PNGs, one CSV, and one HTML table.")
  }

  artifact_paths <- file.path(project_root, records$public_path)
  if (any(!file.exists(artifact_paths))) {
    stop(
      "Tracked publication files are missing: ",
      paste(records$public_path[!file.exists(artifact_paths)], collapse = ", ")
    )
  }
  observed_hashes <- vapply(
    artifact_paths,
    digest::digest,
    character(1L),
    algo = "sha256",
    file = TRUE,
    serialize = FALSE
  )
  if (any(observed_hashes != records$sha256)) {
    stop(
      "Publication-file hash verification failed: ",
      paste(records$public_path[observed_hashes != records$sha256], collapse = ", ")
    )
  }

  generation_scripts <- unique(file.path(project_root, records$generation_script))
  if (any(!file.exists(generation_scripts))) {
    stop("A publication generation script is missing.")
  }

  pages <- c(
    "index.Rmd",
    "regional_analyses.Rmd",
    "methods.Rmd",
    sprintf("figure_%02d.Rmd", 1:5),
    "table_01.Rmd",
    "supplementary.Rmd",
    "data_sources.Rmd",
    "reproduction.Rmd"
  )
  page_paths <- file.path(project_root, "analysis", pages)
  if (any(!file.exists(page_paths))) {
    stop("Workflowr source pages are incomplete.")
  }

  public_text <- paste(
    unlist(lapply(page_paths, readLines, warn = FALSE), use.names = FALSE),
    collapse = "\n"
  )
  prohibited <- c("/Users/", "file://")
  detected <- prohibited[vapply(prohibited, grepl, logical(1L), x = public_text, fixed = TRUE)]
  if (length(detected) > 0L) {
    stop("Local filesystem references were found in public pages: ", paste(detected, collapse = ", "))
  }

  invisible(list(
    pages = pages,
    records = records,
    artifact_paths = artifact_paths
  ))
}

is_direct_call <- any(grepl(
  "validate_site_inputs[.]R$",
  commandArgs(trailingOnly = FALSE)
))
if (is_direct_call) {
  validate_site_inputs(".")
  message("Publication site inputs passed validation.")
}
