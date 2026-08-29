#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(digest)
  library(dplyr)
  library(readr)
  library(tibble)
})

arguments <- commandArgs(trailingOnly = TRUE)
if (length(arguments) != 3L) {
  stop("Usage: build_provenance_manifest.R REPO_ROOT ARCHIVE_ROOT OUTPUT_DIR")
}

repo_root <- normalizePath(arguments[[1]], mustWork = TRUE)
archive_root <- normalizePath(arguments[[2]], mustWork = TRUE)
output_dir <- arguments[[3]]
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

classify_file <- function(path) {
  extension <- tolower(tools::file_ext(path))
  dplyr::case_when(
    extension %in% c("r", "cpp") ~ "code",
    grepl("fitted_model|fitted_mod", path) &
      extension %in% c("rda", "rds", "rdata") ~ "model",
    grepl("result", path, ignore.case = TRUE) &
      extension %in% c("rda", "rds", "rdata", "csv") ~ "result",
    extension %in% c("csv", "txt", "xlsx", "data", "gz") ~ "input",
    extension %in% c("png", "pdf", "svg") ~ "figure",
    extension %in% c("shp", "shx", "dbf", "prj", "cpg", "zip") ~ "geospatial",
    TRUE ~ "other"
  )
}

inventory_tree <- function(root, include_figures) {
  paths <- list.files(
    root,
    recursive = TRUE,
    full.names = TRUE,
    all.files = TRUE,
    no.. = TRUE
  )
  information <- file.info(paths)
  paths <- paths[information$isdir %in% FALSE]
  paths <- paths[!grepl("/(\\.git|\\.Rproj.user)/", paths)]
  paths <- paths[!basename(paths) %in% c(".DS_Store", ".Rhistory")]

  roles <- classify_file(paths)
  if (!include_figures) {
    keep <- roles != "figure"
    paths <- paths[keep]
    roles <- roles[keep]
  }

  information <- file.info(paths)
  tibble(
    relative_path = sub(paste0("^", root, "/"), "", paths),
    role = roles,
    bytes = as.numeric(information$size),
    modified_utc = format(information$mtime, tz = "UTC", usetz = TRUE),
    sha256 = vapply(
      paths,
      digest::digest,
      character(1),
      file = TRUE,
      algo = "sha256"
    )
  ) %>%
    arrange(relative_path)
}

repo_inventory <- inventory_tree(repo_root, include_figures = TRUE)
archive_inventory <- inventory_tree(archive_root, include_figures = FALSE)

repo_scripts <- repo_inventory %>% filter(role == "code")
archive_scripts <- archive_inventory %>% filter(role == "code")
script_hash_map <- repo_scripts %>%
  select(repo_path = relative_path, repo_sha256 = sha256) %>%
  left_join(
    archive_scripts %>%
      select(archive_path = relative_path, archive_sha256 = sha256),
    by = c("repo_sha256" = "archive_sha256"),
    relationship = "many-to-many"
  ) %>%
  mutate(
    match_status = if_else(
      is.na(archive_path),
      "no_exact_match",
      "exact_match"
    )
  ) %>%
  arrange(repo_path, archive_path)

write_csv(repo_inventory, file.path(output_dir, "repo_inventory.csv"))
write_csv(archive_inventory, file.path(output_dir, "archive_inventory.csv"))
write_csv(script_hash_map, file.path(output_dir, "script_hash_map.csv"))
