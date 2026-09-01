#!/usr/bin/env Rscript

script_argument <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(script_argument) != 1L) {
  stop("Unable to determine the supplementary builder path.")
}
script_path <- normalizePath(sub("^--file=", "", script_argument))
project_root <- normalizePath(file.path(dirname(script_path), "..", ".."))

suppressPackageStartupMessages({
  library(digest)
  library(dplyr)
  library(ISOweek)
  library(jsonlite)
  library(lubridate)
  library(readxl)
  library(sf)
  library(tidyr)
  library(yaml)
})

source_files <- c(
  "R/config.R",
  "R/waves.R",
  "R/validation.R",
  "R/us_data.R",
  "R/canada_model.R",
  "R/europe_model.R",
  "R/europe_reporting.R",
  "R/england_wales_model.R",
  "R/ireland_model.R",
  "R/supplementary_bundle.R"
)
invisible(lapply(file.path(project_root, source_files), source))

parse_arguments <- function(arguments) {
  defaults <- list(
    output_root = file.path(
      project_root, "output", "supplementary", "frozen_20260831"
    ),
    legacy_root = Sys.getenv(
      "COVID_EXCESS_LEGACY_ROOT",
      file.path(dirname(project_root), "covid_excess")
    ),
    force = "false"
  )
  if (length(arguments) == 0L) return(defaults)
  if (!all(grepl("^--[A-Za-z0-9_-]+=.+$", arguments))) {
    stop("Arguments must use --name=value syntax.")
  }
  parsed <- sub("^--", "", arguments)
  argument_names <- gsub("-", "_", sub("=.*$", "", parsed))
  values <- sub("^[^=]*=", "", parsed)
  unknown <- setdiff(argument_names, names(defaults))
  if (length(unknown) > 0L) {
    stop("Unknown arguments: ", paste(unknown, collapse = ", "), ".")
  }
  defaults[argument_names] <- as.list(values)
  defaults
}

parse_boolean <- function(value, label) {
  normalized <- tolower(value)
  if (!normalized %in% c("true", "false")) {
    stop(label, " must be true or false.")
  }
  identical(normalized, "true")
}

write_csv_atomic <- function(data, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  temporary <- tempfile(pattern = ".csv-", tmpdir = dirname(path))
  on.exit(unlink(temporary), add = TRUE)
  utils::write.csv(data, temporary, row.names = FALSE, na = "")
  if (!file.rename(temporary, path)) {
    stop("Failed to atomically install CSV: ", path, ".")
  }
  invisible(path)
}

write_json <- function(object, path, pretty = FALSE) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(
    object,
    path,
    auto_unbox = TRUE,
    dataframe = "rows",
    na = "null",
    null = "null",
    digits = 8,
    pretty = pretty
  )
  invisible(path)
}

build_supplementary_geometry <- function(project_root, browser_root) {
  geometry_root <- file.path(browser_root, "geometry")
  dir.create(geometry_root, recursive = TRUE, showWarnings = FALSE)

  europe_input <- readRDS(file.path(
    project_root, "output", "reporting", "inputs", "figure_02_europe_maps.rds"
  ))$map_data
  europe <- europe_input[!duplicated(europe_input$geography), ]
  europe <- sf::st_as_sf(europe[c("geography", "geometry")])
  names(europe)[names(europe) == "geography"] <- "map_id"
  europe <- sf::st_transform(europe, 3857)
  europe$geometry <- sf::st_simplify(
    europe$geometry,
    dTolerance = 2000,
    preserveTopology = TRUE
  )
  europe <- sf::st_transform(europe, 4326)
  sf::st_write(
    europe,
    file.path(geometry_root, "europe.geojson"),
    driver = "GeoJSON",
    delete_dsn = TRUE,
    quiet = TRUE
  )

  north_america_input <- readRDS(file.path(
    project_root,
    "output",
    "reporting",
    "inputs",
    "figure_03_north_america_maps.rds"
  ))$map_data
  north_america <- north_america_input[
    !duplicated(north_america_input$label),
  ]
  north_america <- sf::st_as_sf(north_america[c("label", "geometry")])
  names(north_america)[names(north_america) == "label"] <- "map_id"
  north_america <- sf::st_transform(north_america, 3857)
  north_america$geometry <- sf::st_simplify(
    north_america$geometry,
    dTolerance = 2000,
    preserveTopology = TRUE
  )
  north_america <- sf::st_transform(north_america, 4326)
  sf::st_write(
    north_america,
    file.path(geometry_root, "north_america.geojson"),
    driver = "GeoJSON",
    delete_dsn = TRUE,
    quiet = TRUE
  )
  invisible(c(
    file.path(geometry_root, "europe.geojson"),
    file.path(geometry_root, "north_america.geojson")
  ))
}

build_browser_index <- function(registry, wave_definitions, shard_paths) {
  public_registry <- registry[c(
    "analysis_id",
    "analysis_family",
    "region",
    "geography",
    "geography_label",
    "map_id",
    "age_group",
    "sex",
    "frequency",
    "status",
    "error_message"
  )]
  public_registry$shard <- unname(shard_paths[paste(
    public_registry$analysis_family,
    public_registry$geography,
    sep = "::"
  )])
  family_counts <- as.data.frame(table(
    factor(registry$analysis_family, levels = supplementary_family_levels())
  ), stringsAsFactors = FALSE)
  names(family_counts) <- c("analysis_family", "registered_cohorts")
  family_counts$successful_cohorts <- vapply(
    family_counts$analysis_family,
    function(family) sum(
      registry$analysis_family == family & registry$status == "available"
    ),
    integer(1L)
  )
  list(
    schema_version = "1.0.0",
    frozen_on = "2026-08-31",
    registered_cohorts = nrow(registry),
    successful_cohorts = sum(registry$status == "available"),
    failed_cohorts = sum(registry$status == "model_failed"),
    waves = wave_definitions,
    families = family_counts,
    cohorts = public_registry,
    downloads = list(
      registry = "downloads/registry.csv",
      pointwise = "downloads/pointwise.csv.gz",
      wave_summary = "downloads/wave_summary.csv",
      source_inventory = "downloads/source_inventory.csv",
      observed_source_inventory = "downloads/observed_source_inventory.csv",
      metadata = "downloads/bundle_metadata.yml"
    ),
    geometry = list(
      europe = "geometry/europe.geojson",
      north_america = "geometry/north_america.geojson"
    )
  )
}

build_output_manifest <- function(root) {
  paths <- list.files(root, recursive = TRUE, full.names = TRUE, all.files = FALSE)
  paths <- paths[file.info(paths)$isdir %in% FALSE]
  relative <- substring(paths, nchar(root) + 2L)
  keep <- !relative %in% c("manifest.csv", "complete.flag")
  paths <- paths[keep]
  relative <- relative[keep]
  data.frame(
    path = relative,
    bytes = as.numeric(file.info(paths)$size),
    sha256 = vapply(
      paths,
      digest::digest,
      character(1L),
      algo = "sha256",
      file = TRUE,
      serialize = FALSE
    ),
    stringsAsFactors = FALSE
  )
}

arguments <- parse_arguments(commandArgs(trailingOnly = TRUE))
force <- parse_boolean(arguments$force, "--force")
output_root <- normalizePath(arguments$output_root, winslash = "/", mustWork = FALSE)
legacy_root <- normalizePath(arguments$legacy_root, winslash = "/", mustWork = TRUE)
dir.create(dirname(output_root), recursive = TRUE, showWarnings = FALSE)

if (file.exists(file.path(output_root, "complete.flag")) && !force) {
  stop(
    "A completed supplementary freeze already exists at ", output_root,
    ". Use --force=true only for an intentional replacement."
  )
}
if (dir.exists(output_root)) {
  if (!force) stop("An incomplete output directory already exists: ", output_root, ".")
  backup <- paste0(
    output_root,
    ".superseded_",
    format(Sys.time(), "%Y%m%dT%H%M%S")
  )
  if (!file.rename(output_root, backup)) {
    stop("Failed to preserve the previous supplementary output at ", backup, ".")
  }
  message("Preserved the previous supplementary output at ", backup, ".")
}

staging_root <- tempfile(
  pattern = paste0(basename(output_root), ".building-"),
  tmpdir = dirname(output_root)
)
dir.create(staging_root, recursive = TRUE, showWarnings = FALSE)
completed <- FALSE
on.exit({
  if (!completed && dir.exists(staging_root)) {
    unlink(staging_root, recursive = TRUE)
  }
}, add = TRUE)

config <- read_analysis_config(file.path(project_root, "config", "analysis.yml"))
wave_definitions <- wave_table(config)
registry <- build_supplementary_registry(project_root, legacy_root)
observed_source_inventory <- supplementary_observed_source_inventory(project_root)
validate_supplementary_observed_source_inventory(
  registry,
  observed_source_inventory
)
observed_data <- read_supplementary_observed_data(project_root, config)
write_csv_atomic(registry, file.path(staging_root, "registry.csv"))
write_csv_atomic(
  observed_source_inventory,
  file.path(staging_root, "observed_source_inventory.csv")
)

browser_root <- file.path(staging_root, "browser")
shard_root <- file.path(browser_root, "series")
dir.create(shard_root, recursive = TRUE, showWarnings = FALSE)
pointwise_path <- file.path(staging_root, "pointwise.csv.gz")
pointwise_connection <- gzfile(pointwise_path, open = "wb", compression = 9L)
on.exit(try(close(pointwise_connection), silent = TRUE), add = TRUE)
first_pointwise_write <- TRUE
wave_results <- list()
inventory_results <- list()
shard_paths <- character()

group_key <- paste(registry$analysis_family, registry$geography, sep = "::")
groups <- split(seq_len(nrow(registry)), group_key)
message(
  "Building ", length(groups), " geography shards for ", nrow(registry),
  " registered cohorts."
)

for (group_index in seq_along(groups)) {
  rows <- registry[groups[[group_index]], , drop = FALSE]
  group_pointwise <- list()
  for (row_index in seq_len(nrow(rows))) {
    row <- rows[row_index, , drop = FALSE]
    if (!identical(row$status[[1]], "available")) {
      wave_results[[length(wave_results) + 1L]] <-
        supplementary_failed_wave_rows(row, wave_definitions)
      next
    }
    prediction <- load_supplementary_prediction(
      row,
      observed_data,
      project_root,
      legacy_root
    )
    summaries <- summarize_supplementary_prediction(
      prediction,
      wave_definitions,
      interval = as.numeric(unlist(config$model$posterior_interval))
    )
    utils::write.table(
      summaries$pointwise,
      pointwise_connection,
      sep = ",",
      row.names = FALSE,
      col.names = first_pointwise_write,
      quote = TRUE,
      qmethod = "double",
      na = ""
    )
    first_pointwise_write <- FALSE
    group_pointwise[[length(group_pointwise) + 1L]] <- summaries$pointwise
    wave_results[[length(wave_results) + 1L]] <- summaries$wave
    model_path <- resolve_supplementary_model_path(row, project_root, legacy_root)
    inventory_results[[length(inventory_results) + 1L]] <- data.frame(
      analysis_id = row$analysis_id,
      source_root = row$source_root,
      model_path = row$model_path,
      sha256 = digest::digest(
        model_path,
        algo = "sha256",
        file = TRUE,
        serialize = FALSE
      ),
      bytes = as.numeric(file.info(model_path)$size),
      posterior_draws = ncol(prediction$samples),
      prediction_rows = nrow(prediction$samples),
      stringsAsFactors = FALSE
    )
    rm(prediction, summaries)
  }
  pointwise <- if (length(group_pointwise) > 0L) {
    do.call(rbind, group_pointwise)
  } else {
    data.frame()
  }
  shard <- supplementary_web_shard(rows, pointwise)
  relative_shard <- file.path(
    "series",
    rows$analysis_family[[1]],
    paste0(supplementary_slug(rows$geography[[1]]), ".json")
  )
  write_json(shard, file.path(browser_root, relative_shard))
  shard_paths[[unique(group_key[groups[[group_index]]])]] <- relative_shard
  if (group_index %% 10L == 0L || group_index == length(groups)) {
    message("Completed geography shard ", group_index, " of ", length(groups), ".")
  }
  rm(pointwise, group_pointwise, shard)
  invisible(gc(verbose = FALSE))
}
close(pointwise_connection)

wave_summary <- do.call(rbind, wave_results)
source_inventory <- do.call(rbind, inventory_results)
rownames(wave_summary) <- NULL
rownames(source_inventory) <- NULL
write_csv_atomic(wave_summary, file.path(staging_root, "wave_summary.csv"))
write_csv_atomic(source_inventory, file.path(staging_root, "source_inventory.csv"))
write_json(wave_summary, file.path(browser_root, "wave_summary.json"))
build_supplementary_geometry(project_root, browser_root)

downloads_root <- file.path(browser_root, "downloads")
dir.create(downloads_root, recursive = TRUE, showWarnings = FALSE)
download_sources <- c(
  registry = file.path(staging_root, "registry.csv"),
  pointwise = pointwise_path,
  wave_summary = file.path(staging_root, "wave_summary.csv"),
  source_inventory = file.path(staging_root, "source_inventory.csv"),
  observed_source_inventory = file.path(
    staging_root,
    "observed_source_inventory.csv"
  )
)
download_targets <- file.path(downloads_root, basename(download_sources))
if (!all(file.copy(download_sources, download_targets, overwrite = TRUE))) {
  stop("Failed to stage one or more supplementary downloads.")
}

pointwise_validation <- utils::read.csv(
  gzfile(pointwise_path),
  stringsAsFactors = FALSE,
  check.names = FALSE
)
validate_supplementary_bundle_tables(
  registry,
  pointwise_validation,
  wave_summary,
  source_inventory
)

metadata <- list(
  schema_version = "1.0.0",
  frozen_on = "2026-08-31",
  created_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
  model_refitting_performed = FALSE,
  public_bundle_contains_posterior_draws = FALSE,
  registered_cohorts = nrow(registry),
  successful_cohorts = sum(registry$status == "available"),
  failed_cohorts = sum(registry$status == "model_failed"),
  pointwise_rows = nrow(pointwise_validation),
  wave_rows = nrow(wave_summary),
  source_model_bytes = sum(source_inventory$bytes),
  observed_source_files = nrow(observed_source_inventory),
  observed_source_inventory_sha256 = digest::digest(
    file.path(staging_root, "observed_source_inventory.csv"),
    algo = "sha256", file = TRUE, serialize = FALSE
  ),
  source_roots = list(
    project = "current repository root",
    legacy = "read-only sibling covid_excess root"
  ),
  input_hashes = list(
    analysis_config = digest::digest(
      file.path(project_root, "config", "analysis.yml"),
      algo = "sha256", file = TRUE, serialize = FALSE
    ),
    cohort_registry = digest::digest(
      file.path(project_root, "config", "cohorts.csv"),
      algo = "sha256", file = TRUE, serialize = FALSE
    ),
    eurostat_snapshot = digest::digest(
      file.path(project_root, "data", "raw", "eurostat", "demo_r_mwk_20_linear.csv"),
      algo = "sha256", file = TRUE, serialize = FALSE
    ),
    statcan_snapshot = digest::digest(
      file.path(project_root, "data", "raw", "statcan", "13100768.csv"),
      algo = "sha256", file = TRUE, serialize = FALSE
    )
  ),
  session_info = paste(capture.output(utils::sessionInfo()), collapse = "\n")
)
yaml::write_yaml(metadata, file.path(staging_root, "bundle_metadata.yml"))
invisible(file.copy(
  file.path(staging_root, "bundle_metadata.yml"),
  file.path(downloads_root, "bundle_metadata.yml"),
  overwrite = TRUE
))

index <- build_browser_index(registry, wave_definitions, shard_paths)
write_json(index, file.path(browser_root, "index.json"), pretty = TRUE)

validation_summary <- list(
  status = "passed",
  registered_cohorts = nrow(registry),
  successful_cohorts = length(unique(pointwise_validation$analysis_id)),
  failed_cohorts = sum(registry$status == "model_failed"),
  pointwise_rows = nrow(pointwise_validation),
  wave_rows = nrow(wave_summary),
  geography_shards = length(groups),
  observed_source_files = nrow(observed_source_inventory),
  posterior_draws_published = FALSE,
  completed_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE)
)
saveRDS(validation_summary, file.path(staging_root, "validation_summary.rds"))
manifest <- build_output_manifest(staging_root)
write_csv_atomic(manifest, file.path(staging_root, "manifest.csv"))
writeLines("complete", file.path(staging_root, "complete.flag"))

if (!file.rename(staging_root, output_root)) {
  stop("Failed to install the completed supplementary freeze at ", output_root, ".")
}
completed <- TRUE
message(
  "Supplementary freeze completed: ", output_root, "\n",
  "Registered cohorts: ", validation_summary$registered_cohorts, "\n",
  "Successful cohorts: ", validation_summary$successful_cohorts, "\n",
  "Failed cohorts: ", validation_summary$failed_cohorts, "\n",
  "Pointwise rows: ", validation_summary$pointwise_rows, "\n",
  "Wave rows: ", validation_summary$wave_rows, "\n",
  "Geography shards: ", validation_summary$geography_shards
)
