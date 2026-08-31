#!/usr/bin/env Rscript

script_argument <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(script_argument) != 1L) {
  stop("Unable to determine the Canada refit script path.")
}
script_path <- normalizePath(sub("^--file=", "", script_argument))
project_root <- normalizePath(file.path(dirname(script_path), "..", "..", ".."))

suppressPackageStartupMessages({
  library(aghq)
  library(BayesGP)
  library(digest)
  library(dplyr)
  library(ISOweek)
  library(lubridate)
  library(Matrix)
  library(OSplines)
  library(sGPfit)
  library(TMB)
})
source(file.path(project_root, "R", "canada_model.R"))
source(file.path(project_root, "code", "regions", "canada", "model_functions.R"))

parse_refit_arguments <- function(arguments) {
  defaults <- list(
    data = file.path(project_root, "data", "raw", "statcan", "13100768.csv"),
    output_root = file.path(project_root, "output", "results", "canada"),
    workers = "1",
    model_id = "all",
    force = "false",
    manifest_only = "false"
  )
  if (length(arguments) == 0L) return(defaults)
  if (!all(grepl("^--[A-Za-z0-9_-]+=.+$", arguments))) {
    stop("Arguments must use --name=value syntax.")
  }
  parsed <- sub("^--", "", arguments)
  argument_names <- gsub("-", "_", sub("=.*$", "", parsed), fixed = TRUE)
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

validate_thread_environment <- function() {
  required <- c(
    OMP_NUM_THREADS = "1",
    OPENBLAS_NUM_THREADS = "1",
    MKL_NUM_THREADS = "1",
    BLIS_NUM_THREADS = "1",
    OMP_THREAD_LIMIT = "1"
  )
  observed <- Sys.getenv(names(required))
  invalid <- names(required)[observed != required]
  if (length(invalid) > 0L) {
    stop(
      "Numerical thread variables must all equal 1: ",
      paste(invalid, collapse = ", "),
      "."
    )
  }
  invisible(observed)
}

write_csv_atomic <- function(data, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  temporary <- tempfile(pattern = ".csv-", tmpdir = dirname(path))
  on.exit(unlink(temporary), add = TRUE)
  utils::write.csv(data, temporary, row.names = FALSE, na = "")
  if (!file.rename(temporary, path)) {
    stop("Failed to atomically install CSV output: ", path, ".")
  }
  invisible(path)
}

arguments <- parse_refit_arguments(commandArgs(trailingOnly = TRUE))
workers <- suppressWarnings(as.integer(arguments$workers))
if (is.na(workers) || workers < 1L || workers > 4L) {
  stop("workers must be between 1 and 4.")
}
force <- parse_boolean(arguments$force, "--force")
manifest_only <- parse_boolean(arguments$manifest_only, "--manifest-only")
output_root <- normalizePath(arguments$output_root, mustWork = FALSE)
dir.create(file.path(output_root, "manifests"), recursive = TRUE, showWarnings = FALSE)

data <- dplyr::bind_rows(
  read_canada_model_input(arguments$data, stratified_by_sex = TRUE),
  read_canada_model_input(arguments$data, stratified_by_sex = FALSE)
)
manifest <- canada_model_manifest(data)
manifest_path <- file.path(output_root, "manifests", "model_manifest.csv")
write_csv_atomic(manifest, manifest_path)

metadata <- list(
  started_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
  arguments = arguments,
  workers = workers,
  model_count = nrow(manifest),
  data_sha256 = digest::digest(file = arguments$data, algo = "sha256"),
  model_module_sha256 = digest::digest(
    file = file.path(project_root, "R", "canada_model.R"),
    algo = "sha256"
  ),
  model_functions_sha256 = digest::digest(
    file = file.path(
      project_root, "code", "regions", "canada", "model_functions.R"
    ),
    algo = "sha256"
  ),
  runner_sha256 = digest::digest(file = script_path, algo = "sha256"),
  session_info = utils::sessionInfo()
)
saveRDS(metadata, file.path(output_root, "manifests", "batch_metadata.rds"))

if (manifest_only) {
  message("Wrote the Canada model manifest: ", manifest_path)
  quit(save = "no", status = 0L)
}

validate_thread_environment()
selected_manifest <- manifest
if (!identical(arguments$model_id, "all")) {
  selected_manifest <- manifest[
    manifest$model_id == arguments$model_id,
    ,
    drop = FALSE
  ]
  if (nrow(selected_manifest) != 1L) {
    stop("Unknown --model-id: ", arguments$model_id, ".")
  }
  workers <- 1L
}

run_one <- function(index) {
  row <- selected_manifest[index, , drop = FALSE]
  model_file <- file.path(output_root, "fitted_model", paste0(row$model_id, ".rda"))
  summary_file <- file.path(output_root, "wave_summary", paste0(row$model_id, ".csv"))
  completion_file <- file.path(output_root, "complete", paste0(row$model_id, ".flag"))
  dir.create(dirname(model_file), recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(summary_file), recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(completion_file), recursive = TRUE, showWarnings = FALSE)
  if (!force && all(file.exists(c(model_file, summary_file, completion_file)))) {
    return(data.frame(model_id = row$model_id, status = "already_complete"))
  }

  selected <- canada_model_data(data, row)
  set.seed(row$seed)
  prior_parameter <- list(u = 0.1, alpha = 0.01)
  model_list <- fit_mod_IWP_sGP(
    canada_death = selected,
    prior_IWP = list(prior = "exp", param = prior_parameter, h = 5),
    prior_sGP = list(prior = "exp", param = prior_parameter, h = 1),
    prior_overdis = list(prior = "exp", param = prior_parameter),
    k_IWP = row$k_iwp,
    k_sGP = row$k_sgp,
    prov = row$province,
    Age = row$age_group,
    m = 4
  )
  model_pred <- pred_mortality_obs(
    model_list = model_list,
    refined_pred = model_list$x_full
  )
  save(model_pred, file = model_file, version = 3)
  summary <- excess_mortality_aggregate(
    prov = row$province,
    Age = row$age_group,
    model_pred = model_pred,
    canada_death = selected
  )
  summary$analysis_path <- row$analysis_path
  summary$province <- row$province
  summary$age_group <- row$age_group
  summary$sex <- row$sex
  write_csv_atomic(summary, summary_file)
  writeLines("complete", completion_file)
  data.frame(model_id = row$model_id, status = "complete")
}

statuses <- if (workers == 1L) {
  lapply(seq_len(nrow(selected_manifest)), run_one)
} else {
  parallel::mclapply(
    seq_len(nrow(selected_manifest)),
    run_one,
    mc.cores = workers,
    mc.preschedule = FALSE
  )
}
status <- do.call(rbind, statuses)
status$finished_at <- format(Sys.time(), tz = "UTC", usetz = TRUE)
write_csv_atomic(status, file.path(output_root, "manifests", "batch_status.csv"))
writeLines("complete", file.path(output_root, "batch_complete.flag"))
