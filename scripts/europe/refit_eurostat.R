#!/usr/bin/env Rscript

script_argument <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(script_argument) != 1L) {
  stop("Unable to determine the Europe refit script path.")
}
script_path <- normalizePath(sub("^--file=", "", script_argument))
project_root <- normalizePath(file.path(dirname(script_path), "..", ".."))
source(file.path(project_root, "R", "europe_model.R"))

parse_refit_arguments <- function(arguments) {
  defaults <- list(
    data = file.path(project_root, "EU_analysis", "demo_r_mwk_20_linear.csv"),
    output_root = file.path(
      project_root,
      "output",
      "results",
      "europe_corrected_psd_prior_20260830"
    ),
    workers = "1",
    model_id = "all",
    force = "false",
    manifest_only = "false"
  )
  if (length(arguments) == 0L) {
    return(defaults)
  }
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

write_base_csv_atomic <- function(data, path) {
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
if (is.na(workers) || workers < 1L || workers > 12L) {
  stop("workers must be between 1 and 12.")
}
force <- parse_boolean(arguments$force, "--force")
manifest_only <- parse_boolean(arguments$manifest_only, "--manifest-only")
output_root <- normalizePath(arguments$output_root, mustWork = FALSE)
dir.create(output_root, recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(output_root, "manifests"), recursive = TRUE, showWarnings = FALSE)

data <- read_europe_model_input(arguments$data)
manifest <- build_europe_manifest(data, base_seed = 20260829L)
if (nrow(manifest) != 388L) {
  stop("Expected 388 eligible Eurostat models; found ", nrow(manifest), ".")
}
manifest_path <- file.path(output_root, "manifests", "model_manifest.csv")
write_base_csv_atomic(manifest, manifest_path)

prior_contract <- europe_prior_specification()
stopifnot(
  identical(prior_contract$trend$h, 5),
  identical(prior_contract$seasonal$h, 1)
)

metadata <- list(
  started_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
  host = Sys.info(),
  arguments = arguments,
  workers = workers,
  model_count = nrow(manifest),
  data_sha256 = digest::digest(file = arguments$data, algo = "sha256"),
  model_code_sha256 = digest::digest(
    file = file.path(project_root, "R", "europe_model.R"),
    algo = "sha256"
  ),
  runner_code_sha256 = digest::digest(file = script_path, algo = "sha256"),
  package_provenance = europe_package_provenance(),
  thread_environment = Sys.getenv(c(
    "OMP_NUM_THREADS",
    "OPENBLAS_NUM_THREADS",
    "MKL_NUM_THREADS",
    "BLIS_NUM_THREADS",
    "OMP_THREAD_LIMIT"
  )),
  session_info = utils::sessionInfo()
)
atomic_save_rds(
  metadata,
  file.path(output_root, "manifests", "batch_metadata.rds")
)

if (manifest_only) {
  message("Wrote the 388-model Europe manifest: ", manifest_path)
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
  message("[", index, "/", nrow(selected_manifest), "] ", row$model_id[[1]])
  run_europe_model(
    manifest_row = row,
    data = data,
    output_root = output_root,
    force = force,
    draws = 3000L
  )
}

if (workers == 1L) {
  statuses <- lapply(seq_len(nrow(selected_manifest)), run_one)
} else {
  if (!identical(.Platform$OS.type, "unix")) {
    stop("Parallel Europe refitting requires a Unix-like operating system.")
  }
  statuses <- parallel::mclapply(
    seq_len(nrow(selected_manifest)),
    run_one,
    mc.cores = workers,
    mc.preschedule = FALSE
  )
}
status <- do.call(rbind, statuses)
status$finished_at <- format(Sys.time(), tz = "UTC", usetz = TRUE)
write_base_csv_atomic(
  status,
  file.path(output_root, "manifests", "batch_status.csv")
)

if (any(status$status == "failed")) {
  failed <- status$model_id[status$status == "failed"]
  stop("Europe refit failures: ", paste(failed, collapse = ", "), ".")
}

all_complete <- vapply(manifest$model_id, function(model_id) {
  paths <- europe_output_paths(output_root, model_id)
  file.exists(paths$complete)
}, logical(1))
if (all(all_complete)) {
  atomic_write_lines(
    "complete",
    file.path(output_root, "batch_complete.flag")
  )
}
message(
  "Europe refit command completed: ",
  sum(all_complete),
  "/",
  nrow(manifest),
  " completion flags present."
)
