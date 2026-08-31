#!/usr/bin/env Rscript

script_argument <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(script_argument) != 1L) {
  stop("Unable to determine the England-and-Wales refit script path.")
}
script_path <- normalizePath(sub("^--file=", "", script_argument))
project_root <- normalizePath(file.path(dirname(script_path), "..", ".."))
source(file.path(project_root, "R", "europe_model.R"))
source(file.path(project_root, "R", "england_wales_model.R"))

parse_refit_arguments <- function(arguments) {
  defaults <- list(
    data_root = file.path(project_root, "UK_analysis"),
    output_root = file.path(
      project_root,
      "artifacts",
      "results",
      "england_wales_corrected_20260830"
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
if (is.na(workers) || workers < 1L || workers > 3L) {
  stop("workers must be between 1 and 3.")
}
force <- parse_boolean(arguments$force, "--force")
manifest_only <- parse_boolean(arguments$manifest_only, "--manifest-only")
output_root <- normalizePath(arguments$output_root, mustWork = FALSE)
dir.create(file.path(output_root, "manifests"), recursive = TRUE, showWarnings = FALSE)

data <- read_england_wales_model_input(arguments$data_root)
manifest <- build_england_wales_manifest(data, base_seed = 20260830L)
if (nrow(manifest) != 3L) {
  stop("Expected three England-and-Wales models; found ", nrow(manifest), ".")
}
manifest_path <- file.path(output_root, "manifests", "model_manifest.csv")
write_csv_atomic(manifest, manifest_path)
write_csv_atomic(
  england_wales_source_definitions(),
  file.path(output_root, "manifests", "source_definitions.csv")
)

metadata <- list(
  started_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
  host = Sys.info(),
  arguments = arguments,
  workers = workers,
  model_count = nrow(manifest),
  data_hash = digest::digest(data, algo = "sha256"),
  model_code_sha256 = digest::digest(
    file = file.path(project_root, "R", "england_wales_model.R"),
    algo = "sha256"
  ),
  runner_code_sha256 = digest::digest(file = script_path, algo = "sha256"),
  package_provenance = europe_package_provenance(),
  session_info = utils::sessionInfo()
)
atomic_save_rds(metadata, file.path(output_root, "manifests", "batch_metadata.rds"))

if (manifest_only) {
  message("Wrote the three-model England-and-Wales manifest: ", manifest_path)
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
  run_england_wales_model(
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
  statuses <- parallel::mclapply(
    seq_len(nrow(selected_manifest)),
    run_one,
    mc.cores = workers,
    mc.preschedule = FALSE
  )
}
status <- do.call(rbind, statuses)
status$finished_at <- format(Sys.time(), tz = "UTC", usetz = TRUE)
write_csv_atomic(status, file.path(output_root, "manifests", "batch_status.csv"))

if (any(status$status == "failed")) {
  failed <- status$model_id[status$status == "failed"]
  stop("England-and-Wales refit failures: ", paste(failed, collapse = ", "), ".")
}
all_complete <- vapply(manifest$model_id, function(model_id) {
  file.exists(england_wales_output_paths(output_root, model_id)$complete)
}, logical(1))
if (all(all_complete)) {
  atomic_write_lines("complete", file.path(output_root, "batch_complete.flag"))
}
message(
  "England-and-Wales refit command completed: ",
  sum(all_complete),
  "/",
  nrow(manifest),
  " completion flags present."
)
