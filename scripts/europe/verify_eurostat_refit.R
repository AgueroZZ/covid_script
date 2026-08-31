#!/usr/bin/env Rscript

script_argument <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(script_argument) != 1L) {
  stop("Unable to determine the Europe verification script path.")
}
script_path <- normalizePath(sub("^--file=", "", script_argument))
project_root <- normalizePath(file.path(dirname(script_path), "..", ".."))
source(file.path(project_root, "R", "europe_model.R"))

parse_verification_arguments <- function(arguments) {
  defaults <- list(
    data = file.path(project_root, "EU_analysis", "demo_r_mwk_20_linear.csv"),
    output_root = file.path(
      project_root,
      "output",
      "results",
      "europe_corrected_psd_prior_20260830"
    )
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

write_base_csv_atomic <- function(data, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  temporary <- tempfile(pattern = ".csv-", tmpdir = dirname(path))
  on.exit(unlink(temporary), add = TRUE)
  utils::write.csv(data, temporary, row.names = FALSE, na = "")
  if (!file.rename(temporary, path)) {
    stop("Failed to atomically install verification CSV: ", path, ".")
  }
  invisible(path)
}

arguments <- parse_verification_arguments(commandArgs(trailingOnly = TRUE))
output_root <- normalizePath(arguments$output_root, mustWork = FALSE)
data <- read_europe_model_input(arguments$data)
manifest <- build_europe_manifest(data, base_seed = 20260829L)
if (nrow(manifest) != 388L) {
  stop("Expected 388 models in the verification manifest.")
}

verification_rows <- lapply(seq_len(nrow(manifest)), function(index) {
  row <- manifest[index, , drop = FALSE]
  model_id <- row$model_id[[1]]
  paths <- europe_output_paths(output_root, model_id)
  series <- prepare_europe_series(data, row)
  error_message <- ""
  valid <- tryCatch({
    if (!all(file.exists(c(paths$result, paths$diagnostic, paths$complete)))) {
      stop("Missing result, diagnostic, or completion flag.")
    }
    model_pred <- load_europe_model_pred(paths$result)
    validate_europe_model_pred(
      model_pred,
      expected_rows = nrow(series$full_data),
      expected_draws = 3000L
    )
    diagnostic <- readRDS(paths$diagnostic)
    observed_hash <- digest::digest(file = paths$result, algo = "sha256")
    if (!identical(unname(diagnostic$result_sha256), unname(observed_hash))) {
      stop("Result SHA-256 does not match its diagnostic record.")
    }
    if (!identical(as.integer(diagnostic$convergence), 0L)) {
      stop("Model convergence code is not zero.")
    }
    TRUE
  }, error = function(error) {
    error_message <<- conditionMessage(error)
    FALSE
  })
  data.frame(
    model_id = model_id,
    valid = valid,
    result_bytes = if (file.exists(paths$result))
      unname(file.info(paths$result)$size) else NA_real_,
    message = error_message,
    stringsAsFactors = FALSE
  )
})
verification <- do.call(rbind, verification_rows)

figure_01_path <- europe_output_paths(
  output_root,
  "NL_Y_GE80_T"
)$figure_01
figure_01_valid <- tryCatch({
  if (!file.exists(figure_01_path)) {
    stop("Missing compact Figure 1 component input.")
  }
  validate_europe_figure_01_input(readRDS(figure_01_path))
  TRUE
}, error = function(error) FALSE)

batch_status_path <- file.path(output_root, "manifests", "batch_status.csv")
batch_status_valid <- FALSE
if (file.exists(batch_status_path)) {
  batch_status <- utils::read.csv(batch_status_path, stringsAsFactors = FALSE)
  batch_status_valid <- nrow(batch_status) == 388L &&
    !any(batch_status$status == "failed")
}
batch_complete <- file.exists(file.path(output_root, "batch_complete.flag"))
overall_valid <- all(verification$valid) &&
  figure_01_valid &&
  batch_status_valid &&
  batch_complete

summary <- list(
  verified_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
  model_count = nrow(verification),
  valid_models = sum(verification$valid),
  invalid_models = sum(!verification$valid),
  figure_01_valid = figure_01_valid,
  batch_status_valid = batch_status_valid,
  batch_complete = batch_complete,
  overall_valid = overall_valid,
  total_result_bytes = sum(verification$result_bytes, na.rm = TRUE),
  data_sha256 = digest::digest(file = arguments$data, algo = "sha256"),
  verifier_sha256 = digest::digest(file = script_path, algo = "sha256")
)

write_base_csv_atomic(
  verification,
  file.path(output_root, "verification.csv")
)
atomic_save_rds(
  summary,
  file.path(output_root, "verification_summary.rds")
)
if (overall_valid) {
  atomic_write_lines(
    "verified",
    file.path(output_root, "verified_complete.flag")
  )
} else {
  stop(
    "Europe verification failed: ",
    sum(verification$valid),
    "/388 models valid; Figure 1 = ",
    figure_01_valid,
    "; batch status = ",
    batch_status_valid,
    "; batch complete = ",
    batch_complete,
    "."
  )
}
message("Verified all 388 compact Europe refit outputs.")
