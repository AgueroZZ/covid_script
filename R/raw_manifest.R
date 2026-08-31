raw_manifest_columns <- function() {
  c(
    "dataset_id",
    "provider",
    "snapshot_path",
    "snapshot_date",
    "bytes",
    "sha256",
    "source_url",
    "provider_identifier",
    "access_method",
    "geography",
    "time_coverage",
    "license_note",
    "reproduction_role",
    "tracked_in_git"
  )
}

validate_file_manifest <- function(manifest, root = ".") {
  if (!identical(names(manifest), raw_manifest_columns())) {
    stop("Raw manifest columns do not match the canonical schema.")
  }
  if (nrow(manifest) == 0L || anyDuplicated(manifest$snapshot_path)) {
    stop("Raw snapshot paths must be present and unique.")
  }
  if (any(!nzchar(manifest$dataset_id)) || any(!nzchar(manifest$provider))) {
    stop("Every raw snapshot must have a dataset ID and provider.")
  }
  if (any(is.na(as.Date(manifest$snapshot_date)))) {
    stop("Raw snapshot dates must use ISO YYYY-MM-DD format.")
  }
  if (any(!grepl("^[0-9a-f]{64}$", manifest$sha256))) {
    stop("Raw snapshot SHA-256 values are malformed.")
  }
  if (any(manifest$tracked_in_git != TRUE)) {
    stop("Every canonical raw snapshot must be tracked in Git.")
  }

  paths <- file.path(root, manifest$snapshot_path)
  require_files(paths, "Manifest source")
  observed_bytes <- as.numeric(file.info(paths)$size)
  if (!identical(observed_bytes, as.numeric(manifest$bytes))) {
    stop("One or more raw source byte counts do not match the manifest.")
  }
  observed_hashes <- unname(vapply(
    paths,
    digest::digest,
    character(1L),
    file = TRUE,
    algo = "sha256",
    serialize = FALSE
  ))
  if (!identical(observed_hashes, manifest$sha256)) {
    stop("One or more raw source hashes do not match the manifest.")
  }

  invisible(manifest)
}
