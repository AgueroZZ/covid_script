build_file_manifest <- function(
  paths,
  source_ids,
  snapshot_dates,
  tracked_in_git = TRUE
) {
  if (length(paths) != length(source_ids) ||
      length(paths) != length(snapshot_dates)) {
    stop("Manifest vectors must have equal lengths.")
  }

  require_files(paths, "Raw source")

  tibble::tibble(
    source_id = source_ids,
    relative_path = paths,
    bytes = as.numeric(file.info(paths)$size),
    sha256 = unname(vapply(
      paths,
      digest::digest,
      character(1),
      file = TRUE,
      algo = "sha256"
    )),
    snapshot_date = as.Date(snapshot_dates),
    tracked_in_git = rep_len(tracked_in_git, length(paths))
  )
}

validate_file_manifest <- function(manifest, root = ".") {
  required_columns <- c(
    "source_id",
    "relative_path",
    "bytes",
    "sha256",
    "snapshot_date",
    "tracked_in_git"
  )
  if (!identical(names(manifest), required_columns)) {
    stop("Raw manifest columns do not match the canonical schema.")
  }

  paths <- file.path(root, manifest$relative_path)
  require_files(paths, "Manifest source")
  observed_hashes <- unname(vapply(
    paths,
    digest::digest,
    character(1),
    file = TRUE,
    algo = "sha256"
  ))
  if (!identical(observed_hashes, manifest$sha256)) {
    stop("One or more raw source hashes do not match the manifest.")
  }

  invisible(manifest)
}
