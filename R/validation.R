require_files <- function(paths, label) {
  missing <- paths[!file.exists(paths)]
  if (length(missing) > 0L) {
    stop(label, " files are missing: ", paste(missing, collapse = ", "))
  }
  invisible(paths)
}

write_artifact_manifest <- function(paths, output_path) {
  require_files(paths, "Artifact")
  manifest <- tibble::tibble(
    path = paths,
    bytes = as.numeric(file.info(paths)$size),
    sha256 = vapply(
      paths,
      digest::digest,
      character(1),
      file = TRUE,
      algo = "sha256"
    )
  )
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(manifest, output_path)
  output_path
}

write_csv_artifact <- function(object, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(object, path, na = "")
  path
}
