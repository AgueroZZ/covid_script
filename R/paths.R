project_path <- function(...) {
  here::here(...)
}

artifact_path <- function(config, component, ...) {
  path <- project_path(config$project$artifact_root, component, ...)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  path
}
