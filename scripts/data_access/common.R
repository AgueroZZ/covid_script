parse_download_arguments <- function(arguments, provider) {
  defaults <- list(
    output_dir = file.path("provider_downloads", provider)
  )
  for (argument in arguments) {
    if (!grepl("^--output-dir=", argument)) {
      stop("Only --output-dir=<path> is supported.")
    }
    defaults$output_dir <- sub("^--output-dir=", "", argument)
  }
  defaults
}

prepare_download_directory <- function(path) {
  project_root <- normalizePath(".", winslash = "/", mustWork = TRUE)
  canonical_root <- file.path(project_root, "data", "raw")
  resolved <- if (grepl("^/", path)) path else file.path(project_root, path)
  resolved <- normalizePath(resolved, winslash = "/", mustWork = FALSE)
  if (resolved == canonical_root || startsWith(resolved, paste0(canonical_root, "/"))) {
    stop("Provider downloads must not overwrite the canonical data/raw snapshots.")
  }
  dir.create(resolved, recursive = TRUE, showWarnings = FALSE)
  normalizePath(resolved, winslash = "/", mustWork = TRUE)
}

download_provider_file <- function(url, destination) {
  if (file.exists(destination)) {
    stop("Refusing to overwrite an existing provider download: ", destination)
  }
  old_timeout <- getOption("timeout")
  on.exit(options(timeout = old_timeout), add = TRUE)
  options(timeout = max(600, old_timeout))
  utils::download.file(
    url,
    destination,
    mode = "wb",
    quiet = FALSE,
    headers = c("User-Agent" = "covid-mortality-reproducibility/1.0")
  )
  if (!file.exists(destination) || file.info(destination)$size <= 0L) {
    stop("Provider download is missing or empty: ", destination)
  }
  destination
}

write_download_record <- function(output_dir, provider, urls, files) {
  record <- c(
    paste0("provider=", provider),
    paste0("downloaded_at_utc=", format(Sys.time(), tz = "UTC", usetz = TRUE)),
    "provider_data_may_be_revised_after_download=true",
    "exact_reproduction_uses_repository_snapshots=true",
    paste0("url=", urls),
    paste0("file=", basename(files))
  )
  writeLines(record, file.path(output_dir, "download_record.txt"))
}
