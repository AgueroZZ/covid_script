submission_freeze_contract <- function(root = here::here()) {
  project_path <- function(...) file.path(root, ...)
  list(
    reporting_inputs = project_path(
      "artifacts", "reporting", "inputs",
      c(
        "figure_01_model_illustration.rds",
        "figure_02_europe_maps.rds",
        "figure_03_north_america_maps.rds",
        "figure_04_vaccination_pscore.csv",
        "figure_05_sex_difference.csv"
      )
    ),
    table_inputs = c(
      project_path("artifacts", "results", "europe", "wave_summary.csv"),
      project_path("artifacts", "results", "us", "wave_summary.csv"),
      project_path("artifacts", "data", "europe", "vaccination_membership.csv"),
      project_path("artifacts", "data", "us", "vaccination_membership.csv")
    ),
    final_figures = unlist(lapply(
      sprintf("figure_%02d", 1:5),
      function(id) project_path(
        "artifacts", "figures",
        paste0(
          id,
          c(
            "_model_illustration", "_europe_maps", "_north_america_maps",
            "_vaccination_pscore", "_sex_difference"
          )[as.integer(sub("figure_", "", id))],
          c(".pdf", ".png")
        )
      )
    ), use.names = FALSE),
    final_tables = project_path(
      "artifacts", "tables",
      c("table_01_wave_pscores.csv", "table_01_wave_pscores.html")
    ),
    upstream_completion_flags = c(
      project_path(
        "artifacts", "results", "europe_corrected_psd_prior_20260830",
        "verified_complete.flag"
      ),
      project_path(
        "artifacts", "results", "england_wales_corrected_20260830",
        "batch_complete.flag"
      ),
      project_path(
        "artifacts", "results", "ireland_corrected_20260830",
        "batch_complete.flag"
      ),
      project_path(
        "artifacts", "reporting", "validation",
        "europe_corrected_psd_prior_20260830", "complete.flag"
      )
    ),
    configuration = c(
      project_path("config", "analysis.yml"),
      project_path("config", "manuscript_analysis_contract.yml"),
      project_path("config", "reporting_outputs.csv"),
      project_path("config", "reporting_panels.csv"),
      project_path("config", "reporting_vaccination_groups.csv"),
      project_path("config", "europe_reporting_cohort.csv"),
      project_path("config", "uk_ie_reporting_cohort.csv"),
      project_path("config", "us_reporting_cohort.csv"),
      project_path("config", "us_table_01_cohort.csv")
    )
  )
}

submission_freeze_assert_files <- function(paths, label) {
  missing <- paths[!file.exists(paths)]
  if (length(missing) > 0L) {
    stop(label, " files are missing: ", paste(missing, collapse = ", "), ".")
  }
  empty <- paths[file.info(paths)$size <= 0L]
  if (length(empty) > 0L) {
    stop(label, " files are empty: ", paste(empty, collapse = ", "), ".")
  }
  invisible(paths)
}

validate_submission_freeze_contract <- function(contract) {
  required_groups <- c(
    "reporting_inputs", "table_inputs", "final_figures", "final_tables",
    "upstream_completion_flags", "configuration"
  )
  missing_groups <- setdiff(required_groups, names(contract))
  if (length(missing_groups) > 0L) {
    stop("Submission freeze contract is missing groups: ",
         paste(missing_groups, collapse = ", "), ".")
  }
  paths <- unlist(contract[required_groups], use.names = FALSE)
  if (anyDuplicated(paths)) stop("Submission freeze contract contains duplicate paths.")
  submission_freeze_assert_files(paths, "Submission freeze contract")
  invisible(contract)
}

assert_new_submission_freeze_root <- function(path) {
  if (file.exists(path)) {
    stop("Submission freeze output root already exists: ", path, ".")
  }
  invisible(path)
}

assert_strict_reporting_status <- function(status) {
  required <- c("output_id", "status")
  if (!all(required %in% names(status))) {
    stop("Reporting status must contain output_id and status.")
  }
  incomplete <- status$output_id[status$status != "rendered"]
  if (length(incomplete) > 0L) {
    stop("Strict reporting build did not render: ",
         paste(incomplete, collapse = ", "), ".")
  }
  invisible(status)
}

submission_sha256 <- function(path) {
  digest::digest(file = path, algo = "sha256", serialize = FALSE)
}

copy_submission_freeze_files <- function(
  paths,
  destination_root,
  relative_paths,
  role
) {
  if (length(paths) != length(relative_paths)) {
    stop("Source and relative-path vectors must have equal lengths.")
  }
  if (length(role) == 1L) role <- rep(role, length(paths))
  if (length(role) != length(paths)) stop("Artifact roles must align with paths.")
  submission_freeze_assert_files(paths, "Freeze source")
  if (anyDuplicated(relative_paths)) stop("Freeze destination paths must be unique.")
  destinations <- file.path(destination_root, relative_paths)
  for (i in seq_along(paths)) {
    dir.create(dirname(destinations[[i]]), recursive = TRUE, showWarnings = FALSE)
    copied <- file.copy(paths[[i]], destinations[[i]], overwrite = FALSE, copy.mode = TRUE)
    if (!isTRUE(copied)) stop("Failed to copy freeze artifact: ", paths[[i]], ".")
    if (!identical(submission_sha256(paths[[i]]), submission_sha256(destinations[[i]]))) {
      stop("Freeze copy hash mismatch: ", relative_paths[[i]], ".")
    }
  }
  tibble::tibble(path = destinations, relative_path = relative_paths, role = role)
}

build_submission_freeze_manifest <- function(paths, root, role) {
  submission_freeze_assert_files(paths, "Manifest artifact")
  normalized_root <- paste0(normalizePath(root, winslash = "/", mustWork = TRUE), "/")
  normalized <- normalizePath(paths, winslash = "/", mustWork = TRUE)
  relative <- sub(paste0("^", normalized_root), "", normalized)
  if (length(role) == 1L) role <- rep(role, length(paths))
  tibble::tibble(
    role = role,
    relative_path = relative,
    bytes = as.numeric(file.info(paths)$size),
    sha256 = unname(vapply(paths, submission_sha256, character(1)))
  ) |>
    dplyr::arrange(.data$relative_path)
}

validate_submission_freeze_manifest <- function(manifest, root) {
  required <- c("role", "relative_path", "bytes", "sha256")
  if (!all(required %in% names(manifest))) stop("Freeze manifest schema is incomplete.")
  if (anyDuplicated(manifest$relative_path)) stop("Freeze manifest paths are duplicated.")
  paths <- file.path(root, manifest$relative_path)
  submission_freeze_assert_files(paths, "Freeze manifest")
  observed_size <- as.numeric(file.info(paths)$size)
  observed_hash <- unname(vapply(paths, submission_sha256, character(1)))
  if (!identical(observed_size, as.numeric(manifest$bytes)) ||
      !identical(observed_hash, manifest$sha256)) {
    stop("Freeze manifest does not match the frozen files.")
  }
  invisible(manifest)
}

collect_submission_figure_metadata <- function(pdf_paths, png_paths) {
  submission_freeze_assert_files(c(pdf_paths, png_paths), "Figure QA")
  if (Sys.which("pdfinfo") == "") stop("pdfinfo is required for PDF QA.")
  pdf_rows <- lapply(pdf_paths, function(path) {
    info <- system2("pdfinfo", path, stdout = TRUE, stderr = TRUE)
    pages <- as.integer(trimws(sub("^Pages:[[:space:]]*", "", grep("^Pages:", info, value = TRUE))))
    size <- trimws(sub("^Page size:[[:space:]]*", "", grep("^Page size:", info, value = TRUE)))
    if (length(pages) != 1L || is.na(pages) || pages != 1L || length(size) != 1L) {
      stop("Figure PDF failed one-page metadata QA: ", path, ".")
    }
    tibble::tibble(
      path = path,
      format = "pdf",
      pages = pages,
      dimensions = size,
      bytes = as.numeric(file.info(path)$size)
    )
  })
  png_rows <- lapply(png_paths, function(path) {
    image <- png::readPNG(path, info = TRUE)
    dimensions <- dim(image)
    if (length(dimensions) < 2L || any(dimensions[1:2] <= 0L)) {
      stop("Figure PNG failed dimension QA: ", path, ".")
    }
    tibble::tibble(
      path = path,
      format = "png",
      pages = NA_integer_,
      dimensions = paste(dimensions[2], dimensions[1], sep = "x"),
      bytes = as.numeric(file.info(path)$size)
    )
  })
  dplyr::bind_rows(pdf_rows, png_rows)
}

submission_recursive_files <- function(path) {
  files <- list.files(path, recursive = TRUE, full.names = TRUE, all.files = TRUE)
  files[file.info(files)$isdir %in% FALSE]
}
