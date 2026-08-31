zenodo_list_files <- function(root, pattern = NULL) {
  if (!dir.exists(root)) {
    stop("Required deposit source directory is unavailable: ", root, ".")
  }
  files <- list.files(
    root,
    recursive = TRUE,
    full.names = TRUE,
    all.files = TRUE,
    no.. = TRUE
  )
  files <- files[!file.info(files)$isdir]
  if (!is.null(pattern)) {
    files <- files[grepl(pattern, files, perl = TRUE)]
  }
  sort(files)
}

zenodo_relative_path <- function(path, root) {
  normalized_path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  normalized_root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  prefix <- paste0(normalized_root, "/")
  if (!startsWith(normalized_path, prefix)) {
    stop("Path is outside the project root: ", path, ".")
  }
  substring(normalized_path, nchar(prefix) + 1L)
}

zenodo_tree_records <- function(
    project_root,
    source_root,
    deposit_root,
    role,
    rationale,
    pattern = NULL) {
  absolute_source_root <- file.path(project_root, source_root)
  files <- zenodo_list_files(absolute_source_root, pattern = pattern)
  source_prefix <- paste0(
    normalizePath(absolute_source_root, winslash = "/", mustWork = TRUE),
    "/"
  )
  relative_within_source <- substring(
    normalizePath(files, winslash = "/", mustWork = TRUE),
    nchar(source_prefix) + 1L
  )
  data.frame(
    source_path = vapply(
      files,
      zenodo_relative_path,
      character(1L),
      root = project_root
    ),
    deposit_path = file.path(deposit_root, relative_within_source),
    role = role,
    rationale = rationale,
    stringsAsFactors = FALSE
  )
}

zenodo_file_records <- function(
    project_root,
    source_paths,
    deposit_paths,
    role,
    rationale) {
  if (length(source_paths) != length(deposit_paths)) {
    stop("Source and deposit path vectors must have equal length.")
  }
  absolute_sources <- file.path(project_root, source_paths)
  if (any(!file.exists(absolute_sources))) {
    stop(
      "Required deposit source files are unavailable: ",
      paste(source_paths[!file.exists(absolute_sources)], collapse = ", "),
      "."
    )
  }
  data.frame(
    source_path = source_paths,
    deposit_path = deposit_paths,
    role = role,
    rationale = rationale,
    stringsAsFactors = FALSE
  )
}

zenodo_deposit_selection <- function(project_root = ".") {
  project_root <- normalizePath(project_root, winslash = "/", mustWork = TRUE)
  europe_root <- file.path(
    "output", "results", "europe_corrected_psd_prior_20260830"
  )
  england_wales_root <- file.path(
    "output", "results", "england_wales_corrected_20260830"
  )
  ireland_root <- file.path(
    "output", "results", "ireland_corrected_20260830"
  )
  manuscript_bundle_root <- file.path(
    "output", "results", "zenodo_bundle", "source_artifacts"
  )
  freeze_root <- file.path(
    "output", "submission_freeze", "local_20260831"
  )

  records <- list(
    zenodo_tree_records(
      project_root,
      file.path(europe_root, "fitted_model"),
      file.path("models", "europe", "fitted_model"),
      "canonical_model_fit",
      "Final corrected European mortality fits used by the adopted reporting pipeline.",
      "[.]rda$"
    ),
    zenodo_tree_records(
      project_root,
      file.path(europe_root, "diagnostics"),
      file.path("models", "europe", "diagnostics"),
      "model_diagnostic",
      "Compact diagnostics paired with the final corrected European fits.",
      "[.]rds$"
    ),
    zenodo_tree_records(
      project_root,
      file.path(europe_root, "manifests"),
      file.path("models", "europe", "manifests"),
      "model_manifest",
      "European model inventory, batch status, and fitted-environment provenance."
    ),
    zenodo_file_records(
      project_root,
      file.path(europe_root, c("batch_complete.flag", "verified_complete.flag")),
      file.path(
        "models", "europe", "manifests",
        c("batch_complete.flag", "verified_complete.flag")
      ),
      "completion_record",
      "Final European batch completion and independent verification records."
    ),
    zenodo_tree_records(
      project_root,
      file.path(england_wales_root, "fitted_model"),
      file.path("models", "england_wales", "fitted_model"),
      "canonical_model_fit",
      "Final England and Wales mortality fits used by regional reporting.",
      "[.]rda$"
    ),
    zenodo_tree_records(
      project_root,
      file.path(england_wales_root, "diagnostics"),
      file.path("models", "england_wales", "diagnostics"),
      "model_diagnostic",
      "Compact diagnostics paired with the England and Wales fits.",
      "[.]rds$"
    ),
    zenodo_tree_records(
      project_root,
      file.path(england_wales_root, "wave_summary"),
      file.path("models", "england_wales", "wave_summary"),
      "model_summary",
      "Wave summaries derived from the final England and Wales fits.",
      "[.]csv$"
    ),
    zenodo_tree_records(
      project_root,
      file.path(england_wales_root, "manifests"),
      file.path("models", "england_wales", "manifests"),
      "model_manifest",
      "England and Wales model inventory, source definitions, and provenance."
    ),
    zenodo_file_records(
      project_root,
      file.path(england_wales_root, "batch_complete.flag"),
      file.path("models", "england_wales", "manifests", "batch_complete.flag"),
      "completion_record",
      "Final England and Wales batch completion record."
    ),
    zenodo_tree_records(
      project_root,
      file.path(ireland_root, "fitted_model"),
      file.path("models", "ireland", "fitted_model"),
      "canonical_model_fit",
      "Final Ireland mortality fits used by regional reporting.",
      "[.]rda$"
    ),
    zenodo_tree_records(
      project_root,
      file.path(ireland_root, "diagnostics"),
      file.path("models", "ireland", "diagnostics"),
      "model_diagnostic",
      "Compact diagnostics paired with the Ireland fits.",
      "[.]rds$"
    ),
    zenodo_tree_records(
      project_root,
      file.path(ireland_root, "wave_summary"),
      file.path("models", "ireland", "wave_summary"),
      "model_summary",
      "Wave summaries derived from the final Ireland fits.",
      "[.]csv$"
    ),
    zenodo_tree_records(
      project_root,
      file.path(ireland_root, "manifests"),
      file.path("models", "ireland", "manifests"),
      "model_manifest",
      "Ireland model inventory, batch status, and fitted-environment provenance."
    ),
    zenodo_file_records(
      project_root,
      file.path(ireland_root, "batch_complete.flag"),
      file.path("models", "ireland", "manifests", "batch_complete.flag"),
      "completion_record",
      "Final Ireland batch completion record."
    ),
    zenodo_tree_records(
      project_root,
      file.path(manuscript_bundle_root, "north_america"),
      file.path("models", "north_america"),
      "canonical_model_or_supporting_artifact",
      paste(
        "US posterior predictions, the Canadian reporting object, and geometry",
        "used by the adopted North American outputs."
      )
    ),
    zenodo_tree_records(
      project_root,
      file.path(manuscript_bundle_root, "europe", "geometry"),
      file.path("supporting", "europe", "geometry"),
      "reporting_geometry",
      "European map geometry required to reconstruct the adopted map output."
    ),
    zenodo_tree_records(
      project_root,
      file.path(freeze_root, "configuration"),
      "configuration",
      "analysis_configuration",
      "Exact configuration and reporting contracts used by the completed freeze."
    ),
    zenodo_tree_records(
      project_root,
      file.path(freeze_root, "reporting_inputs"),
      "reporting_inputs",
      "reporting_input",
      "Final standardized inputs for Figures 1--5."
    ),
    zenodo_tree_records(
      project_root,
      file.path(freeze_root, "table_inputs"),
      "table_inputs",
      "reporting_input",
      "Final standardized inputs for Table 1."
    ),
    zenodo_tree_records(
      project_root,
      file.path(freeze_root, "figures"),
      file.path("publication_artifacts", "figures"),
      "publication_artifact",
      "Final PDF and PNG manuscript figures included for self-contained verification."
    ),
    zenodo_tree_records(
      project_root,
      file.path(freeze_root, "tables"),
      file.path("publication_artifacts", "tables"),
      "publication_artifact",
      "Final CSV and HTML manuscript table included for self-contained verification."
    ),
    zenodo_file_records(
      project_root,
      file.path(
        freeze_root,
        "manifests",
        c(
          "artifact_manifest.csv",
          "figure_metadata.csv",
          "reporting_status.csv",
          "repository_manifest.csv"
        )
      ),
      file.path(
        "freeze_provenance",
        c(
          "artifact_manifest.csv",
          "figure_metadata.csv",
          "reporting_status.csv",
          "repository_manifest.csv"
        )
      ),
      "freeze_manifest",
      "Relative-path manifests from the completed local submission freeze."
    ),
    zenodo_file_records(
      project_root,
      file.path(freeze_root, c("git_head.txt", "session_info.txt")),
      file.path("freeze_provenance", c("git_head.txt", "session_info.txt")),
      "freeze_manifest",
      "Repository and R-session provenance from the completed local freeze."
    )
  )
  selection <- do.call(rbind, records)
  rownames(selection) <- NULL
  selection[order(selection$deposit_path), , drop = FALSE]
}

validate_zenodo_deposit_selection <- function(selection, project_root = ".") {
  required_columns <- c("source_path", "deposit_path", "role", "rationale")
  if (!all(required_columns %in% names(selection))) {
    stop("Deposit selection is missing required columns.")
  }
  if (nrow(selection) == 0L) {
    stop("Deposit selection is empty.")
  }
  project_root <- normalizePath(project_root, winslash = "/", mustWork = TRUE)
  absolute_sources <- file.path(project_root, selection$source_path)
  if (any(!file.exists(absolute_sources))) {
    stop("Deposit selection contains unavailable source files.")
  }
  if (anyDuplicated(selection$deposit_path)) {
    stop("Deposit paths must be unique.")
  }
  invalid_deposit_paths <- grepl(
    "^(?:/|[A-Za-z]:)|(?:^|/)\\.\\.(?:/|$)",
    selection$deposit_path,
    perl = TRUE
  )
  if (any(invalid_deposit_paths)) {
    stop("Deposit paths must be relative and cannot contain parent traversal.")
  }
  prohibited_source_patterns <- c(
    "/validation/", "/legacy/", "/build/", "/logs/",
    "stale", "failed", "/manuscript/"
  )
  prohibited_sources <- Reduce(
    `|`,
    lapply(
      prohibited_source_patterns,
      grepl,
      x = paste0("/", selection$source_path),
      fixed = TRUE
    )
  )
  if (any(prohibited_sources)) {
    stop("Deposit selection contains a prohibited transient or superseded source.")
  }
  prohibited_extensions <- c("o", "so", "log", "pid", "doc", "docx")
  if (any(tolower(tools::file_ext(selection$source_path)) %in% prohibited_extensions)) {
    stop("Deposit selection contains a prohibited file type.")
  }
  invisible(selection)
}

zenodo_exclusion_inventory <- function(project_root = ".") {
  project_root <- normalizePath(project_root, winslash = "/", mustWork = TRUE)
  records <- data.frame(
    source_group = c(
      "output/validation",
      "output/legacy",
      "output/results/zenodo_bundle/source_artifacts/europe/fitted_predictions",
      "output/results/zenodo_bundle/source_artifacts/europe/figure_01",
      "output/results/zenodo_bundle/source_artifacts/europe/supporting",
      "output/results/ireland_corrected_20260830/build",
      "output/reporting",
      "output/submission_freeze/local_20260831_stale_before_visual_label_fix",
      "output/submission_freeze/local_20260831_failed_sandbox_processx",
      "output/submission_freeze/local_20260831_failed_missing_renv_dependencies",
      "output/submission_freeze/local_20260831_failed_old_suppression_gate",
      "output/manuscript"
    ),
    classification = c(
      "validation_only",
      "superseded",
      "superseded",
      "superseded",
      "duplicated_or_superseded",
      "compiler_product",
      "regenerable_derivative",
      "stale_freeze",
      "failed_run",
      "failed_run",
      "failed_run",
      "private_manuscript"
    ),
    rationale = c(
      "Validation copies and sensitivity outputs duplicate canonical fits or can be regenerated.",
      "Historical and superseded artifacts are not part of the final scientific baseline.",
      "Historical European fits are replaced by the corrected 2026-08-30 production batch.",
      "Historical Figure 1 model objects are replaced by the corrected production fit and final reporting input.",
      "Tracked raw snapshots and current reporting inputs replace these historical support files.",
      "Compiled TMB objects are platform-specific and reproducible from tracked source code.",
      "Installed reporting derivatives are reproduced from the selected fits and final freeze inputs.",
      "This freeze predates the final visual-label correction.",
      "The run did not complete because process execution was restricted.",
      "The run did not complete because the pinned environment was unavailable.",
      "The run did not pass the final suppression-validation gate.",
      "Manuscript DOCX files are not model-result data and are maintained separately."
    ),
    stringsAsFactors = FALSE
  )
  summaries <- lapply(records$source_group, function(relative_root) {
    absolute_root <- file.path(project_root, relative_root)
    if (!dir.exists(absolute_root)) {
      return(c(files = 0, bytes = 0))
    }
    files <- zenodo_list_files(absolute_root)
    c(files = length(files), bytes = sum(file.info(files)$size))
  })
  records$files <- vapply(summaries, `[[`, numeric(1L), "files")
  records$bytes <- vapply(summaries, `[[`, numeric(1L), "bytes")
  records
}

zenodo_sha256 <- function(paths) {
  if (!requireNamespace("digest", quietly = TRUE)) {
    stop("Package 'digest' is required to build the Zenodo deposit.")
  }
  vapply(
    paths,
    digest::digest,
    character(1L),
    algo = "sha256",
    file = TRUE,
    USE.NAMES = FALSE
  )
}

write_zenodo_readme <- function(deposit_root, manifest, repository_commit) {
  role_groups <- split(manifest$bytes, manifest$role)
  role_lines <- vapply(names(role_groups), function(role) {
    values <- role_groups[[role]]
    paste0(
      "- `", role, "`: ", length(values), " files; ",
      sprintf("%.3f", sum(values) / 1024^3), " GiB"
    )
  }, character(1L))
  readme <- c(
    "# COVID-19 excess mortality fitted results",
    "",
    paste(
      "This deposit contains the canonical fitted mortality objects, compact",
      "diagnostics, reporting inputs, and map geometry required to reconstruct",
      "the adopted manuscript outputs without repeating computationally intensive",
      "model fitting."
    ),
    "",
    "## Repository",
    "",
    "- URL: https://github.com/AgueroZZ/covid_script",
    paste0("- Commit: `", repository_commit, "`"),
    "- Freeze tag: `submission-freeze-20260831`",
    "",
    "The exact raw-data snapshots and all analysis code are versioned in the GitHub repository. Provider-hosted data may change; exact reproduction uses the repository snapshots.",
    "",
    "## Contents",
    "",
    role_lines,
    "",
    paste0(
      "The payload contains ", nrow(manifest), " files totaling ",
      sprintf("%.3f", sum(manifest$bytes) / 1024^3), " GiB."
    ),
    "",
    "## Reproduction",
    "",
    "Restore the repository environment and verify the raw snapshots:",
    "",
    "```bash",
    "Rscript -e 'renv::restore()'",
    "Rscript scripts/data_access/verify_snapshots.R",
    "```",
    "",
    "Install this deposit's directories at a local path and follow `documentation/reproduction.md` in the linked repository. The reporting website itself reads tracked publication artifacts and never fits a model.",
    "",
    "## Integrity and provenance",
    "",
    "- `manifests/deposit_inventory.csv` identifies every source role and SHA-256 checksum.",
    "- `manifests/exclusion_inventory.csv` records the local output groups deliberately excluded from the deposit.",
    "- `SHA256SUMS` verifies every deposited file except the checksum file itself.",
    "- Local absolute paths, logs, compiler products, failed runs, stale freezes, validation duplicates, and manuscript working files are excluded.",
    "",
    "## Scientific scope",
    "",
    "- Figure 4 panels labeled `All Ages` estimate ages 40--79.",
    "- Figure 5 Europe uses ages 40--79 for both observed and expected deaths.",
    "- US reporting retains the prespecified historical fit-quality cohorts and fixed vaccination thresholds recorded in the repository configuration."
  )
  writeLines(readme, file.path(deposit_root, "README.md"), useBytes = TRUE)
}

build_zenodo_deposit <- function(
    selection,
    deposit_root,
    project_root = ".",
    metadata_template = file.path("config", "zenodo_metadata.yml")) {
  validate_zenodo_deposit_selection(selection, project_root = project_root)
  project_root <- normalizePath(project_root, winslash = "/", mustWork = TRUE)
  if (file.exists(deposit_root) || dir.exists(deposit_root)) {
    stop("Deposit destination already exists: ", deposit_root, ".")
  }
  dir.create(deposit_root, recursive = TRUE, showWarnings = FALSE)
  source_files <- file.path(project_root, selection$source_path)
  destination_files <- file.path(deposit_root, selection$deposit_path)
  invisible(lapply(unique(dirname(destination_files)), dir.create, recursive = TRUE))
  copied <- file.copy(source_files, destination_files, copy.mode = TRUE)
  if (!all(copied)) {
    stop("Failed to copy one or more Zenodo deposit files.")
  }
  source_bytes <- file.info(source_files)$size
  destination_bytes <- file.info(destination_files)$size
  if (!identical(unname(source_bytes), unname(destination_bytes))) {
    stop("One or more copied deposit files changed size.")
  }
  manifest <- selection
  manifest$bytes <- destination_bytes
  manifest$sha256 <- zenodo_sha256(destination_files)
  dir.create(file.path(deposit_root, "manifests"), showWarnings = FALSE)
  utils::write.csv(
    manifest,
    file.path(deposit_root, "manifests", "deposit_inventory.csv"),
    row.names = FALSE,
    na = ""
  )
  utils::write.csv(
    zenodo_exclusion_inventory(project_root),
    file.path(deposit_root, "manifests", "exclusion_inventory.csv"),
    row.names = FALSE,
    na = ""
  )
  repository_commit <- system2(
    "git",
    c("-C", project_root, "rev-parse", "HEAD"),
    stdout = TRUE
  )
  if (length(repository_commit) != 1L || nchar(repository_commit) != 40L) {
    stop("Unable to resolve the repository commit for the deposit.")
  }
  template_path <- file.path(project_root, metadata_template)
  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop("Package 'yaml' is required to build the Zenodo metadata draft.")
  }
  metadata <- yaml::read_yaml(template_path)
  metadata$repository <- list(
    url = "https://github.com/AgueroZZ/covid_script",
    commit = repository_commit,
    freeze_tag = "submission-freeze-20260831"
  )
  dir.create(file.path(deposit_root, "metadata"), showWarnings = FALSE)
  yaml::write_yaml(
    metadata,
    file.path(deposit_root, "metadata", "zenodo_metadata_draft.yml")
  )
  write_zenodo_readme(deposit_root, manifest, repository_commit)

  checksum_files <- zenodo_list_files(deposit_root)
  checksum_relative <- substring(
    normalizePath(checksum_files, winslash = "/", mustWork = TRUE),
    nchar(normalizePath(deposit_root, winslash = "/", mustWork = TRUE)) + 2L
  )
  checksum_lines <- paste(zenodo_sha256(checksum_files), checksum_relative)
  writeLines(checksum_lines, file.path(deposit_root, "SHA256SUMS"), useBytes = TRUE)
  invisible(manifest)
}

validate_zenodo_deposit <- function(deposit_root) {
  checksum_path <- file.path(deposit_root, "SHA256SUMS")
  if (!file.exists(checksum_path)) {
    stop("Deposit checksum file is unavailable.")
  }
  checksum_lines <- readLines(checksum_path, warn = FALSE)
  expected <- sub(" .*", "", checksum_lines)
  relative_paths <- sub("^[0-9a-f]{64} ", "", checksum_lines)
  paths <- file.path(deposit_root, relative_paths)
  if (any(!file.exists(paths))) {
    stop("Deposit checksum inventory references missing files.")
  }
  observed <- zenodo_sha256(paths)
  if (!identical(expected, observed)) {
    stop("Zenodo deposit checksum validation failed.")
  }
  text_files <- paths[tolower(tools::file_ext(paths)) %in% c(
    "csv", "json", "md", "txt", "yml", "yaml"
  )]
  text <- unlist(lapply(text_files, readLines, warn = FALSE), use.names = FALSE)
  if (any(grepl("/Users/|file://", text, perl = TRUE))) {
    stop("Zenodo deposit text files contain local filesystem references.")
  }
  invisible(data.frame(
    files = length(paths),
    bytes = sum(file.info(paths)$size),
    stringsAsFactors = FALSE
  ))
}
