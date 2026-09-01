#!/usr/bin/env Rscript

project_root <- normalizePath(".", winslash = "/", mustWork = TRUE)
source(file.path(project_root, "scripts", "publication", "validate_site_inputs.R"))
inputs <- validate_site_inputs(project_root)

if (!requireNamespace("workflowr", quietly = TRUE)) {
  stop("Package 'workflowr' is required to build the publication website.")
}
analysis_assets <- file.path(project_root, "analysis", "assets")
docs_assets <- file.path(project_root, "docs", "assets")
asset_directories <- c(
  file.path(analysis_assets, "figures"),
  file.path(analysis_assets, "tables"),
  file.path(docs_assets, "figures"),
  file.path(docs_assets, "tables")
)
invisible(lapply(asset_directories, dir.create, recursive = TRUE, showWarnings = FALSE))

copy_publication_assets <- function(destination_root) {
  for (i in seq_len(nrow(inputs$records))) {
    kind <- if (inputs$records$format[[i]] %in% c("pdf", "png")) "figures" else "tables"
    destination <- file.path(destination_root, kind, basename(inputs$artifact_paths[[i]]))
    copied <- file.copy(inputs$artifact_paths[[i]], destination, overwrite = TRUE, copy.mode = TRUE)
    if (!copied) {
      stop("Failed to stage publication asset: ", inputs$records$public_path[[i]])
    }
  }
}

validate_supplementary_manifest <- function(root) {
  manifest <- utils::read.csv(
    file.path(root, "manifest.csv"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  paths <- file.path(root, manifest$path)
  if (any(!file.exists(paths))) {
    stop("A frozen supplementary artifact listed in the manifest is missing.")
  }
  hashes <- vapply(
    paths,
    digest::digest,
    character(1L),
    algo = "sha256",
    file = TRUE,
    serialize = FALSE
  )
  if (any(hashes != manifest$sha256)) {
    stop(
      "Frozen supplementary hash verification failed: ",
      paste(manifest$path[hashes != manifest$sha256], collapse = ", "),
      "."
    )
  }
  invisible(manifest)
}

copy_directory_contents <- function(source, destination) {
  files <- list.files(
    source,
    recursive = TRUE,
    full.names = TRUE,
    all.files = FALSE
  )
  files <- files[file.info(files)$isdir %in% FALSE]
  relative <- substring(files, nchar(source) + 2L)
  targets <- file.path(destination, relative)
  invisible(lapply(unique(dirname(targets)), dir.create, recursive = TRUE, showWarnings = FALSE))
  copied <- file.copy(files, targets, overwrite = TRUE, copy.mode = TRUE)
  if (!all(copied)) {
    stop("Failed to copy one or more supplementary browser assets.")
  }
  invisible(targets)
}

copy_supplementary_assets <- function(destination_root) {
  destination <- file.path(destination_root, "supplementary")
  dir.create(destination, recursive = TRUE, showWarnings = FALSE)
  table_explorer_destination <- file.path(destination, "table_explorer")
  if (dir.exists(table_explorer_destination)) {
    unlink(table_explorer_destination, recursive = TRUE, force = TRUE)
  }
  if (dir.exists(table_explorer_destination)) {
    stop("Failed to clear obsolete supplementary table explorer assets.")
  }
  copied <- unlist(lapply(inputs$supplementary_roots, function(root) {
    copy_directory_contents(file.path(root, "browser"), destination)
  }), use.names = FALSE)
  interactive_targets <- file.path(
    destination,
    basename(inputs$supplementary_sources)
  )
  if (!all(file.copy(
    inputs$supplementary_sources,
    interactive_targets,
    overwrite = TRUE,
    copy.mode = TRUE
  ))) {
    stop("Failed to stage the supplementary JavaScript or stylesheet.")
  }
  invisible(c(copied, interactive_targets))
}

invisible(lapply(inputs$supplementary_roots, validate_supplementary_manifest))
copy_publication_assets(analysis_assets)
copy_supplementary_assets(analysis_assets)
invisible(callr::r_safe(
  function(files, root) {
    options(workflowr.view = FALSE)
    workflowr::wflow_build(
      files = files,
      make = FALSE,
      update = TRUE,
      view = FALSE,
      project = root
    )
  },
  args = list(
    files = file.path("analysis", inputs$pages),
    root = project_root
  ),
  show = TRUE
))
copy_publication_assets(docs_assets)
copy_supplementary_assets(docs_assets)

expected_html <- file.path(
  project_root,
  "docs",
  sub("[.]Rmd$", ".html", inputs$pages)
)
if (any(!file.exists(expected_html))) {
  stop("The workflowr build did not produce every required page.")
}

html_text <- paste(
  unlist(lapply(expected_html, readLines, warn = FALSE), use.names = FALSE),
  collapse = "\n"
)
prohibited <- c("/Users/", "file://")
detected <- prohibited[vapply(prohibited, grepl, logical(1L), x = html_text, fixed = TRUE)]
if (length(detected) > 0L) {
  stop("Local filesystem references were found in generated HTML: ", paste(detected, collapse = ", "))
}

extract_local_resources <- function(html_path) {
  content <- paste(readLines(html_path, warn = FALSE), collapse = "\n")
  matches <- regmatches(
    content,
    gregexpr("(?:href|src)=\"[^\"]+\"", content, perl = TRUE)
  )[[1L]]
  if (identical(matches, "")) {
    return(character())
  }
  resources <- sub("^(?:href|src)=\"", "", matches, perl = TRUE)
  resources <- sub("\"$", "", resources)
  resources <- resources[!grepl(
    "^(?:https?:|mailto:|tel:|javascript:|data:|#|//)",
    resources,
    perl = TRUE
  )]
  resources <- resources[!grepl("['+]", resources)]
  resources <- sub("[?#].*$", "", resources)
  unique(resources[nzchar(resources)])
}

missing_resources <- character()
for (html_path in expected_html) {
  resources <- extract_local_resources(html_path)
  resolved <- file.path(dirname(html_path), resources)
  missing <- resources[!file.exists(resolved)]
  if (length(missing) > 0L) {
    missing_resources <- c(
      missing_resources,
      paste0(basename(html_path), ": ", missing)
    )
  }
}
if (length(missing_resources) > 0L) {
  stop("Generated HTML contains missing local resources: ", paste(missing_resources, collapse = ", "))
}

page_text <- vapply(
  expected_html,
  function(path) paste(readLines(path, warn = FALSE), collapse = "\n"),
  character(1L)
)
page_dirty <- vapply(
  inputs$pages,
  function(page) {
    status <- system2(
      "git",
      c("status", "--porcelain", "--", file.path("analysis", page)),
      stdout = TRUE,
      stderr = TRUE
    )
    length(status) > 0L
  },
  logical(1L)
)
workflowr_checks <- vapply(seq_along(page_text), function(index) {
  expected_success <- if (page_dirty[[index]]) 6L else 7L
  expected_danger <- if (page_dirty[[index]]) 1L else 0L
  grepl(
    paste0(
      "Checks:</strong>[\\s\\S]{0,500}text-success[\\s\\S]{0,100}",
      expected_success,
      "[\\s\\S]{0,500}text-danger[\\s\\S]{0,100}",
      expected_danger
    ),
    page_text[[index]],
    perl = TRUE
  )
}, logical(1L))
workflowr_source_status <- vapply(seq_along(page_text), function(index) {
  expected <- if (page_dirty[[index]]) {
    "R Markdown file:</strong> uncommitted"
  } else {
    "R Markdown file:</strong> up-to-date"
  }
  grepl(expected, page_text[[index]], fixed = TRUE)
}, logical(1L))
workflowr_sources <- workflowr_source_status &
  grepl("Environment:</strong> empty", page_text, fixed = TRUE)
if (any(!workflowr_checks | !workflowr_sources)) {
  stop(
    "One or more pages failed the workflowr reproducibility checks: ",
    paste(basename(expected_html[!workflowr_checks | !workflowr_sources]), collapse = ", ")
  )
}
if (any(page_dirty)) {
  message(
    "Validated ", sum(page_dirty),
    " development page(s) with the sole workflowr warning being their current ",
    "uncommitted Git status."
  )
}

code_pages <- c(
  sprintf("figure_%02d.html", 1:5),
  "table_01.html",
  "supplementary.html",
  "data_sources.html",
  "reproduction.html"
)
code_html <- page_text[match(code_pages, basename(expected_html))]
if (any(!grepl("code-folding-btn", code_html, fixed = TRUE))) {
  stop("A page with displayed code is missing its code-folding control.")
}

message("Built and validated ", length(expected_html), " workflowr pages in docs/.")
