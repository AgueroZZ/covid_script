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

copy_publication_assets(analysis_assets)
options(workflowr.view = FALSE)
workflowr::wflow_build(
  files = file.path("analysis", inputs$pages),
  make = FALSE,
  update = TRUE,
  view = FALSE,
  project = project_root
)
copy_publication_assets(docs_assets)

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
prohibited <- c("/Users/", "covid_agents", "AGENTS.md", "CLAUDE.md", ".codex/")
detected <- prohibited[vapply(prohibited, grepl, logical(1L), x = html_text, fixed = TRUE)]
if (length(detected) > 0L) {
  stop("Private or internal references were found in generated HTML: ", paste(detected, collapse = ", "))
}

message("Built and validated ", length(expected_html), " workflowr pages in docs/.")
