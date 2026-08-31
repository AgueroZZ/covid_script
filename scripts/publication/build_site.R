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

text_site_files <- list.files(
  file.path(project_root, "docs"),
  pattern = "[.](css|csv|html|js|svg|txt)$",
  recursive = TRUE,
  full.names = TRUE
)
for (path in text_site_files) {
  lines <- sub("[ \\t]+$", "", readLines(path, warn = FALSE))
  while (length(lines) > 0L && !nzchar(lines[[length(lines)]])) {
    lines <- lines[-length(lines)]
  }
  writeLines(lines, path, useBytes = TRUE)
}

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
workflowr_checks <- vapply(
  page_text,
  function(content) grepl(
    paste0(
      "Checks:</strong>[\\s\\S]{0,500}text-success[\\s\\S]{0,100}7",
      "[\\s\\S]{0,500}text-danger[\\s\\S]{0,100}0"
    ),
    content,
    perl = TRUE
  ),
  logical(1L)
)
workflowr_sources <- grepl(
  "R Markdown file:</strong> up-to-date",
  page_text,
  fixed = TRUE
) & grepl("Environment:</strong> empty", page_text, fixed = TRUE)
if (any(!workflowr_checks | !workflowr_sources)) {
  stop(
    "One or more pages failed the workflowr reproducibility checks: ",
    paste(basename(expected_html[!workflowr_checks | !workflowr_sources]), collapse = ", ")
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
