#!/usr/bin/env Rscript

source(here::here("R", "manuscript_claims.R"))

output_dir <- here::here(
  "artifacts", "validation", "manuscript_claim_ledger_20260831"
)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

claims <- read_manuscript_claim_registry()
evidence <- build_manuscript_computed_evidence()

status_summary <- claims |>
  dplyr::count(.data$status, name = "claims") |>
  dplyr::arrange(.data$status)
required_edits <- claims |>
  dplyr::filter(.data$status %in% c(
    "requires_manuscript_edit", "unsupported_remove_or_reproduce"
  )) |>
  dplyr::select(
    "claim_id", "document", "section", "claim_text",
    "computed_value", "status", "required_action"
  )

source_files <- c(
  manuscript_tracked = "/Users/ziangzhang/Desktop/covid_mortality/covid_agents/manuscript_July30_2026_wave_definitions_tracked.docx",
  appendix_tracked = "/Users/ziangzhang/Desktop/covid_mortality/covid_agents/appendices_tracked.docx",
  manuscript_accepted_text = "/tmp/covid_manuscript_accepted.txt",
  appendix_accepted_text = "/tmp/covid_appendix_accepted.txt"
)
if (!all(file.exists(source_files))) {
  stop("Accepted-view source files are missing; regenerate the temporary document extracts.")
}
source_hashes <- tibble::tibble(
  source_id = names(source_files),
  path = unname(source_files),
  sha256 = vapply(
    source_files,
    function(path) digest::digest(
      file = path,
      algo = "sha256",
      serialize = FALSE
    ),
    character(1)
  )
)

readr::write_csv(claims, file.path(output_dir, "claim_ledger.csv"), na = "")
readr::write_csv(status_summary, file.path(output_dir, "claim_status_summary.csv"))
readr::write_csv(evidence, file.path(output_dir, "computed_evidence.csv"), na = "")
readr::write_csv(source_hashes, file.path(output_dir, "source_hashes.csv"))
readr::write_csv(required_edits, file.path(output_dir, "manuscript_required_edits.csv"), na = "")

documentation_path <- here::here(
  "docs", "validation", "manuscript-numerical-claims.md"
)
dir.create(dirname(documentation_path), recursive = TRUE, showWarnings = FALSE)
lines <- c(
  "# Manuscript Numerical Claim Ledger",
  "",
  "This ledger audits the accepted view of the tracked manuscript and appendix.",
  "It uses the frozen analysis contract and frozen reporting inputs; it does not",
  "modify either Word source.",
  "",
  "## Status summary",
  "",
  "| Status | Claims |",
  "|---|---:|",
  paste0("| ", status_summary$status, " | ", status_summary$claims, " |"),
  "",
  "## Point-by-point ledger",
  ""
)
for (i in seq_len(nrow(claims))) {
  claim <- claims[i, ]
  action <- if (is.na(claim$required_action) || claim$required_action == "") {
    "No manuscript change required."
  } else {
    claim$required_action
  }
  lines <- c(
    lines,
    paste0("### ", claim$claim_id, " - ", claim$section),
    "",
    paste0("- Current wording: ", claim$claim_text),
    paste0("- Verified value/evidence: ", claim$computed_value),
    paste0("- Status: `", claim$status, "`"),
    paste0("- Action: ", action),
    paste0("- Evidence: `", claim$evidence_path, "`"),
    ""
  )
}
writeLines(lines, documentation_path)

session <- capture.output(sessionInfo())
writeLines(session, file.path(output_dir, "session_info.txt"))

todo_path <- "/Users/ziangzhang/Desktop/covid_mortality/covid_agents/manuscript_to_do_list.md"
todo <- paste(readLines(todo_path, warn = FALSE), collapse = "\n")
missing_todo_ids <- setdiff(required_edits$claim_id, regmatches(
  todo,
  gregexpr("(ABS|MTH|RES|APP)-[0-9]{3}", todo, perl = TRUE)
)[[1]])
if (length(missing_todo_ids) > 0L) {
  stop(
    "Manuscript to-do list is missing actionable claim IDs: ",
    paste(missing_todo_ids, collapse = ", "),
    "."
  )
}

if (nrow(claims) == sum(status_summary$claims) && length(missing_todo_ids) == 0L) {
  writeLines(
    c(
      "Manuscript numerical claim ledger complete.",
      paste0("Claims: ", nrow(claims)),
      paste0("Actionable manuscript edits: ", nrow(required_edits))
    ),
    file.path(output_dir, "complete.flag")
  )
}
