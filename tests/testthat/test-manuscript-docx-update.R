manuscript_docx_validation_root <- here::here(
  "artifacts", "validation", "manuscript_docx_update_20260831"
)

read_docx_validation_text <- function(document) {
  path <- file.path(
    manuscript_docx_validation_root,
    "text",
    paste0(document, "_accepted.txt")
  )
  expect_true(file.exists(path))
  paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

test_that("tracked manuscript validation locks the corrected implementation", {
  manuscript <- read_docx_validation_text("manuscript")
  expect_match(manuscript, "second-order integrated Wiener process \\(IWP2\\)")
  expect_match(manuscript, "periods of 12, 6, 4, and 3 months", fixed = TRUE)
  expect_match(
    manuscript,
    "Europe and England-and-Wales fits used BayesGP with five-point adaptive Gauss-Hermite quadrature specified explicitly",
    fixed = TRUE
  )
  expect_match(
    manuscript,
    "Ireland used the equivalent TMB implementation with five-point quadrature",
    fixed = TRUE
  )
  expect_match(
    manuscript,
    "US and Canadian fits used BayesGP with its four-point quadrature default",
    fixed = TRUE
  )
  expect_match(
    manuscript,
    "posterior predictive counts were drawn directly at the quarterly level",
    fixed = TRUE
  )
  expect_match(manuscript, "January 5, 1981–August 28, 2023", fixed = TRUE)
  expect_false(grepl("third-order integrated Wiener", manuscript, fixed = TRUE))
  expect_false(grepl("Bayesian hierarchical models", manuscript, fixed = TRUE))
})

test_that("tracked manuscript validation locks the final vaccination cohorts", {
  manuscript <- read_docx_validation_text("manuscript")
  expect_match(
    manuscript,
    "Arkansas, California, New York, and Texas fell between the fixed thresholds",
    fixed = TRUE
  )
  expect_match(manuscript, "Portugal, and Spain", fixed = TRUE)

  us <- readr::read_csv(
    here::here("config", "us_reporting_cohort.csv"),
    show_col_types = FALSE
  )
  europe <- readr::read_csv(
    here::here("artifacts", "data", "europe", "vaccination_membership.csv"),
    show_col_types = FALSE
  )
  expect_true(all(us$vaccination_group[us$geography %in% c(
    "Arkansas", "California", "New York", "Texas"
  )] == "neither"))
  expect_identical(
    europe$vaccination_group[europe$geography %in% "PL"],
    "neither"
  )
  expect_identical(
    europe$vaccination_group[europe$geography %in% "PT"],
    "high"
  )
})

test_that("accepted appendix states figure-specific estimands", {
  appendix <- read_docx_validation_text("appendix")
  expect_match(
    appendix,
    "Figure 4 uses ages 40–59 and 60–79 in both Europe and the US",
    fixed = TRUE
  )
  expect_match(
    appendix,
    "US 0–84 Figure 5 estimand combines ages 0–44, 45–64, and 65–84",
    fixed = TRUE
  )
  expect_match(appendix, "female-minus-male P-score difference", fixed = TRUE)
})

test_that("tracked documents retain actual Word revisions", {
  report_path <- file.path(
    manuscript_docx_validation_root,
    "structural_validation.json"
  )
  expect_true(file.exists(report_path))
  report <- jsonlite::read_json(report_path, simplifyVector = TRUE)
  expect_equal(report$manuscript$replacements, 65)
  expect_equal(report$manuscript$revisions$deletions, 65)
  expect_equal(report$manuscript$revisions$insertions, 62)
  expect_equal(report$manuscript$revisions$format_changes, 2)
  expect_lte(report$manuscript$abstract_words, 250)
  expect_equal(report$appendix$replacements, 11)
  expect_equal(report$appendix$revisions$deletions, 11)
  expect_equal(report$appendix$revisions$insertions, 11)
  expect_equal(report$appendix$math_inventory$oMath, 4)
})

test_that("the installed BayesGP quadrature default is four points", {
  skip_if_not_installed("BayesGP")
  expect_identical(formals(BayesGP::model_fit)$aghq_k, 4)
  expect_identical(formals(BayesGP::model_fit)$M, 3000)
})
