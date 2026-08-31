source(testthat::test_path("..", "..", "R", "zenodo_deposit.R"))

testthat::test_that("Zenodo selection validation accepts relative scientific files", {
  temporary_root <- tempfile("zenodo-selection-")
  dir.create(file.path(temporary_root, "output", "results"), recursive = TRUE)
  source_path <- file.path("output", "results", "model.rda")
  writeLines("model", file.path(temporary_root, source_path))
  selection <- data.frame(
    source_path = source_path,
    deposit_path = file.path("models", "model.rda"),
    role = "canonical_model_fit",
    rationale = "Test fixture.",
    stringsAsFactors = FALSE
  )
  testthat::expect_invisible(
    validate_zenodo_deposit_selection(selection, temporary_root)
  )
})

testthat::test_that("Zenodo selection validation rejects transient sources", {
  temporary_root <- tempfile("zenodo-selection-")
  dir.create(file.path(temporary_root, "output", "validation"), recursive = TRUE)
  source_path <- file.path("output", "validation", "model.rda")
  writeLines("model", file.path(temporary_root, source_path))
  selection <- data.frame(
    source_path = source_path,
    deposit_path = file.path("models", "model.rda"),
    role = "canonical_model_fit",
    rationale = "Test fixture.",
    stringsAsFactors = FALSE
  )
  testthat::expect_error(
    validate_zenodo_deposit_selection(selection, temporary_root),
    "prohibited transient"
  )
})

testthat::test_that("Local canonical results produce a complete deposit selection", {
  project_root <- normalizePath(testthat::test_path("..", ".."))
  required_root <- file.path(
    project_root,
    "output",
    "results",
    "europe_corrected_psd_prior_20260830"
  )
  testthat::skip_if_not(dir.exists(required_root))
  selection <- zenodo_deposit_selection(project_root)
  testthat::expect_invisible(
    validate_zenodo_deposit_selection(selection, project_root)
  )
  testthat::expect_equal(
    sum(
      selection$role == "canonical_model_fit" &
        grepl("^models/europe/fitted_model/", selection$deposit_path)
    ),
    388L
  )
  testthat::expect_equal(
    sum(
      selection$role == "canonical_model_fit" &
        grepl("^models/england_wales/fitted_model/", selection$deposit_path)
    ),
    3L
  )
  testthat::expect_equal(
    sum(
      selection$role == "canonical_model_fit" &
        grepl("^models/ireland/fitted_model/", selection$deposit_path)
    ),
    4L
  )
  testthat::expect_false(any(grepl(
    "/(?:validation|legacy|build|logs)/|stale|failed",
    paste0("/", selection$source_path),
    perl = TRUE
  )))
})
