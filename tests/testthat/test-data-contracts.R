source(here::here("R", "validation.R"))
source(here::here("R", "raw_manifest.R"))

test_that("all registered sources have stable identifiers", {
  sources <- readr::read_csv(
    here::here("config", "data_sources.csv"),
    show_col_types = FALSE
  )

  expect_equal(anyDuplicated(sources$dataset_id), 0L)
  expect_false(any(is.na(sources$local_snapshot)))
  expect_true(all(nzchar(sources$local_snapshot)))
  expect_true(all(file.exists(here::here(sources$access_script))))
})

test_that("raw manifest has the required schema", {
  manifest <- readr::read_csv(
    here::here("data", "raw", "manifest.csv"),
    show_col_types = FALSE
  )

  expect_identical(
    names(manifest),
    raw_manifest_columns()
  )
})

test_that("every registered snapshot is versioned and hash-valid", {
  manifest <- utils::read.csv(
    here::here("data", "raw", "manifest.csv"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  expect_equal(nrow(manifest), 24L)
  expect_true(all(manifest$tracked_in_git))
  expect_equal(anyDuplicated(manifest$snapshot_path), 0L)
  expect_silent(validate_file_manifest(manifest, root = here::here()))
})
