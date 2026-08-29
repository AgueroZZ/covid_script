test_that("all registered sources have stable identifiers", {
  sources <- readr::read_csv(
    here::here("config", "data_sources.csv"),
    show_col_types = FALSE
  )

  expect_equal(anyDuplicated(sources$source_id), 0L)
  expect_false(any(is.na(sources$local_snapshot)))
  expect_true(all(nzchar(sources$local_snapshot)))
})

test_that("raw manifest has the required schema", {
  manifest <- readr::read_csv(
    here::here("data", "raw", "manifest.csv"),
    show_col_types = FALSE
  )

  expect_identical(
    names(manifest),
    c(
      "source_id",
      "relative_path",
      "bytes",
      "sha256",
      "snapshot_date",
      "tracked_in_git"
    )
  )
})
