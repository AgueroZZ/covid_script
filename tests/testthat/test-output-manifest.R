source(here::here("R", "validation.R"))

test_that("artifact manifests contain valid SHA-256 hashes", {
  fixture <- tempfile(fileext = ".txt")
  writeLines("reproducible", fixture)
  manifest_path <- tempfile(fileext = ".csv")

  write_artifact_manifest(fixture, manifest_path)
  manifest <- readr::read_csv(manifest_path, show_col_types = FALSE)

  expect_equal(nrow(manifest), 1L)
  expect_equal(nchar(manifest$sha256), 64L)
})
