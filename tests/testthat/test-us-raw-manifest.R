source(here::here("R", "validation.R"))
source(here::here("R", "raw_manifest.R"))

test_that("all registered US raw sources match their hashes", {
  manifest <- readr::read_csv(
    here::here("data", "raw", "manifest.csv"),
    show_col_types = FALSE
  )
  us_sources <- c(
    "cdc_wonder_sex",
    "cdc_wonder_non_sex",
    "owid_vaccination",
    "us_census_boundary"
  )
  us_manifest <- manifest[manifest$source_id %in% us_sources, ]

  expect_equal(nrow(us_manifest), 16L)
  expect_true(all(grepl("^[0-9a-f]{64}$", us_manifest$sha256)))
  expect_silent(validate_file_manifest(us_manifest, root = here::here()))
})
