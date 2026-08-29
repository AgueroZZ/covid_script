source(here::here("R", "validation.R"))
source(here::here("R", "raw_manifest.R"))
source(here::here("R", "us_verification.R"))

test_that("US verifier reports missing production artifacts", {
  root <- tempfile("incomplete-us-run-")
  dir.create(root)
  expect_error(
    verify_us_completion(root),
    "US completion files are missing"
  )
})
