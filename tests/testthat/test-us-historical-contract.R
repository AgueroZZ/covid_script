source(here::here("R", "validation.R"))
source(here::here("R", "us_historical.R"))

test_that("optional historical US archive matches the audited inventory", {
  archive_root <- Sys.getenv("COVID_HISTORICAL_NORTH_AMERICA_ROOT")
  skip_if(!nzchar(archive_root), "Historical archive root is not configured.")
  inventory <- inventory_us_historical_archive(archive_root)

  expect_true(all(inventory$matches_expected))
})
