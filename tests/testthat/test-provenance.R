test_that("provenance manifests cover the audited trees", {
  repo <- readr::read_csv(
    here::here("docs", "provenance", "repo_inventory.csv"),
    show_col_types = FALSE
  )
  archive <- readr::read_csv(
    here::here("docs", "provenance", "archive_inventory.csv"),
    show_col_types = FALSE
  )
  mapping <- readr::read_csv(
    here::here("docs", "provenance", "script_hash_map.csv"),
    show_col_types = FALSE
  )

  expect_gt(nrow(repo), 100)
  expect_gt(nrow(archive), 1000)
  expect_true(all(nchar(repo$sha256) == 64L))
  expect_true(all(nchar(archive$sha256) == 64L))
  expect_true(any(
    mapping$repo_path == "USA_analysis/02_analysis.R" &
      mapping$match_status == "exact_match"
  ))
  expect_true(any(
    mapping$repo_path == "Canada_analysis/02_analysis.R" &
      mapping$match_status == "exact_match"
  ))
})
