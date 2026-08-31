source(here::here("R", "submission_freeze.R"))

test_that("submission freeze contract covers every adopted reporting artifact", {
  contract <- submission_freeze_contract()
  expect_equal(length(contract$reporting_inputs), 5L)
  expect_equal(length(contract$table_inputs), 4L)
  expect_equal(length(contract$final_figures), 10L)
  expect_equal(length(contract$final_tables), 2L)
  expect_equal(length(contract$upstream_completion_flags), 4L)
  expect_equal(anyDuplicated(unlist(contract, use.names = FALSE)), 0L)
})

test_that("freeze contract validation rejects missing or empty files", {
  existing <- tempfile()
  empty <- tempfile()
  writeLines("valid", existing)
  file.create(empty)
  expect_silent(submission_freeze_assert_files(existing, "fixture"))
  expect_error(
    submission_freeze_assert_files(c(existing, empty), "fixture"),
    "empty"
  )
  expect_error(
    submission_freeze_assert_files("does-not-exist", "fixture"),
    "missing"
  )
})

test_that("freeze manifest verifies copied bytes and hashes", {
  root <- tempfile("freeze-root-")
  dir.create(root)
  source <- tempfile(fileext = ".txt")
  writeLines("submission freeze fixture", source)
  copied <- copy_submission_freeze_files(
    source,
    destination_root = root,
    relative_paths = "inputs/fixture.txt",
    role = "fixture"
  )
  manifest <- build_submission_freeze_manifest(copied$path, root, copied$role)
  expect_silent(validate_submission_freeze_manifest(manifest, root))
  expect_equal(manifest$relative_path, "inputs/fixture.txt")
  expect_equal(nchar(manifest$sha256), 64L)
})

test_that("strict reporting status rejects every non-rendered output", {
  good <- data.frame(output_id = c("a", "b"), status = "rendered")
  bad <- data.frame(
    output_id = c("a", "b"),
    status = c("rendered", "skipped_caption_review")
  )
  expect_silent(assert_strict_reporting_status(good))
  expect_error(assert_strict_reporting_status(bad), "b")
})

test_that("freeze output root must be new", {
  output <- tempfile("existing-freeze-")
  dir.create(output)
  expect_error(assert_new_submission_freeze_root(output), "already exists")
  expect_silent(assert_new_submission_freeze_root(tempfile("new-freeze-")))
})
