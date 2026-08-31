source(here::here("R", "canada_model.R"))

test_that("Statistics Canada snapshots produce the registered analysis strata", {
  path <- here::here("data", "raw", "statcan", "13100768.csv")
  sex <- read_canada_model_input(path, stratified_by_sex = TRUE)
  total <- read_canada_model_input(path, stratified_by_sex = FALSE)

  expect_setequal(unique(sex$age), c("0-44", "45-64", "65-84", "85+"))
  expect_setequal(unique(sex$sex), c("Females", "Males"))
  expect_identical(unique(total$sex), "total")
  expect_setequal(unique(sex$province), unname(canada_province_labels()))
  expect_true(all(sex$date == as.Date(sex$date)))
  expect_true(all(total$death >= 0))
})

test_that("the Canada refit entry point writes a complete manifest", {
  runner <- here::here("scripts", "model_fitting", "canada", "refit.R")
  output_root <- tempfile("canada-manifest-")
  on.exit(unlink(output_root, recursive = TRUE), add = TRUE)
  status <- system2(
    file.path(R.home("bin"), "Rscript"),
    c(
      "--vanilla",
      runner,
      paste0("--output-root=", output_root),
      "--manifest-only=true"
    ),
    stdout = TRUE,
    stderr = TRUE
  )
  expect_null(attr(status, "status"), info = paste(status, collapse = "\n"))
  manifest <- utils::read.csv(file.path(
    output_root, "manifests", "model_manifest.csv"
  ))
  expect_gt(nrow(manifest), 0L)
  expect_setequal(
    unique(manifest$analysis_path),
    c("sex_stratified", "non_sex_stratified")
  )
  expect_true(all(manifest$training_observations > 0L))
  expect_equal(anyDuplicated(manifest$model_id), 0L)
})
