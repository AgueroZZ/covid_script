test_that("the targets graph declares the foundation outputs", {
  manifest <- targets::tar_manifest(
    script = here::here("_targets.R"),
    callr_function = NULL
  )

  expect_true(all(c(
    "analysis_config_file",
    "analysis_config",
    "wave_definitions",
    "foundation_manifest"
  ) %in% manifest$name))
})
