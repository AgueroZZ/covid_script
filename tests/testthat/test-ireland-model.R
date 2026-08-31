model_path <- testthat::test_path("..", "..", "R", "ireland_model.R")

test_that("the Ireland model module exists", {
  expect_true(file.exists(model_path))
})

test_that("Ireland retains its source-specific quarterly contract", {
  source(model_path, local = TRUE)

  expect_identical(
    ireland_age_groups(),
    c("25-44", "45-64", "65-84", "85+")
  )
  expect_identical(ireland_geography(), "Republic of Ireland")
  expect_identical(ireland_harmonics(), 1L)

  data <- read_ireland_model_input(testthat::test_path(
    "..", "..", "data", "raw", "cso", "ireland_quarterly_deaths.csv"
  ))
  expect_identical(min(data$date), as.Date("2010-03-31"))
  expect_identical(max(data$date), as.Date("2023-03-31"))
  expect_true(all(table(data$age_group) == 53L))
  expect_true(all(data$source_frequency == "quarterly"))
})

test_that("Ireland uses documented quarter-end wave assignment", {
  source(model_path, local = TRUE)

  dates <- as.Date(c(
    "2020-03-31", "2020-06-30", "2020-09-30", "2020-12-31",
    "2021-06-30", "2021-09-30", "2021-12-31", "2022-03-31"
  ))
  expect_identical(
    ireland_wave_from_quarter_end(dates),
    c(
      "initial", "initial", "initial", "alpha",
      "alpha", "delta", "delta", "omicron"
    )
  )
})

test_that("quarterly predictive draws are direct integer Poisson counts", {
  source(model_path, local = TRUE)

  weekly_rate <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 3L, ncol = 2L)
  aggregation <- matrix(c(1, 0.5, 0, 0.5, 1, 1), nrow = 2L, byrow = TRUE)
  draws <- draw_ireland_quarterly_poisson(
    weekly_rate,
    aggregation,
    seed = 11L
  )

  expect_identical(dim(draws), c(2L, 2L))
  expect_true(all(draws >= 0L))
  expect_true(all(draws == floor(draws)))
  expect_identical(typeof(draws), "integer")
})

test_that("the Ireland manifest contains all four fitted source bands", {
  source(model_path, local = TRUE)

  data <- read_ireland_model_input(testthat::test_path(
    "..", "..", "data", "raw", "cso", "ireland_quarterly_deaths.csv"
  ))
  manifest <- build_ireland_manifest(data, base_seed = 20260830L)
  expect_equal(nrow(manifest), 4L)
  expect_setequal(manifest$age_group, ireland_age_groups())
  expect_equal(manifest$seed, 20260830L + seq_len(4L))
  expect_true(all(manifest$training_rows == 40L))
})

test_that("Ireland uses a converted one-harmonic seasonal prior", {
  source(model_path, local = TRUE)
  source(testthat::test_path(
    "..", "..", "code", "regions", "ireland", "model_functions.R"
  ), local = TRUE)

  prior <- ireland_converted_prior_specification()
  expected <- prior_conversion_sGP_m(
    d = 1,
    prior = list(u = 0.1, a = 0.01),
    a = 2 * pi,
    m = 1
  )
  expect_equal(prior$seasonal, expected)
  expect_false(identical(prior$seasonal, list(u = 0.1, a = 0.01)))
})

test_that("the Ireland CLI writes the complete four-model manifest", {
  source(model_path, local = TRUE)
  runner <- testthat::test_path(
    "..", "..", "scripts", "model_fitting", "ireland", "refit.R"
  )
  output_root <- tempfile("ireland-manifest-")
  on.exit(unlink(output_root, recursive = TRUE), add = TRUE)
  status <- system2(
    command = file.path(R.home("bin"), "Rscript"),
    args = c(
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
    output_root,
    "manifests",
    "model_manifest.csv"
  ))
  expect_equal(nrow(manifest), 4L)
  expect_setequal(manifest$age_group, ireland_age_groups())
})
