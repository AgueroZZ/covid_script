test_that("the Europe production manifest locks the complete Eurostat grid", {
  model_path <- testthat::test_path("..", "..", "R", "europe_model.R")
  expect_true(file.exists(model_path))
  source(model_path, local = TRUE)

  data_path <- testthat::test_path(
    "..",
    "..",
    "data",
    "raw",
    "eurostat",
    "demo_r_mwk_20_linear.csv"
  )
  data <- read_europe_model_input(data_path)
  manifest <- build_europe_manifest(data, base_seed = 20260829L)

  expect_equal(nrow(manifest), 388L)
  expect_equal(length(unique(manifest$model_id)), 388L)
  expect_equal(sum(manifest$sex == "T"), 132L)
  expect_equal(sum(manifest$sex == "F"), 128L)
  expect_equal(sum(manifest$sex == "M"), 128L)
  expect_equal(sort(unique(manifest$age)), sort(c(
    "Y20-39",
    "Y40-59",
    "Y60-79",
    "Y_GE80"
  )))
  expect_true(all(manifest$year_span >= 10))
  expect_true(all(manifest$training_rows > 0))
  expect_true(all(manifest$prediction_end > as.Date("2022-01-01")))
  expect_equal(manifest$seed, 20260829L + seq_len(388L))
})

test_that("the compact Europe result matches the historical artifact contract", {
  model_path <- testthat::test_path("..", "..", "R", "europe_model.R")
  source(model_path, local = TRUE)

  dates <- as.Date("2020-01-06") + 7 * 0:2
  samples <- matrix(
    as.integer(seq_len(9L)),
    nrow = 3L,
    ncol = 3L
  )
  model_pred <- list(
    samples = samples,
    summary = data.frame(
      mean = rowMeans(samples),
      upper = apply(samples, 1, max),
      lower = apply(samples, 1, min),
      x = 0:2,
      time = dates
    )
  )

  expect_invisible(validate_europe_model_pred(
    model_pred,
    expected_rows = 3L,
    expected_draws = 3L
  ))
  expect_identical(typeof(model_pred$samples), "integer")
  expect_identical(
    names(model_pred$summary),
    c("mean", "upper", "lower", "x", "time")
  )
})

test_that("the production scripts freeze BayesGP and the total thread cap", {
  runner_path <- testthat::test_path(
    "..",
    "..",
    "scripts",
    "model_fitting",
    "europe",
    "refit_eurostat.R"
  )
  verifier_path <- testthat::test_path(
    "..",
    "..",
    "scripts",
    "model_fitting",
    "europe",
    "verify_eurostat_refit.R"
  )
  model_path <- testthat::test_path("..", "..", "R", "europe_model.R")
  expect_true(file.exists(runner_path))
  expect_true(file.exists(verifier_path))
  expect_true(file.exists(model_path))

  runner <- paste(readLines(runner_path, warn = FALSE), collapse = "\n")
  model <- paste(readLines(model_path, warn = FALSE), collapse = "\n")
  production_code <- paste(runner, model, sep = "\n")
  expect_match(production_code, "BayesGP::model_fit", fixed = TRUE)
  expect_match(production_code, "h = 5", fixed = TRUE)
  expect_match(production_code, "h = 1", fixed = TRUE)
  expect_match(production_code, "aghq_k = 5", fixed = TRUE)
  expect_match(production_code, "M = 3000", fixed = TRUE)
  expect_match(production_code, "region = full_region", fixed = TRUE)
  expect_match(runner, "mc.preschedule = FALSE", fixed = TRUE)
  expect_match(runner, "workers must be between 1 and 12", fixed = TRUE)
  expect_match(runner, 'gsub("-", "_"', fixed = TRUE)

  required_thread_variables <- c(
    "OMP_NUM_THREADS",
    "OPENBLAS_NUM_THREADS",
    "MKL_NUM_THREADS",
    "BLIS_NUM_THREADS",
    "OMP_THREAD_LIMIT"
  )
  for (variable in required_thread_variables) {
    expect_match(runner, variable, fixed = TRUE)
  }
})

test_that("the production CLI accepts documented hyphenated arguments", {
  runner_path <- testthat::test_path(
    "..",
    "..",
    "scripts",
    "model_fitting",
    "europe",
    "refit_eurostat.R"
  )
  output_root <- tempfile("europe-manifest-")
  on.exit(unlink(output_root, recursive = TRUE), add = TRUE)
  status <- system2(
    command = file.path(R.home("bin"), "Rscript"),
    args = c(
      "--vanilla",
      runner_path,
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
  expect_equal(nrow(manifest), 388L)
})

test_that("the Netherlands Figure 1 artifact remains compact and render-ready", {
  model_path <- testthat::test_path("..", "..", "R", "europe_model.R")
  source(model_path, local = TRUE)

  component <- data.frame(
    date = as.Date("2020-01-06") + 7 * 0:2,
    mean = c(1, 2, 3),
    lower = c(0, 1, 2),
    upper = c(2, 3, 4)
  )
  input <- list(
    overall = transform(component, observed = c(1, 2, 4))[
      c("date", "observed", "mean", "lower", "upper")
    ],
    trend = component,
    seasonal = component,
    training_boundary = as.Date("2020-01-01")
  )

  expect_invisible(validate_europe_figure_01_input(input))
  expect_false(any(vapply(input, inherits, logical(1), what = "FitResult")))
})
