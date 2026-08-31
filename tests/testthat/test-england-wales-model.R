model_path <- testthat::test_path(
  "..", "..", "R", "england_wales_model.R"
)

test_that("the England-and-Wales model module exists", {
  expect_true(file.exists(model_path))
})

test_that("England-and-Wales source bands and geography are explicit", {
  source(model_path, local = TRUE)

  expect_identical(
    england_wales_age_groups(),
    c("Under 65", "65-84", "85+")
  )
  expect_identical(england_wales_geography(), "England and Wales")
})

test_that("England-and-Wales dates follow the Eurostat ISO-week convention", {
  source(model_path, local = TRUE)

  observed <- england_wales_iso_monday(
    c(2020L, 2020L, 2021L),
    c(52L, 53L, 1L)
  )
  expected <- ISOweek::ISOweek2date(c(
    "2020-W52-1",
    "2020-W53-1",
    "2021-W01-1"
  ))

  expect_identical(observed, expected)
  expect_identical(as.integer(diff(observed)), c(7L, 7L))
})

test_that("England-and-Wales source definitions preserve the 2021 break", {
  source(model_path, local = TRUE)

  definitions <- england_wales_source_definitions()
  expect_identical(definitions$period, c("1981-2020", "2021-2023"))
  expect_match(definitions$count_definition[[1]], "occurrence")
  expect_match(definitions$count_definition[[1]], "usual residents")
  expect_match(definitions$count_definition[[2]], "registration")
  expect_match(definitions$count_definition[[2]], "non-residents")
})

test_that("the tracked ONS inputs build three complete weekly series", {
  skip_if_not_installed("readxl")
  source(model_path, local = TRUE)

  root <- testthat::test_path("..", "..", "UK_analysis")
  data <- read_england_wales_model_input(root)
  manifest <- build_england_wales_manifest(data, base_seed = 20260830L)

  expect_setequal(unique(data$age_group), england_wales_age_groups())
  expect_equal(nrow(manifest), 3L)
  expect_equal(manifest$seed, 20260830L + seq_len(3L))
  expect_true(all(manifest$training_rows > 0L))
  expect_identical(max(data$date), as.Date("2023-08-28"))

  spacing <- split(data$date, data$age_group)
  expect_true(all(vapply(
    spacing,
    function(dates) {
      gaps <- as.integer(diff(sort(unique(dates))))
      sum(gaps == 7L) == 2223L &&
        sum(gaps == 14L) == 1L &&
        all(gaps %in% c(7L, 14L))
    },
    logical(1)
  )))
  expect_true(all(vapply(
    spacing,
    function(dates) {
      dates <- sort(unique(dates))
      index <- which(diff(dates) == 14)
      identical(dates[index], as.Date("2020-12-21")) &&
        identical(dates[index + 1L], as.Date("2021-01-04"))
    },
    logical(1)
  )))
})

test_that("England and Wales use the corrected Europe prior contract", {
  source(model_path, local = TRUE)

  prior <- england_wales_prior_specification()
  expect_identical(england_wales_harmonics(), 4L)
  expect_identical(prior$trend$h, 5)
  expect_identical(prior$seasonal$h, 1)
})

test_that("the England-and-Wales CLI writes the complete manifest", {
  source(model_path, local = TRUE)
  runner <- testthat::test_path(
    "..", "..", "scripts", "england_wales", "refit.R"
  )
  output_root <- tempfile("england-wales-manifest-")
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
  expect_equal(nrow(manifest), 3L)
  expect_setequal(manifest$age_group, england_wales_age_groups())
})

test_that("England-and-Wales wave summaries use ratio-of-totals P-scores", {
  source(testthat::test_path("..", "..", "R", "europe_model.R"), local = TRUE)
  source(model_path, local = TRUE)
  dates <- as.Date(c("2020-03-02", "2020-03-09", "2020-11-02"))
  samples <- matrix(
    as.integer(c(10, 20, 12, 18, 5, 6)),
    nrow = 3L,
    ncol = 2L
  )
  model_pred <- list(
    samples = samples,
    summary = data.frame(
      mean = rowMeans(samples),
      upper = apply(samples, 1L, max),
      lower = apply(samples, 1L, min),
      x = 0:2,
      time = dates
    )
  )
  full_data <- data.frame(
    date = dates,
    observed_deaths = c(15L, 25L, 8L)
  )
  summary <- summarize_england_wales_waves(model_pred, full_data)
  initial <- summary[summary$wave == "initial", , drop = FALSE]

  expected_draws <- colSums(samples[1:2, , drop = FALSE])
  expected_pscore <- (40 - expected_draws) / expected_draws
  expect_equal(initial$p_med, median(expected_pscore))
})
