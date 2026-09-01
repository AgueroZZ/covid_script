source(here::here("R", "config.R"))
source(here::here("R", "waves.R"))
source(here::here("R", "us_data.R"))
source(here::here("R", "canada_model.R"))
source(here::here("R", "supplementary_bundle.R"))

test_that("the supplementary registry covers every registered cohort", {
  registry <- build_supplementary_registry(
    project_root = here::here(),
    legacy_root = here::here("..", "covid_excess")
  )

  expect_equal(nrow(registry), 1119L)
  expect_equal(anyDuplicated(registry$analysis_id), 0L)
  expect_equal(
    unname(as.integer(table(factor(
      registry$analysis_family,
      levels = c(
        "europe", "us_non_sex", "us_sex", "canada_non_sex",
        "canada_sex", "england_wales", "ireland"
      )
    )))),
    c(388L, 204L, 408L, 40L, 72L, 3L, 4L)
  )
  expect_equal(sum(registry$status == "model_failed"), 1L)
  expect_identical(
    registry$analysis_id[registry$status == "model_failed"],
    "us__us-sex__vermont__0-44__female"
  )
  expect_true(all(registry$frequency %in% c("monthly", "weekly", "quarterly")))
  expect_true(all(registry$sex %in% c("total", "female", "male")))
  expect_true(all(registry$source_root %in% c("project", "legacy")))
  expect_false(any(grepl("^/", registry$model_path)))
})

test_that("every canonical observed snapshot has a frozen hash record", {
  registry <- build_supplementary_registry(
    project_root = here::here(),
    legacy_root = here::here("..", "covid_excess")
  )
  inventory <- supplementary_observed_source_inventory(here::here())

  expect_equal(nrow(inventory), 21L)
  expect_setequal(
    inventory$observed_source_id,
    unique(registry$observed_source_id)
  )
  expect_equal(anyDuplicated(inventory$source_path), 0L)
  expect_true(all(grepl("^[a-f0-9]{64}$", inventory$sha256)))
  expect_true(all(inventory$bytes > 0))
  expect_false(any(grepl("^/", inventory$source_path)))
  expect_silent(validate_supplementary_observed_source_inventory(
    registry,
    inventory
  ))
})

synthetic_supplementary_prediction <- function() {
  samples <- matrix(
    c(
      10, 20, 30, 40,
      20, 20, 40, 40
    ),
    nrow = 4L,
    ncol = 2L
  )
  list(
    analysis_id = "synthetic__example",
    analysis_family = "synthetic",
    geography = "EX",
    geography_label = "Example",
    map_id = "EX",
    age_group = "20-39",
    sex = "total",
    frequency = "monthly",
    dates = as.Date(c(
      "2020-03-31", "2020-10-31", "2020-11-30", "2021-07-31"
    )),
    observed_deaths = c(30, 30, 60, 60),
    samples = samples,
    expected_mean = rowMeans(samples),
    expected_lower = apply(samples, 1L, min),
    expected_upper = apply(samples, 1L, max)
  )
}

test_that("supplementary summaries use pointwise draws and half-open waves", {
  config <- read_analysis_config(here::here("config", "analysis.yml"))
  summary <- summarize_supplementary_prediction(
    synthetic_supplementary_prediction(),
    wave_table(config)
  )

  expect_equal(nrow(summary$pointwise), 4L)
  expect_equal(summary$pointwise$p_mean[[1]], mean(c(2, 0.5)))
  expect_true(all(
    summary$pointwise$p_lower <= summary$pointwise$p_median &
      summary$pointwise$p_median <= summary$pointwise$p_upper
  ))
  expect_identical(summary$wave$wave, c("initial", "alpha", "delta", "omicron"))
  expect_equal(summary$wave$observed_periods, c(2L, 1L, 1L, 0L))
  expect_identical(
    summary$wave$status,
    c("success", "success", "success", "no_observed_periods")
  )
  expect_equal(summary$wave$observed_deaths[[1]], 60)
  expect_equal(summary$wave$p_median[[1]], median(c(1, 0.5)))
})

test_that("undefined p-scores do not create infinite public values", {
  prediction <- synthetic_supplementary_prediction()
  prediction$samples[1L, ] <- 0
  prediction$expected_mean[[1L]] <- 0
  prediction$expected_lower[[1L]] <- 0
  prediction$expected_upper[[1L]] <- 0
  config <- read_analysis_config(here::here("config", "analysis.yml"))
  summary <- summarize_supplementary_prediction(prediction, wave_table(config))

  expect_true(all(is.na(summary$pointwise[1L, c(
    "p_mean", "p_variance", "p_lower", "p_median", "p_upper"
  )])))
  expect_false(any(is.infinite(unlist(summary))))
})

test_that("the bundle validator enforces coverage and safe web fields", {
  registry <- data.frame(
    analysis_id = "synthetic__example",
    analysis_family = "synthetic",
    geography = "EX",
    geography_label = "Example",
    map_id = "EX",
    age_group = "20-39",
    sex = "total",
    frequency = "monthly",
    source_root = "project",
    model_path = "output/example.rda",
    observed_source_id = "synthetic",
    source_kind = "test_compact_prediction",
    status = "available",
    error_message = NA_character_,
    stringsAsFactors = FALSE
  )
  config <- read_analysis_config(here::here("config", "analysis.yml"))
  summaries <- summarize_supplementary_prediction(
    synthetic_supplementary_prediction(),
    wave_table(config)
  )
  inventory <- data.frame(
    analysis_id = "synthetic__example",
    source_root = "project",
    model_path = "output/example.rda",
    sha256 = paste(rep("a", 64L), collapse = ""),
    bytes = 1,
    stringsAsFactors = FALSE
  )

  expect_silent(validate_supplementary_bundle_tables(
    registry, summaries$pointwise, summaries$wave, inventory
  ))
  shard <- supplementary_web_shard(registry, summaries$pointwise)
  expect_false(any(c("samples", "model_path", "source_root") %in% names(shard$series)))
  expect_false(grepl("/Users/", jsonlite::toJSON(shard), fixed = TRUE))
})
