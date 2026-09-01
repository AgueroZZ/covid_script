source(here::here("R", "config.R"))
source(here::here("R", "validation.R"))
source(here::here("R", "reporting.R"))
source(here::here("R", "vaccination.R"))
source(here::here("R", "supplementary_vaccination_groups.R"))

test_that("European vaccination membership uses the frozen reference-date rule", {
  config <- read_analysis_config(here::here("config", "analysis.yml"))
  vaccination <- prepare_europe_vaccination(
    here::here("data", "raw", "owid", "europe_vaccination_snapshot.rda"),
    config
  )

  expect_equal(nrow(vaccination), 50L)
  expect_equal(anyDuplicated(vaccination$geography), 0L)
  expect_true(all(vaccination$date <= as.Date(config$vaccination$reference_date)))
  expect_true("EL" %in% vaccination$geography)
  expect_false("GR" %in% vaccination$geography)
})

synthetic_vaccination_prediction <- function(
  geography,
  observed,
  samples,
  dates = as.Date(c("2020-03-31", "2020-04-30"))
) {
  list(
    geography = geography,
    dates = dates,
    observed_deaths = observed,
    samples = samples
  )
}

test_that("sex contrasts are summarized draw by draw after exact date alignment", {
  female <- synthetic_vaccination_prediction(
    "Example",
    c(12, 24),
    matrix(c(10, 20, 12, 24, 15, 30), nrow = 2L)
  )
  male <- synthetic_vaccination_prediction(
    "Example",
    c(10, 18),
    matrix(c(8, 16, 10, 18, 12, 20), nrow = 2L)
  )
  female_pscore <- sweep(female$samples, 1L, female$observed_deaths, "-") * -1 /
    female$samples
  male_pscore <- sweep(male$samples, 1L, male$observed_deaths, "-") * -1 /
    male$samples
  expected <- female_pscore - male_pscore

  summary <- summarize_vaccination_sex_contrast(female, male)

  expect_equal(summary$date, female$dates)
  expect_equal(summary$mean, rowMeans(expected))
  expect_equal(summary$variance, apply(expected, 1L, stats::var))
})

test_that("explorer aggregation matches the reporting fixed-effect contract", {
  input <- data.frame(
    geography = rep(c("A", "B", "C"), each = 2L),
    date = rep(as.Date(c("2020-03-31", "2020-04-30")), 3L),
    mean = c(0.1, 0.2, 0.3, 0.4, -0.1, 0),
    variance = c(0.04, 0.09, 0.01, 0.04, 0.16, 0.25),
    stringsAsFactors = FALSE
  )
  membership <- data.frame(
    geography = c("A", "B", "C"),
    vaccination_group = c("high", "high", "low"),
    stringsAsFactors = FALSE
  )
  joined <- merge(input, membership, by = "geography", sort = FALSE)
  reporting <- aggregate_fixed_effect_summary(
    joined,
    c("vaccination_group", "date")
  )
  explorer <- aggregate_vaccination_group_summary(input, membership)
  keys <- c("vaccination_group", "date")
  reporting <- reporting[do.call(order, reporting[keys]), ]
  explorer <- explorer[do.call(order, explorer[keys]), ]

  expect_equal(explorer$mean, reporting$mean)
  expect_equal(explorer$variance, reporting$variance)
  expect_equal(explorer$jurisdictions, reporting$jurisdictions)
})

test_that("panel coverage uses usable 2020-onward summary periods", {
  dates <- as.Date(c(
    "2020-01-31", "2020-02-29", "2020-03-31", "2020-04-30"
  ))
  input <- data.frame(
    figure = "figure_05",
    region = "us",
    geography = rep(c("Complete", "Sparse"), each = length(dates)),
    geography_label = rep(c("Complete", "Sparse"), each = length(dates)),
    age_group = "0-44",
    frequency = "monthly",
    date = rep(dates, 2L),
    mean = c(rep(0.1, 4L), 0.2, 0.2, NA_real_, 0.2),
    variance = c(rep(0.01, 4L), 0.02, 0.02, NA_real_, 0.02),
    stringsAsFactors = FALSE
  )

  coverage <- vaccination_panel_coverage(
    input,
    analysis_start = as.Date("2020-01-01"),
    minimum_fraction = 0.95
  )
  coverage <- coverage[match(c("Complete", "Sparse"), coverage$geography), ]

  expect_equal(coverage$usable_observations, c(4L, 3L))
  expect_equal(coverage$expected_observations, c(4L, 4L))
  expect_equal(coverage$missing_observations, c(0L, 1L))
  expect_equal(coverage$coverage_fraction, c(1, 0.75))
  expect_identical(coverage$default_eligible, c(TRUE, FALSE))
})

test_that("web shards contain summaries but no model objects or draws", {
  input <- data.frame(
    figure = "figure_04",
    region = "europe",
    geography = "EL",
    geography_label = "Greece",
    age_group = "20-39",
    frequency = "weekly",
    date = as.Date(c("2020-03-01", "2020-03-08")),
    mean = c(0.1, 0.2),
    variance = c(0.01, 0.02),
    stringsAsFactors = FALSE
  )
  shard <- vaccination_explorer_web_shard(input)
  serialized <- jsonlite::toJSON(shard, auto_unbox = TRUE)

  expect_equal(length(shard$series), 1L)
  expect_equal(shard$series[[1]]$usable_observations, 2L)
  expect_equal(shard$series[[1]]$expected_observations, 2L)
  expect_equal(shard$series[[1]]$missing_observations, 0L)
  expect_equal(shard$series[[1]]$coverage_fraction, 1)
  expect_true(shard$series[[1]]$default_eligible)
  expect_false(grepl("samples", serialized, fixed = TRUE))
  expect_false(grepl("model_path", serialized, fixed = TRUE))
  expect_false(grepl("/Users/", serialized, fixed = TRUE))
})
