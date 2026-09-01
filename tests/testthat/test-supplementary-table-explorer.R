source(here::here("R", "reporting.R"))
source(here::here("R", "tables.R"))
source(here::here("R", "supplementary_table_explorer.R"))

synthetic_table_explorer_rows <- function(
  analysis_family,
  geography,
  geography_label,
  age_group,
  sexes,
  values,
  statuses = "success",
  frequency = "weekly"
) {
  waves <- c("initial", "alpha", "delta", "omicron")
  grid <- expand.grid(
    sex = sexes,
    wave = waves,
    stringsAsFactors = FALSE
  )
  grid$analysis_family <- analysis_family
  grid$geography <- geography
  grid$geography_label <- geography_label
  grid$age_group <- age_group
  grid$frequency <- frequency
  grid$p_median <- unname(values[grid$sex])
  if (length(statuses) == 1L && is.null(names(statuses))) {
    statuses <- stats::setNames(rep(statuses, length(sexes)), sexes)
  }
  grid$status <- unname(statuses[grid$sex])
  grid
}

test_that("expanded table prefers paired sex results and retains total fallbacks", {
  waves <- c("initial", "alpha", "delta", "omicron")
  paired <- synthetic_table_explorer_rows(
    "europe", "AA", "Alpha", "60-79",
    c("female", "male"),
    c(female = 0.2, male = 0.1)
  )
  paired_total <- synthetic_table_explorer_rows(
    "europe", "AA", "Alpha", "60-79",
    "total", c(total = 0.15)
  )
  germany_total <- synthetic_table_explorer_rows(
    "europe", "DE", "Germany", "GE80",
    "total", c(total = 0.25)
  )
  canada_total <- synthetic_table_explorer_rows(
    "canada_non_sex", "PE", "Prince Edward Island", "85+",
    "total", c(total = 0.3)
  )
  vermont <- synthetic_table_explorer_rows(
    "us_sex", "Vermont", "Vermont", "0-44",
    c("female", "male"),
    c(female = NA_real_, male = 0.4),
    c(female = "model_failed", male = "success"),
    frequency = "monthly"
  )
  input <- rbind(paired, paired_total, germany_total, canada_total, vermont)
  input$wave <- factor(input$wave, levels = waves)
  input <- input[order(input$analysis_family, input$geography, input$sex, input$wave), ]
  input$wave <- as.character(input$wave)

  europe_vaccination <- data.frame(
    date = as.Date(c("2021-07-01", "2021-07-01")),
    geography = c("AA", "DE"),
    people_vaccinated_per_hundred = c(60, 55),
    vaccination_group = c("high", "neither"),
    stringsAsFactors = FALSE
  )
  us_vaccination <- data.frame(
    date = as.Date("2021-07-01"),
    geography = "Vermont",
    people_vaccinated_per_hundred = 65,
    vaccination_group = "high",
    stringsAsFactors = FALSE
  )

  result <- build_supplementary_table_explorer(
    input,
    europe_vaccination,
    us_vaccination
  )

  expect_equal(nrow(result), 4L)
  alpha <- result[result$geography == "AA", ]
  expect_identical(alpha$initial, "M: 0.100; F: 0.200")
  expect_identical(alpha$estimand_sex_group, "Male and female")
  expect_identical(alpha$population_view, "Sex-stratified (M/F)")
  expect_identical(alpha$geography_display, "Alpha (AA)")
  expect_identical(alpha$result_status, "available")

  germany <- result[result$geography == "DE", ]
  expect_identical(germany$initial, "Total: 0.250")
  expect_identical(germany$estimand_sex_group, "Total")
  expect_identical(germany$population_view, "Total population")
  expect_identical(germany$geography_display, "Germany (DE)")

  canada <- result[result$geography == "PE", ]
  expect_identical(canada$geography_display, "Prince Edward Island (PE)")
  expect_true(is.na(canada$people_vaccinated_per_hundred))
  expect_true(is.na(canada$vaccination_group))
  expect_true(is.na(canada$vaccination_measurement_date))

  partial <- result[result$geography == "Vermont", ]
  expect_identical(partial$initial, "M: 0.400; F: unavailable")
  expect_identical(partial$population_view, "Sex-stratified (M/F)")
  expect_identical(partial$geography_display, "Vermont (VT)")
  expect_identical(partial$result_status, "partial")
  expect_equal(anyDuplicated(result[c(
    "region_set", "geography", "estimand_age_group"
  )]), 0L)
})

test_that("expanded frozen table exactly contains manuscript Table 1", {
  paths <- c(
    wave = here::here(
      "output", "supplementary", "frozen_20260831", "wave_summary.csv"
    ),
    europe_vaccination = here::here(
      "output", "data", "europe", "vaccination_membership.csv"
    ),
    us_vaccination = here::here(
      "output", "data", "us", "vaccination_membership.csv"
    ),
    manuscript = here::here(
      "tables", "manuscript", "table_01_wave_pscores.csv"
    )
  )
  skip_if_not(all(file.exists(paths)))

  manuscript <- utils::read.csv(
    paths[["manuscript"]],
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  expanded <- build_supplementary_table_explorer(
    utils::read.csv(paths[["wave"]], stringsAsFactors = FALSE),
    utils::read.csv(
      paths[["europe_vaccination"]],
      stringsAsFactors = FALSE
    ),
    utils::read.csv(
      paths[["us_vaccination"]],
      stringsAsFactors = FALSE
    ),
    manuscript_table = manuscript
  )

  expect_equal(nrow(expanded), 587L)
  expect_equal(sum(expanded$in_manuscript_table_1 == "Yes"), 26L)
  expect_equal(sum(is.na(expanded$people_vaccinated_per_hundred)), 47L)
  expect_equal(sum(expanded$estimand_sex_group == "Male and female"), 368L)
  expect_equal(sum(expanded$estimand_sex_group == "Total"), 219L)
  expect_equal(
    sum(expanded$population_view == "Sex-stratified (M/F)"),
    368L
  )
  expect_equal(
    sum(expanded$population_view == "Total population"),
    219L
  )
  expect_equal(sum(expanded$result_status == "partial"), 3L)
  expect_identical(
    unique(expanded$geography_display[
      expanded$geography == "Massachusetts"
    ]),
    "Massachusetts (MA)"
  )
  expect_identical(
    unique(expanded$geography_display[expanded$geography == "ON"]),
    "Ontario (ON)"
  )
  expect_identical(
    unique(expanded$geography_display[expanded$geography == "BE"]),
    "Belgium (BE)"
  )
  expect_silent(validate_supplementary_table_against_manuscript(
    expanded,
    manuscript
  ))
})

test_that("public table bundle separates reader fields from local QA", {
  root <- here::here(
    "output",
    "supplementary",
    "table_explorer_20260901_v2",
    "browser",
    "table_explorer"
  )
  index_path <- file.path(root, "index.json")
  csv_path <- file.path(root, "downloads", "expanded_table_01.csv")
  skip_if_not(file.exists(index_path) && file.exists(csv_path))

  index <- jsonlite::read_json(index_path, simplifyVector = TRUE)
  csv <- utils::read.csv(
    csv_path,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  qa_only <- c("result_status", "in_manuscript_table_1")

  expect_identical(index$schema_version, "1.1.0")
  expect_equal(index$row_count, 587L)
  expect_equal(nrow(index$columns), 11L)
  expect_equal(nrow(index$download_columns), 16L)
  expect_identical(
    as.integer(index$population_views$rows),
    c(368L, 219L)
  )
  expect_false(any(qa_only %in% names(index$rows)))
  expect_false(any(qa_only %in% names(csv)))
  expect_identical(names(csv), index$download_columns$key)
  expect_equal(nrow(csv), 587L)
})
