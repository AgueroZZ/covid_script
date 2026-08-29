us_analysis_id <- function(analysis_path, geography, age_group, sex) {
  slug <- function(value) {
    value <- tolower(value)
    value <- gsub("[^a-z0-9]+", "-", value)
    gsub("(^-|-$)", "", value)
  }
  paste(
    "us",
    slug(analysis_path),
    slug(geography),
    slug(age_group),
    slug(sex),
    sep = "__"
  )
}

us_expected_months <- function(
  start = as.Date("1999-01-01"),
  end = as.Date("2023-08-01")
) {
  lubridate::ceiling_date(seq(start, end, by = "month"), "month") -
    lubridate::days(1)
}

build_us_cohort_inventory <- function(data, analysis_path, config) {
  expected_months <- us_expected_months()
  wave <- assign_wave(data$date, config)
  augmented <- dplyr::mutate(data, wave = wave)

  inventory <- augmented |>
    dplyr::group_by(geography, age_group, sex) |>
    dplyr::summarise(
      data_start = min(date),
      data_end = max(date),
      n_observations = dplyr::n(),
      training_observations = sum(date < as.Date("2020-01-01")),
      training_year_span = {
        years <- lubridate::year(date[date < as.Date("2020-01-01")])
        if (length(years) == 0L) NA_integer_ else diff(range(years))
      },
      initial_observations = sum(wave == "initial", na.rm = TRUE),
      alpha_observations = sum(wave == "alpha", na.rm = TRUE),
      delta_observations = sum(wave == "delta", na.rm = TRUE),
      omicron_observations = sum(wave == "omicron", na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      analysis_id = us_analysis_id(
        analysis_path,
        geography,
        age_group,
        sex
      ),
      region = analysis_path,
      included = TRUE,
      exclusion_reason = NA_character_,
      n_expected = length(expected_months),
      missing_or_suppressed = n_expected - n_observations,
      complete_296 = missing_or_suppressed == 0L,
      legacy_attempted = TRUE
    ) |>
    dplyr::select(
      analysis_id,
      region,
      geography,
      age_group,
      sex,
      included,
      exclusion_reason,
      data_start,
      data_end,
      n_observations,
      missing_or_suppressed,
      n_expected,
      complete_296,
      legacy_attempted,
      training_observations,
      training_year_span,
      initial_observations,
      alpha_observations,
      delta_observations,
      omicron_observations
    ) |>
    dplyr::arrange(geography, age_group, sex)

  if (anyDuplicated(inventory$analysis_id)) {
    stop("US cohort analysis identifiers must be unique.")
  }
  if (any(inventory$missing_or_suppressed < 0L)) {
    stop("US cohort observations exceed the expected monthly calendar.")
  }

  inventory
}

cohort_registry_rows <- function(inventory) {
  inventory |>
    dplyr::transmute(
      analysis_id,
      region,
      jurisdiction = geography,
      age_group,
      sex,
      included,
      exclusion_reason,
      data_start,
      data_end,
      n_observations,
      missing_or_suppressed
    )
}
