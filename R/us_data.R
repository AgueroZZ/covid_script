us_sex_source_paths <- function() {
  file.path(
    "data",
    "raw",
    "cdc_wonder",
    "sex_stratified",
    c(
      "Multiple Cause of Death, 1999-2002.txt",
      "Multiple Cause of Death, 2003-2006.txt",
      "Multiple Cause of Death, 2007-2010.txt",
      "Multiple Cause of Death, 2011-2014.txt",
      "Multiple Cause of Death, 2015-2018.txt",
      "Multiple Cause of Death, 2019-2020.txt",
      "Multiple Cause of Death, 2021-2023.txt"
    )
  )
}

us_non_sex_source_paths <- function() {
  file.path(
    "data",
    "raw",
    "cdc_wonder",
    "non_sex_stratified",
    c(
      "USA_monthly_1999_2002.txt",
      "USA_monthly_2003_2006.txt",
      "USA_monthly_2007_2010.txt",
      "USA_monthly_2011_2014.txt",
      "USA_monthly_2015_2017.txt",
      "USA_monthly_2018_2020.txt",
      "USA_monthly_2021_2023.txt"
    )
  )
}

select_first_column <- function(data, candidates, label) {
  selected <- candidates[candidates %in% names(data)]
  if (length(selected) != 1L) {
    stop(
      "Expected exactly one ",
      label,
      " column; found: ",
      paste(selected, collapse = ", ")
    )
  }
  data[[selected]]
}

us_month_end <- function(month_code) {
  month_start <- as.Date(paste0(month_code, "/01"))
  lubridate::ceiling_date(month_start, unit = "month") - lubridate::days(1)
}

map_us_sex_age <- function(age) {
  age <- trimws(age)
  dplyr::case_when(
    age %in% c(
      "< 1 year",
      "1-4 years",
      "5-14 years",
      "15-24 years",
      "25-34 years",
      "35-44 years"
    ) ~ "0-44",
    age %in% c("45-54 years", "55-64 years") ~ "45-64",
    age %in% c("65-74 years", "75-84 years") ~ "65-84",
    age == "85+ years" ~ "GE85",
    TRUE ~ NA_character_
  )
}

map_us_non_sex_age <- function(age) {
  age <- trimws(age)
  dplyr::case_when(
    age %in% c(
      "20-24 years",
      "25-29 years",
      "30-34 years",
      "35-39 years"
    ) ~ "20-39",
    age %in% c(
      "40-44 years",
      "45-49 years",
      "50-54 years",
      "55-59 years"
    ) ~ "40-59",
    age %in% c(
      "60-64 years",
      "65-69 years",
      "70-74 years",
      "75-79 years"
    ) ~ "60-79",
    age %in% c(
      "80-84 years",
      "85-89 years",
      "90-94 years",
      "95-99 years",
      "100+ years"
    ) ~ "GE80",
    TRUE ~ NA_character_
  )
}

read_us_wonder_file <- function(path, stratified_by_sex) {
  require_files(path, "CDC WONDER")
  raw <- utils::read.delim(
    path,
    check.names = TRUE,
    stringsAsFactors = FALSE
  )

  state <- select_first_column(raw, c("State", "Residence.State"), "state")
  age <- select_first_column(
    raw,
    c("Ten.Year.Age.Groups", "Five.Year.Age.Groups"),
    "age"
  )
  month <- select_first_column(raw, "Month.Code", "month")
  source_year <- select_first_column(raw, "Year.Code", "year")
  deaths <- suppressWarnings(as.numeric(select_first_column(
    raw,
    "Deaths",
    "death count"
  )))

  month[!nzchar(trimws(month))] <- NA_character_
  footer <- is.na(month) & (is.na(state) | !nzchar(trimws(state))) & is.na(deaths)
  if (!any(footer)) {
    stop("Each CDC WONDER source file must contain footer or note rows.")
  }

  sex <- if (stratified_by_sex) {
    dplyr::recode(
      select_first_column(raw, "Gender.Code", "sex"),
      F = "female",
      M = "male",
      .default = NA_character_
    )
  } else {
    rep("total", nrow(raw))
  }

  age_group <- if (stratified_by_sex) {
    map_us_sex_age(age)
  } else {
    map_us_non_sex_age(age)
  }

  tibble::tibble(
    date = us_month_end(month),
    geography = trimws(state),
    age_group = age_group,
    sex = sex,
    observed_deaths = deaths,
    suppression_status = "observed",
    count_definition = "residence_state_all_cause_deaths",
    source_frequency = "monthly",
    source_id = if (stratified_by_sex) {
      "cdc_wonder_sex"
    } else {
      "cdc_wonder_non_sex"
    },
    source_file = path,
    source_year = suppressWarnings(as.integer(source_year))
  ) |>
    dplyr::filter(!footer, !is.na(age_group), !is.na(sex))
}

standardize_us_wonder <- function(paths, stratified_by_sex) {
  require_files(paths, "CDC WONDER")
  standardized <- dplyr::bind_rows(lapply(
    paths,
    read_us_wonder_file,
    stratified_by_sex = stratified_by_sex
  )) |>
    dplyr::group_by(
      date,
      geography,
      age_group,
      sex,
      suppression_status,
      count_definition,
      source_frequency,
      source_id,
      source_file,
      source_year
    ) |>
    dplyr::summarise(
      observed_deaths = sum(observed_deaths),
      .groups = "drop"
    ) |>
    dplyr::select(
      date,
      geography,
      age_group,
      sex,
      observed_deaths,
      suppression_status,
      count_definition,
      source_frequency,
      source_id,
      source_file,
      source_year
    ) |>
    dplyr::arrange(date, geography, age_group, sex)

  if (any(standardized$observed_deaths < 0) ||
      any(standardized$observed_deaths %% 1 != 0)) {
    stop("Observed US death counts must be non-negative integers.")
  }

  key <- standardized[c("date", "geography", "age_group", "sex")]
  if (anyDuplicated(key)) {
    stop("Standardized US observations contain duplicate analysis keys.")
  }

  standardized
}

us_model_input <- function(data, analysis_end) {
  data |>
    dplyr::filter(date <= as.Date(analysis_end)) |>
    dplyr::mutate(
      log_days = log(as.numeric(lubridate::days_in_month(date)))
    )
}

write_rds_artifact <- function(object, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(object, path)
  path
}
