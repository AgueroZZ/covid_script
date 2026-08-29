load_single_rda_object <- function(path, expected_name) {
  require_files(path, "Historical RDA")
  environment <- new.env(parent = emptyenv())
  loaded <- load(path, envir = environment)
  if (!expected_name %in% loaded) {
    stop("Historical object '", expected_name, "' was not found in ", path)
  }
  environment[[expected_name]]
}

canonicalize_historical_us_data <- function(path, stratified_by_sex) {
  historical <- load_single_rda_object(path, "USA_monthly")
  historical <- historical[
    !is.na(historical$date) & nzchar(historical$state),
  ]
  historical$age_group <- if (stratified_by_sex) {
    dplyr::recode(historical$age, `Over 85` = "GE85")
  } else {
    dplyr::recode(historical$age, `Over 80` = "GE80")
  }
  allowed_ages <- if (stratified_by_sex) {
    c("0-44", "45-64", "65-84", "GE85")
  } else {
    c("20-39", "40-59", "60-79", "GE80")
  }
  historical <- historical[historical$age_group %in% allowed_ages, ]
  historical$sex_canonical <- if (stratified_by_sex) {
    dplyr::recode(historical$sex, F = "female", M = "male")
  } else {
    "total"
  }

  historical |>
    dplyr::transmute(
      date,
      geography = state,
      age_group,
      sex = sex_canonical,
      observed_deaths = Deaths
    ) |>
    dplyr::arrange(date, geography, age_group, sex)
}

canonical_us_comparison_columns <- function(standardized) {
  standardized |>
    dplyr::select(date, geography, age_group, sex, observed_deaths) |>
    dplyr::arrange(date, geography, age_group, sex)
}

us_data_values_match <- function(current, historical) {
  isTRUE(all.equal(
    as.data.frame(current),
    as.data.frame(historical),
    check.attributes = FALSE,
    tolerance = 0
  ))
}

inventory_us_historical_archive <- function(archive_root) {
  sex_root <- file.path(archive_root, "sex-stratified", "USA")
  non_sex_root <- file.path(archive_root, "non-stratified", "USA")
  require_files(
    c(
      file.path(sex_root, "USA_monthly.rda"),
      file.path(sex_root, "USA_monthly_result.rda"),
      file.path(non_sex_root, "USA_monthly.rda"),
      file.path(non_sex_root, "USA_monthly_result.rda")
    ),
    "Historical US archive"
  )

  sex_fit_names <- basename(list.files(
    file.path(sex_root, "fitted_model"),
    pattern = "[.]rda$",
    full.names = TRUE
  ))
  non_sex_fit_names <- basename(list.files(
    file.path(non_sex_root, "fitted_model"),
    pattern = "[.]rda$",
    full.names = TRUE
  ))
  sex_all_age <- grepl("_age_all_sex_[FM][.]rda$", sex_fit_names)
  sex_grouped_age <- grepl(
    "_age_(0-44|45-64|65-84|Over 85)_sex_[FM][.]rda$",
    sex_fit_names
  )
  sex_result <- load_single_rda_object(
    file.path(sex_root, "USA_monthly_result.rda"),
    "model_result_all"
  )
  non_sex_result <- load_single_rda_object(
    file.path(non_sex_root, "USA_monthly_result.rda"),
    "model_result_all"
  )

  tibble::tibble(
    check = c(
      "sex_grouped_age_fits",
      "sex_all_age_fits",
      "non_sex_fits",
      "historical_sex_summary_rows",
      "historical_non_sex_summary_rows"
    ),
    observed = c(
      sum(sex_grouped_age),
      sum(sex_all_age),
      length(non_sex_fit_names),
      nrow(sex_result),
      nrow(non_sex_result)
    ),
    expected = c(407L, 102L, 204L, 1625L, 816L)
  ) |>
    dplyr::mutate(matches_expected = observed == expected)
}
