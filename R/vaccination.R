load_named_rda_object <- function(path, object_name) {
  require_files(path, "RDA input")
  environment <- new.env(parent = emptyenv())
  loaded <- load(path, envir = environment)
  if (!object_name %in% loaded) {
    stop("Object '", object_name, "' was not found in ", path, ".")
  }
  environment[[object_name]]
}

classify_vaccination_coverage <- function(rate, low_below, high_above) {
  dplyr::case_when(
    is.na(rate) ~ NA_character_,
    rate < low_below ~ "low",
    rate > high_above ~ "high",
    TRUE ~ "neither"
  )
}

prepare_europe_vaccination <- function(path, config) {
  vaccination <- load_named_rda_object(path, "vac_data_eu")
  required <- c("date", "iso2", config$vaccination$metric)
  missing <- setdiff(required, names(vaccination))
  if (length(missing) > 0L) {
    stop(
      "European vaccination data are missing: ",
      paste(missing, collapse = ", ")
    )
  }

  rules <- config$vaccination$classification_rules$europe
  prepared <- vaccination |>
    dplyr::transmute(
      date = as.Date(date),
      geography = dplyr::if_else(iso2 == "GR", "EL", iso2),
      people_vaccinated_per_hundred = .data[[config$vaccination$metric]],
      vaccination_group = classify_vaccination_coverage(
        people_vaccinated_per_hundred,
        low_below = rules$low_below,
        high_above = rules$high_above
      )
    ) |>
    dplyr::arrange(geography)

  reference_date <- as.Date(config$vaccination$reference_date)
  if (any(prepared$date > reference_date) || anyDuplicated(prepared$geography)) {
    stop("European vaccination membership violates its frozen date or key rule.")
  }
  prepared
}

prepare_us_vaccination <- function(path, config) {
  vaccination <- load_named_rda_object(
    path,
    "us_state_vaccinations_select"
  )
  required <- c("date", "location", config$vaccination$metric)
  missing <- setdiff(required, names(vaccination))
  if (length(missing) > 0L) {
    stop("US vaccination data are missing: ", paste(missing, collapse = ", "))
  }

  rules <- config$vaccination$classification_rules$us
  prepared <- vaccination |>
    dplyr::transmute(
      date = as.Date(date),
      geography = location,
      people_vaccinated_per_hundred = .data[[config$vaccination$metric]],
      vaccination_group = classify_vaccination_coverage(
        people_vaccinated_per_hundred,
        low_below = rules$low_below,
        high_above = rules$high_above
      )
    ) |>
    dplyr::arrange(geography)

  reference_date <- as.Date(config$vaccination$reference_date)
  if (!all(prepared$date == reference_date)) {
    stop("Every US vaccination record must use the configured reference date.")
  }
  if (anyDuplicated(prepared$geography)) {
    stop("US vaccination data must contain one row per jurisdiction.")
  }

  prepared
}

historical_us_reporting_cohorts <- function(geographies) {
  ordered <- sort(unique(geographies))
  if (length(ordered) != 51L) {
    stop("Historical US reporting subsets require exactly 51 jurisdictions.")
  }
  sex_indices <- c(
    1,
    3:7,
    10:11,
    13:19,
    21:26,
    29,
    31:34,
    36:39,
    41,
    43:45,
    47:50
  )
  non_sex_indices <- c(
    1,
    3:8,
    10:11,
    13:26,
    28:34,
    36:39,
    41,
    43:45,
    47:50
  )

  dplyr::bind_rows(
    tibble::tibble(
      analysis_path = "us_sex",
      geography = ordered[sex_indices]
    ),
    tibble::tibble(
      analysis_path = "us_non_sex",
      geography = ordered[non_sex_indices]
    )
  ) |>
    dplyr::mutate(cohort_role = "historical_positional_subset")
}
