read_analysis_config <- function(path = "config/analysis.yml") {
  config <- yaml::read_yaml(path)
  validate_analysis_config(config)
  config
}

validate_analysis_config <- function(config) {
  required_sections <- c(
    "project",
    "training",
    "waves",
    "vaccination",
    "model",
    "regions"
  )
  missing_sections <- setdiff(required_sections, names(config))
  if (length(missing_sections) > 0L) {
    stop(
      "Missing configuration sections: ",
      paste(missing_sections, collapse = ", ")
    )
  }

  if (!identical(config$model$trend, "IWP2")) {
    stop("The canonical trend specification must be IWP2.")
  }

  seasonal_periods <- as.numeric(
    unlist(config$model$seasonal_period_months, use.names = FALSE)
  )
  if (!isTRUE(all.equal(seasonal_periods, c(12, 6, 4, 3)))) {
    stop("Seasonal periods must be 12, 6, 4, and 3 months.")
  }

  if (isTRUE(config$model$population_offset)) {
    stop("The implemented death-count model does not use a population offset.")
  }

  if (!identical(config$vaccination$metric, "people_vaccinated_per_hundred")) {
    stop("Vaccination coverage must use people_vaccinated_per_hundred.")
  }

  us_thresholds <- c(
    as.numeric(config$vaccination$classification_rules$us$low_below),
    as.numeric(config$vaccination$classification_rules$us$high_above)
  )
  if (!isTRUE(all.equal(us_thresholds, c(42, 62)))) {
    stop("US vaccination groups must use fixed thresholds below 42 and above 62.")
  }

  europe_thresholds <- c(
    as.numeric(config$vaccination$classification_rules$europe$low_below),
    as.numeric(config$vaccination$classification_rules$europe$high_above)
  )
  if (!isTRUE(all.equal(europe_thresholds, c(41, 53)))) {
    stop("European vaccination groups must use fixed thresholds below 41 and above 53.")
  }

  invisible(config)
}
