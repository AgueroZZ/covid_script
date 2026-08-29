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

  quantiles <- c(
    as.numeric(config$vaccination$low_quantile),
    as.numeric(config$vaccination$high_quantile)
  )
  if (!isTRUE(all.equal(quantiles, c(0.15, 0.85)))) {
    stop("Vaccination groups must use the 15th and 85th percentiles.")
  }

  invisible(config)
}
