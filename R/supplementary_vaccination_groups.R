vaccination_explorer_age_groups <- function() {
  list(
    europe = c("20-39", "40-59", "60-79", "GE80"),
    us_figure_04 = c("20-39", "40-59", "60-79", "GE80"),
    us_figure_05 = c("0-44", "45-64", "65-84", "GE85")
  )
}

prediction_pscore_draws <- function(
  prediction,
  denominator_epsilon = 0,
  zero_as_missing = TRUE
) {
  required <- c("dates", "observed_deaths", "samples")
  missing <- setdiff(required, names(prediction))
  if (length(missing) > 0L || !is.matrix(prediction$samples)) {
    stop("A prediction is missing the dates, observed deaths, or sample matrix.")
  }
  if (nrow(prediction$samples) != length(prediction$dates) ||
      nrow(prediction$samples) != length(prediction$observed_deaths)) {
    stop("Prediction dates, observations, and sample rows do not align.")
  }
  observed <- matrix(
    prediction$observed_deaths,
    nrow = nrow(prediction$samples),
    ncol = ncol(prediction$samples)
  )
  output <- (observed - prediction$samples) /
    (prediction$samples + denominator_epsilon)
  if (identical(denominator_epsilon, 0) && zero_as_missing) {
    output[prediction$samples == 0] <- NA_real_
  }
  output
}

summarize_vaccination_sex_contrast <- function(
  female,
  male,
  denominator_epsilon = 0,
  zero_as_missing = TRUE,
  remove_missing_draws = TRUE
) {
  common_dates <- intersect(as.Date(female$dates), as.Date(male$dates))
  common_dates <- sort(as.Date(common_dates, origin = "1970-01-01"))
  if (length(common_dates) == 0L) {
    stop("Female and male predictions have no common dates.")
  }
  female_draws <- prediction_pscore_draws(
    female,
    denominator_epsilon,
    zero_as_missing
  )[
    match(common_dates, as.Date(female$dates)),
    ,
    drop = FALSE
  ]
  male_draws <- prediction_pscore_draws(
    male,
    denominator_epsilon,
    zero_as_missing
  )[
    match(common_dates, as.Date(male$dates)),
    ,
    drop = FALSE
  ]
  number_of_draws <- min(ncol(female_draws), ncol(male_draws))
  contrast <- female_draws[, seq_len(number_of_draws), drop = FALSE] -
    male_draws[, seq_len(number_of_draws), drop = FALSE]
  mean_value <- rowMeans(contrast, na.rm = remove_missing_draws)
  mean_value[!is.finite(mean_value)] <- NA_real_
  variance_value <- if (requireNamespace("matrixStats", quietly = TRUE)) {
    matrixStats::rowVars(contrast, na.rm = remove_missing_draws)
  } else {
    apply(contrast, 1L, stats::var, na.rm = remove_missing_draws)
  }
  variance_value[!is.finite(variance_value)] <- NA_real_

  data.frame(
    date = common_dates,
    mean = mean_value,
    variance = variance_value,
    lower = mean_value - 1.96 * sqrt(variance_value),
    upper = mean_value + 1.96 * sqrt(variance_value),
    posterior_draws = number_of_draws,
    stringsAsFactors = FALSE
  )
}

validate_vaccination_geography_summaries <- function(data) {
  required <- c(
    "figure", "region", "geography", "geography_label", "age_group",
    "frequency", "date", "mean", "variance"
  )
  missing <- setdiff(required, names(data))
  if (length(missing) > 0L) {
    stop(
      "Vaccination geography summaries are missing columns: ",
      paste(missing, collapse = ", "),
      "."
    )
  }
  keys <- c("figure", "region", "geography", "age_group", "date")
  if (anyDuplicated(data[keys])) {
    stop("Vaccination geography summaries contain duplicated keys.")
  }
  finite <- is.finite(data$mean) & is.finite(data$variance)
  if (any(data$variance[finite] < 0)) {
    stop("Vaccination geography summaries contain negative variances.")
  }
  invisible(data)
}

aggregate_vaccination_group_summary <- function(data, membership) {
  required_data <- c("geography", "date", "mean", "variance")
  required_membership <- c("geography", "vaccination_group")
  if (!all(required_data %in% names(data)) ||
      !all(required_membership %in% names(membership))) {
    stop("Vaccination aggregation inputs are incomplete.")
  }
  if (anyDuplicated(membership$geography)) {
    stop("Vaccination membership must contain one row per geography.")
  }
  selected_membership <- membership[
    membership$vaccination_group %in% c("high", "low"),
    required_membership,
    drop = FALSE
  ]
  joined <- merge(data, selected_membership, by = "geography", sort = FALSE)
  if (nrow(joined) == 0L) {
    return(data.frame(
      vaccination_group = character(),
      date = as.Date(character()),
      mean = numeric(),
      variance = numeric(),
      lower = numeric(),
      upper = numeric(),
      jurisdictions = integer(),
      contributing_jurisdictions = integer(),
      interval_method = character(),
      stringsAsFactors = FALSE
    ))
  }
  expected_geographies <- table(selected_membership$vaccination_group)
  date_coverage <- stats::aggregate(
    geography ~ vaccination_group + date,
    joined,
    function(values) length(unique(values))
  )
  names(date_coverage)[names(date_coverage) == "geography"] <- "geographies"
  date_coverage$required <- as.integer(
    expected_geographies[date_coverage$vaccination_group]
  )
  complete_dates <- date_coverage[
    date_coverage$geographies == date_coverage$required,
    c("vaccination_group", "date"),
    drop = FALSE
  ]
  joined <- merge(
    joined,
    complete_dates,
    by = c("vaccination_group", "date"),
    sort = FALSE
  )
  groups <- split(
    joined,
    interaction(
      joined[c("vaccination_group", "date")],
      drop = TRUE,
      lex.order = TRUE
    )
  )
  rows <- lapply(groups, function(group) {
    usable <- is.finite(group$mean) & is.finite(group$variance) &
      group$variance > 0
    if (!any(usable)) return(NULL)
    weights <- 1 / group$variance[usable]
    weights <- weights / sum(weights)
    mean_value <- sum(weights * group$mean[usable])
    variance_value <- sum(weights^2 * group$variance[usable])
    data.frame(
      vaccination_group = group$vaccination_group[[1]],
      date = as.Date(group$date[[1]]),
      mean = mean_value,
      variance = variance_value,
      lower = mean_value - 1.96 * sqrt(variance_value),
      upper = mean_value + 1.96 * sqrt(variance_value),
      jurisdictions = as.integer(expected_geographies[
        group$vaccination_group[[1]]
      ]),
      contributing_jurisdictions = sum(usable),
      interval_method = "fixed_effect_normal_approximation",
      stringsAsFactors = FALSE
    )
  })
  output <- do.call(rbind, rows)
  rownames(output) <- NULL
  output
}

compare_vaccination_manuscript_summary <- function(
  reconstructed,
  installed,
  tolerance = 1e-10
) {
  keys <- c("region", "age_group", "vaccination_group", "date")
  measures <- c("mean", "variance", "lower", "upper")
  missing <- setdiff(c(keys, measures, "jurisdictions"), names(reconstructed))
  if (length(missing) > 0L) {
    stop("Reconstructed manuscript summary is incomplete.")
  }
  reconstructed$date <- as.Date(reconstructed$date)
  installed$date <- as.Date(installed$date)
  comparison <- merge(
    reconstructed,
    installed,
    by = keys,
    all = TRUE,
    suffixes = c("_reconstructed", "_installed")
  )
  if (nrow(comparison) != nrow(reconstructed) ||
      nrow(comparison) != nrow(installed)) {
    stop("Reconstructed manuscript summary has different row keys.")
  }
  differences <- unlist(lapply(measures, function(column) {
    abs(
      comparison[[paste0(column, "_reconstructed")]] -
        comparison[[paste0(column, "_installed")]]
    )
  }))
  maximum <- max(differences, na.rm = TRUE)
  jurisdiction_mismatch <- any(
    comparison$jurisdictions_reconstructed !=
      comparison$jurisdictions_installed
  )
  if (jurisdiction_mismatch || !is.finite(maximum) || maximum > tolerance) {
    stop(
      "Reconstructed manuscript summary does not match the installed input ",
      "(maximum numeric difference: ",
      format(maximum, scientific = TRUE),
      "; jurisdiction mismatch: ",
      jurisdiction_mismatch,
      ")."
    )
  }
  data.frame(
    compared_rows = nrow(comparison),
    max_abs_numeric_difference = maximum,
    tolerance = tolerance,
    pass = TRUE,
    stringsAsFactors = FALSE
  )
}

vaccination_explorer_web_shard <- function(data) {
  validate_vaccination_geography_summaries(data)
  identifiers <- unique(data[c("figure", "region", "age_group", "frequency")])
  if (nrow(identifiers) != 1L) {
    stop("A vaccination explorer shard must contain one figure-region-age panel.")
  }
  groups <- split(data, data$geography)
  series <- lapply(groups, function(group) {
    group <- group[order(as.Date(group$date)), , drop = FALSE]
    list(
      geography = group$geography[[1]],
      geography_label = group$geography_label[[1]],
      date = format(as.Date(group$date), "%Y-%m-%d"),
      mean = group$mean,
      variance = group$variance
    )
  })
  list(
    schema_version = "1.0.0",
    figure = identifiers$figure[[1]],
    region = identifiers$region[[1]],
    age_group = identifiers$age_group[[1]],
    frequency = identifiers$frequency[[1]],
    series = unname(series)
  )
}
