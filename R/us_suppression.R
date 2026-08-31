us_reporting_cohort_columns <- function() {
  c(
    "geography",
    "vaccination_group",
    "figure_03",
    "figure_04",
    "figure_05",
    "table_01"
  )
}

read_us_reporting_cohort <- function(path) {
  cohort <- readr::read_csv(path, show_col_types = FALSE)
  missing <- setdiff(us_reporting_cohort_columns(), names(cohort))
  if (length(missing) > 0L) {
    stop("US reporting cohort is missing columns: ", paste(missing, collapse = ", "))
  }
  if (nrow(cohort) != 51L || anyDuplicated(cohort$geography)) {
    stop("US reporting cohort must contain 51 unique jurisdictions.")
  }
  boolean_columns <- c("figure_03", "figure_04", "figure_05", "table_01")
  for (column in boolean_columns) {
    values <- tolower(as.character(cohort[[column]]))
    if (!all(values %in% c("true", "false"))) {
      stop("US reporting cohort flag is not boolean: ", column, ".")
    }
    cohort[[column]] <- values == "true"
  }
  if (!all(cohort$vaccination_group %in% c("high", "low", "neither"))) {
    stop("US reporting cohort contains an invalid vaccination group.")
  }
  cohort
}

read_us_completeness_inventory <- function(path) {
  inventory <- readr::read_csv(path, show_col_types = FALSE) |>
    dplyr::filter(region %in% c("us_non_sex", "us_sex")) |>
    dplyr::mutate(
      missing_or_suppressed = as.integer(missing_or_suppressed),
      complete = missing_or_suppressed == 0L
    )
  if (nrow(inventory) != 612L || anyDuplicated(inventory$analysis_id)) {
    stop("US completeness inventory must contain 612 unique strata.")
  }
  if (any(inventory$missing_or_suppressed < 0L)) {
    stop("US completeness inventory contains negative missing counts.")
  }
  inventory
}

us_dependency_rows <- function(
  output_id,
  geographies,
  region,
  age_groups,
  sexes
) {
  grid <- expand.grid(
    jurisdiction = geographies,
    age_group = age_groups,
    sex = sexes,
    stringsAsFactors = FALSE
  )
  dplyr::mutate(
    tibble::as_tibble(grid),
    output_id = output_id,
    region = region,
    .before = 1
  )
}

build_us_adopted_dependency_registry <- function(inventory, cohort) {
  dependencies <- dplyr::bind_rows(
    us_dependency_rows(
      "figure_03",
      cohort$geography[cohort$figure_03],
      "us_non_sex",
      c("40-59", "60-79"),
      "total"
    ),
    us_dependency_rows(
      "figure_04",
      cohort$geography[cohort$figure_04],
      "us_non_sex",
      c("40-59", "60-79"),
      "total"
    ),
    us_dependency_rows(
      "figure_05",
      cohort$geography[cohort$figure_05],
      "us_sex",
      c("0-44", "45-64", "65-84"),
      c("female", "male")
    ),
    us_dependency_rows(
      "table_01",
      cohort$geography[cohort$table_01],
      "us_sex",
      "65-84",
      c("female", "male")
    )
  )
  if (anyDuplicated(dependencies[c(
    "output_id", "region", "jurisdiction", "age_group", "sex"
  )])) {
    stop("US adopted-output dependencies must be unique.")
  }
  inventory_columns <- c(
    "analysis_id",
    "region",
    "jurisdiction",
    "age_group",
    "sex",
    "data_start",
    "data_end",
    "n_observations",
    "missing_or_suppressed",
    "complete"
  )
  output <- dependencies |>
    dplyr::left_join(
      inventory[, inventory_columns],
      by = c("region", "jurisdiction", "age_group", "sex")
    ) |>
    dplyr::left_join(
      cohort |>
        dplyr::select(geography, vaccination_group),
      by = c("jurisdiction" = "geography")
    ) |>
    dplyr::arrange(output_id, jurisdiction, age_group, sex)
  if (any(is.na(output$analysis_id))) {
    stop("One or more adopted-output dependencies lack a cohort inventory row.")
  }
  output
}

summarize_us_adopted_dependencies <- function(dependencies) {
  dependencies |>
    dplyr::group_by(output_id) |>
    dplyr::summarise(
      dependency_rows = dplyr::n(),
      geographies = dplyr::n_distinct(jurisdiction),
      incomplete_dependencies = sum(!complete),
      incomplete_geographies = dplyr::n_distinct(jurisdiction[!complete]),
      maximum_missing_or_suppressed = max(missing_or_suppressed),
      .groups = "drop"
    )
}

load_us_suppression_rda <- function(path, object_name) {
  if (!file.exists(path)) {
    stop("Required US suppression input does not exist: ", path)
  }
  environment <- new.env(parent = emptyenv())
  loaded <- load(path, envir = environment)
  if (!object_name %in% loaded) {
    stop("Object '", object_name, "' was not found in ", path, ".")
  }
  environment[[object_name]]
}

read_us_sex_observed_bundle <- function(bundle_root) {
  data <- load_us_suppression_rda(
    file.path(
      bundle_root,
      "source_artifacts",
      "north_america",
      "us_sex",
      "supporting",
      "USA_monthly.rda"
    ),
    "USA_monthly"
  )
  data |>
    dplyr::transmute(
      date = as.Date(date),
      geography = state,
      age_group = age,
      sex = dplyr::recode(as.character(sex), F = "female", M = "male"),
      observed_deaths = as.numeric(Deaths)
    )
}

load_us_sex_bundle_prediction <- function(
  bundle_root,
  observed,
  geography,
  age_group,
  sex
) {
  sex_code <- dplyr::recode(sex, female = "F", male = "M")
  path <- file.path(
    bundle_root,
    "source_artifacts",
    "north_america",
    "us_sex",
    "fitted_predictions",
    paste0(geography, "_age_", age_group, "_sex_", sex_code, ".rda")
  )
  model_prediction <- load_us_suppression_rda(path, "model_pred")
  dates <- as.Date(model_prediction$summary$time)
  selected <- observed[
    observed$geography == geography &
      observed$age_group == age_group &
      observed$sex == sex,
  ]
  observed_index <- match(dates, selected$date)
  if (any(is.na(observed_index))) {
    stop("Observed deaths are incomplete for staged prediction: ", basename(path), ".")
  }
  list(
    analysis_id = us_analysis_id("us_sex", geography, age_group, sex),
    analysis_path = "us_sex",
    geography = geography,
    age_group = age_group,
    sex = sex,
    dates = dates,
    observed_deaths = selected$observed_deaths[observed_index],
    samples = model_prediction$samples
  )
}

load_us_figure05_bundle_predictions <- function(bundle_root, cohort) {
  observed <- read_us_sex_observed_bundle(bundle_root)
  geographies <- cohort$geography[cohort$figure_05]
  output <- list()
  for (geography in geographies) {
    for (age_group in c("0-44", "45-64", "65-84")) {
      for (sex in c("female", "male")) {
        key <- paste(geography, age_group, sex, sep = "::")
        output[[key]] <- load_us_sex_bundle_prediction(
          bundle_root,
          observed,
          geography,
          age_group,
          sex
        )
      }
    }
  }
  output
}

get_us_suppression_prediction <- function(predictions, geography, age, sex) {
  prediction <- predictions[[paste(geography, age, sex, sep = "::")]]
  if (is.null(prediction)) {
    stop("Missing staged prediction: ", geography, ", ", age, ", ", sex, ".")
  }
  prediction
}

us_figure05_component_ages <- function() {
  list(
    `0-84` = c("0-44", "45-64", "65-84"),
    `0-44` = "0-44",
    `45-64` = "45-64",
    `65-84` = "65-84"
  )
}

us_suppression_pscore_samples <- function(prediction) {
  observed <- matrix(
    prediction$observed_deaths,
    nrow = nrow(prediction$samples),
    ncol = ncol(prediction$samples)
  )
  (observed - prediction$samples) / prediction$samples
}

compute_us_suppression_sex_contrast <- function(female, male) {
  common_dates <- intersect(female$dates, male$dates)
  common_dates <- sort(as.Date(common_dates, origin = "1970-01-01"))
  female_draws <- us_suppression_pscore_samples(female)[
    match(common_dates, female$dates),
    ,
    drop = FALSE
  ]
  male_draws <- us_suppression_pscore_samples(male)[
    match(common_dates, male$dates),
    ,
    drop = FALSE
  ]
  number_of_draws <- min(ncol(female_draws), ncol(male_draws))
  list(
    geography = female$geography,
    age_group = female$age_group,
    dates = common_dates,
    samples = female_draws[, seq_len(number_of_draws), drop = FALSE] -
      male_draws[, seq_len(number_of_draws), drop = FALSE]
  )
}

us_complete_figure05_geographies <- function(
  dependencies,
  component_ages = c("0-44", "45-64", "65-84")
) {
  selected <- dependencies |>
    dplyr::filter(output_id == "figure_05", age_group %in% component_ages) |>
    dplyr::group_by(jurisdiction) |>
    dplyr::summarise(complete = all(complete), .groups = "drop")
  selected$jurisdiction[selected$complete]
}

us_figure05_variant_membership <- function(
  cohort,
  dependencies,
  variant = c(
    "historical",
    "panel_specific_complete_case",
    "strict_complete_case",
    "exclude_idaho",
    "exclude_idaho_new_mexico"
  ),
  component_ages
) {
  variant <- match.arg(variant)
  selected <- cohort[cohort$figure_05, c("geography", "vaccination_group")]
  if (variant == "historical") {
    return(selected)
  }
  explicit_exclusions <- switch(
    variant,
    exclude_idaho = "Idaho",
    exclude_idaho_new_mexico = c("Idaho", "New Mexico"),
    NULL
  )
  if (!is.null(explicit_exclusions)) {
    return(selected[!selected$geography %in% explicit_exclusions, , drop = FALSE])
  }
  required_ages <- if (variant == "strict_complete_case") {
    c("0-44", "45-64", "65-84")
  } else {
    component_ages
  }
  complete <- us_complete_figure05_geographies(dependencies, required_ages)
  selected[selected$geography %in% complete, , drop = FALSE]
}

build_us_figure05_variant <- function(
  predictions,
  cohort,
  dependencies,
  variant = c(
    "historical",
    "panel_specific_complete_case",
    "strict_complete_case",
    "exclude_idaho",
    "exclude_idaho_new_mexico"
  )
) {
  variant <- match.arg(variant)
  rows <- list()
  for (display_age in names(us_figure05_component_ages())) {
    component_ages <- us_figure05_component_ages()[[display_age]]
    membership <- us_figure05_variant_membership(
      cohort,
      dependencies,
      variant,
      component_ages
    )
    for (vaccination_group in c("high", "low")) {
      geographies <- membership$geography[
        membership$vaccination_group == vaccination_group
      ]
      contrasts <- lapply(geographies, function(geography) {
        female_parts <- lapply(component_ages, function(age) {
          get_us_suppression_prediction(predictions, geography, age, "female")
        })
        male_parts <- lapply(component_ages, function(age) {
          get_us_suppression_prediction(predictions, geography, age, "male")
        })
        female <- if (length(component_ages) == 1L) {
          female_parts[[1]]
        } else {
          combine_us_age_predictions(
            female_parts,
            age_groups = component_ages,
            combined_label = display_age
          )
        }
        male <- if (length(component_ages) == 1L) {
          male_parts[[1]]
        } else {
          combine_us_age_predictions(
            male_parts,
            age_groups = component_ages,
            combined_label = display_age
          )
        }
        compute_us_suppression_sex_contrast(female, male)
      })
      draw_objects <- lapply(contrasts, function(contrast) {
        list(
          geography = contrast$geography,
          age_group = contrast$age_group,
          dates = contrast$dates,
          samples = contrast$samples
        )
      })
      rows[[paste(display_age, vaccination_group, sep = "::")]] <-
        aggregate_inverse_variance_trajectory(
          draw_objects,
          group_label = vaccination_group,
          estimand = display_age
        ) |>
        dplyr::transmute(
          region = "United States",
          age_group = display_age,
          vaccination_group = group,
          date,
          mean,
          variance,
          lower,
          upper,
          jurisdictions,
          interval_method
        )
    }
  }
  dplyr::bind_rows(rows) |>
    dplyr::arrange(age_group, vaccination_group, date)
}

compare_us_figure05_baseline <- function(reconstructed, installed, tolerance = 1e-12) {
  keys <- c("region", "age_group", "vaccination_group", "date")
  measures <- c("mean", "variance", "lower", "upper")
  installed <- installed |>
    dplyr::filter(region == "United States") |>
    dplyr::mutate(date = as.Date(date))
  comparison <- dplyr::full_join(
    reconstructed,
    installed,
    by = keys,
    suffix = c("_reconstructed", "_installed")
  )
  if (nrow(comparison) != nrow(reconstructed) || nrow(comparison) != nrow(installed)) {
    stop("Reconstructed Figure 5 baseline has different row keys from the installed input.")
  }
  differences <- unlist(lapply(measures, function(column) {
    abs(comparison[[paste0(column, "_reconstructed")]] -
      comparison[[paste0(column, "_installed")]])
  }))
  if (any(comparison$jurisdictions_reconstructed != comparison$jurisdictions_installed) ||
      any(comparison$interval_method_reconstructed != comparison$interval_method_installed) ||
      max(differences, na.rm = TRUE) > tolerance) {
    stop("Reconstructed Figure 5 baseline does not match the installed input.")
  }
  tibble::tibble(
    compared_rows = nrow(comparison),
    max_abs_numeric_difference = max(differences, na.rm = TRUE),
    tolerance = tolerance,
    pass = TRUE
  )
}

compare_us_figure05_sensitivity <- function(historical, candidate, variant) {
  keys <- c("region", "age_group", "vaccination_group", "date")
  comparison <- dplyr::inner_join(
    historical,
    candidate,
    by = keys,
    suffix = c("_historical", "_candidate")
  ) |>
    dplyr::mutate(
      variant = variant,
      mean_difference = mean_candidate - mean_historical,
      abs_mean_difference = abs(mean_difference),
      sign_reversal = sign(mean_candidate) != sign(mean_historical) &
        abs(mean_candidate) >= 0.02 & abs(mean_historical) >= 0.02
    )
  comparison
}

maximum_true_run <- function(values) {
  values[is.na(values)] <- FALSE
  runs <- rle(values)
  if (!any(runs$values)) 0L else max(runs$lengths[runs$values])
}

filter_us_figure05_decision_period <- function(
  data,
  analysis_start = NULL,
  analysis_end = NULL
) {
  if (!is.null(analysis_start)) {
    data <- data[as.Date(data$date) >= as.Date(analysis_start), , drop = FALSE]
  }
  if (!is.null(analysis_end)) {
    data <- data[as.Date(data$date) <= as.Date(analysis_end), , drop = FALSE]
  }
  if (nrow(data) == 0L) stop("Figure 5 decision period contains no rows.")
  data
}

summarize_us_figure05_sensitivity <- function(
  comparison,
  analysis_start = NULL,
  analysis_end = NULL
) {
  comparison <- filter_us_figure05_decision_period(
    comparison,
    analysis_start,
    analysis_end
  )
  comparison |>
    dplyr::group_by(variant, age_group, vaccination_group) |>
    dplyr::arrange(date, .by_group = TRUE) |>
    dplyr::summarise(
      historical_rows = dplyr::n_distinct(date),
      maximum_absolute_mean_change = max(abs_mean_difference, na.rm = TRUE),
      median_absolute_mean_change = stats::median(abs_mean_difference, na.rm = TRUE),
      sign_reversal_rows = sum(sign_reversal, na.rm = TRUE),
      maximum_consecutive_sign_reversal = maximum_true_run(sign_reversal),
      historical_jurisdictions = max(jurisdictions_historical),
      candidate_jurisdictions = max(jurisdictions_candidate),
      .groups = "drop"
    )
}

us_figure05_group_difference <- function(data, suffix) {
  high <- data |>
    dplyr::filter(vaccination_group == "high") |>
    dplyr::select(region, age_group, date, high = mean)
  low <- data |>
    dplyr::filter(vaccination_group == "low") |>
    dplyr::select(region, age_group, date, low = mean)
  output <- dplyr::inner_join(high, low, by = c("region", "age_group", "date")) |>
    dplyr::mutate(group_difference = low - high) |>
    dplyr::select(region, age_group, date, group_difference)
  names(output)[names(output) == "group_difference"] <- paste0(
    "group_difference_",
    suffix
  )
  output
}

compare_us_figure05_ordering <- function(
  historical,
  candidate,
  variant,
  analysis_start = NULL,
  analysis_end = NULL
) {
  comparison <- dplyr::inner_join(
    us_figure05_group_difference(historical, "historical"),
    us_figure05_group_difference(candidate, "candidate"),
    by = c("region", "age_group", "date")
  ) |>
    dplyr::mutate(
      variant = variant,
      ordering_reversal =
        sign(group_difference_historical) != sign(group_difference_candidate) &
        abs(group_difference_historical) >= 0.02 &
        abs(group_difference_candidate) >= 0.02
    )
  comparison <- filter_us_figure05_decision_period(
    comparison,
    analysis_start,
    analysis_end
  )
  comparison |>
    dplyr::group_by(variant, age_group) |>
    dplyr::arrange(date, .by_group = TRUE) |>
    dplyr::summarise(
      compared_rows = dplyr::n(),
      ordering_reversal_rows = sum(ordering_reversal, na.rm = TRUE),
      maximum_consecutive_ordering_reversal = maximum_true_run(ordering_reversal),
      .groups = "drop"
    )
}

decide_us_figure05_sensitivity <- function(summary, ordering) {
  variants <- unique(summary$variant)
  dplyr::bind_rows(lapply(variants, function(variant) {
    selected <- summary[summary$variant == variant, ]
    selected_ordering <- ordering[ordering$variant == variant, ]
    maximum_change <- max(selected$maximum_absolute_mean_change, na.rm = TRUE)
    maximum_sign_run <- max(
      selected$maximum_consecutive_sign_reversal,
      na.rm = TRUE
    )
    maximum_ordering_run <- max(
      selected_ordering$maximum_consecutive_ordering_reversal,
      na.rm = TRUE
    )
    tibble::tibble(
      variant = variant,
      maximum_absolute_mean_change = maximum_change,
      maximum_consecutive_sign_reversal = maximum_sign_run,
      maximum_consecutive_ordering_reversal = maximum_ordering_run,
      change_threshold = 0.05,
      consecutive_reversal_threshold = 3L,
      pass = maximum_change < 0.05 &&
        maximum_sign_run < 3L &&
        maximum_ordering_run < 3L
    )
  }))
}
