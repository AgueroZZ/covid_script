posterior_quantiles <- function(samples, probabilities = c(0.025, 0.5, 0.975)) {
  stats::quantile(
    samples,
    probs = probabilities,
    na.rm = TRUE,
    names = FALSE
  )
}

prediction_pscore_samples <- function(prediction) {
  expected <- prediction$samples
  observed <- matrix(
    prediction$observed_deaths,
    nrow = nrow(expected),
    ncol = ncol(expected)
  )
  pscore <- (observed - expected) / expected
  pscore[expected == 0] <- NA_real_
  pscore
}

summarize_pointwise_pscore <- function(prediction, config) {
  samples <- prediction_pscore_samples(prediction)
  interval <- as.numeric(unlist(config$model$posterior_interval))
  quantiles <- t(apply(
    samples,
    1,
    stats::quantile,
    probs = c(interval[[1]], 0.5, interval[[2]]),
    na.rm = TRUE,
    names = FALSE
  ))

  tibble::tibble(
    analysis_id = prediction$analysis_id,
    analysis_path = prediction$analysis_path,
    geography = prediction$geography,
    age_group = prediction$age_group,
    sex = prediction$sex,
    date = prediction$dates,
    p_mean = rowMeans(samples, na.rm = TRUE),
    p_variance = apply(samples, 1, stats::var, na.rm = TRUE),
    p_lower = quantiles[, 1],
    p_median = quantiles[, 2],
    p_upper = quantiles[, 3],
    posterior_draws = ncol(samples),
    status = "success"
  )
}

summarize_wave_ratio <- function(prediction, config) {
  definitions <- wave_table(config)
  interval <- as.numeric(unlist(config$model$posterior_interval))

  dplyr::bind_rows(lapply(seq_len(nrow(definitions)), function(index) {
    in_wave <- prediction$dates >= definitions$start[[index]] &
      prediction$dates < definitions$end_exclusive[[index]]
    if (!any(in_wave)) {
      return(tibble::tibble(
        analysis_id = prediction$analysis_id,
        analysis_path = prediction$analysis_path,
        geography = prediction$geography,
        age_group = prediction$age_group,
        sex = prediction$sex,
        wave = definitions$wave[[index]],
        start = definitions$start[[index]],
        end_exclusive = definitions$end_exclusive[[index]],
        observed_months = 0L,
        delta_lower = NA_real_,
        delta_median = NA_real_,
        delta_upper = NA_real_,
        p_lower = NA_real_,
        p_median = NA_real_,
        p_upper = NA_real_,
        status = "no_observed_months"
      ))
    }

    expected_total <- colSums(
      prediction$samples[in_wave, , drop = FALSE]
    )
    observed_total <- sum(prediction$observed_deaths[in_wave])
    excess <- observed_total - expected_total
    pscore <- excess / expected_total
    pscore[expected_total == 0] <- NA_real_
    delta_quantiles <- posterior_quantiles(
      excess,
      c(interval[[1]], 0.5, interval[[2]])
    )
    pscore_quantiles <- posterior_quantiles(
      pscore,
      c(interval[[1]], 0.5, interval[[2]])
    )

    tibble::tibble(
      analysis_id = prediction$analysis_id,
      analysis_path = prediction$analysis_path,
      geography = prediction$geography,
      age_group = prediction$age_group,
      sex = prediction$sex,
      wave = definitions$wave[[index]],
      start = definitions$start[[index]],
      end_exclusive = definitions$end_exclusive[[index]],
      observed_months = sum(in_wave),
      delta_lower = delta_quantiles[[1]],
      delta_median = delta_quantiles[[2]],
      delta_upper = delta_quantiles[[3]],
      p_lower = pscore_quantiles[[1]],
      p_median = pscore_quantiles[[2]],
      p_upper = pscore_quantiles[[3]],
      status = "success"
    )
  }))
}

failed_us_result_rows <- function(run, config, result_type) {
  if (result_type == "wave") {
    definitions <- wave_table(config)
    return(tibble::tibble(
      analysis_id = run$analysis_id,
      analysis_path = NA_character_,
      geography = NA_character_,
      age_group = NA_character_,
      sex = NA_character_,
      wave = definitions$wave,
      start = definitions$start,
      end_exclusive = definitions$end_exclusive,
      observed_months = NA_integer_,
      delta_lower = NA_real_,
      delta_median = NA_real_,
      delta_upper = NA_real_,
      p_lower = NA_real_,
      p_median = NA_real_,
      p_upper = NA_real_,
      status = "model_failed"
    ))
  }
  tibble::tibble(
    analysis_id = run$analysis_id,
    status = "model_failed",
    error_message = run$error_message
  )
}

us_wave_summary_from_run <- function(run, config) {
  if (!identical(run$status, "success")) {
    return(failed_us_result_rows(run, config, "wave"))
  }
  summarize_wave_ratio(run$prediction, config)
}

us_pointwise_summary_from_run <- function(run, config) {
  if (!identical(run$status, "success")) {
    return(failed_us_result_rows(run, config, "pointwise"))
  }
  summarize_pointwise_pscore(run$prediction, config)
}

combine_us_age_predictions <- function(
  predictions,
  age_groups = c("0-44", "45-64", "65-84"),
  combined_label = "0-84"
) {
  selected <- predictions[vapply(
    predictions,
    function(prediction) prediction$age_group %in% age_groups,
    logical(1)
  )]
  if (length(selected) != length(age_groups) ||
      !setequal(vapply(selected, `[[`, character(1), "age_group"), age_groups)) {
    stop("All required age-specific predictions are needed for a 0-84 result.")
  }
  common_dates <- Reduce(intersect, lapply(selected, `[[`, "dates"))
  common_dates <- sort(as.Date(common_dates, origin = "1970-01-01"))
  if (length(common_dates) == 0L) {
    stop("Age-specific predictions have no common dates.")
  }
  draw_counts <- vapply(selected, function(x) ncol(x$samples), integer(1))
  if (length(unique(draw_counts)) != 1L) {
    stop("Age-specific predictions must have the same posterior draw count.")
  }

  sample_matrices <- lapply(selected, function(prediction) {
    prediction$samples[match(common_dates, prediction$dates), , drop = FALSE]
  })
  observed_vectors <- lapply(selected, function(prediction) {
    prediction$observed_deaths[match(common_dates, prediction$dates)]
  })

  combined <- selected[[1]]
  combined$analysis_id <- paste0(
    "combined__",
    combined$geography,
    "__",
    combined$sex,
    "__0-84"
  )
  combined$age_group <- combined_label
  combined$dates <- common_dates
  combined$samples <- Reduce(`+`, sample_matrices)
  combined$observed_deaths <- Reduce(`+`, observed_vectors)
  combined$summary <- NULL
  combined
}

compute_us_sex_contrast <- function(female_prediction, male_prediction) {
  if (!identical(female_prediction$geography, male_prediction$geography) ||
      !identical(female_prediction$age_group, male_prediction$age_group)) {
    stop("Sex contrasts require matching geography and age group.")
  }
  common_dates <- intersect(female_prediction$dates, male_prediction$dates)
  common_dates <- sort(as.Date(common_dates, origin = "1970-01-01"))
  female_index <- match(common_dates, female_prediction$dates)
  male_index <- match(common_dates, male_prediction$dates)
  female_samples <- prediction_pscore_samples(female_prediction)[
    female_index,
    ,
    drop = FALSE
  ]
  male_samples <- prediction_pscore_samples(male_prediction)[
    male_index,
    ,
    drop = FALSE
  ]
  n_draws <- min(ncol(female_samples), ncol(male_samples))
  contrast_samples <- female_samples[, seq_len(n_draws), drop = FALSE] -
    male_samples[, seq_len(n_draws), drop = FALSE]
  quantiles <- t(apply(
    contrast_samples,
    1,
    stats::quantile,
    probs = c(0.025, 0.5, 0.975),
    na.rm = TRUE,
    names = FALSE
  ))

  list(
    geography = female_prediction$geography,
    age_group = female_prediction$age_group,
    dates = common_dates,
    samples = contrast_samples,
    summary = tibble::tibble(
      geography = female_prediction$geography,
      age_group = female_prediction$age_group,
      contrast = "female_minus_male",
      date = common_dates,
      mean = rowMeans(contrast_samples, na.rm = TRUE),
      variance = apply(contrast_samples, 1, stats::var, na.rm = TRUE),
      lower = quantiles[, 1],
      median = quantiles[, 2],
      upper = quantiles[, 3]
    )
  )
}

build_us_sex_contrasts <- function(model_runs) {
  predictions <- lapply(
    model_runs[vapply(model_runs, function(run) run$status == "success", logical(1))],
    `[[`,
    "prediction"
  )
  geographies <- sort(unique(vapply(predictions, `[[`, character(1), "geography")))
  age_groups <- c("0-44", "45-64", "65-84", "GE85")
  contrasts <- list()

  for (geography in geographies) {
    female_combined <- NULL
    male_combined <- NULL
    for (age_group in age_groups) {
      selected <- predictions[vapply(predictions, function(prediction) {
        prediction$geography == geography && prediction$age_group == age_group
      }, logical(1))]
      sexes <- vapply(selected, `[[`, character(1), "sex")
      if (all(c("female", "male") %in% sexes)) {
        contrasts[[paste(geography, age_group, sep = "::")]] <-
          compute_us_sex_contrast(
            selected[[which(sexes == "female")]],
            selected[[which(sexes == "male")]]
          )
      }
    }

    for (sex in c("female", "male")) {
      selected <- predictions[vapply(predictions, function(prediction) {
        prediction$geography == geography && prediction$sex == sex
      }, logical(1))]
      if (all(c("0-44", "45-64", "65-84") %in%
              vapply(selected, `[[`, character(1), "age_group"))) {
        combined <- combine_us_age_predictions(selected)
        if (sex == "female") female_combined <- combined
        if (sex == "male") male_combined <- combined
      }
    }
    if (!is.null(female_combined) && !is.null(male_combined)) {
      contrasts[[paste(geography, "0-84", sep = "::")]] <-
        compute_us_sex_contrast(female_combined, male_combined)
    }
  }
  contrasts
}

bind_us_sex_contrast_summaries <- function(contrasts) {
  dplyr::bind_rows(lapply(contrasts, `[[`, "summary"))
}

as_pscore_draw_object <- function(prediction) {
  list(
    geography = prediction$geography,
    age_group = prediction$age_group,
    dates = prediction$dates,
    samples = prediction_pscore_samples(prediction)
  )
}

aggregate_inverse_variance_trajectory <- function(
  draw_objects,
  group_label,
  estimand
) {
  if (length(draw_objects) == 0L) {
    stop("At least one jurisdiction draw object is required.")
  }
  common_dates <- Reduce(intersect, lapply(draw_objects, `[[`, "dates"))
  common_dates <- sort(as.Date(common_dates, origin = "1970-01-01"))
  if (length(common_dates) == 0L) {
    stop("Jurisdiction draw objects have no common dates.")
  }

  means <- sapply(draw_objects, function(object) {
    samples <- object$samples[match(common_dates, object$dates), , drop = FALSE]
    rowMeans(samples, na.rm = TRUE)
  })
  variances <- sapply(draw_objects, function(object) {
    samples <- object$samples[match(common_dates, object$dates), , drop = FALSE]
    apply(samples, 1, stats::var, na.rm = TRUE)
  })
  means <- as.matrix(means)
  variances <- as.matrix(variances)
  inverse_variance <- ifelse(
    is.finite(variances) & variances > 0,
    1 / variances,
    NA_real_
  )
  denominators <- rowSums(inverse_variance, na.rm = TRUE)
  if (any(denominators <= 0)) {
    stop("Inverse-variance weights are undefined for one or more dates.")
  }
  weights <- inverse_variance / denominators
  weighted_mean <- rowSums(weights * means, na.rm = TRUE)
  weighted_variance <- rowSums(weights^2 * variances, na.rm = TRUE)

  tibble::tibble(
    group = group_label,
    estimand = estimand,
    date = common_dates,
    mean = weighted_mean,
    variance = weighted_variance,
    lower = weighted_mean - 1.96 * sqrt(weighted_variance),
    upper = weighted_mean + 1.96 * sqrt(weighted_variance),
    jurisdictions = length(draw_objects),
    interval_method = "fixed_effect_normal_approximation"
  )
}
