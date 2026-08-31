corrected_europe_affected_outputs <- function() {
  c("figure_01", "figure_02", "figure_04", "figure_05", "table_01")
}

read_corrected_europe_contract <- function(
  refit_root,
  cohort_path,
  data_path
) {
  required <- c(
    file.path(refit_root, "verified_complete.flag"),
    file.path(refit_root, "batch_complete.flag"),
    file.path(refit_root, "verification_summary.rds"),
    file.path(refit_root, "manifests", "model_manifest.csv"),
    cohort_path,
    data_path
  )
  missing <- required[!file.exists(required)]
  if (length(missing) > 0L) {
    stop(
      "Corrected Europe reporting inputs are missing: ",
      paste(missing, collapse = ", "),
      "."
    )
  }

  verification <- readRDS(file.path(refit_root, "verification_summary.rds"))
  if (!isTRUE(verification$overall_valid) ||
      !identical(as.integer(verification$valid_models), 388L) ||
      !identical(as.integer(verification$invalid_models), 0L)) {
    stop("The corrected Europe refit has not passed the 388-model verifier.")
  }
  observed_hash <- digest::digest(file = data_path, algo = "sha256")
  if (!identical(unname(verification$data_sha256), unname(observed_hash))) {
    stop("The corrected Europe input data hash does not match verification.")
  }

  manifest <- utils::read.csv(
    file.path(refit_root, "manifests", "model_manifest.csv"),
    stringsAsFactors = FALSE
  )
  cohort <- utils::read.csv(cohort_path, stringsAsFactors = FALSE)
  cohort_flags <- c(
    "corrected_model_available",
    "figure_02",
    "figure_04",
    "figure_05",
    "table_01"
  )
  for (column in cohort_flags) {
    normalized <- tolower(as.character(cohort[[column]]))
    if (!all(normalized %in% c("true", "false"))) {
      stop("Europe reporting cohort flag is not boolean: ", column, ".")
    }
    cohort[[column]] <- normalized == "true"
  }
  if (nrow(manifest) != 388L || anyDuplicated(manifest$model_id)) {
    stop("The corrected Europe manifest must contain 388 unique model IDs.")
  }
  corrected_geographies <- cohort$geography[cohort$corrected_model_available]
  if (!setequal(unique(manifest$geo), corrected_geographies)) {
    stop("The reporting cohort does not match the verified Eurostat geographies.")
  }
  expected_ages <- c("Y20-39", "Y40-59", "Y60-79", "Y_GE80")
  if (!setequal(unique(manifest$age), expected_ages)) {
    stop("The corrected Europe manifest has unexpected age groups.")
  }
  result_paths <- file.path(
    refit_root,
    "fitted_model",
    paste0(manifest$model_id, ".rda")
  )
  if (!all(file.exists(result_paths))) {
    stop("One or more verified compact Europe results are unavailable locally.")
  }

  list(
    refit_root = normalizePath(refit_root),
    cohort_path = normalizePath(cohort_path),
    data_path = normalizePath(data_path),
    verification = verification,
    manifest = manifest,
    cohort = cohort
  )
}

read_corrected_europe_observed <- function(data_path) {
  data <- utils::read.csv(data_path, stringsAsFactors = FALSE)
  required <- c("geo", "age", "sex", "TIME_PERIOD", "OBS_VALUE")
  missing <- setdiff(required, names(data))
  if (length(missing) > 0L) {
    stop("Observed Eurostat data are missing: ", paste(missing, collapse = ", "), ".")
  }
  data.frame(
    geography = data$geo,
    age_group = data$age,
    sex = as.character(data$sex),
    date = ISOweek::ISOweek2date(paste0(data$TIME_PERIOD, "-1")),
    observed_deaths = data$OBS_VALUE,
    stringsAsFactors = FALSE
  )
}

load_corrected_europe_prediction <- function(
  refit_root,
  observed,
  geography,
  age_group,
  sex
) {
  model_id <- paste(geography, age_group, sex, sep = "_")
  path <- file.path(refit_root, "fitted_model", paste0(model_id, ".rda"))
  if (!file.exists(path)) {
    stop("Missing corrected compact prediction: ", path, ".")
  }
  environment <- new.env(parent = emptyenv())
  loaded <- load(path, envir = environment)
  if (!identical(loaded, "model_pred")) {
    stop("Corrected compact prediction must contain only model_pred: ", path, ".")
  }
  model_pred <- environment$model_pred
  if (!is.matrix(model_pred$samples) || ncol(model_pred$samples) != 3000L) {
    stop("Corrected prediction must contain exactly 3,000 posterior draws: ", model_id, ".")
  }
  dates <- as.Date(model_pred$summary$time)
  selected <- observed[
    observed$geography == geography &
      observed$age_group == age_group &
      observed$sex == sex,
    ,
    drop = FALSE
  ]
  observed_index <- match(dates, selected$date)
  if (anyNA(observed_index)) {
    stop("Observed deaths do not align with corrected prediction dates: ", model_id, ".")
  }
  if (nrow(model_pred$samples) != length(dates)) {
    stop("Corrected prediction rows and dates differ: ", model_id, ".")
  }
  list(
    model_id = model_id,
    geography = geography,
    age_group = age_group,
    sex = sex,
    dates = dates,
    observed_deaths = selected$observed_deaths[observed_index],
    samples = model_pred$samples
  )
}

combine_corrected_europe_predictions <- function(
  predictions,
  combined_age_group
) {
  if (length(predictions) < 2L) {
    stop("Age aggregation requires at least two predictions.")
  }
  geographies <- vapply(predictions, `[[`, character(1), "geography")
  sexes <- vapply(predictions, `[[`, character(1), "sex")
  draws <- vapply(predictions, function(prediction) ncol(prediction$samples), integer(1))
  if (length(unique(geographies)) != 1L ||
      length(unique(sexes)) != 1L ||
      length(unique(draws)) != 1L) {
    stop("Age aggregation requires matching geography, sex, and draw counts.")
  }
  common_dates <- Reduce(intersect, lapply(predictions, `[[`, "dates"))
  common_dates <- sort(as.Date(common_dates, origin = "1970-01-01"))
  if (length(common_dates) == 0L) {
    stop("Age aggregation predictions have no common dates.")
  }
  combined_samples <- Reduce(`+`, lapply(predictions, function(prediction) {
    prediction$samples[
      match(common_dates, prediction$dates),
      ,
      drop = FALSE
    ]
  }))
  combined_observed <- Reduce(`+`, lapply(predictions, function(prediction) {
    prediction$observed_deaths[match(common_dates, prediction$dates)]
  }))
  list(
    model_id = paste(geographies[[1]], combined_age_group, sexes[[1]], sep = "_"),
    geography = geographies[[1]],
    age_group = combined_age_group,
    sex = sexes[[1]],
    dates = common_dates,
    observed_deaths = combined_observed,
    samples = combined_samples
  )
}

corrected_europe_pscore_draws <- function(prediction) {
  observed <- matrix(
    prediction$observed_deaths,
    nrow = nrow(prediction$samples),
    ncol = ncol(prediction$samples)
  )
  expected <- prediction$samples
  list(
    geography = prediction$geography,
    age_group = prediction$age_group,
    dates = prediction$dates,
    samples = (observed - expected) / (expected + .Machine$double.eps)
  )
}

corrected_europe_sex_contrast_draws <- function(female, male) {
  if (!identical(female$geography, male$geography) ||
      !identical(female$age_group, male$age_group)) {
    stop("Sex contrasts require matching geography and age group.")
  }
  common_dates <- intersect(female$dates, male$dates)
  common_dates <- sort(as.Date(common_dates, origin = "1970-01-01"))
  female_draws <- corrected_europe_pscore_draws(female)$samples[
    match(common_dates, female$dates),
    ,
    drop = FALSE
  ]
  male_draws <- corrected_europe_pscore_draws(male)$samples[
    match(common_dates, male$dates),
    ,
    drop = FALSE
  ]
  if (ncol(female_draws) != ncol(male_draws)) {
    stop("Sex contrasts require matching posterior draw counts.")
  }
  list(
    geography = female$geography,
    age_group = female$age_group,
    dates = common_dates,
    samples = female_draws - male_draws
  )
}

aggregate_corrected_europe_draws <- function(draw_objects) {
  if (length(draw_objects) == 0L) {
    stop("Jurisdiction aggregation requires at least one draw object.")
  }
  common_dates <- Reduce(intersect, lapply(draw_objects, `[[`, "dates"))
  common_dates <- sort(as.Date(common_dates, origin = "1970-01-01"))
  means <- vapply(draw_objects, function(object) {
    rowMeans(object$samples[match(common_dates, object$dates), , drop = FALSE])
  }, numeric(length(common_dates)))
  variances <- vapply(draw_objects, function(object) {
    apply(
      object$samples[match(common_dates, object$dates), , drop = FALSE],
      1,
      stats::var
    )
  }, numeric(length(common_dates)))
  means <- as.matrix(means)
  variances <- as.matrix(variances)
  inverse_variance <- ifelse(
    is.finite(variances) & variances > 0,
    1 / variances,
    NA_real_
  )
  denominator <- rowSums(inverse_variance, na.rm = TRUE)
  if (any(!is.finite(denominator) | denominator <= 0)) {
    stop("A corrected Europe aggregation date has no usable jurisdiction variance.")
  }
  weights <- inverse_variance / denominator
  mean <- rowSums(weights * means, na.rm = TRUE)
  variance <- rowSums(weights^2 * variances, na.rm = TRUE)
  data.frame(
    date = common_dates,
    mean = mean,
    variance = variance,
    lower = mean - 1.96 * sqrt(variance),
    upper = mean + 1.96 * sqrt(variance),
    jurisdictions = length(draw_objects),
    interval_method = "fixed_effect_normal_approximation",
    stringsAsFactors = FALSE
  )
}

corrected_europe_sex_label <- function(sex) {
  labels <- c(F = "female", M = "male", T = "total")
  output <- unname(labels[sex])
  if (anyNA(output)) {
    stop("Unsupported corrected Europe sex code.")
  }
  output
}

summarize_corrected_europe_waves <- function(
  predictions,
  wave_definitions,
  analysis_path = "europe_sex"
) {
  required_wave_columns <- c("wave", "start", "end_exclusive")
  missing <- setdiff(required_wave_columns, names(wave_definitions))
  if (length(missing) > 0L) {
    stop("Wave definitions are missing: ", paste(missing, collapse = ", "), ".")
  }
  rows <- lapply(predictions, function(prediction) {
    dplyr::bind_rows(lapply(seq_len(nrow(wave_definitions)), function(index) {
      selected <- prediction$dates >= wave_definitions$start[[index]] &
        prediction$dates < wave_definitions$end_exclusive[[index]]
      if (!any(selected)) {
        stop(
          "No prediction dates for ",
          prediction$model_id %||% prediction$geography,
          " in wave ",
          wave_definitions$wave[[index]],
          "."
        )
      }
      expected <- colSums(prediction$samples[selected, , drop = FALSE])
      observed <- sum(prediction$observed_deaths[selected])
      pscore <- (observed - expected) / expected
      interval <- stats::quantile(
        pscore,
        probs = c(0.025, 0.5, 0.975),
        names = FALSE
      )
      data.frame(
        analysis_path = analysis_path,
        geography = prediction$geography,
        age_group = sub("^Y", "", prediction$age_group),
        sex = corrected_europe_sex_label(prediction$sex),
        wave = wave_definitions$wave[[index]],
        wave_start = as.Date(wave_definitions$start[[index]]),
        wave_end_exclusive = as.Date(wave_definitions$end_exclusive[[index]]),
        p_lower = interval[[1]],
        p_median = interval[[2]],
        p_upper = interval[[3]],
        observed_deaths = observed,
        posterior_draws = length(expected),
        status = "success",
        stringsAsFactors = FALSE
      )
    }))
  })
  output <- dplyr::bind_rows(rows)
  if (anyDuplicated(output[c("analysis_path", "geography", "age_group", "sex", "wave")])) {
    stop("Corrected Europe wave summaries contain duplicate keys.")
  }
  output
}

build_corrected_europe_wave_summary <- function(
  contract,
  observed,
  wave_definitions
) {
  map_geographies <- contract$cohort$geography[contract$cohort$figure_02]
  table_geographies <- contract$cohort$geography[contract$cohort$table_01]
  specifications <- dplyr::bind_rows(
    tidyr::crossing(
      geography = map_geographies,
      age_group = c("Y40-59", "Y60-79"),
      sex = "T"
    ),
    tidyr::crossing(
      geography = table_geographies,
      age_group = c("Y40-59", "Y60-79"),
      sex = c("F", "M")
    )
  )
  rows <- lapply(seq_len(nrow(specifications)), function(index) {
    prediction <- load_corrected_europe_prediction(
      contract$refit_root,
      observed,
      specifications$geography[[index]],
      specifications$age_group[[index]],
      specifications$sex[[index]]
    )
    summarize_corrected_europe_waves(list(prediction), wave_definitions)
  })
  dplyr::bind_rows(rows)
}

build_corrected_europe_prediction_set <- function(
  contract,
  observed,
  geographies,
  ages = c("Y40-59", "Y60-79"),
  sexes = c("T", "F", "M")
) {
  output <- list()
  for (geography in geographies) {
    for (age in ages) {
      for (sex in sexes) {
        key <- paste(geography, age, sex, sep = "::")
        output[[key]] <- load_corrected_europe_prediction(
          contract$refit_root,
          observed,
          geography,
          age,
          sex
        )
      }
    }
  }
  output
}

get_corrected_europe_prediction <- function(
  predictions,
  geography,
  age_group,
  sex
) {
  key <- paste(geography, age_group, sex, sep = "::")
  output <- predictions[[key]]
  if (is.null(output)) {
    stop("Corrected Europe prediction set is missing key: ", key, ".")
  }
  output
}

aggregate_corrected_europe_by_vaccination <- function(
  predictions,
  vaccination,
  cohort,
  age_groups,
  contrast = FALSE
) {
  required_vaccination <- c("geography", "vaccination_group")
  if (!all(required_vaccination %in% names(vaccination))) {
    stop("Corrected Europe vaccination membership is incomplete.")
  }
  included <- cohort$geography[if (contrast) cohort$figure_05 else cohort$figure_04]
  selected_vaccination <- vaccination[
    vaccination$geography %in% included &
      vaccination$vaccination_group %in% c("high", "low"),
    ,
    drop = FALSE
  ]
  if (!setequal(selected_vaccination$geography, included)) {
    stop("Corrected Europe vaccination membership does not match the frozen cohort.")
  }
  rows <- list()
  for (age_group in names(age_groups)) {
    component_ages <- age_groups[[age_group]]
    for (vaccination_group in c("high", "low")) {
      geographies <- selected_vaccination$geography[
        selected_vaccination$vaccination_group == vaccination_group
      ]
      draw_objects <- lapply(geographies, function(geography) {
        if (contrast) {
          female_parts <- lapply(component_ages, function(age) {
            get_corrected_europe_prediction(predictions, geography, age, "F")
          })
          male_parts <- lapply(component_ages, function(age) {
            get_corrected_europe_prediction(predictions, geography, age, "M")
          })
          female <- if (length(female_parts) == 1L) female_parts[[1]] else
            combine_corrected_europe_predictions(female_parts, age_group)
          male <- if (length(male_parts) == 1L) male_parts[[1]] else
            combine_corrected_europe_predictions(male_parts, age_group)
          corrected_europe_sex_contrast_draws(female, male)
        } else {
          parts <- lapply(component_ages, function(age) {
            get_corrected_europe_prediction(predictions, geography, age, "T")
          })
          prediction <- if (length(parts) == 1L) parts[[1]] else
            combine_corrected_europe_predictions(parts, age_group)
          corrected_europe_pscore_draws(prediction)
        }
      })
      rows[[paste(age_group, vaccination_group, sep = "::")]] <-
        dplyr::mutate(
          aggregate_corrected_europe_draws(draw_objects),
          region = "Europe",
          age_group = age_group,
          vaccination_group = vaccination_group,
          .before = 1
        )
    }
  }
  dplyr::bind_rows(rows)
}

build_corrected_europe_map_input <- function(
  wave_summary,
  geometry_path
) {
  if (!file.exists(geometry_path)) {
    stop("European map geometry is unavailable: ", geometry_path, ".")
  }
  result <- wave_summary |>
    dplyr::filter(
      sex == "total",
      age_group %in% c("40-59", "60-79"),
      wave %in% c("initial", "delta")
    ) |>
    dplyr::transmute(
      join_code = dplyr::recode(geography, EL = "GR"),
      label = dplyr::recode(geography, EL = "GR"),
      age_group,
      wave,
      p_median
    )
  if (nrow(result) != 33L * 2L * 2L) {
    stop("Corrected Figure 2 requires 33 geographies by two ages and two waves.")
  }
  geometry <- sf::st_read(geometry_path, quiet = TRUE) |>
    sf::st_transform(3035) |>
    dplyr::filter(!is.na(ISO_A2_EH), ISO_A2_EH != "-99") |>
    dplyr::select(join_code = ISO_A2_EH, geometry) |>
    sf::st_crop(c(
      xmin = 2700000,
      ymin = 1530000,
      xmax = 5686000,
      ymax = 4660000
    )) |>
    dplyr::group_by(join_code) |>
    dplyr::summarise(geometry = sf::st_union(geometry), .groups = "drop")
  inset_geometry <- sf::st_as_sf(
    data.frame(
      join_code = c("MT", "CY", "IS", "AM"),
      x = c(4910146, 5182984, 3071476, 5670000),
      y = c(1580000, 1580000, 4596126, 2200000),
      stringsAsFactors = FALSE
    ),
    coords = c("x", "y"),
    crs = 3035
  )
  geometry <- dplyr::bind_rows(
    geometry[!geometry$join_code %in% inset_geometry$join_code, ],
    inset_geometry
  )
  map_data <- dplyr::inner_join(geometry, result, by = "join_code")
  if (length(unique(map_data$label)) != 33L) {
    missing <- setdiff(unique(result$label), unique(map_data$label))
    stop(
      "Corrected Figure 2 geometry is incomplete: ",
      paste(missing, collapse = ", "),
      "."
    )
  }
  list(
    map_data = map_data,
    scope = list(
      source = "verified Eurostat corrected-prior refit",
      geographies = sort(unique(map_data$label)),
      excluded_pending_refit = c("England and Wales", "Ireland")
    )
  )
}

`%||%` <- function(left, right) {
  if (is.null(left)) right else left
}
