#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(aghq)
  library(digest)
  library(dplyr)
  library(here)
  library(ISOweek)
  library(Matrix)
  library(OSplines)
  library(readr)
  library(sf)
  library(sGPfit)
  library(TMB)
  library(yaml)
})

source(here::here("R", "config.R"))
source(here::here("R", "validation.R"))
source(here::here("R", "vaccination.R"))
source(here::here("R", "waves.R"))

sf::sf_use_s2(FALSE)

parse_arguments <- function(arguments) {
  defaults <- list(
    archive_root = "/Users/ziangzhang/Desktop/covid_mortality/covid_excess",
    bundle_root = here::here("artifacts", "results", "zenodo_bundle"),
    stage = "true"
  )
  if (length(arguments) == 0L) {
    return(defaults)
  }
  if (!all(grepl("^--[A-Za-z0-9_-]+=.+$", arguments))) {
    stop("Arguments must use --name=value syntax.")
  }
  parsed <- sub("^--", "", arguments)
  names_parsed <- sub("=.*$", "", parsed)
  values <- sub("^[^=]*=", "", parsed)
  unknown <- setdiff(names_parsed, names(defaults))
  if (length(unknown) > 0L) {
    stop("Unknown arguments: ", paste(unknown, collapse = ", "), ".")
  }
  defaults[names_parsed] <- as.list(values)
  defaults
}

load_rda_object <- function(path, object_name) {
  environment <- new.env(parent = emptyenv())
  loaded <- load(path, envir = environment)
  if (!object_name %in% loaded) {
    stop("Object '", object_name, "' was not found in ", path, ".")
  }
  environment[[object_name]]
}

copy_preserving_name <- function(source, destination_directory) {
  if (!all(file.exists(source))) {
    stop("Missing source artifacts: ", paste(source[!file.exists(source)], collapse = ", "))
  }
  dir.create(destination_directory, recursive = TRUE, showWarnings = FALSE)
  destination <- file.path(destination_directory, basename(source))
  copied <- file.copy(source, destination, overwrite = TRUE, copy.mode = TRUE)
  if (!all(copied)) {
    stop("Failed to stage one or more source artifacts.")
  }
  tibble::tibble(
    source_path = normalizePath(source),
    bundle_path = normalizePath(destination),
    bytes = file.info(destination)$size
  )
}

stage_source_artifacts <- function(archive_root, bundle_root) {
  source_root <- file.path(bundle_root, "source_artifacts")
  europe_root <- file.path(archive_root, "Europe", "stratified")
  us_non_sex_root <- file.path(
    archive_root,
    "North_America",
    "non-stratified",
    "USA"
  )
  us_sex_root <- file.path(
    archive_root,
    "North_America",
    "sex-stratified",
    "USA"
  )
  canada_root <- file.path(
    archive_root,
    "North_America",
    "non-stratified",
    "Canada"
  )

  europe_geographies <- c(
    "AM", "AT", "BE", "BG", "DK", "ES", "FI", "HR",
    "HU", "IT", "LV", "PT", "RO", "RS", "SK"
  )
  europe_fits <- unlist(lapply(europe_geographies, function(geography) {
    as.vector(outer(
      c("Y40-59", "Y60-79"),
      c("T", "F", "M"),
      function(age, sex) file.path(
        europe_root,
        "fitted_model",
        paste0(geography, "_", age, "_", sex, ".rda")
      )
    ))
  }))

  us_non_sex_geographies <- c(
    "Alabama", "Connecticut", "Idaho", "Louisiana", "Maine", "Maryland",
    "Massachusetts", "Mississippi", "New Hampshire", "New Jersey",
    "New Mexico", "Pennsylvania", "Tennessee"
  )
  us_non_sex_fits <- unlist(lapply(us_non_sex_geographies, function(geography) {
    file.path(
      us_non_sex_root,
      "fitted_model",
      paste0(geography, "_age_", c("40-59", "60-79"), ".rda")
    )
  }))

  us_sex_geographies <- c(
    "Alabama", "Connecticut", "Idaho", "Louisiana", "Maryland",
    "Massachusetts", "Mississippi", "New Jersey", "New Mexico",
    "Pennsylvania", "Tennessee"
  )
  us_sex_fits <- unlist(lapply(us_sex_geographies, function(geography) {
    as.vector(outer(
      c("0-44", "45-64", "65-84"),
      c("F", "M"),
      function(age, sex) file.path(
        us_sex_root,
        "fitted_model",
        paste0(geography, "_age_", age, "_sex_", sex, ".rda")
      )
    ))
  }))

  europe_shape <- file.path(
    europe_root,
    paste0("ne_10m_admin_0_countries_lakes.", c("shp", "shx", "dbf", "prj"))
  )
  us_shape <- file.path(
    us_non_sex_root,
    "cb_2018_us_state_500k",
    paste0("cb_2018_us_state_500k.", c("shp", "shx", "dbf", "prj"))
  )
  canada_shape <- file.path(
    canada_root,
    "lpr_000b21a_e",
    paste0("lpr_000b21a_e.", c("shp", "shx", "dbf", "prj"))
  )

  records <- list(
    copy_preserving_name(
      europe_fits,
      file.path(source_root, "europe", "fitted_predictions")
    ),
    copy_preserving_name(
      c(
        file.path(europe_root, "script", "NL_Y_GE80_T_model_list.rda"),
        file.path(europe_root, "fitted_model", "NL_Y_GE80_T.rda")
      ),
      file.path(source_root, "europe", "figure_01")
    ),
    copy_preserving_name(
      c(
        file.path(europe_root, "script", "function.R"),
        file.path(europe_root, "demo_r_mwk_20_linear.csv"),
        file.path(europe_root, "vac_data_eu.rda"),
        file.path(europe_root, "result", "result_all_age_complete.rda")
      ),
      file.path(source_root, "europe", "supporting")
    ),
    copy_preserving_name(
      europe_shape,
      file.path(source_root, "europe", "geometry")
    ),
    copy_preserving_name(
      us_non_sex_fits,
      file.path(source_root, "north_america", "us_non_sex", "fitted_predictions")
    ),
    copy_preserving_name(
      c(
        file.path(us_non_sex_root, "USA_monthly.rda"),
        file.path(us_non_sex_root, "USA_monthly_result.rda")
      ),
      file.path(source_root, "north_america", "us_non_sex", "supporting")
    ),
    copy_preserving_name(
      us_shape,
      file.path(source_root, "north_america", "us_non_sex", "geometry")
    ),
    copy_preserving_name(
      us_sex_fits,
      file.path(source_root, "north_america", "us_sex", "fitted_predictions")
    ),
    copy_preserving_name(
      c(
        file.path(us_sex_root, "USA_monthly.rda"),
        file.path(us_sex_root, "USA_monthly_result.rda"),
        file.path(us_sex_root, "us_state_vaccinations_select.rda")
      ),
      file.path(source_root, "north_america", "us_sex", "supporting")
    ),
    copy_preserving_name(
      file.path(canada_root, "Canada_weekly_result.rda"),
      file.path(source_root, "north_america", "canada_non_sex", "supporting")
    ),
    copy_preserving_name(
      canada_shape,
      file.path(source_root, "north_america", "canada_non_sex", "geometry")
    )
  )
  dplyr::bind_rows(records)
}

prepare_europe_vaccination <- function(source_root, config) {
  vaccination <- load_rda_object(
    file.path(source_root, "europe", "supporting", "vac_data_eu.rda"),
    "vac_data_eu"
  )
  vaccination$iso2[vaccination$location == "United Kingdom"] <- "UK"
  rules <- config$vaccination$classification_rules$europe
  vaccination |>
    dplyr::transmute(
      date = as.Date(date),
      geography = iso2,
      people_vaccinated_per_hundred,
      vaccination_group = classify_vaccination_coverage(
        people_vaccinated_per_hundred,
        low_below = rules$low_below,
        high_above = rules$high_above
      )
    ) |>
    dplyr::arrange(geography)
}

prepare_us_vaccination_from_bundle <- function(source_root, config) {
  prepare_us_vaccination(
    file.path(
      source_root,
      "north_america",
      "us_sex",
      "supporting",
      "us_state_vaccinations_select.rda"
    ),
    config
  )
}

read_europe_observed <- function(source_root) {
  observed <- readr::read_csv(
    file.path(
      source_root,
      "europe",
      "supporting",
      "demo_r_mwk_20_linear.csv"
    ),
    show_col_types = FALSE
  )
  observed |>
    dplyr::transmute(
      date = ISOweek::ISOweek2date(paste0(TIME_PERIOD, "-1")),
      geography = geo,
      age_group = age,
      sex = as.character(sex),
      observed_deaths = OBS_VALUE
    )
}

read_us_observed <- function(source_root, analysis_path) {
  path <- file.path(
    source_root,
    "north_america",
    analysis_path,
    "supporting",
    "USA_monthly.rda"
  )
  data <- load_rda_object(path, "USA_monthly")
  data |>
    dplyr::transmute(
      date = as.Date(date),
      geography = state,
      age_group = age,
      sex = if ("sex" %in% names(data)) as.character(sex) else "T",
      observed_deaths = Deaths
    )
}

load_prediction <- function(path, observed, geography, age_group, sex) {
  model_prediction <- load_rda_object(path, "model_pred")
  dates <- as.Date(model_prediction$summary$time)
  selected <- observed[
    observed$geography == geography &
      observed$age_group == age_group &
      observed$sex == sex,
  ]
  observed_index <- match(dates, selected$date)
  if (any(is.na(observed_index))) {
    stop("Observed deaths are incomplete for ", basename(path), ".")
  }
  list(
    geography = geography,
    age_group = age_group,
    sex = sex,
    dates = dates,
    observed_deaths = selected$observed_deaths[observed_index],
    samples = model_prediction$samples
  )
}

combine_predictions <- function(predictions, combined_age_group) {
  common_dates <- Reduce(intersect, lapply(predictions, `[[`, "dates"))
  common_dates <- sort(as.Date(common_dates, origin = "1970-01-01"))
  combined <- predictions[[1]]
  combined$age_group <- combined_age_group
  combined$dates <- common_dates
  combined$observed_deaths <- Reduce(`+`, lapply(predictions, function(prediction) {
    prediction$observed_deaths[match(common_dates, prediction$dates)]
  }))
  combined$samples <- Reduce(`+`, lapply(predictions, function(prediction) {
    prediction$samples[match(common_dates, prediction$dates), , drop = FALSE]
  }))
  combined
}

pscore_draw_object <- function(prediction) {
  observed <- matrix(
    prediction$observed_deaths,
    nrow = nrow(prediction$samples),
    ncol = ncol(prediction$samples)
  )
  list(
    geography = prediction$geography,
    age_group = prediction$age_group,
    dates = prediction$dates,
    samples = (observed - prediction$samples) / prediction$samples
  )
}

sex_contrast_draw_object <- function(female, male) {
  common_dates <- intersect(female$dates, male$dates)
  common_dates <- sort(as.Date(common_dates, origin = "1970-01-01"))
  female_draws <- pscore_draw_object(female)$samples[
    match(common_dates, female$dates),
    ,
    drop = FALSE
  ]
  male_draws <- pscore_draw_object(male)$samples[
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

aggregate_draw_objects <- function(draw_objects) {
  common_dates <- Reduce(intersect, lapply(draw_objects, `[[`, "dates"))
  common_dates <- sort(as.Date(common_dates, origin = "1970-01-01"))
  means <- sapply(draw_objects, function(object) {
    rowMeans(object$samples[match(common_dates, object$dates), , drop = FALSE])
  })
  variances <- sapply(draw_objects, function(object) {
    apply(
      object$samples[match(common_dates, object$dates), , drop = FALSE],
      1,
      stats::var
    )
  })
  means <- as.matrix(means)
  variances <- as.matrix(variances)
  inverse_variance <- ifelse(
    is.finite(variances) & variances > 0,
    1 / variances,
    NA_real_
  )
  denominator <- rowSums(inverse_variance, na.rm = TRUE)
  weights <- inverse_variance / denominator
  mean <- rowSums(weights * means, na.rm = TRUE)
  variance <- rowSums(weights^2 * variances, na.rm = TRUE)
  tibble::tibble(
    date = common_dates,
    mean = mean,
    variance = variance,
    lower = mean - 1.96 * sqrt(variance),
    upper = mean + 1.96 * sqrt(variance),
    jurisdictions = length(draw_objects),
    interval_method = "fixed_effect_normal_approximation"
  )
}

build_figure_01_input <- function(source_root) {
  function_path <- file.path(source_root, "europe", "supporting", "function.R")
  source(function_path, local = globalenv())
  model_list <- load_rda_object(
    file.path(source_root, "europe", "figure_01", "NL_Y_GE80_T_model_list.rda"),
    "model_list"
  )
  overall <- load_rda_object(
    file.path(source_root, "europe", "figure_01", "NL_Y_GE80_T.rda"),
    "model_pred"
  )
  set.seed(20260829)
  trend <- pred_mortality(
    model_list,
    component = "trend",
    scale = "original",
    refined_pred = model_list$x_full
  )
  set.seed(20260829)
  seasonal <- pred_mortality(
    model_list,
    component = "seasonal",
    refined_pred = model_list$x_full
  )
  observed <- read_europe_observed(source_root) |>
    dplyr::filter(
      geography == "NL",
      age_group == "Y_GE80",
      sex == "T"
    )
  overall_dates <- as.Date(overall$summary$time)
  observed_index <- match(overall_dates, observed$date)
  list(
    overall = tibble::tibble(
      date = overall_dates,
      observed = observed$observed_deaths[observed_index],
      mean = overall$summary$mean,
      lower = overall$summary$lower,
      upper = overall$summary$upper
    ),
    trend = dplyr::transmute(
      trend$summary,
      date = as.Date(time),
      mean,
      lower,
      upper
    ),
    seasonal = dplyr::transmute(
      seasonal$summary,
      date = as.Date(time),
      mean,
      lower,
      upper
    ),
    training_boundary = as.Date("2020-01-01")
  )
}

build_europe_map_input <- function(source_root) {
  result <- load_rda_object(
    file.path(source_root, "europe", "supporting", "result_all_age_complete.rda"),
    "model_result_all"
  ) |>
    dplyr::filter(
      sex == "T",
      age %in% c("Y40-59", "Y60-79"),
      wave %in% c("initial", "delta")
    ) |>
    dplyr::transmute(
      join_code = dplyr::recode(country, EL = "GR", GB = "GB"),
      label = dplyr::recode(country, GB = "UK"),
      age_group = sub("^Y", "", age),
      wave,
      p_median = p_med
    )
  geometry <- sf::st_read(
    file.path(source_root, "europe", "geometry", "ne_10m_admin_0_countries_lakes.shp"),
    quiet = TRUE
  ) |>
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
    tibble::tibble(
      join_code = c("MT", "CY", "IS", "AM"),
      x = c(4910146, 5182984, 3071476, 5670000),
      y = c(1580000, 1580000, 4596126, 2200000)
    ),
    coords = c("x", "y"),
    crs = 3035
  )
  geometry <- dplyr::bind_rows(
    geometry[!geometry$join_code %in% inset_geometry$join_code, ],
    inset_geometry
  )
  list(map_data = dplyr::inner_join(geometry, result, by = "join_code"))
}

build_north_america_map_input <- function(source_root) {
  us_result <- load_rda_object(
    file.path(
      source_root,
      "north_america",
      "us_non_sex",
      "supporting",
      "USA_monthly_result.rda"
    ),
    "model_result_all"
  ) |>
    dplyr::filter(age %in% c("40-59", "60-79"), wave %in% c("initial", "delta"))
  us_geometry <- sf::st_read(
    file.path(
      source_root,
      "north_america",
      "us_non_sex",
      "geometry",
      "cb_2018_us_state_500k.shp"
    ),
    quiet = TRUE
  ) |>
    dplyr::filter(!STUSPS %in% c("AK", "HI", "AS", "PR", "MP", "VI", "GU")) |>
    dplyr::select(geography = NAME, label = STUSPS, geometry)
  us_map <- dplyr::inner_join(us_geometry, us_result, by = c("geography" = "state")) |>
    dplyr::transmute(
      label,
      age_group = age,
      wave,
      p_median = p_med,
      geometry
    )

  canada_result <- load_rda_object(
    file.path(
      source_root,
      "north_america",
      "canada_non_sex",
      "supporting",
      "Canada_weekly_result.rda"
    ),
    "model_result_all"
  ) |>
    dplyr::filter(age %in% c("45-64", "65-84"), wave %in% c("initial", "delta")) |>
    dplyr::mutate(
      age_group = dplyr::recode(age, `45-64` = "40-59", `65-84` = "60-79")
    )
  province_codes <- tibble::tribble(
    ~province, ~geography,
    "NL", "Newfoundland and Labrador",
    "PE", "Prince Edward Island",
    "NS", "Nova Scotia",
    "NB", "New Brunswick",
    "QC", "Quebec",
    "ON", "Ontario",
    "MB", "Manitoba",
    "SK", "Saskatchewan",
    "AB", "Alberta",
    "BC", "British Columbia"
  )
  canada_geometry <- sf::st_read(
    file.path(
      source_root,
      "north_america",
      "canada_non_sex",
      "geometry",
      "lpr_000b21a_e.shp"
    ),
    quiet = TRUE
  ) |>
    sf::st_transform(3347) |>
    sf::st_simplify(dTolerance = 5000) |>
    sf::st_transform(sf::st_crs(us_geometry)) |>
    dplyr::select(geography = PRENAME, geometry)
  canada_map <- canada_result |>
    dplyr::left_join(province_codes, by = "province") |>
    dplyr::inner_join(canada_geometry, by = "geography") |>
    sf::st_as_sf() |>
    dplyr::transmute(
      label = province,
      age_group,
      wave,
      p_median = p_med,
      geometry
    )
  list(map_data = dplyr::bind_rows(us_map, canada_map))
}

build_region_predictions <- function(
  source_root,
  region,
  geographies,
  ages,
  sexes,
  observed
) {
  output <- list()
  for (geography in geographies) {
    for (age in ages) {
      for (sex in sexes) {
        path <- if (region == "europe") {
          file.path(
            source_root,
            "europe",
            "fitted_predictions",
            paste0(geography, "_", age, "_", sex, ".rda")
          )
        } else if (region == "us_non_sex") {
          file.path(
            source_root,
            "north_america",
            "us_non_sex",
            "fitted_predictions",
            paste0(geography, "_age_", age, ".rda")
          )
        } else {
          file.path(
            source_root,
            "north_america",
            "us_sex",
            "fitted_predictions",
            paste0(geography, "_age_", age, "_sex_", sex, ".rda")
          )
        }
        key <- paste(geography, age, sex, sep = "::")
        output[[key]] <- load_prediction(
          path,
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

get_prediction <- function(predictions, geography, age, sex) {
  predictions[[paste(geography, age, sex, sep = "::")]]
}

aggregate_by_vaccination <- function(
  predictions,
  vaccination,
  age_groups,
  region_label,
  contrast = FALSE
) {
  rows <- list()
  selected_vaccination <- vaccination[
    vaccination$vaccination_group %in% c("high", "low"),
  ]
  for (age_group in names(age_groups)) {
    component_ages <- age_groups[[age_group]]
    for (vaccination_group in c("high", "low")) {
      geographies <- selected_vaccination$geography[
        selected_vaccination$vaccination_group == vaccination_group
      ]
      draw_objects <- lapply(geographies, function(geography) {
        if (contrast) {
          female_parts <- lapply(component_ages, function(age) {
            get_prediction(predictions, geography, age, "F")
          })
          male_parts <- lapply(component_ages, function(age) {
            get_prediction(predictions, geography, age, "M")
          })
          female <- if (length(component_ages) == 1L) {
            female_parts[[1]]
          } else {
            combine_predictions(female_parts, age_group)
          }
          male <- if (length(component_ages) == 1L) {
            male_parts[[1]]
          } else {
            combine_predictions(male_parts, age_group)
          }
          sex_contrast_draw_object(female, male)
        } else {
          parts <- lapply(component_ages, function(age) {
            get_prediction(predictions, geography, age, "T")
          })
          prediction <- if (length(component_ages) == 1L) {
            parts[[1]]
          } else {
            combine_predictions(parts, age_group)
          }
          pscore_draw_object(prediction)
        }
      })
      rows[[paste(age_group, vaccination_group, sep = "::")]] <-
        aggregate_draw_objects(draw_objects) |>
        dplyr::mutate(
          region = region_label,
          age_group = age_group,
          vaccination_group = vaccination_group,
          .before = 1
        )
    }
  }
  dplyr::bind_rows(rows)
}

build_historical_europe_all_age_contrast <- function(
  predictions,
  observed,
  vaccination
) {
  rows <- list()
  selected_vaccination <- vaccination[
    vaccination$vaccination_group %in% c("high", "low"),
  ]
  for (vaccination_group in c("high", "low")) {
    geographies <- selected_vaccination$geography[
      selected_vaccination$vaccination_group == vaccination_group
    ]
    draw_objects <- lapply(geographies, function(geography) {
      female <- combine_predictions(
        list(
          get_prediction(predictions, geography, "Y40-59", "F"),
          get_prediction(predictions, geography, "Y60-79", "F")
        ),
        "historical_incoherent_all_age"
      )
      male <- combine_predictions(
        list(
          get_prediction(predictions, geography, "Y40-59", "M"),
          get_prediction(predictions, geography, "Y60-79", "M")
        ),
        "historical_incoherent_all_age"
      )

      historical_observed <- observed |>
        dplyr::filter(
          .data$geography == .env$geography,
          age_group %in% c("Y20-39", "Y40-59", "Y60-79"),
          sex %in% c("F", "M")
        ) |>
        dplyr::group_by(date, sex) |>
        dplyr::summarise(
          observed_deaths = sum(observed_deaths),
          .groups = "drop"
        )
      female_observed <- historical_observed[
        historical_observed$sex == "F",
      ]
      male_observed <- historical_observed[
        historical_observed$sex == "M",
      ]
      female$observed_deaths <- female_observed$observed_deaths[
        match(female$dates, female_observed$date)
      ]
      male$observed_deaths <- male_observed$observed_deaths[
        match(male$dates, male_observed$date)
      ]
      if (anyNA(female$observed_deaths) || anyNA(male$observed_deaths)) {
        stop(
          "Historical European all-age observations are incomplete for ",
          geography,
          "."
        )
      }
      sex_contrast_draw_object(female, male)
    })
    rows[[vaccination_group]] <- aggregate_draw_objects(draw_objects) |>
      dplyr::mutate(
        region = "Europe",
        age_group = "historical_incoherent_all_age",
        vaccination_group = vaccination_group,
        observed_age_groups = "20-39+40-59+60-79",
        expected_age_groups = "40-59+60-79",
        scientific_status = "historical_incoherent_estimand",
        .before = 1
      )
  }
  dplyr::bind_rows(rows)
}

wave_summary_from_predictions <- function(predictions, config, analysis_end = NULL) {
  definitions <- wave_table(config)
  dplyr::bind_rows(lapply(predictions, function(prediction) {
    dplyr::bind_rows(lapply(seq_len(nrow(definitions)), function(index) {
      selected <- prediction$dates >= definitions$start[[index]] &
        prediction$dates < definitions$end_exclusive[[index]]
      if (!is.null(analysis_end)) {
        selected <- selected & prediction$dates <= as.Date(analysis_end)
      }
      expected <- colSums(prediction$samples[selected, , drop = FALSE])
      observed <- sum(prediction$observed_deaths[selected])
      pscore <- (observed - expected) / expected
      interval <- stats::quantile(
        pscore,
        probs = c(0.025, 0.5, 0.975),
        names = FALSE
      )
      tibble::tibble(
        analysis_path = "us_sex",
        geography = prediction$geography,
        age_group = prediction$age_group,
        sex = dplyr::recode(prediction$sex, F = "female", M = "male"),
        wave = definitions$wave[[index]],
        p_lower = interval[[1]],
        p_median = interval[[2]],
        p_upper = interval[[3]],
        status = "success"
      )
    }))
  }))
}

standardize_europe_wave_summary <- function(source_root) {
  result <- load_rda_object(
    file.path(source_root, "europe", "supporting", "result_all_age_complete.rda"),
    "model_result_all"
  )
  excluded_geographies <- c("AD", "CY", "DE", "EE", "IS", "LI", "LU", "ME", "MT", "SI")
  result |>
    dplyr::filter(!country %in% excluded_geographies) |>
    dplyr::transmute(
      analysis_path = "europe_sex",
      geography = dplyr::recode(country, EL = "GR", GB = "UK"),
      age_group = sub("^Y", "", age),
      sex = dplyr::recode(sex, F = "female", M = "male", T = "total"),
      wave,
      p_lower,
      p_median = p_med,
      p_upper,
      status = "success"
    )
}

write_bundle_readme <- function(bundle_root, source_manifest, output_status) {
  readme <- c(
    "# Manuscript result bundle",
    "",
    "This local bundle contains the fitted posterior predictions and standardized inputs required by the five adopted manuscript figures and Table 1.",
    "",
    "## Scope",
    "",
    "- Source artifacts are copied from the historical `covid_excess` archive without modification.",
    "- Only jurisdictions and age/sex strata used by adopted manuscript outputs are included.",
    "- The reporting-ready files are also installed into the repository's standard `artifacts/` paths.",
    "- The bundle is intentionally ignored by Git and is suitable for a separate Zenodo deposit.",
    "",
    "## Reproduction",
    "",
    "From the repository root:",
    "",
    "```bash",
    "Rscript --vanilla scripts/reporting/run_all.R",
    "```",
    "",
    "To rebuild standardized reporting inputs from the staged fitted predictions without consulting the historical archive:",
    "",
    "```bash",
    "Rscript --vanilla scripts/reporting/prepare_manuscript_bundle.R --stage=false",
    "```",
    "",
    "## Scientific caveats",
    "",
    "- Figure 4 panels historically labeled `All Ages` estimate ages 40--79.",
    "- Figure 5 Europe uses ages 40--79 for both observed and expected deaths. The historical observed 20--79 versus expected 40--79 mismatch is retained only as a separately named comparison input.",
    "- US reporting uses the historical fit-quality subsets and then applies the fixed vaccination thresholds in `config/analysis.yml`.",
    "",
    "## Inventory",
    "",
    paste0("- Source files: ", nrow(source_manifest)),
    paste0("- Source size: ", round(sum(source_manifest$bytes) / 1024^3, 3), " GiB"),
    "- `manifests/source_provenance.csv` records every historical source path.",
    "- `manifests/sha256.csv` records checksums for every bundled file.",
    "- `manifests/output_status.csv` records the manuscript-output validation status."
  )
  writeLines(readme, file.path(bundle_root, "README.md"), useBytes = TRUE)
  readr::write_csv(output_status, file.path(bundle_root, "manifests", "output_status.csv"))
}

arguments <- parse_arguments(commandArgs(trailingOnly = TRUE))
config <- read_analysis_config(here::here("config", "analysis.yml"))
bundle_root <- normalizePath(
  arguments$bundle_root,
  mustWork = FALSE
)
dir.create(bundle_root, recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(bundle_root, "manifests"), recursive = TRUE, showWarnings = FALSE)

provenance_path <- file.path(bundle_root, "manifests", "source_provenance.csv")
if (tolower(arguments$stage) == "true") {
  source_manifest <- stage_source_artifacts(arguments$archive_root, bundle_root)
  readr::write_csv(source_manifest, provenance_path)
} else {
  if (!file.exists(provenance_path)) {
    stop("The staged bundle is missing source_provenance.csv.")
  }
  source_manifest <- readr::read_csv(provenance_path, show_col_types = FALSE)
}

source_root <- file.path(bundle_root, "source_artifacts")
reporting_input_root <- file.path(bundle_root, "reporting_inputs")
dir.create(reporting_input_root, recursive = TRUE, showWarnings = FALSE)

europe_vaccination <- prepare_europe_vaccination(source_root, config)
us_vaccination <- prepare_us_vaccination_from_bundle(source_root, config)
historical_cohorts <- historical_us_reporting_cohorts(us_vaccination$geography)
us_non_sex_membership <- us_vaccination |>
  dplyr::inner_join(
    historical_cohorts |>
      dplyr::filter(analysis_path == "us_non_sex") |>
      dplyr::select(geography),
    by = "geography"
  )
us_sex_membership <- us_vaccination |>
  dplyr::inner_join(
    historical_cohorts |>
      dplyr::filter(analysis_path == "us_sex") |>
      dplyr::select(geography),
    by = "geography"
  )

figure_01 <- build_figure_01_input(source_root)
figure_02 <- build_europe_map_input(source_root)
figure_03 <- build_north_america_map_input(source_root)

europe_observed <- read_europe_observed(source_root)
europe_reporting_membership <- europe_vaccination |>
  dplyr::filter(geography %in% c(
    "AM", "AT", "BE", "BG", "DK", "ES", "FI", "HR",
    "HU", "IT", "LV", "PT", "RO", "RS", "SK"
  ))
europe_predictions <- build_region_predictions(
  source_root,
  "europe",
  europe_reporting_membership$geography,
  c("Y40-59", "Y60-79"),
  c("T", "F", "M"),
  europe_observed
)

us_non_sex_observed <- read_us_observed(source_root, "us_non_sex")
us_non_sex_predictions <- build_region_predictions(
  source_root,
  "us_non_sex",
  us_non_sex_membership$geography[
    us_non_sex_membership$vaccination_group %in% c("high", "low")
  ],
  c("40-59", "60-79"),
  "T",
  us_non_sex_observed
)

figure_04 <- dplyr::bind_rows(
  aggregate_by_vaccination(
    europe_predictions,
    europe_reporting_membership,
    list(`40-79` = c("Y40-59", "Y60-79"), `40-59` = "Y40-59", `60-79` = "Y60-79"),
    "Europe"
  ),
  aggregate_by_vaccination(
    us_non_sex_predictions,
    us_non_sex_membership,
    list(`40-79` = c("40-59", "60-79"), `40-59` = "40-59", `60-79` = "60-79"),
    "United States"
  )
)

us_sex_observed <- read_us_observed(source_root, "us_sex")
us_sex_predictions <- build_region_predictions(
  source_root,
  "us_sex",
  us_sex_membership$geography[
    us_sex_membership$vaccination_group %in% c("high", "low")
  ],
  c("0-44", "45-64", "65-84"),
  c("F", "M"),
  us_sex_observed
)

historical_figure_05_europe <- build_historical_europe_all_age_contrast(
  europe_predictions,
  europe_observed,
  europe_reporting_membership
)

figure_05 <- dplyr::bind_rows(
  aggregate_by_vaccination(
    europe_predictions,
    europe_reporting_membership,
    list(
      `40-79` = c("Y40-59", "Y60-79"),
      `40-59` = "Y40-59",
      `60-79` = "Y60-79"
    ),
    "Europe",
    contrast = TRUE
  ),
  aggregate_by_vaccination(
    us_sex_predictions,
    us_sex_membership,
    list(
      `0-84` = c("0-44", "45-64", "65-84"),
      `0-44` = "0-44",
      `45-64` = "45-64",
      `65-84` = "65-84"
    ),
    "United States",
    contrast = TRUE
  )
)

europe_wave_summary <- standardize_europe_wave_summary(source_root)
us_table_predictions <- us_sex_predictions[vapply(
  us_sex_predictions,
  function(prediction) prediction$age_group == "65-84",
  logical(1)
)]
us_wave_summary <- wave_summary_from_predictions(
  us_table_predictions,
  config,
  analysis_end = config$regions$us_sex$analysis_end
)

saveRDS(figure_01, file.path(reporting_input_root, "figure_01_model_illustration.rds"))
saveRDS(figure_02, file.path(reporting_input_root, "figure_02_europe_maps.rds"))
saveRDS(figure_03, file.path(reporting_input_root, "figure_03_north_america_maps.rds"))
readr::write_csv(
  figure_04,
  file.path(reporting_input_root, "figure_04_vaccination_pscore.csv")
)
readr::write_csv(
  figure_05,
  file.path(reporting_input_root, "figure_05_sex_difference.csv")
)
readr::write_csv(
  historical_figure_05_europe,
  file.path(
    reporting_input_root,
    "figure_05_europe_historical_incoherent.csv"
  )
)
readr::write_csv(
  europe_wave_summary,
  file.path(reporting_input_root, "europe_wave_summary.csv")
)
readr::write_csv(
  us_wave_summary,
  file.path(reporting_input_root, "us_wave_summary.csv")
)
readr::write_csv(
  europe_vaccination,
  file.path(reporting_input_root, "europe_vaccination_membership.csv")
)
readr::write_csv(
  us_vaccination,
  file.path(reporting_input_root, "us_vaccination_membership.csv")
)

runtime_paths <- c(
  figure_01_model_illustration.rds = here::here(
    "artifacts", "reporting", "inputs", "figure_01_model_illustration.rds"
  ),
  figure_02_europe_maps.rds = here::here(
    "artifacts", "reporting", "inputs", "figure_02_europe_maps.rds"
  ),
  figure_03_north_america_maps.rds = here::here(
    "artifacts", "reporting", "inputs", "figure_03_north_america_maps.rds"
  ),
  figure_04_vaccination_pscore.csv = here::here(
    "artifacts", "reporting", "inputs", "figure_04_vaccination_pscore.csv"
  ),
  figure_05_sex_difference.csv = here::here(
    "artifacts", "reporting", "inputs", "figure_05_sex_difference.csv"
  ),
  figure_05_europe_historical_incoherent.csv = here::here(
    "artifacts",
    "reporting",
    "validation",
    "figure_05_europe_historical_incoherent.csv"
  ),
  europe_wave_summary.csv = here::here(
    "artifacts", "results", "europe", "wave_summary.csv"
  ),
  us_wave_summary.csv = here::here(
    "artifacts", "results", "us", "wave_summary.csv"
  ),
  europe_vaccination_membership.csv = here::here(
    "artifacts", "data", "europe", "vaccination_membership.csv"
  ),
  us_vaccination_membership.csv = here::here(
    "artifacts", "data", "us", "vaccination_membership.csv"
  )
)
for (source_name in names(runtime_paths)) {
  destination <- runtime_paths[[source_name]]
  dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)
  copied <- file.copy(
    file.path(reporting_input_root, source_name),
    destination,
    overwrite = TRUE
  )
  if (!copied) {
    stop("Failed to install reporting input: ", source_name, ".")
  }
}

output_status <- tibble::tribble(
  ~output_id, ~dependency_status, ~render_status, ~note,
  "figure_01", "complete", "ready", "Standardized from the Netherlands GE80 fitted model.",
  "figure_02", "complete", "ready", "European wave summaries and map geometry are bundled.",
  "figure_03", "complete", "ready", "US and Canadian wave summaries and map geometry are bundled.",
  "figure_04", "complete", "ready_with_caption_change", "All Ages represents ages 40-79.",
  "figure_05", "complete", "ready", "The European combined-age panel uses ages 40-79 for both observed and expected deaths; the incompatible historical panel is preserved separately for comparison.",
  "table_01", "complete", "ready", "US Omicron values are rebuilt from post-2022 posterior predictions."
)
write_bundle_readme(bundle_root, source_manifest, output_status)

bundle_files <- list.files(bundle_root, recursive = TRUE, full.names = TRUE)
bundle_files <- bundle_files[file.info(bundle_files)$isdir %in% FALSE]
bundle_files <- bundle_files[basename(bundle_files) != "sha256.csv"]
checksum <- tibble::tibble(
  path = substring(
    normalizePath(bundle_files),
    nchar(normalizePath(bundle_root)) + 2L
  ),
  bytes = file.info(bundle_files)$size,
  sha256 = vapply(
    bundle_files,
    digest::digest,
    character(1),
    algo = "sha256",
    file = TRUE
  )
)
readr::write_csv(checksum, file.path(bundle_root, "manifests", "sha256.csv"))

message("Prepared manuscript bundle: ", bundle_root)
message("Bundled source artifacts: ", nrow(source_manifest))
message("Bundled source size (GiB): ", round(sum(source_manifest$bytes) / 1024^3, 3))
