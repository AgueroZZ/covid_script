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
  library(sGPfit)
  library(TMB)
})

source(here::here("R", "config.R"))
source(here::here("R", "waves.R"))
source(here::here("R", "reporting.R"))
source(here::here("R", "figures.R"))

parse_arguments <- function(arguments) {
  defaults <- list(
    archive_root = Sys.getenv(
      "COVID_HISTORICAL_ARCHIVE",
      unset = here::here("output", "legacy", "historical_archive")
    ),
    output_root = here::here(
      "output",
      "validation",
      "figure_01_europe_seasonal_prior"
    ),
    seed = "20260829",
    force_refit = "false"
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

as_flag <- function(value, label) {
  normalized <- tolower(value)
  if (!normalized %in% c("true", "false")) {
    stop(label, " must be true or false.")
  }
  identical(normalized, "true")
}

load_rda_object <- function(path, object_name) {
  environment <- new.env(parent = emptyenv())
  loaded <- load(path, envir = environment)
  if (!object_name %in% loaded) {
    stop("Object '", object_name, "' was not found in ", path, ".")
  }
  environment[[object_name]]
}

require_exact_historical_model <- function(model_list) {
  expected <- list(
    country = "NL",
    p = 2,
    m = 4,
    k_IWP = 100,
    k_sGP = 40
  )
  for (name in names(expected)) {
    if (!isTRUE(all.equal(model_list[[name]], expected[[name]]))) {
      stop(
        "Historical Figure 1 model has unexpected ", name, ": ",
        paste(model_list[[name]], collapse = ", "), "."
      )
    }
  }
  data <- model_list$full_data
  if (!all(data$geo == "NL") || !all(data$age == "Y_GE80") ||
      !all(data$sex == "T")) {
    stop("Historical Figure 1 data are not exactly NL / Y_GE80 / T.")
  }
  training <- model_list$data
  if (max(training$date) >= as.Date("2020-01-01")) {
    stop("Historical Figure 1 training data extend into 2020 or later.")
  }
  invisible(model_list)
}

compile_template <- function(output_root, force_rebuild = FALSE) {
  build_root <- file.path(output_root, "build")
  dir.create(build_root, recursive = TRUE, showWarnings = FALSE)
  source_path <- here::here("code", "regions", "europe", "bayesgp_model.cpp")
  staged_source <- file.path(build_root, "tut.cpp")
  source_hash <- digest::digest(file = source_path, algo = "sha256")
  hash_path <- file.path(build_root, "tut.cpp.sha256")
  prior_hash <- if (file.exists(hash_path)) readLines(hash_path, warn = FALSE) else ""
  dll_path <- TMB::dynlib(file.path(build_root, "tut"))
  rebuild <- force_rebuild || !file.exists(dll_path) ||
    !identical(prior_hash, source_hash)

  if (rebuild) {
    copied <- file.copy(source_path, staged_source, overwrite = TRUE)
    if (!copied) {
      stop("Failed to stage the Europe TMB template.")
    }
    previous_directory <- getwd()
    on.exit(setwd(previous_directory), add = TRUE)
    setwd(build_root)
    message("Compiling the repository Europe TMB template in ", build_root, ".")
    TMB::compile("tut.cpp", flags = "-O2")
    writeLines(source_hash, hash_path)
    setwd(previous_directory)
  }
  if (!file.exists(dll_path)) {
    stop("Compiled Europe TMB library was not created at ", dll_path, ".")
  }
  dyn.load(dll_path)
  dll_path
}

summarize_component <- function(prediction, model_name, component_name) {
  tibble::tibble(
    model = model_name,
    component = component_name,
    date = as.Date(prediction$summary$time),
    mean = prediction$summary$mean,
    lower = prediction$summary$lower,
    upper = prediction$summary$upper
  )
}

summarize_pscore <- function(prediction, observed, model_name) {
  observed_matrix <- matrix(
    observed,
    nrow = nrow(prediction$samples),
    ncol = ncol(prediction$samples)
  )
  draws <- (observed_matrix - prediction$samples) /
    (prediction$samples + .Machine$double.eps)
  list(
    draws = draws,
    summary = tibble::tibble(
      model = model_name,
      date = as.Date(prediction$summary$time),
      median = apply(draws, 1, stats::median),
      lower = apply(draws, 1, stats::quantile, probs = 0.025),
      upper = apply(draws, 1, stats::quantile, probs = 0.975)
    )
  )
}

summarize_wave_pscore <- function(prediction, observed, dates, model_name, config) {
  wave <- assign_wave(dates, config)
  rows <- lapply(stats::na.omit(unique(wave)), function(wave_name) {
    selected <- which(wave == wave_name)
    expected <- prediction$samples[selected, , drop = FALSE]
    observed_total <- sum(observed[selected])
    draw_summary <- (observed_total - colSums(expected)) / colSums(expected)
    tibble::tibble(
      model = model_name,
      wave = wave_name,
      start = min(dates[selected]),
      end = max(dates[selected]),
      observations = length(selected),
      median = stats::median(draw_summary),
      lower = stats::quantile(draw_summary, 0.025),
      upper = stats::quantile(draw_summary, 0.975)
    )
  })
  dplyr::bind_rows(rows)
}

figure_input <- function(overall, trend, seasonal, observed) {
  list(
    overall = tibble::tibble(
      date = as.Date(overall$summary$time),
      observed = observed,
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

draw_interval <- function(date, lower, upper, color, alpha = 0.13) {
  graphics::polygon(
    c(date, rev(date)),
    c(lower, rev(upper)),
    border = NA,
    col = grDevices::adjustcolor(color, alpha.f = alpha)
  )
}

render_paired_figure <- function(raw, corrected, output_pdf) {
  components <- c("overall", "trend", "seasonal")
  labels <- c("weekly deaths", "seasonally adjusted deaths", "(log) relative rate")
  titles <- c("Overall", "Trend", "Seasonal")
  draw <- function() {
    old_parameters <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(old_parameters), add = TRUE)
    graphics::par(mfrow = c(2, 3), mar = c(3.8, 4.2, 2.2, 1), las = 1)
    for (row in c("Historical raw prior", "Corrected PSD prior")) {
      selected_input <- if (row == "Historical raw prior") raw else corrected
      for (index in seq_along(components)) {
        component <- components[[index]]
        raw_values <- raw[[component]]
        corrected_values <- corrected[[component]]
        observed <- if (component == "overall") raw$overall$observed else NULL
        limits <- range(
          c(
            raw_values$lower,
            raw_values$upper,
            corrected_values$lower,
            corrected_values$upper,
            observed
          ),
          na.rm = TRUE
        )
        data <- selected_input[[component]]
        graphics::plot(
          data$date,
          data$mean,
          type = "n",
          xlab = "",
          ylab = labels[[index]],
          ylim = limits,
          bty = "l"
        )
        draw_interval(data$date, data$lower, data$upper, "grey35", 0.18)
        graphics::lines(data$date, data$mean, col = "#4C5FFF", lwd = 1)
        if (!is.null(observed)) {
          graphics::points(data$date, observed, pch = 1, cex = 0.3, col = "grey25")
        }
        graphics::abline(v = as.Date("2020-01-01"), col = "#C77CFF", lty = 3)
        graphics::title(main = paste(row, titles[[index]], sep = ": "), cex.main = 0.9)
      }
    }
  }
  render_submission_figure(draw, output_pdf, width = 13, height = 7.4)
}

render_overlay_figure <- function(raw, corrected, raw_pscore, corrected_pscore, output_pdf) {
  panels <- list(
    list(raw = raw$overall, corrected = corrected$overall, y = "weekly deaths", title = "Overall"),
    list(raw = raw$trend, corrected = corrected$trend, y = "seasonally adjusted deaths", title = "Trend"),
    list(raw = raw$seasonal, corrected = corrected$seasonal, y = "(log) relative rate", title = "Seasonal"),
    list(raw = raw_pscore, corrected = corrected_pscore, y = "P-score", title = "Pointwise P-score")
  )
  draw <- function() {
    old_parameters <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(old_parameters), add = TRUE)
    graphics::par(mfrow = c(2, 2), mar = c(4, 4.3, 2.3, 1), las = 1)
    for (index in seq_along(panels)) {
      panel <- panels[[index]]
      raw_data <- panel$raw
      corrected_data <- panel$corrected
      raw_center <- if ("mean" %in% names(raw_data)) raw_data$mean else raw_data$median
      corrected_center <- if ("mean" %in% names(corrected_data)) corrected_data$mean else corrected_data$median
      limits <- range(
        c(raw_data$lower, raw_data$upper, corrected_data$lower, corrected_data$upper),
        na.rm = TRUE
      )
      graphics::plot(
        raw_data$date,
        raw_center,
        type = "n",
        xlab = "",
        ylab = panel$y,
        ylim = limits,
        bty = "l"
      )
      draw_interval(raw_data$date, raw_data$lower, raw_data$upper, "#4C5FFF")
      draw_interval(corrected_data$date, corrected_data$lower, corrected_data$upper, "#D55E00")
      graphics::lines(raw_data$date, raw_center, col = "#4C5FFF", lwd = 1)
      graphics::lines(corrected_data$date, corrected_center, col = "#D55E00", lwd = 1)
      if (index == 1L) {
        graphics::points(raw$overall$date, raw$overall$observed, pch = 1, cex = 0.25, col = "grey30")
      }
      if (index == 4L) {
        graphics::abline(h = 0, col = "grey55", lty = 2)
      }
      graphics::abline(v = as.Date("2020-01-01"), col = "#C77CFF", lty = 3)
      graphics::title(main = panel$title)
      if (index == 1L) {
        graphics::legend(
          "topright",
          legend = c("Historical raw prior", "Corrected PSD prior"),
          col = c("#4C5FFF", "#D55E00"),
          lwd = 2,
          bty = "n",
          cex = 0.8
        )
      }
    }
  }
  render_submission_figure(draw, output_pdf, width = 12, height = 8)
}

arguments <- parse_arguments(commandArgs(trailingOnly = TRUE))
seed <- suppressWarnings(as.integer(arguments$seed))
if (is.na(seed)) {
  stop("--seed must be an integer.")
}
force_refit <- as_flag(arguments$force_refit, "--force_refit")
output_root <- normalizePath(
  arguments$output_root,
  mustWork = FALSE
)
dir.create(output_root, recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(output_root, "models"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(output_root, "figures"), recursive = TRUE, showWarnings = FALSE)

historical_model_path <- file.path(
  arguments$archive_root,
  "Europe",
  "stratified",
  "script",
  "NL_Y_GE80_T_model_list.rda"
)
historical_prediction_path <- file.path(
  arguments$archive_root,
  "Europe",
  "stratified",
  "fitted_model",
  "NL_Y_GE80_T.rda"
)
reporting_require_files(
  c(historical_model_path, historical_prediction_path),
  "historical Figure 1 source"
)

dll_path <- compile_template(output_root, force_rebuild = force_refit)
source(here::here(
  "code", "regions", "europe", "model_functions.R"
), local = globalenv())

historical_model <- load_rda_object(historical_model_path, "model_list")
require_exact_historical_model(historical_model)

raw_prior <- list(u = 0.1, a = 0.01)
trend_psd_prior <- list(u = 0.1, a = 0.01)
trend_sd_prior <- OSplines:::prior_conversion_IWP(
  d = 5,
  prior = trend_psd_prior,
  p = 2
)
corrected_seasonal_prior <- prior_conversion_sGP_m(
  d = 1,
  prior = raw_prior,
  a = 2 * pi,
  m = 4
)

corrected_model_path <- file.path(
  output_root,
  "models",
  "NL_Y_GE80_T_corrected_seasonal_prior_model_list.rds"
)
if (file.exists(corrected_model_path) && !force_refit) {
  message("Loading cached corrected-prior pilot fit from ", corrected_model_path, ".")
  corrected_model <- readRDS(corrected_model_path)
} else {
  message("Fitting corrected-prior Netherlands Y_GE80 total model.")
  corrected_model <- fit_mod_IWP_sGP(
    world_death = historical_model$full_data,
    prior_IWP = trend_sd_prior,
    prior_sGP = corrected_seasonal_prior,
    prior_overdis = raw_prior,
    k_IWP = 100,
    k_sGP = 40,
    country = "NL",
    p = 2,
    a = 2 * pi,
    m = 4,
    accuracy = 0.001
  )
  saveRDS(corrected_model, corrected_model_path, compress = FALSE)
}
require_exact_historical_model(corrected_model)

message("Generating paired posterior summaries with fixed seeds.")
set.seed(seed)
historical_overall <- pred_mortality_obs(
  historical_model,
  refined_pred = historical_model$x_full,
  M1 = 3000,
  M2 = 1
)
set.seed(seed)
corrected_overall <- pred_mortality_obs(
  corrected_model,
  refined_pred = corrected_model$x_full,
  M1 = 3000,
  M2 = 1
)

set.seed(seed + 1L)
historical_trend <- pred_mortality(
  historical_model,
  component = "trend",
  scale = "original",
  refined_pred = historical_model$x_full
)
set.seed(seed + 1L)
corrected_trend <- pred_mortality(
  corrected_model,
  component = "trend",
  scale = "original",
  refined_pred = corrected_model$x_full
)

set.seed(seed + 2L)
historical_seasonal <- pred_mortality(
  historical_model,
  component = "seasonal",
  refined_pred = historical_model$x_full
)
set.seed(seed + 2L)
corrected_seasonal <- pred_mortality(
  corrected_model,
  component = "seasonal",
  refined_pred = corrected_model$x_full
)

dates <- as.Date(historical_overall$summary$time)
observed <- historical_model$full_data$OBS_VALUE
if (length(observed) != length(dates)) {
  stop("Observed and predicted Figure 1 time series have different lengths.")
}

historical_input <- figure_input(
  historical_overall,
  historical_trend,
  historical_seasonal,
  observed
)
corrected_input <- figure_input(
  corrected_overall,
  corrected_trend,
  corrected_seasonal,
  observed
)

historical_pscore <- summarize_pscore(historical_overall, observed, "historical_raw")
corrected_pscore <- summarize_pscore(corrected_overall, observed, "corrected_psd")
component_summary <- dplyr::bind_rows(
  summarize_component(historical_overall, "historical_raw", "overall"),
  summarize_component(corrected_overall, "corrected_psd", "overall"),
  summarize_component(historical_trend, "historical_raw", "trend"),
  summarize_component(corrected_trend, "corrected_psd", "trend"),
  summarize_component(historical_seasonal, "historical_raw", "seasonal"),
  summarize_component(corrected_seasonal, "corrected_psd", "seasonal")
)
pscore_summary <- dplyr::bind_rows(
  historical_pscore$summary,
  corrected_pscore$summary
)
config <- read_analysis_config(here::here("config", "analysis.yml"))
wave_summary <- dplyr::bind_rows(
  summarize_wave_pscore(
    historical_overall,
    observed,
    dates,
    "historical_raw",
    config
  ),
  summarize_wave_pscore(
    corrected_overall,
    observed,
    dates,
    "corrected_psd",
    config
  )
)

paired_components <- component_summary |>
  dplyr::select(model, component, date, mean) |>
  tidyr::pivot_wider(names_from = model, values_from = mean) |>
  dplyr::mutate(difference = corrected_psd - historical_raw)
paired_pscore <- pscore_summary |>
  dplyr::select(model, date, median) |>
  tidyr::pivot_wider(names_from = model, values_from = median) |>
  dplyr::mutate(difference = corrected_psd - historical_raw)

metric_rows <- paired_components |>
  dplyr::mutate(period = ifelse(date < as.Date("2020-01-01"), "training", "post_2020")) |>
  dplyr::group_by(component, period) |>
  dplyr::summarise(
    mean_difference = mean(difference),
    mean_absolute_difference = mean(abs(difference)),
    root_mean_square_difference = sqrt(mean(difference^2)),
    maximum_absolute_difference = max(abs(difference)),
    .groups = "drop"
  )
pscore_metric_rows <- paired_pscore |>
  dplyr::filter(date >= as.Date("2020-01-01")) |>
  dplyr::summarise(
    component = "pointwise_pscore",
    period = "post_2020",
    mean_difference = mean(difference),
    mean_absolute_difference = mean(abs(difference)),
    root_mean_square_difference = sqrt(mean(difference^2)),
    maximum_absolute_difference = max(abs(difference))
  )
metrics <- dplyr::bind_rows(metric_rows, pscore_metric_rows)

interval_width_metrics <- component_summary |>
  dplyr::mutate(
    interval_width = upper - lower,
    period = ifelse(date < as.Date("2020-01-01"), "training", "post_2020")
  ) |>
  dplyr::group_by(model, component, period) |>
  dplyr::summarise(
    mean_interval_width = mean(interval_width),
    .groups = "drop"
  ) |>
  dplyr::bind_rows(
    pscore_summary |>
      dplyr::mutate(
        interval_width = upper - lower,
        component = "pointwise_pscore",
        period = ifelse(date < as.Date("2020-01-01"), "training", "post_2020")
      ) |>
      dplyr::group_by(model, component, period) |>
      dplyr::summarise(
        mean_interval_width = mean(interval_width),
        .groups = "drop"
      )
  )

prior_parameters <- tibble::tibble(
  model = c("historical_raw", "corrected_psd"),
  semantic_scale = c("internal_sgp_sd", "one_year_predictive_sd"),
  declared_u = c(0.1, 0.1),
  alpha = c(0.01, 0.01),
  internal_u = c(0.1, corrected_seasonal_prior$u),
  equivalent_one_year_psd_u = c(
    0.1 * (0.1 / corrected_seasonal_prior$u),
    0.1
  )
)

readr::write_csv(component_summary, file.path(output_root, "component_timewise.csv"))
readr::write_csv(pscore_summary, file.path(output_root, "pointwise_pscore.csv"))
readr::write_csv(wave_summary, file.path(output_root, "wave_pscore.csv"))
readr::write_csv(metrics, file.path(output_root, "difference_metrics.csv"))
readr::write_csv(
  interval_width_metrics,
  file.path(output_root, "interval_width_metrics.csv")
)
readr::write_csv(prior_parameters, file.path(output_root, "prior_parameters.csv"))
saveRDS(historical_input, file.path(output_root, "figure_01_historical_raw_input.rds"))
saveRDS(corrected_input, file.path(output_root, "figure_01_corrected_psd_input.rds"))

historical_outputs <- render_figure_01(
  historical_input,
  file.path(output_root, "figures", "figure_01_historical_raw_prior.pdf")
)
corrected_outputs <- render_figure_01(
  corrected_input,
  file.path(output_root, "figures", "figure_01_corrected_psd_prior.pdf")
)
paired_outputs <- render_paired_figure(
  historical_input,
  corrected_input,
  file.path(output_root, "figures", "figure_01_prior_side_by_side.pdf")
)
overlay_outputs <- render_overlay_figure(
  historical_input,
  corrected_input,
  historical_pscore$summary,
  corrected_pscore$summary,
  file.path(output_root, "figures", "figure_01_prior_overlay.pdf")
)

convergence_value <- function(model_list) {
  value <- model_list$model$optresults$convergence
  if (is.null(value)) NA_integer_ else as.integer(value)
}
hyperparameter_modes <- dplyr::bind_rows(
  lapply(
    list(historical_raw = historical_model, corrected_psd = corrected_model),
    function(model_list) {
      mode <- model_list$model$optresults$mode
      tibble::tibble(
        hyperparameter = c(
          "theta_trend",
          "theta_seasonal",
          "theta_overdispersion"
        ),
        theta_mode = as.numeric(mode),
        sd_mode = exp(-0.5 * as.numeric(mode))
      )
    }
  ),
  .id = "model"
)
readr::write_csv(
  hyperparameter_modes,
  file.path(output_root, "hyperparameter_modes.csv")
)
provenance <- list(
  created_at = format(Sys.time(), tz = "America/Chicago", usetz = TRUE),
  analysis = list(
    geography = "NL",
    age_group = "Y_GE80",
    sex = "T",
    training_end = max(historical_model$data$date),
    prediction_end = max(historical_model$full_data$date),
    p = 2,
    m = 4,
    k_IWP = 100,
    k_sGP = 40,
    seed = seed
  ),
  priors = prior_parameters,
  convergence = list(
    historical_raw = convergence_value(historical_model),
    corrected_psd = convergence_value(corrected_model)
  ),
  hyperparameter_modes = hyperparameter_modes,
  inputs = list(
    historical_model_path = normalizePath(historical_model_path),
    historical_model_sha256 = digest::digest(
      file = historical_model_path,
      algo = "sha256"
    ),
    historical_prediction_path = normalizePath(historical_prediction_path),
    historical_prediction_sha256 = digest::digest(
      file = historical_prediction_path,
      algo = "sha256"
    ),
    tmb_source_path = normalizePath(here::here(
      "code", "regions", "europe", "bayesgp_model.cpp"
    )),
    tmb_source_sha256 = digest::digest(
      file = here::here("code", "regions", "europe", "bayesgp_model.cpp"),
      algo = "sha256"
    ),
    compiled_dll_path = normalizePath(dll_path)
  ),
  packages = vapply(
    c("aghq", "OSplines", "sGPfit", "TMB"),
    function(package) as.character(utils::packageVersion(package)),
    character(1)
  ),
  outputs = c(historical_outputs, corrected_outputs, paired_outputs, overlay_outputs),
  session_info = utils::sessionInfo()
)
saveRDS(provenance, file.path(output_root, "provenance.rds"))
writeLines("complete", file.path(output_root, "complete.flag"))

message("Completed Figure 1 seasonal-prior pilot: ", output_root)
