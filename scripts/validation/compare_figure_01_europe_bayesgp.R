#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(aghq)
  library(BayesGP)
  library(digest)
  library(dplyr)
  library(here)
  library(ISOweek)
  library(lubridate)
  library(Matrix)
  library(OSplines)
  library(sGPfit)
  library(TMB)
})

source(here::here("R", "config.R"))
source(here::here("R", "waves.R"))
source(here::here("R", "reporting.R"))
source(here::here("R", "figures.R"))

required_bayesgp_version <- "0.1.3"

equivalence_thresholds <- list(
  matrix_max_absolute_difference = 1e-8,
  theta_mode_max_absolute_difference = 0.02,
  overall_post_2020_mean_absolute_relative_difference = 0.005,
  overall_post_2020_max_absolute_relative_difference = 0.02,
  trend_post_2020_mean_absolute_relative_difference = 0.005,
  seasonal_post_2020_mean_absolute_difference = 0.002,
  wave_median_pscore_max_absolute_difference = 0.005,
  wave_interval_endpoint_max_absolute_difference = 0.01
)

parse_arguments <- function(arguments) {
  defaults <- list(
    archive_root = Sys.getenv(
      "COVID_HISTORICAL_ARCHIVE",
      unset = here::here("output", "legacy", "historical_archive")
    ),
    custom_model_path = here::here(
      "output",
      "validation",
      "figure_01_europe_seasonal_prior",
      "models",
      "NL_Y_GE80_T_corrected_seasonal_prior_model_list.rds"
    ),
    output_root = here::here(
      "output",
      "validation",
      "figure_01_europe_bayesgp_equivalence"
    ),
    seed = "20260829",
    force_refit = "false"
  )
  if (length(arguments) == 0L) {
    return(defaults)
  }
  if (!all(grepl("^--[A-Za-z0-9_-]=?.+$", arguments))) {
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

reconstruct_locked_model_from_csv <- function(path) {
  full_data <- utils::read.csv(path) |>
    dplyr::filter(
      geo == "NL",
      age == "Y_GE80",
      sex == "T"
    )
  full_data$date <- ISOweek::ISOweek2date(
    paste0(full_data$TIME_PERIOD, "-1")
  )
  full_data$Year <- lubridate::year(full_data$date)
  x_full <- (
    as.numeric(full_data$date) - min(as.numeric(full_data$date))
  ) / 365
  full_data$x <- x_full
  training_data <- full_data |>
    dplyr::filter(Year < 2020)

  list(
    country = "NL",
    p = 2,
    m = 4,
    k_IWP = 100,
    k_sGP = 40,
    full_data = full_data,
    data = training_data,
    x_full = x_full
  )
}

require_locked_model <- function(model_list) {
  expected <- list(
    country = "NL",
    p = 2,
    m = 4,
    k_IWP = 100,
    k_sGP = 40
  )
  for (name in names(expected)) {
    if (!isTRUE(all.equal(model_list[[name]], expected[[name]]))) {
      stop("Unexpected ", name, " in the locked Figure 1 model.")
    }
  }
  if (!all(model_list$full_data$geo == "NL") ||
      !all(model_list$full_data$age == "Y_GE80") ||
      !all(model_list$full_data$sex == "T")) {
    stop("The locked data are not exactly NL / Y_GE80 / T.")
  }
  if (max(model_list$data$date) >= as.Date("2020-01-01")) {
    stop("The locked training data extend into 2020 or later.")
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
    TMB::compile("tut.cpp", flags = "-O2")
    writeLines(source_hash, hash_path)
    setwd(previous_directory)
  }
  if (!file.exists(dll_path)) {
    stop("The compiled Europe TMB library was not created.")
  }
  dyn.load(dll_path)
  dll_path
}

maximum_absolute_difference <- function(left, right) {
  if (!identical(dim(left), dim(right))) {
    return(Inf)
  }
  max(abs(as.matrix(left) - as.matrix(right)))
}

summary_from_samples <- function(samples, dates) {
  list(
    samples = samples,
    summary = tibble::tibble(
      time = as.Date(dates),
      mean = rowMeans(as.matrix(samples)),
      lower = apply(samples, 1, stats::quantile, probs = 0.025),
      upper = apply(samples, 1, stats::quantile, probs = 0.975)
    )
  )
}

bayesgp_component_samples <- function(model, refined_x, seed, draws = 3000L) {
  set.seed(seed)
  posterior <- aghq::sample_marginal(model$mod, M = draws)
  iwp <- model$instances[[1]]
  sgp <- model$instances[[2]]
  trend <- as.matrix(
    BayesGP:::compute_post_fun_iwp(
      samps = posterior$samps[model$random_samp_indexes[[1]], , drop = FALSE],
      knots = iwp@knots,
      refined_x = refined_x,
      global_samps = posterior$samps[
        model$boundary_samp_indexes[[1]],
        ,
        drop = FALSE
      ],
      intercept_samps = posterior$samps[
        model$fixed_samp_indexes$intercept,
        ,
        drop = FALSE
      ],
      p = iwp@order
    )[, -1, drop = FALSE]
  )
  seasonal <- as.matrix(
    BayesGP:::compute_post_fun_sgp(
      samps = posterior$samps[model$random_samp_indexes[[2]], , drop = FALSE],
      k = sgp@k,
      a = sgp@a,
      m = sgp@m,
      region = sgp@region,
      refined_x = refined_x,
      global_samps = posterior$samps[
        model$boundary_samp_indexes[[2]],
        ,
        drop = FALSE
      ]
    )[, -1, drop = FALSE]
  )
  list(
    posterior = posterior,
    trend = trend,
    seasonal = seasonal
  )
}

bayesgp_predictions <- function(model, refined_x, dates, seed, draws = 3000L) {
  overall_components <- bayesgp_component_samples(
    model,
    refined_x,
    seed,
    draws
  )
  latent <- overall_components$trend + overall_components$seasonal
  overdispersion_sd <- exp(
    -0.5 * overall_components$posterior$thetasamples[[3]]
  )
  iid_noise <- stats::rnorm(
    draws * nrow(latent),
    sd = rep(overdispersion_sd, each = nrow(latent))
  )
  iid_noise <- matrix(iid_noise, nrow = nrow(latent), ncol = draws)
  rate <- exp(latent + iid_noise)
  overall_samples <- matrix(
    stats::rpois(length(rate), lambda = as.numeric(rate)),
    nrow = nrow(rate),
    ncol = ncol(rate)
  )

  trend_components <- bayesgp_component_samples(
    model,
    refined_x,
    seed + 1L,
    draws
  )
  seasonal_components <- bayesgp_component_samples(
    model,
    refined_x,
    seed + 2L,
    draws
  )

  list(
    overall = summary_from_samples(overall_samples, dates),
    trend = summary_from_samples(exp(trend_components$trend), dates),
    seasonal = summary_from_samples(seasonal_components$seasonal, dates)
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
    pscore <- (observed_total - colSums(expected)) / colSums(expected)
    tibble::tibble(
      model = model_name,
      wave = wave_name,
      median = stats::median(pscore),
      lower = stats::quantile(pscore, 0.025),
      upper = stats::quantile(pscore, 0.975)
    )
  })
  dplyr::bind_rows(rows)
}

metric_row <- function(metric, value, threshold, structural = FALSE) {
  tibble::tibble(
    metric = metric,
    value = as.numeric(value),
    threshold = as.numeric(threshold),
    operator = "<=",
    structural = structural,
    pass = is.finite(value) && value <= threshold
  )
}

write_validation_csv <- function(data, path) {
  utils::write.csv(data, path, row.names = FALSE, na = "")
}

draw_interval <- function(date, lower, upper, color, alpha = 0.12) {
  graphics::polygon(
    c(date, rev(date)),
    c(lower, rev(upper)),
    border = NA,
    col = grDevices::adjustcolor(color, alpha.f = alpha)
  )
}

render_overlay <- function(custom, bayesgp, custom_pscore, bayesgp_pscore, output_pdf) {
  panels <- list(
    list(custom = custom$overall$summary, bayesgp = bayesgp$overall$summary, title = "Overall", y = "weekly deaths"),
    list(custom = custom$trend$summary, bayesgp = bayesgp$trend$summary, title = "Trend", y = "seasonally adjusted deaths"),
    list(custom = custom$seasonal$summary, bayesgp = bayesgp$seasonal$summary, title = "Seasonal", y = "(log) relative rate"),
    list(custom = custom_pscore, bayesgp = bayesgp_pscore, title = "Pointwise P-score", y = "P-score")
  )
  draw <- function() {
    old_parameters <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(old_parameters), add = TRUE)
    graphics::par(mfrow = c(2, 2), mar = c(4, 4.2, 2.2, 1), las = 1)
    for (index in seq_along(panels)) {
      panel <- panels[[index]]
      custom_center <- if ("mean" %in% names(panel$custom)) {
        panel$custom$mean
      } else {
        panel$custom$median
      }
      bayesgp_center <- if ("mean" %in% names(panel$bayesgp)) {
        panel$bayesgp$mean
      } else {
        panel$bayesgp$median
      }
      custom_date <- if ("time" %in% names(panel$custom)) panel$custom$time else panel$custom$date
      bayesgp_date <- if ("time" %in% names(panel$bayesgp)) panel$bayesgp$time else panel$bayesgp$date
      limits <- range(
        c(
          panel$custom$lower,
          panel$custom$upper,
          panel$bayesgp$lower,
          panel$bayesgp$upper
        ),
        na.rm = TRUE
      )
      graphics::plot(
        custom_date,
        custom_center,
        type = "n",
        xlab = "",
        ylab = panel$y,
        ylim = limits,
        bty = "l"
      )
      draw_interval(custom_date, panel$custom$lower, panel$custom$upper, "#4C5FFF")
      draw_interval(bayesgp_date, panel$bayesgp$lower, panel$bayesgp$upper, "#D55E00")
      graphics::lines(custom_date, custom_center, col = "#4C5FFF", lwd = 1)
      graphics::lines(bayesgp_date, bayesgp_center, col = "#D55E00", lwd = 1)
      graphics::abline(v = as.Date("2020-01-01"), col = "#C77CFF", lty = 3)
      if (index == 4L) {
        graphics::abline(h = 0, col = "grey55", lty = 2)
      }
      graphics::title(main = panel$title)
      if (index == 1L) {
        graphics::legend(
          "topright",
          legend = c("Corrected custom TMB", "BayesGP 0.1.3"),
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
if (!identical(as.character(utils::packageVersion("BayesGP")), required_bayesgp_version)) {
  stop(
    "BayesGP ", required_bayesgp_version, " is required; found ",
    as.character(utils::packageVersion("BayesGP")), "."
  )
}

output_root <- normalizePath(arguments$output_root, mustWork = FALSE)
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
historical_csv_path <- file.path(
  arguments$archive_root,
  "Europe",
  "stratified",
  "demo_r_mwk_20_linear.csv"
)
if (file.exists(historical_model_path)) {
  historical_input_path <- historical_model_path
  historical_input_type <- "locked_model_object"
  historical_model <- load_rda_object(historical_model_path, "model_list")
} else {
  reporting_require_files(historical_csv_path, "historical Eurostat input")
  historical_input_path <- historical_csv_path
  historical_input_type <- "reconstructed_from_csv"
  historical_model <- reconstruct_locked_model_from_csv(historical_csv_path)
}
require_locked_model(historical_model)

dll_path <- compile_template(output_root, force_rebuild = force_refit)
source(here::here(
  "code", "regions", "europe", "model_functions.R"
), local = globalenv())

raw_custom_prior <- list(u = 0.1, a = 0.01)
trend_bayesgp_conversion <- BayesGP::prior_conversion_iwp(
  d = 5,
  prior = list(u = 0.1, alpha = 0.01),
  p = 2
)
trend_custom_prior <- list(
  u = trend_bayesgp_conversion$u,
  a = trend_bayesgp_conversion$alpha
)
seasonal_custom_prior <- prior_conversion_sGP_m(
  d = 1,
  prior = raw_custom_prior,
  a = 2 * pi,
  m = 4
)

custom_model_path <- arguments$custom_model_path
if (!file.exists(custom_model_path)) {
  custom_model_path <- file.path(
    output_root,
    "models",
    "NL_Y_GE80_T_corrected_custom_tmb_model.rds"
  )
}
if (file.exists(custom_model_path) && !force_refit) {
  message("Loading corrected custom-TMB model from ", custom_model_path, ".")
  custom_model <- readRDS(custom_model_path)
} else {
  message("Fitting corrected custom-TMB comparison model.")
  custom_model <- fit_mod_IWP_sGP(
    world_death = historical_model$full_data,
    prior_IWP = trend_custom_prior,
    prior_sGP = seasonal_custom_prior,
    prior_overdis = raw_custom_prior,
    k_IWP = 100,
    k_sGP = 40,
    country = "NL",
    p = 2,
    a = 2 * pi,
    m = 4,
    accuracy = 0.001
  )
  saveRDS(custom_model, custom_model_path, compress = FALSE)
}
require_locked_model(custom_model)

training_data <- custom_model$data |>
  dplyr::mutate(
    x1 = x,
    x2 = x,
    observation_id = seq_len(dplyr::n())
  )
full_region <- range(custom_model$x_full)

trend_prior <- list(
  prior = "exp",
  param = list(u = 0.1, alpha = 0.01),
  h = 5
)
seasonal_prior <- list(
  prior = "exp",
  param = list(u = 0.1, alpha = 0.01),
  h = 1
)
overdispersion_prior <- list(
  prior = "exp",
  param = list(u = 0.1, alpha = 0.01)
)

bayesgp_model_path <- file.path(
  output_root,
  "models",
  "NL_Y_GE80_T_corrected_bayesgp_0.1.3_model.rds"
)
if (file.exists(bayesgp_model_path) && !force_refit) {
  message("Loading BayesGP comparison model from ", bayesgp_model_path, ".")
  bayesgp_model <- readRDS(bayesgp_model_path)
} else {
  message("Fitting BayesGP 0.1.3 comparison model.")
  set.seed(seed)
  bayesgp_model <- BayesGP::model_fit(
    OBS_VALUE ~
      f(
        x1,
        model = "IWP",
        order = 2,
        sd.prior = trend_prior,
        boundary.prior = list(prec = 0.001, mean = 0),
        k = 100,
        initial_location = "left",
        region = full_region
      ) +
      f(
        x2,
        model = "sGP",
        a = 2 * pi,
        k = 40,
        sd.prior = seasonal_prior,
        boundary.prior = list(prec = 0.001, mean = 0),
        m = 4,
        accuracy = 0.001,
        region = full_region
      ) +
      f(
        observation_id,
        model = "IID",
        sd.prior = overdispersion_prior
      ),
    data = training_data,
    family = "Poisson",
    control.family = NULL,
    control.fixed = list(intercept = list(prec = 0.001, mean = 0)),
    aghq_k = 5,
    M = 3000
  )
  saveRDS(bayesgp_model, bayesgp_model_path, compress = FALSE)
}

custom_knots <- seq(
  min(custom_model$data$x),
  max(custom_model$x_full),
  length.out = 100
)
custom_iwp_x <- OSplines::global_poly_helper(custom_model$data$x, p = 2)
custom_iwp_b <- OSplines:::local_poly_helper(
  knots = custom_knots,
  refined_x = custom_model$data$x,
  p = 2
)
custom_iwp_p <- compute_weights_precision(custom_knots)
custom_sgp <- create_sGP_design_m_sB(
  k = 40,
  a = 2 * pi,
  m = 4,
  refined_x = custom_model$data$x,
  knots_range = full_region
)
custom_sgp_p <- create_sGP_prec_m_sB(
  k = 40,
  a = 2 * pi,
  m = 4,
  knots_range = full_region,
  accuracy = 0.001
)

bayesgp_iwp <- bayesgp_model$instances[[1]]
bayesgp_sgp <- bayesgp_model$instances[[2]]
bayesgp_iwp_x <- cbind(1, bayesgp_iwp@X)

structural_rows <- dplyr::bind_rows(
  metric_row(
    "training_response_max_absolute_difference",
    max(abs(custom_model$data$OBS_VALUE - training_data$OBS_VALUE)),
    0,
    TRUE
  ),
  metric_row(
    "training_time_max_absolute_difference",
    max(abs(custom_model$data$x - training_data$x)),
    0,
    TRUE
  ),
  metric_row(
    "iwp_x_max_absolute_difference",
    maximum_absolute_difference(custom_iwp_x, bayesgp_iwp_x),
    equivalence_thresholds$matrix_max_absolute_difference,
    TRUE
  ),
  metric_row(
    "iwp_b_max_absolute_difference",
    maximum_absolute_difference(custom_iwp_b, bayesgp_iwp@B),
    equivalence_thresholds$matrix_max_absolute_difference,
    TRUE
  ),
  metric_row(
    "iwp_p_max_absolute_difference",
    maximum_absolute_difference(custom_iwp_p, bayesgp_iwp@P),
    equivalence_thresholds$matrix_max_absolute_difference,
    TRUE
  ),
  metric_row(
    "sgp_x_max_absolute_difference",
    maximum_absolute_difference(custom_sgp$X, bayesgp_sgp@X),
    equivalence_thresholds$matrix_max_absolute_difference,
    TRUE
  ),
  metric_row(
    "sgp_b_max_absolute_difference",
    maximum_absolute_difference(custom_sgp$B, bayesgp_sgp@B),
    equivalence_thresholds$matrix_max_absolute_difference,
    TRUE
  ),
  metric_row(
    "sgp_p_max_absolute_difference",
    maximum_absolute_difference(custom_sgp_p, bayesgp_sgp@P),
    equivalence_thresholds$matrix_max_absolute_difference,
    TRUE
  ),
  metric_row(
    "iwp_internal_prior_u_absolute_difference",
    abs(trend_custom_prior$u - bayesgp_iwp@sd.prior$param$u),
    abs(trend_custom_prior$u) * 1e-10,
    TRUE
  ),
  metric_row(
    "sgp_internal_prior_u_absolute_difference",
    abs(seasonal_custom_prior$u - bayesgp_sgp@sd.prior$param$u),
    abs(seasonal_custom_prior$u) * 1e-10,
    TRUE
  )
)
write_validation_csv(
  structural_rows,
  file.path(output_root, "structural_equivalence.csv")
)

dates <- as.Date(custom_model$full_data$date)
observed <- custom_model$full_data$OBS_VALUE
set.seed(seed)
custom_overall <- pred_mortality_obs(
  custom_model,
  refined_pred = custom_model$x_full,
  M1 = 3000,
  M2 = 1
)
set.seed(seed + 1L)
custom_trend <- pred_mortality(
  custom_model,
  component = "trend",
  scale = "original",
  refined_pred = custom_model$x_full
)
set.seed(seed + 2L)
custom_seasonal <- pred_mortality(
  custom_model,
  component = "seasonal",
  refined_pred = custom_model$x_full
)
custom_predictions <- list(
  overall = custom_overall,
  trend = custom_trend,
  seasonal = custom_seasonal
)
bayesgp_prediction <- bayesgp_predictions(
  bayesgp_model,
  custom_model$x_full,
  dates,
  seed,
  draws = 3000
)

post_2020 <- dates >= as.Date("2020-01-01")
overall_relative_difference <- (
  bayesgp_prediction$overall$summary$mean - custom_overall$summary$mean
) / pmax(abs(custom_overall$summary$mean), .Machine$double.eps)
trend_relative_difference <- (
  bayesgp_prediction$trend$summary$mean - custom_trend$summary$mean
) / pmax(abs(custom_trend$summary$mean), .Machine$double.eps)
seasonal_difference <- bayesgp_prediction$seasonal$summary$mean -
  custom_seasonal$summary$mean

custom_pscore <- summarize_pscore(custom_overall, observed, "custom_tmb")
bayesgp_pscore <- summarize_pscore(
  bayesgp_prediction$overall,
  observed,
  "bayesgp_0.1.3"
)
config <- read_analysis_config(here::here("config", "analysis.yml"))
wave_summary <- dplyr::bind_rows(
  summarize_wave_pscore(
    custom_overall,
    observed,
    dates,
    "custom_tmb",
    config
  ),
  summarize_wave_pscore(
    bayesgp_prediction$overall,
    observed,
    dates,
    "bayesgp_0.1.3",
    config
  )
)
wave_comparison <- wave_summary |>
  tidyr::pivot_wider(
    names_from = model,
    values_from = c(median, lower, upper)
  ) |>
  dplyr::mutate(
    median_difference = `median_bayesgp_0.1.3` - median_custom_tmb,
    lower_difference = `lower_bayesgp_0.1.3` - lower_custom_tmb,
    upper_difference = `upper_bayesgp_0.1.3` - upper_custom_tmb
  )
write_validation_csv(
  wave_comparison,
  file.path(output_root, "wave_equivalence.csv")
)

custom_theta <- as.numeric(custom_model$model$optresults$mode)
bayesgp_theta <- as.numeric(bayesgp_model$mod$optresults$mode)
numerical_rows <- dplyr::bind_rows(
  metric_row(
    "theta_mode_max_absolute_difference",
    max(abs(custom_theta - bayesgp_theta)),
    equivalence_thresholds$theta_mode_max_absolute_difference
  ),
  metric_row(
    "overall_post_2020_mean_absolute_relative_difference",
    mean(abs(overall_relative_difference[post_2020])),
    equivalence_thresholds$overall_post_2020_mean_absolute_relative_difference
  ),
  metric_row(
    "overall_post_2020_max_absolute_relative_difference",
    max(abs(overall_relative_difference[post_2020])),
    equivalence_thresholds$overall_post_2020_max_absolute_relative_difference
  ),
  metric_row(
    "trend_post_2020_mean_absolute_relative_difference",
    mean(abs(trend_relative_difference[post_2020])),
    equivalence_thresholds$trend_post_2020_mean_absolute_relative_difference
  ),
  metric_row(
    "seasonal_post_2020_mean_absolute_difference",
    mean(abs(seasonal_difference[post_2020])),
    equivalence_thresholds$seasonal_post_2020_mean_absolute_difference
  ),
  metric_row(
    "wave_median_pscore_max_absolute_difference",
    max(abs(wave_comparison$median_difference)),
    equivalence_thresholds$wave_median_pscore_max_absolute_difference
  ),
  metric_row(
    "wave_interval_endpoint_max_absolute_difference",
    max(abs(c(wave_comparison$lower_difference, wave_comparison$upper_difference))),
    equivalence_thresholds$wave_interval_endpoint_max_absolute_difference
  )
)
write_validation_csv(
  numerical_rows,
  file.path(output_root, "numerical_equivalence.csv")
)

decision_rows <- dplyr::bind_rows(structural_rows, numerical_rows)
overall_pass <- all(decision_rows$pass)
decision <- tibble::tibble(
  implementation = "BayesGP 0.1.3",
  reference = "corrected custom Europe TMB",
  structural_pass = all(structural_rows$pass),
  numerical_pass = all(numerical_rows$pass),
  overall_pass = overall_pass
)
write_validation_csv(
  decision,
  file.path(output_root, "equivalence_decision.csv")
)

component_timewise <- dplyr::bind_rows(
  tibble::tibble(
    implementation = "custom_tmb",
    component = "overall",
    date = dates,
    mean = custom_overall$summary$mean,
    lower = custom_overall$summary$lower,
    upper = custom_overall$summary$upper
  ),
  tibble::tibble(
    implementation = "bayesgp_0.1.3",
    component = "overall",
    date = dates,
    mean = bayesgp_prediction$overall$summary$mean,
    lower = bayesgp_prediction$overall$summary$lower,
    upper = bayesgp_prediction$overall$summary$upper
  ),
  tibble::tibble(
    implementation = "custom_tmb",
    component = "trend",
    date = dates,
    mean = custom_trend$summary$mean,
    lower = custom_trend$summary$lower,
    upper = custom_trend$summary$upper
  ),
  tibble::tibble(
    implementation = "bayesgp_0.1.3",
    component = "trend",
    date = dates,
    mean = bayesgp_prediction$trend$summary$mean,
    lower = bayesgp_prediction$trend$summary$lower,
    upper = bayesgp_prediction$trend$summary$upper
  ),
  tibble::tibble(
    implementation = "custom_tmb",
    component = "seasonal",
    date = dates,
    mean = custom_seasonal$summary$mean,
    lower = custom_seasonal$summary$lower,
    upper = custom_seasonal$summary$upper
  ),
  tibble::tibble(
    implementation = "bayesgp_0.1.3",
    component = "seasonal",
    date = dates,
    mean = bayesgp_prediction$seasonal$summary$mean,
    lower = bayesgp_prediction$seasonal$summary$lower,
    upper = bayesgp_prediction$seasonal$summary$upper
  )
)
write_validation_csv(
  component_timewise,
  file.path(output_root, "component_timewise.csv")
)
write_validation_csv(
  dplyr::bind_rows(custom_pscore$summary, bayesgp_pscore$summary),
  file.path(output_root, "pointwise_pscore.csv")
)

figure_outputs <- render_overlay(
  custom_predictions,
  bayesgp_prediction,
  custom_pscore$summary,
  bayesgp_pscore$summary,
  file.path(output_root, "figures", "figure_01_implementation_overlay.pdf")
)

provenance <- list(
  created_at = format(Sys.time(), tz = "America/Chicago", usetz = TRUE),
  analysis = list(
    geography = "NL",
    age_group = "Y_GE80",
    sex = "T",
    training_start = min(custom_model$data$date),
    training_end = max(custom_model$data$date),
    prediction_end = max(custom_model$full_data$date),
    seed = seed,
    draws = 3000,
    aghq_nodes = 5,
    p = 2,
    m = 4,
    k_IWP = 100,
    k_sGP = 40
  ),
  thresholds = equivalence_thresholds,
  decision = decision,
  structural = structural_rows,
  numerical = numerical_rows,
  convergence = list(
    custom_tmb = custom_model$model$optresults$convergence,
    bayesgp = bayesgp_model$mod$optresults$convergence
  ),
  paths = list(
    historical_input = normalizePath(historical_input_path),
    historical_input_type = historical_input_type,
    historical_input_sha256 = digest::digest(
      file = historical_input_path,
      algo = "sha256"
    ),
    custom_model = normalizePath(custom_model_path),
    bayesgp_model = normalizePath(bayesgp_model_path),
    custom_tmb_source = normalizePath(here::here(
      "code", "regions", "europe", "bayesgp_model.cpp"
    )),
    custom_tmb_dll = normalizePath(dll_path)
  ),
  packages = vapply(
    c("BayesGP", "OSplines", "sGPfit", "TMB", "aghq"),
    function(package) as.character(utils::packageVersion(package)),
    character(1)
  ),
  figure_outputs = figure_outputs,
  session_info = utils::sessionInfo()
)
saveRDS(provenance, file.path(output_root, "provenance.rds"))

if (overall_pass) {
  writeLines("pass", file.path(output_root, "equivalence_pass.flag"))
} else if (file.exists(file.path(output_root, "equivalence_pass.flag"))) {
  unlink(file.path(output_root, "equivalence_pass.flag"))
}
writeLines("complete", file.path(output_root, "complete.flag"))

if (!overall_pass) {
  stop(
    "BayesGP 0.1.3 did not pass the frozen Europe implementation-equivalence gate."
  )
}
message("BayesGP 0.1.3 passed the Europe implementation-equivalence gate.")
