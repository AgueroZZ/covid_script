stable_analysis_seed <- function(base_seed, analysis_id, stage = "fit") {
  hash_value <- digest::digest2int(paste(analysis_id, stage, sep = "::"))
  seed <- (as.double(base_seed) + abs(as.double(hash_value))) %%
    (.Machine$integer.max - 1)
  as.integer(seed + 1)
}

us_model_priors <- function() {
  probability_statement <- list(u = 0.1, alpha = 0.01)
  list(
    iwp = list(
      prior = "exp",
      param = probability_statement,
      h = 5
    ),
    seasonal = list(
      prior = "exp",
      param = probability_statement,
      h = 1
    ),
    overdispersion = list(
      prior = "exp",
      param = probability_statement
    )
  )
}

us_knot_counts <- function(branch) {
  years <- lubridate::year(branch$date)
  year_span <- diff(range(years))
  if (year_span >= 10) {
    list(iwp = 100L, seasonal = 40L)
  } else {
    list(iwp = 50L, seasonal = 20L)
  }
}

split_us_model_branches <- function(data, analysis_path) {
  key <- interaction(
    data$geography,
    data$age_group,
    data$sex,
    drop = TRUE,
    lex.order = TRUE
  )
  branches <- split(data, key)
  lapply(branches, function(branch) {
    branch <- dplyr::arrange(branch, date)
    attributes(branch)$analysis_id <- us_analysis_id(
      analysis_path,
      unique(branch$geography),
      unique(branch$age_group),
      unique(branch$sex)
    )
    attributes(branch)$analysis_path <- analysis_path
    branch
  })
}

select_us_model_branch <- function(branches, analysis_id) {
  identifiers <- vapply(branches, us_branch_analysis_id, character(1))
  selected <- which(identifiers == analysis_id)
  if (length(selected) != 1L) {
    stop("Expected exactly one US model smoke branch: ", analysis_id)
  }
  branches[[selected]]
}

us_branch_analysis_id <- function(branch) {
  analysis_id <- attr(branch, "analysis_id", exact = TRUE)
  if (is.null(analysis_id) || length(analysis_id) != 1L) {
    stop("A US model branch must have exactly one analysis identifier.")
  }
  analysis_id
}

prepare_us_model_frame <- function(branch, training_end = as.Date("2019-12-31")) {
  branch <- dplyr::arrange(branch, date)
  training <- dplyr::filter(branch, date <= as.Date(training_end))
  if (nrow(training) == 0L) {
    stop("The branch has no pre-2020 observations.")
  }

  origin <- min(branch$date)
  branch$x <- as.numeric(branch$date - origin) / 365
  training <- dplyr::filter(branch, date <= as.Date(training_end))
  training$x1 <- training$x
  training$x2 <- training$x
  training$x3 <- training$x

  list(
    training = training,
    full = branch,
    x_full = branch$x,
    origin = origin
  )
}

us_model_formula <- function() {
  observed_deaths ~
    f(
      x = x1,
      model = "IWP",
      order = 2,
      sd.prior = prior_iwp,
      k = k_iwp,
      initial_location = "left"
    ) +
    f(
      x = x2,
      model = "sGP",
      a = 2 * pi,
      k = k_seasonal,
      sd.prior = prior_seasonal,
      m = 4,
      accuracy = 0.001,
      region = prediction_region
    ) +
    f(
      x = x3,
      model = "IID",
      sd.prior = prior_overdispersion
    ) +
    offset(log_days)
}

fit_us_bayesgp <- function(branch, config) {
  analysis_id <- us_branch_analysis_id(branch)
  frame <- prepare_us_model_frame(
    branch,
    training_end = config$training$final_date
  )
  knots <- us_knot_counts(branch)
  priors <- us_model_priors()
  seed <- stable_analysis_seed(
    config$model$base_seed,
    analysis_id,
    stage = "fit"
  )
  set.seed(seed)

  f <- BayesGP::f
  prior_iwp <- priors$iwp
  prior_seasonal <- priors$seasonal
  prior_overdispersion <- priors$overdispersion
  k_iwp <- knots$iwp
  k_seasonal <- knots$seasonal
  prediction_region <- range(frame$x_full)
  model_formula <- us_model_formula()
  environment(model_formula) <- environment()

  fitted <- BayesGP::model_fit(
    formula = model_formula,
    data = frame$training,
    family = "Poisson"
  )
  fitted$analysis_id <- analysis_id
  fitted$analysis_path <- attr(branch, "analysis_path", exact = TRUE)
  fitted$x_full <- frame$x_full
  fitted$full_data <- frame$full
  fitted$reproducibility <- list(
    fit_seed = seed,
    k_iwp = k_iwp,
    k_seasonal = k_seasonal,
    posterior_draws = ncol(fitted$samps$samps),
    training_end = as.Date(config$training$final_date),
    calendar_day_offset = TRUE,
    population_offset = FALSE
  )
  fitted
}

predict_us_mortality <- function(fitted, config) {
  refined_x <- fitted$x_full
  analysis_id <- fitted$analysis_id
  seed <- stable_analysis_seed(
    config$model$base_seed,
    analysis_id,
    stage = "posterior-predictive"
  )
  set.seed(seed)

  posterior <- fitted$samps
  iwp_index <- fitted$random_samp_indexes[[1]]
  seasonal_index <- fitted$random_samp_indexes[[2]]
  iwp_samples <- as.matrix(BayesGP:::compute_post_fun_iwp(
    samps = posterior$samps[iwp_index, , drop = FALSE],
    knots = fitted$instances[[1]]@knots,
    refined_x = refined_x,
    global_samps = posterior$samps[
      fitted$boundary_samp_indexes[[1]],
      ,
      drop = FALSE
    ],
    intercept_samps = posterior$samps[
      fitted$fixed_samp_indexes$intercept,
      ,
      drop = FALSE
    ],
    p = fitted$instances[[1]]@order
  )[, -1, drop = FALSE])
  seasonal_samples <- as.matrix(BayesGP:::compute_post_fun_sgp(
    samps = posterior$samps[seasonal_index, , drop = FALSE],
    k = fitted$instances[[2]]@k,
    a = fitted$instances[[2]]@a,
    m = fitted$instances[[2]]@m,
    region = fitted$instances[[2]]@region,
    refined_x = refined_x,
    global_samps = posterior$samps[
      fitted$boundary_samp_indexes[[2]],
      ,
      drop = FALSE
    ]
  )[, -1, drop = FALSE])

  n_draws <- ncol(posterior$samps)
  latent_samples <- iwp_samples + seasonal_samples
  overdispersion_sd <- exp(-0.5 * posterior$thetasamples[[3]])
  noise <- matrix(
    stats::rnorm(
      nrow(latent_samples) * n_draws,
      sd = rep(overdispersion_sd, each = nrow(latent_samples))
    ),
    nrow = nrow(latent_samples),
    ncol = n_draws
  )
  log_days <- matrix(
    fitted$full_data$log_days,
    nrow = nrow(latent_samples),
    ncol = n_draws
  )
  expected_rate <- exp(latent_samples + noise + log_days)
  predictive_samples <- matrix(
    stats::rpois(length(expected_rate), lambda = as.numeric(expected_rate)),
    nrow = nrow(expected_rate),
    ncol = ncol(expected_rate)
  )
  interval <- as.numeric(unlist(config$model$posterior_interval))

  list(
    analysis_id = analysis_id,
    analysis_path = fitted$analysis_path,
    geography = unique(fitted$full_data$geography),
    age_group = unique(fitted$full_data$age_group),
    sex = unique(fitted$full_data$sex),
    dates = fitted$full_data$date,
    observed_deaths = fitted$full_data$observed_deaths,
    samples = predictive_samples,
    summary = tibble::tibble(
      date = fitted$full_data$date,
      mean = rowMeans(predictive_samples),
      lower = apply(predictive_samples, 1, stats::quantile, probs = interval[[1]]),
      upper = apply(predictive_samples, 1, stats::quantile, probs = interval[[2]])
    ),
    reproducibility = c(
      fitted$reproducibility,
      list(posterior_predictive_seed = seed)
    )
  )
}

run_us_model_branch <- function(branch, config) {
  analysis_id <- us_branch_analysis_id(branch)
  started_at <- Sys.time()
  tryCatch(
    {
      fitted <- fit_us_bayesgp(branch, config)
      prediction <- predict_us_mortality(fitted, config)
      list(
        analysis_id = analysis_id,
        status = "success",
        error_message = NA_character_,
        started_at = started_at,
        completed_at = Sys.time(),
        fit = fitted,
        prediction = prediction
      )
    },
    error = function(error) {
      list(
        analysis_id = analysis_id,
        status = "failed",
        error_message = conditionMessage(error),
        started_at = started_at,
        completed_at = Sys.time(),
        fit = NULL,
        prediction = NULL
      )
    }
  )
}

run_us_model_smoke <- function(branch, config) {
  run <- run_us_model_branch(branch, config)
  if (!identical(run$status, "success")) {
    stop("US model smoke failed: ", run$error_message)
  }
  run
}

us_model_run_status <- function(run) {
  tibble::tibble(
    analysis_id = run$analysis_id,
    status = run$status,
    error_message = run$error_message,
    started_at = as.character(run$started_at),
    completed_at = as.character(run$completed_at)
  )
}

write_us_fit_artifact <- function(run, config) {
  path <- artifact_path(
    config,
    "models",
    "us",
    paste0(run$analysis_id, ".rds")
  )
  payload <- run
  payload$prediction <- NULL
  saveRDS(payload, path)
  path
}

write_us_prediction_artifact <- function(run, config) {
  path <- artifact_path(
    config,
    "results",
    "us",
    "predictions",
    paste0(run$analysis_id, ".rds")
  )
  payload <- run
  payload$fit <- NULL
  saveRDS(payload, path)
  path
}
