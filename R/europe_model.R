europe_age_groups <- function() {
  c("Y20-39", "Y40-59", "Y60-79", "Y_GE80")
}

europe_required_columns <- function() {
  c("geo", "age", "sex", "TIME_PERIOD", "OBS_VALUE")
}

read_europe_model_input <- function(path) {
  if (!file.exists(path)) {
    stop("Europe model input does not exist: ", path, ".")
  }
  data <- utils::read.csv(path, stringsAsFactors = FALSE)
  missing <- setdiff(europe_required_columns(), names(data))
  if (length(missing) > 0L) {
    stop("Europe model input is missing: ", paste(missing, collapse = ", "), ".")
  }
  data <- data[data$age %in% europe_age_groups(), , drop = FALSE]
  data$date <- ISOweek::ISOweek2date(paste0(data$TIME_PERIOD, "-1"))
  data$Year <- as.integer(format(data$date, "%Y"))
  data <- data[order(data$geo, data$age, data$sex, data$date), , drop = FALSE]
  rownames(data) <- NULL
  data
}

build_europe_manifest <- function(
  data,
  base_seed = 20260829L,
  minimum_year_span = 10,
  required_prediction_end_after = as.Date("2022-01-01")
) {
  keys <- unique(data[c("geo", "age", "sex")])
  age_order <- match(keys$age, europe_age_groups())
  sex_order <- match(keys$sex, c("T", "F", "M"))
  keys <- keys[order(keys$geo, age_order, sex_order), , drop = FALSE]

  rows <- lapply(seq_len(nrow(keys)), function(index) {
    selected <- data[
      data$geo == keys$geo[[index]] &
        data$age == keys$age[[index]] &
        data$sex == keys$sex[[index]],
      ,
      drop = FALSE
    ]
    year_span <- diff(range(selected$Year))
    training_rows <- sum(selected$date < as.Date("2020-01-01"))
    prediction_end <- max(selected$date)
    data.frame(
      geo = keys$geo[[index]],
      age = keys$age[[index]],
      sex = keys$sex[[index]],
      full_rows = nrow(selected),
      training_rows = training_rows,
      prediction_start = min(selected$date),
      prediction_end = prediction_end,
      year_span = year_span,
      eligible = year_span >= minimum_year_span &&
        training_rows > 0L &&
        prediction_end > required_prediction_end_after,
      stringsAsFactors = FALSE
    )
  })
  manifest <- do.call(rbind, rows)
  manifest <- manifest[manifest$eligible, , drop = FALSE]
  manifest$eligible <- NULL
  manifest <- manifest[
    order(
      manifest$geo,
      match(manifest$age, europe_age_groups()),
      match(manifest$sex, c("T", "F", "M"))
    ),
    ,
    drop = FALSE
  ]
  rownames(manifest) <- NULL
  manifest$model_index <- seq_len(nrow(manifest))
  manifest$model_id <- paste(
    manifest$geo,
    manifest$age,
    manifest$sex,
    sep = "_"
  )
  manifest$seed <- as.integer(base_seed) + manifest$model_index
  manifest$k_IWP <- ifelse(manifest$year_span >= 10, 100L, 50L)
  manifest$k_sGP <- ifelse(manifest$year_span >= 10, 40L, 20L)
  manifest <- manifest[c(
    "model_index",
    "model_id",
    "geo",
    "age",
    "sex",
    "full_rows",
    "training_rows",
    "prediction_start",
    "prediction_end",
    "year_span",
    "k_IWP",
    "k_sGP",
    "seed"
  )]
  manifest
}

prepare_europe_series <- function(data, manifest_row) {
  selected <- data[
    data$geo == manifest_row$geo[[1]] &
      data$age == manifest_row$age[[1]] &
      data$sex == manifest_row$sex[[1]],
    ,
    drop = FALSE
  ]
  if (identical(manifest_row$geo[[1]], "ES")) {
    selected <- selected[selected$date != as.Date("2020-12-28"), , drop = FALSE]
  }
  selected <- selected[order(selected$date), , drop = FALSE]
  rownames(selected) <- NULL
  selected$x <- (
    as.numeric(selected$date) - min(as.numeric(selected$date))
  ) / 365
  training <- selected[selected$date < as.Date("2020-01-01"), , drop = FALSE]
  training$x1 <- training$x
  training$x2 <- training$x
  training$observation_id <- seq_len(nrow(training))
  if (nrow(training) == 0L) {
    stop("No pre-2020 training observations for ", manifest_row$model_id[[1]], ".")
  }
  list(
    full_data = selected,
    training_data = training,
    x_full = selected$x,
    full_region = range(selected$x)
  )
}

europe_prior_specification <- function() {
  list(
    trend = list(
      prior = "exp",
      param = list(u = 0.1, alpha = 0.01),
      h = 5
    ),
    seasonal = list(
      prior = "exp",
      param = list(u = 0.1, alpha = 0.01),
      h = 1
    ),
    overdispersion = list(
      prior = "exp",
      param = list(u = 0.1, alpha = 0.01)
    )
  )
}

fit_europe_bayesgp <- function(series, manifest_row) {
  required_version <- "0.1.3"
  installed_version <- as.character(utils::packageVersion("BayesGP"))
  if (!identical(installed_version, required_version)) {
    stop("BayesGP ", required_version, " is required; found ", installed_version, ".")
  }
  prior <- europe_prior_specification()
  full_region <- series$full_region
  k_IWP <- as.integer(manifest_row$k_IWP[[1]])
  k_sGP <- as.integer(manifest_row$k_sGP[[1]])
  f <- BayesGP::f

  set.seed(as.integer(manifest_row$seed[[1]]))
  BayesGP::model_fit(
    OBS_VALUE ~
      f(
        x1,
        model = "IWP",
        order = 2,
        sd.prior = prior$trend,
        boundary.prior = list(prec = 0.001, mean = 0),
        k = k_IWP,
        initial_location = "left",
        region = full_region
      ) +
      f(
        x2,
        model = "sGP",
        a = 2 * pi,
        k = k_sGP,
        sd.prior = prior$seasonal,
        boundary.prior = list(prec = 0.001, mean = 0),
        m = 4,
        accuracy = 0.001,
        region = full_region
      ) +
      f(
        observation_id,
        model = "IID",
        sd.prior = prior$overdispersion
      ),
    data = series$training_data,
    family = "Poisson",
    control.family = NULL,
    control.fixed = list(intercept = list(prec = 0.001, mean = 0)),
    aghq_k = 5,
    M = 3000,
    envir = environment()
  )
}

europe_latent_components <- function(fitted_model, refined_x) {
  posterior <- fitted_model$samps
  iwp <- fitted_model$instances[[1]]
  sgp <- fitted_model$instances[[2]]
  trend <- as.matrix(
    BayesGP:::compute_post_fun_iwp(
      samps = posterior$samps[
        fitted_model$random_samp_indexes[[1]],
        ,
        drop = FALSE
      ],
      knots = iwp@knots,
      refined_x = refined_x,
      global_samps = posterior$samps[
        fitted_model$boundary_samp_indexes[[1]],
        ,
        drop = FALSE
      ],
      intercept_samps = posterior$samps[
        fitted_model$fixed_samp_indexes$intercept,
        ,
        drop = FALSE
      ],
      p = iwp@order
    )[, -1, drop = FALSE]
  )
  seasonal <- as.matrix(
    BayesGP:::compute_post_fun_sgp(
      samps = posterior$samps[
        fitted_model$random_samp_indexes[[2]],
        ,
        drop = FALSE
      ],
      k = sgp@k,
      a = sgp@a,
      m = sgp@m,
      region = sgp@region,
      refined_x = refined_x,
      global_samps = posterior$samps[
        fitted_model$boundary_samp_indexes[[2]],
        ,
        drop = FALSE
      ]
    )[, -1, drop = FALSE]
  )
  list(trend = trend, seasonal = seasonal)
}

europe_component_summary <- function(samples, dates, transform = identity) {
  transformed <- transform(samples)
  data.frame(
    date = as.Date(dates),
    mean = rowMeans(transformed),
    lower = apply(transformed, 1, stats::quantile, probs = 0.025),
    upper = apply(transformed, 1, stats::quantile, probs = 0.975)
  )
}

predict_europe_compact <- function(fitted_model, series, seed, draws = 3000L) {
  if (ncol(fitted_model$samps$samps) != draws) {
    stop("The fitted posterior does not contain exactly ", draws, " draws.")
  }
  component <- europe_latent_components(fitted_model, series$x_full)
  latent <- component$trend + component$seasonal
  overdispersion_sd <- exp(-0.5 * fitted_model$samps$thetasamples[[3]])
  set.seed(as.integer(seed) + 1000000L)
  iid_noise <- stats::rnorm(
    draws * nrow(latent),
    sd = rep(overdispersion_sd, each = nrow(latent))
  )
  iid_noise <- matrix(iid_noise, nrow = nrow(latent), ncol = draws)
  rate <- exp(latent + iid_noise)
  samples <- matrix(
    stats::rpois(length(rate), lambda = as.numeric(rate)),
    nrow = nrow(rate),
    ncol = ncol(rate)
  )
  storage.mode(samples) <- "integer"
  summary <- data.frame(
    mean = rowMeans(samples),
    upper = apply(samples, 1, stats::quantile, probs = 0.975),
    lower = apply(samples, 1, stats::quantile, probs = 0.025),
    x = series$x_full,
    time = as.Date(series$full_data$date)
  )
  list(
    model_pred = list(samples = samples, summary = summary),
    trend = europe_component_summary(
      component$trend,
      series$full_data$date,
      transform = exp
    ),
    seasonal = europe_component_summary(
      component$seasonal,
      series$full_data$date
    )
  )
}

validate_europe_model_pred <- function(
  model_pred,
  expected_rows,
  expected_draws = 3000L
) {
  if (!identical(names(model_pred), c("samples", "summary"))) {
    stop("model_pred must contain only samples and summary.")
  }
  if (!is.matrix(model_pred$samples) ||
      !identical(typeof(model_pred$samples), "integer")) {
    stop("model_pred samples must be an integer matrix.")
  }
  if (!identical(dim(model_pred$samples), c(
    as.integer(expected_rows),
    as.integer(expected_draws)
  ))) {
    stop("model_pred sample dimensions do not match the expected contract.")
  }
  expected_names <- c("mean", "upper", "lower", "x", "time")
  if (!identical(names(model_pred$summary), expected_names)) {
    stop("model_pred summary columns do not match the historical contract.")
  }
  if (nrow(model_pred$summary) != expected_rows) {
    stop("model_pred summary rows do not match the sample rows.")
  }
  if (!inherits(model_pred$summary$time, "Date")) {
    stop("model_pred summary time must be a Date vector.")
  }
  invisible(model_pred)
}

build_europe_figure_01_input <- function(prediction, series) {
  model_pred <- prediction$model_pred
  list(
    overall = data.frame(
      date = as.Date(model_pred$summary$time),
      observed = series$full_data$OBS_VALUE,
      mean = model_pred$summary$mean,
      lower = model_pred$summary$lower,
      upper = model_pred$summary$upper
    ),
    trend = prediction$trend,
    seasonal = prediction$seasonal,
    training_boundary = as.Date("2020-01-01")
  )
}

validate_europe_figure_01_input <- function(input) {
  required <- c("overall", "trend", "seasonal", "training_boundary")
  if (!identical(names(input), required)) {
    stop("Figure 1 input does not match the compact component contract.")
  }
  if (!identical(
    names(input$overall),
    c("date", "observed", "mean", "lower", "upper")
  )) {
    stop("Figure 1 overall columns are invalid.")
  }
  component_names <- c("date", "mean", "lower", "upper")
  if (!identical(names(input$trend), component_names) ||
      !identical(names(input$seasonal), component_names)) {
    stop("Figure 1 component columns are invalid.")
  }
  row_counts <- c(nrow(input$overall), nrow(input$trend), nrow(input$seasonal))
  if (length(unique(row_counts)) != 1L || row_counts[[1]] == 0L) {
    stop("Figure 1 components have incompatible row counts.")
  }
  invisible(input)
}

europe_output_paths <- function(output_root, model_id) {
  list(
    result = file.path(output_root, "fitted_model", paste0(model_id, ".rda")),
    diagnostic = file.path(
      output_root,
      "diagnostics",
      paste0(model_id, ".rds")
    ),
    complete = file.path(output_root, "complete", paste0(model_id, ".flag")),
    figure_01 = file.path(
      output_root,
      "figure_01",
      "figure_01_model_illustration.rds"
    )
  )
}

atomic_save_model_pred <- function(model_pred, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  temporary <- tempfile(pattern = ".model-pred-", tmpdir = dirname(path))
  on.exit(unlink(temporary), add = TRUE)
  save(model_pred, file = temporary, compress = "gzip")
  if (!file.rename(temporary, path)) {
    stop("Failed to atomically install compact result: ", path, ".")
  }
  invisible(path)
}

atomic_save_rds <- function(object, path, compress = "gzip") {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  temporary <- tempfile(pattern = ".rds-", tmpdir = dirname(path))
  on.exit(unlink(temporary), add = TRUE)
  saveRDS(object, temporary, compress = compress)
  if (!file.rename(temporary, path)) {
    stop("Failed to atomically install RDS output: ", path, ".")
  }
  invisible(path)
}

atomic_write_lines <- function(text, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  temporary <- tempfile(pattern = ".flag-", tmpdir = dirname(path))
  on.exit(unlink(temporary), add = TRUE)
  writeLines(text, temporary)
  if (!file.rename(temporary, path)) {
    stop("Failed to atomically install text output: ", path, ".")
  }
  invisible(path)
}

load_europe_model_pred <- function(path) {
  environment <- new.env(parent = emptyenv())
  loaded <- load(path, envir = environment)
  if (!identical(loaded, "model_pred")) {
    stop("Compact result must contain exactly one object named model_pred: ", path)
  }
  environment$model_pred
}

europe_package_provenance <- function() {
  packages <- c("BayesGP", "OSplines", "sGPfit", "TMB", "aghq", "ISOweek")
  rows <- lapply(packages, function(package) {
    description <- utils::packageDescription(package)
    data.frame(
      package = package,
      version = as.character(description$Version),
      remote_sha = if (is.null(description$RemoteSha)) NA_character_ else
        as.character(description$RemoteSha),
      remote_repo = if (is.null(description$RemoteRepo)) NA_character_ else
        as.character(description$RemoteRepo),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

validate_existing_europe_output <- function(paths, series, draws = 3000L) {
  if (!all(file.exists(c(paths$result, paths$diagnostic, paths$complete)))) {
    return(FALSE)
  }
  valid <- tryCatch({
    model_pred <- load_europe_model_pred(paths$result)
    validate_europe_model_pred(model_pred, nrow(series$full_data), draws)
    diagnostic <- readRDS(paths$diagnostic)
    identical(
      unname(diagnostic$result_sha256),
      unname(digest::digest(file = paths$result, algo = "sha256"))
    ) && identical(diagnostic$status, "complete")
  }, error = function(error) FALSE)
  isTRUE(valid)
}

run_europe_model <- function(
  manifest_row,
  data,
  output_root,
  force = FALSE,
  draws = 3000L
) {
  model_id <- manifest_row$model_id[[1]]
  paths <- europe_output_paths(output_root, model_id)
  series <- prepare_europe_series(data, manifest_row)
  if (!force && validate_existing_europe_output(paths, series, draws)) {
    return(data.frame(
      model_id = model_id,
      status = "skipped_valid",
      elapsed_seconds = 0,
      message = "Existing compact output passed validation.",
      stringsAsFactors = FALSE
    ))
  }

  started <- Sys.time()
  result <- tryCatch({
    fitted_model <- fit_europe_bayesgp(series, manifest_row)
    prediction <- predict_europe_compact(
      fitted_model,
      series,
      seed = manifest_row$seed[[1]],
      draws = draws
    )
    validate_europe_model_pred(
      prediction$model_pred,
      expected_rows = nrow(series$full_data),
      expected_draws = draws
    )
    atomic_save_model_pred(prediction$model_pred, paths$result)

    if (identical(model_id, "NL_Y_GE80_T")) {
      figure_01 <- build_europe_figure_01_input(prediction, series)
      validate_europe_figure_01_input(figure_01)
      atomic_save_rds(figure_01, paths$figure_01)
    }

    diagnostic <- list(
      status = "complete",
      model_id = model_id,
      geography = manifest_row$geo[[1]],
      age = manifest_row$age[[1]],
      sex = manifest_row$sex[[1]],
      seed = manifest_row$seed[[1]],
      rows = list(
        training = nrow(series$training_data),
        prediction = nrow(series$full_data),
        draws = draws
      ),
      dates = list(
        training_start = min(series$training_data$date),
        training_end = max(series$training_data$date),
        prediction_start = min(series$full_data$date),
        prediction_end = max(series$full_data$date)
      ),
      model = list(
        family = "Poisson",
        trend = "IWP2",
        seasonal_harmonics = 4L,
        k_IWP = manifest_row$k_IWP[[1]],
        k_sGP = manifest_row$k_sGP[[1]],
        aghq_nodes = 5L,
        prior = europe_prior_specification()
      ),
      internal_priors = list(
        trend = fitted_model$instances[[1]]@sd.prior$param,
        seasonal = fitted_model$instances[[2]]@sd.prior$param,
        overdispersion = fitted_model$instances[[3]]@sd.prior$param
      ),
      convergence = fitted_model$mod$optresults$convergence,
      theta_mode = as.numeric(fitted_model$mod$optresults$mode),
      input_sha256 = digest::digest(series$full_data, algo = "sha256"),
      result_sha256 = digest::digest(file = paths$result, algo = "sha256"),
      result_bytes = unname(file.info(paths$result)$size),
      packages = europe_package_provenance(),
      threads = Sys.getenv(c(
        "OMP_NUM_THREADS",
        "OPENBLAS_NUM_THREADS",
        "MKL_NUM_THREADS",
        "BLIS_NUM_THREADS",
        "OMP_THREAD_LIMIT"
      )),
      elapsed_seconds = as.numeric(difftime(Sys.time(), started, units = "secs")),
      completed_at = format(Sys.time(), tz = "UTC", usetz = TRUE)
    )
    atomic_save_rds(diagnostic, paths$diagnostic)
    atomic_write_lines("complete", paths$complete)
    rm(fitted_model)
    gc(verbose = FALSE)
    data.frame(
      model_id = model_id,
      status = "complete",
      elapsed_seconds = diagnostic$elapsed_seconds,
      message = "",
      stringsAsFactors = FALSE
    )
  }, error = function(error) {
    data.frame(
      model_id = model_id,
      status = "failed",
      elapsed_seconds = as.numeric(difftime(Sys.time(), started, units = "secs")),
      message = conditionMessage(error),
      stringsAsFactors = FALSE
    )
  })
  result
}
