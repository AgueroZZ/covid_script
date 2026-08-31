ireland_age_groups <- function() {
  c("25-44", "45-64", "65-84", "85+")
}

ireland_geography <- function() {
  "Republic of Ireland"
}

ireland_harmonics <- function() {
  1L
}

ireland_age_from_source <- function(age) {
  age <- trimws(as.character(age))
  ifelse(
    age %in% c("25 - 34 years", "35 - 44 years"),
    "25-44",
    ifelse(
      age %in% c("45 - 54 years", "55 - 64 years"),
      "45-64",
      ifelse(
        age %in% c("65 - 74 years", "75 - 84 years"),
        "65-84",
        ifelse(age == "85 years and over", "85+", NA_character_)
      )
    )
  )
}

ireland_quarter_end <- function(quarter) {
  quarter <- as.character(quarter)
  year <- as.integer(substr(quarter, 1L, 4L))
  q <- substr(quarter, 5L, 6L)
  suffix <- c(Q1 = "03-31", Q2 = "06-30", Q3 = "09-30", Q4 = "12-31")
  if (any(!q %in% names(suffix))) {
    stop("Ireland quarter labels must use YYYYQ1 through YYYYQ4.")
  }
  as.Date(paste(year, suffix[q], sep = "-"))
}

read_ireland_model_input <- function(path) {
  if (!file.exists(path)) {
    stop("Ireland input does not exist: ", path, ".")
  }
  raw <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  required <- c("Quarter", "Age Group", "VALUE")
  actual_names <- gsub("\\.", " ", names(raw))
  names(raw) <- actual_names
  missing <- setdiff(required, names(raw))
  if (length(missing) > 0L) {
    stop("Ireland input is missing: ", paste(missing, collapse = ", "), ".")
  }
  raw$age_group <- ireland_age_from_source(raw$`Age Group`)
  raw <- raw[!is.na(raw$age_group), , drop = FALSE]
  raw$date <- ireland_quarter_end(raw$Quarter)
  aggregated <- raw |>
    dplyr::group_by(.data$date, .data$Quarter, .data$age_group) |>
    dplyr::summarise(
      observed_deaths = sum(.data$VALUE),
      .groups = "drop"
    )
  data <- data.frame(
    date = as.Date(aggregated$date),
    quarter = aggregated$Quarter,
    geography = ireland_geography(),
    age_group = aggregated$age_group,
    sex = "total",
    observed_deaths = as.integer(round(aggregated$observed_deaths)),
    count_definition = "deaths registered in the Republic of Ireland",
    source_frequency = "quarterly",
    source_id = "cso_ireland_quarterly_deaths_2010_2023",
    stringsAsFactors = FALSE
  )
  data <- data[order(
    match(data$age_group, ireland_age_groups()),
    data$date
  ), , drop = FALSE]
  rownames(data) <- NULL
  validate_ireland_model_input(data)
  data
}

validate_ireland_model_input <- function(data) {
  required <- c(
    "date", "quarter", "geography", "age_group", "sex",
    "observed_deaths", "count_definition", "source_frequency", "source_id"
  )
  missing <- setdiff(required, names(data))
  if (length(missing) > 0L) {
    stop("Ireland model input is missing: ", paste(missing, collapse = ", "), ".")
  }
  if (!inherits(data$date, "Date")) {
    stop("Ireland dates must be Date values.")
  }
  if (!setequal(unique(data$age_group), ireland_age_groups())) {
    stop("Ireland age groups do not match the source contract.")
  }
  if (anyNA(data$observed_deaths) || any(data$observed_deaths < 0L)) {
    stop("Ireland death counts must be observed non-negative integers.")
  }
  if (!all(data$source_frequency == "quarterly")) {
    stop("Ireland model rows must be quarterly.")
  }
  keys <- data[c("date", "geography", "age_group", "sex")]
  if (anyDuplicated(keys)) {
    stop("Ireland model rows are not unique.")
  }
  invisible(data)
}

ireland_wave_from_quarter_end <- function(date) {
  date <- as.Date(date)
  assigned <- rep(NA_character_, length(date))
  definitions <- data.frame(
    wave = c("initial", "alpha", "delta", "omicron"),
    start = as.Date(c(
      "2020-03-01", "2020-11-01", "2021-07-01", "2022-01-01"
    )),
    end_exclusive = as.Date(c(
      "2020-11-01", "2021-07-01", "2022-01-01", "2024-04-01"
    )),
    stringsAsFactors = FALSE
  )
  for (index in seq_len(nrow(definitions))) {
    selected <- date >= definitions$start[[index]] &
      date < definitions$end_exclusive[[index]]
    assigned[selected] <- definitions$wave[[index]]
  }
  assigned
}

draw_ireland_quarterly_poisson <- function(
  weekly_rate,
  aggregation,
  seed = NULL
) {
  weekly_rate <- as.matrix(weekly_rate)
  aggregation <- as.matrix(aggregation)
  if (ncol(aggregation) != nrow(weekly_rate)) {
    stop("Ireland aggregation columns must match weekly-rate rows.")
  }
  if (anyNA(weekly_rate) || any(weekly_rate < 0)) {
    stop("Ireland weekly rates must be observed non-negative values.")
  }
  if (!is.null(seed)) {
    set.seed(as.integer(seed))
  }
  quarterly_rate <- aggregation %*% weekly_rate
  draws <- matrix(
    stats::rpois(
      length(quarterly_rate),
      lambda = as.numeric(quarterly_rate)
    ),
    nrow = nrow(quarterly_rate),
    ncol = ncol(quarterly_rate)
  )
  storage.mode(draws) <- "integer"
  draws
}

ireland_converted_prior_specification <- function() {
  raw <- list(u = 0.1, a = 0.01)
  seasonal_correction <- sum(vapply(
    seq_len(ireland_harmonics()),
    function(harmonic) {
      sGPfit::compute_d_step_sGPsd(
        d = 1,
        a = harmonic * 2 * pi
      )
    },
    numeric(1)
  ))
  list(
    trend = OSplines:::prior_conversion_IWP(d = 5, prior = raw, p = 2),
    seasonal = list(u = raw$u / seasonal_correction, a = raw$a),
    overdispersion = raw,
    predictive_sd_horizon_years = list(trend = 5, seasonal = 1)
  )
}

build_ireland_manifest <- function(data, base_seed = 20260830L) {
  validate_ireland_model_input(data)
  rows <- lapply(seq_along(ireland_age_groups()), function(index) {
    age_group <- ireland_age_groups()[[index]]
    selected <- data[data$age_group == age_group, , drop = FALSE]
    data.frame(
      model_index = index,
      model_id = paste0(
        "Ireland_",
        c("25_44", "45_64", "65_84", "85_plus")[[index]]
      ),
      geography = ireland_geography(),
      age_group = age_group,
      sex = "total",
      full_rows = nrow(selected),
      training_rows = sum(selected$date < as.Date("2020-01-01")),
      prediction_start = min(selected$date),
      prediction_end = max(selected$date),
      k_IWP = 100L,
      k_sGP = 40L,
      seed = as.integer(base_seed) + index,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

prepare_ireland_series <- function(data, manifest_row) {
  selected <- data[
    data$age_group == manifest_row$age_group[[1]],
    ,
    drop = FALSE
  ]
  selected <- selected[order(selected$date), , drop = FALSE]
  selected$deaths <- selected$observed_deaths
  selected$Year <- as.integer(format(selected$date, "%Y"))
  selected$new_age_group <- selected$age_group
  selected
}

ireland_output_paths <- function(output_root, model_id) {
  list(
    result = file.path(output_root, "fitted_model", paste0(model_id, ".rda")),
    summary = file.path(output_root, "wave_summary", paste0(model_id, ".csv")),
    diagnostic = file.path(
      output_root,
      "diagnostics",
      paste0(model_id, ".rds")
    ),
    complete = file.path(output_root, "complete", paste0(model_id, ".flag"))
  )
}

save_ireland_model_pred_atomic <- function(model_pred, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  temporary <- tempfile(pattern = ".ireland-model-pred-", tmpdir = dirname(path))
  on.exit(unlink(temporary), add = TRUE)
  save(model_pred, file = temporary, compress = "gzip")
  if (!file.rename(temporary, path)) {
    stop("Failed to atomically install Ireland result: ", path, ".")
  }
  invisible(path)
}

validate_ireland_model_pred <- function(
  model_pred,
  expected_rows = 53L,
  expected_draws = 3000L
) {
  if (!is.list(model_pred) || !identical(names(model_pred), c("samples", "summary"))) {
    stop("Ireland model_pred must contain samples and summary.")
  }
  if (!is.matrix(model_pred$samples) ||
      !identical(typeof(model_pred$samples), "integer")) {
    stop("Ireland posterior predictive samples must be an integer matrix.")
  }
  if (!identical(
    dim(model_pred$samples),
    c(as.integer(expected_rows), as.integer(expected_draws))
  )) {
    stop("Ireland posterior predictive sample dimensions are invalid.")
  }
  if (!identical(names(model_pred$summary), c("mean", "upper", "lower", "time"))) {
    stop("Ireland posterior predictive summary columns are invalid.")
  }
  if (!inherits(model_pred$summary$time, "Date")) {
    stop("Ireland posterior predictive time must be a Date vector.")
  }
  invisible(model_pred)
}

ireland_package_provenance <- function() {
  packages <- c("BayesGP", "OSplines", "sGPfit", "TMB", "aghq", "Matrix")
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

run_ireland_model <- function(
  manifest_row,
  data,
  output_root,
  force = FALSE,
  draws = 3000L
) {
  model_id <- manifest_row$model_id[[1]]
  paths <- ireland_output_paths(output_root, model_id)
  series <- prepare_ireland_series(data, manifest_row)
  if (!force && all(file.exists(c(
    paths$result,
    paths$summary,
    paths$diagnostic,
    paths$complete
  )))) {
    existing_valid <- tryCatch({
      environment <- new.env(parent = emptyenv())
      loaded <- load(paths$result, envir = environment)
      identical(loaded, "model_pred") &&
        isTRUE(validate_ireland_model_pred(
          environment$model_pred,
          expected_rows = nrow(series),
          expected_draws = draws
        ) |> is.list())
    }, error = function(error) FALSE)
    if (isTRUE(existing_valid)) {
      return(data.frame(
        model_id = model_id,
        status = "skipped_valid",
        elapsed_seconds = 0,
        message = "Existing Ireland output passed validation.",
        stringsAsFactors = FALSE
      ))
    }
  }

  started <- Sys.time()
  tryCatch({
    prior <- ireland_converted_prior_specification()
    set.seed(as.integer(manifest_row$seed[[1]]))
    model_list <- fit_mod_IWP_sGP(
      quarterly_data = series,
      prior_IWP = prior$trend,
      prior_sGP = prior$seasonal,
      prior_overdis = prior$overdispersion,
      k_IWP = manifest_row$k_IWP[[1]],
      k_sGP = manifest_row$k_sGP[[1]],
      m = ireland_harmonics(),
      accuracy = 0.001
    )
    set.seed(as.integer(manifest_row$seed[[1]]) + 1000000L)
    model_pred <- pred_mortality_obs(
      model_list = model_list,
      refined_pred = model_list$x_full,
      M1 = draws,
      M2 = 1,
      aggregate_quarterly = TRUE
    )
    validate_ireland_model_pred(
      model_pred,
      expected_rows = nrow(series),
      expected_draws = draws
    )
    wave_summary <- excess_mortality_aggregate(
      model_pred = model_pred,
      full_data = series
    )
    wave_summary$geography <- ireland_geography()
    wave_summary$age_group <- manifest_row$age_group[[1]]
    wave_summary$wave_assignment <- "quarter_end"

    save_ireland_model_pred_atomic(model_pred, paths$result)
    dir.create(dirname(paths$summary), recursive = TRUE, showWarnings = FALSE)
    utils::write.csv(wave_summary, paths$summary, row.names = FALSE)
    convergence <- tryCatch(
      model_list$model$optresults$convergence,
      error = function(error) NA_integer_
    )
    diagnostic <- list(
      status = "complete",
      model_id = model_id,
      geography = ireland_geography(),
      age_group = manifest_row$age_group[[1]],
      sex = "total",
      seed = manifest_row$seed[[1]],
      rows = list(
        training = manifest_row$training_rows[[1]],
        prediction = nrow(series),
        draws = draws
      ),
      dates = list(
        training_start = min(series$date[series$Year < 2020L]),
        training_end = max(series$date[series$Year < 2020L]),
        prediction_start = min(series$date),
        prediction_end = max(series$date)
      ),
      data_contract = list(
        source_frequency = "quarterly",
        count_definition = unique(series$count_definition),
        wave_assignment = "whole quarter assigned by quarter-end date"
      ),
      model = list(
        family = "Poisson",
        trend = "IWP2",
        seasonal_harmonics = ireland_harmonics(),
        quarterly_likelihood = "Poisson(R %*% weekly_rate)",
        posterior_prediction = "Poisson(R_full %*% weekly_rate_draw)",
        k_IWP = manifest_row$k_IWP[[1]],
        k_sGP = manifest_row$k_sGP[[1]],
        aghq_nodes = 5L,
        prior = prior
      ),
      convergence = convergence,
      input_sha256 = digest::digest(series, algo = "sha256"),
      result_sha256 = digest::digest(file = paths$result, algo = "sha256"),
      result_bytes = unname(file.info(paths$result)$size),
      packages = ireland_package_provenance(),
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
    dir.create(dirname(paths$diagnostic), recursive = TRUE, showWarnings = FALSE)
    dir.create(dirname(paths$complete), recursive = TRUE, showWarnings = FALSE)
    saveRDS(diagnostic, paths$diagnostic, compress = "gzip")
    writeLines("complete", paths$complete)
    rm(model_list)
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
}
