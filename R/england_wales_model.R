england_wales_age_groups <- function() {
  c("Under 65", "65-84", "85+")
}

england_wales_geography <- function() {
  "England and Wales"
}

england_wales_harmonics <- function() {
  4L
}

england_wales_iso_monday <- function(year, week) {
  ISOweek::ISOweek2date(sprintf(
    "%04d-W%02d-1",
    as.integer(year),
    as.integer(week)
  ))
}

england_wales_source_definitions <- function() {
  data.frame(
    period = c("1981-2020", "2021-2023"),
    count_definition = c(
      paste(
        "death occurrence among usual residents of England and Wales;",
        "the source includes deaths registered by December 31, 2020"
      ),
      paste(
        "death registration in England and Wales, including non-residents;",
        "the source is provisional"
      )
    ),
    source_frequency = rep("weekly", 2L),
    stringsAsFactors = FALSE
  )
}

england_wales_age_from_historical <- function(age) {
  mapped <- ifelse(
    age == "Under 65",
    "Under 65",
    ifelse(
      age %in% c("65-74", "75-84"),
      "65-84",
      ifelse(age == "85 and over", "85+", NA_character_)
    )
  )
  mapped
}

england_wales_age_from_detailed <- function(age) {
  age <- trimws(as.character(age))
  under_65 <- c(
    "<1", "1-4", "01-04", "05-09", "5-9", "10-14", "15-19",
    "20-24", "25-29", "30-34", "35-39", "40-44", "45-49",
    "50-54", "55-59", "60-64"
  )
  mapped <- ifelse(
    age %in% under_65,
    "Under 65",
    ifelse(
      age %in% c("65-69", "70-74", "75-79", "80-84"),
      "65-84",
      ifelse(age %in% c("85-89", "90+"), "85+", NA_character_)
    )
  )
  mapped
}

read_england_wales_historical <- function(path) {
  if (!file.exists(path)) {
    stop("England-and-Wales historical workbook does not exist: ", path, ".")
  }
  raw <- readxl::read_excel(path, sheet = "Table 2", skip = 3)
  year_columns <- grep("^[0-9]{4}$", names(raw), value = TRUE)
  if (length(year_columns) == 0L) {
    stop("No year columns were found in the historical workbook.")
  }
  long <- tidyr::pivot_longer(
    raw,
    cols = dplyr::all_of(year_columns),
    names_to = "calendar_year",
    values_to = "observed_deaths"
  )
  long$date_daily <- as.Date(ISOdate(
    as.integer(long$calendar_year),
    as.integer(long$Month),
    as.integer(long$Day)
  ))
  long$iso_year <- lubridate::isoyear(long$date_daily)
  long$iso_week <- lubridate::isoweek(long$date_daily)

  weekly_region <- long |>
    dplyr::group_by(
      .data$`Region code`,
      .data$Age,
      .data$iso_year,
      .data$iso_week
    ) |>
    dplyr::summarise(
      observed_deaths = sum(.data$observed_deaths),
      observed_days = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::filter(.data$observed_days == 7L)

  weekly <- weekly_region |>
    dplyr::mutate(
      age_group = england_wales_age_from_historical(.data$Age)
    ) |>
    dplyr::filter(!is.na(.data$age_group)) |>
    dplyr::group_by(.data$iso_year, .data$iso_week, .data$age_group) |>
    dplyr::summarise(
      observed_deaths = sum(.data$observed_deaths),
      .groups = "drop"
    )

  definition <- england_wales_source_definitions()[1L, , drop = FALSE]
  data.frame(
    date = england_wales_iso_monday(weekly$iso_year, weekly$iso_week),
    geography = england_wales_geography(),
    age_group = weekly$age_group,
    sex = "total",
    observed_deaths = as.integer(round(weekly$observed_deaths)),
    iso_year = as.integer(weekly$iso_year),
    iso_week = as.integer(weekly$iso_week),
    count_definition = definition$count_definition,
    source_frequency = definition$source_frequency,
    source_period = definition$period,
    source_id = "ons_england_wales_occurrence_1981_2020",
    stringsAsFactors = FALSE
  )
}

read_england_wales_2021 <- function(path) {
  raw <- readxl::read_excel(
    path,
    sheet = "Weekly figures 2021",
    skip = 16,
    n_max = 20
  )
  if (ncol(raw) < 53L) {
    stop("The 2021 workbook does not contain 52 weekly columns.")
  }
  raw <- raw[, seq_len(53L), drop = FALSE]
  names(raw) <- c("source_age", as.character(seq_len(52L)))
  long <- tidyr::pivot_longer(
    raw,
    cols = -"source_age",
    names_to = "iso_week",
    values_to = "observed_deaths"
  )
  long$iso_week <- as.integer(long$iso_week)
  long$iso_year <- 2021L
  long
}

read_england_wales_later_year <- function(path, year, n_weeks) {
  raw <- readxl::read_excel(
    path,
    sheet = "2",
    skip = 6,
    n_max = n_weeks
  )
  if (ncol(raw) < 23L) {
    stop("The ", year, " workbook does not contain the expected age columns.")
  }
  selected <- raw[, c(1L, 4:23), drop = FALSE]
  names(selected)[[1]] <- "iso_week"
  long <- tidyr::pivot_longer(
    selected,
    cols = -"iso_week",
    names_to = "source_age",
    values_to = "observed_deaths"
  )
  long$iso_week <- as.integer(long$iso_week)
  long$iso_year <- as.integer(year)
  long
}

read_england_wales_recent <- function(root) {
  paths <- c(
    `2021` = file.path(root, "publishedweek522021.xlsx"),
    `2022` = file.path(root, "publicationfileweek522022.xlsx"),
    `2023` = file.path(root, "publicationfileweek352023.xlsx")
  )
  missing <- paths[!file.exists(paths)]
  if (length(missing) > 0L) {
    stop("Missing England-and-Wales workbooks: ", paste(missing, collapse = ", "), ".")
  }
  long <- dplyr::bind_rows(
    read_england_wales_2021(paths[["2021"]]),
    read_england_wales_later_year(paths[["2022"]], 2022L, 52L),
    read_england_wales_later_year(paths[["2023"]], 2023L, 35L)
  )
  long$iso_week <- as.integer(long$iso_week)
  long$observed_deaths <- as.numeric(long$observed_deaths)
  long$age_group <- england_wales_age_from_detailed(long$source_age)
  long <- long[!is.na(long$age_group), , drop = FALSE]
  weekly <- long |>
    dplyr::group_by(.data$iso_year, .data$iso_week, .data$age_group) |>
    dplyr::summarise(
      observed_deaths = sum(.data$observed_deaths),
      .groups = "drop"
    )
  definition <- england_wales_source_definitions()[2L, , drop = FALSE]
  data.frame(
    date = england_wales_iso_monday(weekly$iso_year, weekly$iso_week),
    geography = england_wales_geography(),
    age_group = weekly$age_group,
    sex = "total",
    observed_deaths = as.integer(round(weekly$observed_deaths)),
    iso_year = as.integer(weekly$iso_year),
    iso_week = as.integer(weekly$iso_week),
    count_definition = definition$count_definition,
    source_frequency = definition$source_frequency,
    source_period = definition$period,
    source_id = "ons_england_wales_registration_2021_2023",
    stringsAsFactors = FALSE
  )
}

validate_england_wales_model_input <- function(data) {
  required <- c(
    "date", "geography", "age_group", "sex", "observed_deaths",
    "iso_year", "iso_week", "count_definition", "source_frequency",
    "source_period", "source_id"
  )
  missing <- setdiff(required, names(data))
  if (length(missing) > 0L) {
    stop("England-and-Wales input is missing: ", paste(missing, collapse = ", "), ".")
  }
  if (!inherits(data$date, "Date")) {
    stop("England-and-Wales dates must be Date values.")
  }
  if (!setequal(unique(data$age_group), england_wales_age_groups())) {
    stop("England-and-Wales age groups do not match the source contract.")
  }
  if (anyNA(data$observed_deaths) || any(data$observed_deaths < 0L)) {
    stop("England-and-Wales death counts must be observed non-negative integers.")
  }
  keys <- data[c("date", "geography", "age_group", "sex")]
  if (anyDuplicated(keys)) {
    stop("England-and-Wales model rows are not unique.")
  }
  invisible(data)
}

read_england_wales_model_input <- function(root) {
  historical <- read_england_wales_historical(file.path(
    root,
    "dailydeaths19812020.xlsx"
  ))
  recent <- read_england_wales_recent(root)
  data <- dplyr::bind_rows(historical, recent)
  data <- data[order(
    match(data$age_group, england_wales_age_groups()),
    data$date
  ), , drop = FALSE]
  rownames(data) <- NULL
  validate_england_wales_model_input(data)
  data
}

build_england_wales_manifest <- function(data, base_seed = 20260830L) {
  validate_england_wales_model_input(data)
  rows <- lapply(seq_along(england_wales_age_groups()), function(index) {
    age_group <- england_wales_age_groups()[[index]]
    selected <- data[data$age_group == age_group, , drop = FALSE]
    data.frame(
      model_index = index,
      model_id = paste0(
        "England_Wales_",
        c("under_65", "65_84", "85_plus")[[index]]
      ),
      geo = "UK",
      geography = england_wales_geography(),
      age = age_group,
      age_group = age_group,
      sex = "T",
      full_rows = nrow(selected),
      training_rows = sum(selected$date < as.Date("2020-01-01")),
      prediction_start = min(selected$date),
      prediction_end = max(selected$date),
      year_span = diff(range(as.integer(format(selected$date, "%Y")))),
      k_IWP = 100L,
      k_sGP = 40L,
      seed = as.integer(base_seed) + index,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

prepare_england_wales_series <- function(data, manifest_row) {
  selected <- data[
    data$age_group == manifest_row$age_group[[1]],
    ,
    drop = FALSE
  ]
  selected <- selected[order(selected$date), , drop = FALSE]
  selected$OBS_VALUE <- selected$observed_deaths
  selected$x <- (
    as.numeric(selected$date) - min(as.numeric(selected$date))
  ) / 365
  training <- selected[selected$date < as.Date("2020-01-01"), , drop = FALSE]
  training$x1 <- training$x
  training$x2 <- training$x
  training$observation_id <- seq_len(nrow(training))
  list(
    full_data = selected,
    training_data = training,
    x_full = selected$x,
    full_region = range(selected$x)
  )
}

england_wales_prior_specification <- function() {
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

fit_england_wales_bayesgp <- function(series, manifest_row) {
  if (!exists("fit_europe_bayesgp", mode = "function")) {
    stop("Source R/europe_model.R before fitting England and Wales.")
  }
  fit_europe_bayesgp(series, manifest_row)
}

england_wales_output_paths <- function(output_root, model_id) {
  list(
    result = file.path(output_root, "fitted_model", paste0(model_id, ".rda")),
    summary = file.path(
      output_root,
      "wave_summary",
      paste0(model_id, ".csv")
    ),
    diagnostic = file.path(
      output_root,
      "diagnostics",
      paste0(model_id, ".rds")
    ),
    complete = file.path(output_root, "complete", paste0(model_id, ".flag"))
  )
}

summarize_england_wales_waves <- function(model_pred, full_data) {
  validate_europe_model_pred(
    model_pred,
    expected_rows = nrow(full_data),
    expected_draws = ncol(model_pred$samples)
  )
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
  rows <- lapply(seq_len(nrow(definitions)), function(index) {
    selected <- model_pred$summary$time >= definitions$start[[index]] &
      model_pred$summary$time < definitions$end_exclusive[[index]]
    if (!any(selected)) {
      return(NULL)
    }
    expected <- colSums(model_pred$samples[selected, , drop = FALSE])
    observed <- sum(full_data$observed_deaths[selected])
    excess <- observed - expected
    pscore <- excess / expected
    data.frame(
      wave = definitions$wave[[index]],
      wave_start = definitions$start[[index]],
      wave_end_exclusive = definitions$end_exclusive[[index]],
      delta_upper = unname(stats::quantile(excess, 0.975)),
      delta_med = stats::median(excess),
      delta_lower = unname(stats::quantile(excess, 0.025)),
      p_upper = unname(stats::quantile(pscore, 0.975)),
      p_med = stats::median(pscore),
      p_lower = unname(stats::quantile(pscore, 0.025)),
      observed_deaths = observed,
      posterior_draws = length(expected),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

write_england_wales_wave_summary <- function(model_pred, series, path, age_group) {
  summary <- summarize_england_wales_waves(model_pred, series$full_data)
  summary$geography <- england_wales_geography()
  summary$age_group <- age_group
  summary$source_frequency <- "weekly"
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(summary, path, row.names = FALSE)
  summary
}

validate_existing_england_wales_output <- function(
  paths,
  series,
  draws = 3000L
) {
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

run_england_wales_model <- function(
  manifest_row,
  data,
  output_root,
  force = FALSE,
  draws = 3000L
) {
  model_id <- manifest_row$model_id[[1]]
  paths <- england_wales_output_paths(output_root, model_id)
  series <- prepare_england_wales_series(data, manifest_row)
  if (!force && validate_existing_england_wales_output(paths, series, draws)) {
    model_pred <- load_europe_model_pred(paths$result)
    write_england_wales_wave_summary(
      model_pred,
      series,
      paths$summary,
      manifest_row$age_group[[1]]
    )
    return(data.frame(
      model_id = model_id,
      status = "skipped_valid",
      elapsed_seconds = 0,
      message = "Existing compact output passed validation.",
      stringsAsFactors = FALSE
    ))
  }

  started <- Sys.time()
  tryCatch({
    fitted_model <- fit_england_wales_bayesgp(series, manifest_row)
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
    write_england_wales_wave_summary(
      prediction$model_pred,
      series,
      paths$summary,
      manifest_row$age_group[[1]]
    )
    diagnostic <- list(
      status = "complete",
      model_id = model_id,
      geography = england_wales_geography(),
      manuscript_label = "UK",
      age_group = manifest_row$age_group[[1]],
      sex = "total",
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
      data_contract = list(
        source_definitions = england_wales_source_definitions(),
        source_frequency = "weekly",
        date_convention = "ISO-week Monday",
        source_boundary_gap = c(
          last_occurrence_week = "2020-12-21",
          first_registration_week = "2021-01-04"
        )
      ),
      model = list(
        family = "Poisson",
        trend = "IWP2",
        seasonal_harmonics = england_wales_harmonics(),
        k_IWP = manifest_row$k_IWP[[1]],
        k_sGP = manifest_row$k_sGP[[1]],
        aghq_nodes = 5L,
        prior = england_wales_prior_specification()
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
}
