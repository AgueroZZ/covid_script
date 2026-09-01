supplementary_family_levels <- function() {
  c(
    "europe",
    "us_non_sex",
    "us_sex",
    "canada_non_sex",
    "canada_sex",
    "england_wales",
    "ireland"
  )
}

supplementary_expected_family_counts <- function() {
  c(
    europe = 388L,
    us_non_sex = 204L,
    us_sex = 408L,
    canada_non_sex = 40L,
    canada_sex = 72L,
    england_wales = 3L,
    ireland = 4L
  )
}

supplementary_europe_labels <- function() {
  c(
    AM = "Armenia", AT = "Austria", BE = "Belgium", BG = "Bulgaria",
    CH = "Switzerland", CY = "Cyprus", CZ = "Czechia", DE = "Germany",
    DK = "Denmark", EE = "Estonia", EL = "Greece", ES = "Spain",
    FI = "Finland", FR = "France", HR = "Croatia", HU = "Hungary",
    IS = "Iceland", IT = "Italy", LI = "Liechtenstein", LT = "Lithuania",
    LU = "Luxembourg", LV = "Latvia", ME = "Montenegro", MT = "Malta",
    NL = "Netherlands", NO = "Norway", PL = "Poland", PT = "Portugal",
    RO = "Romania", RS = "Serbia", SE = "Sweden", SI = "Slovenia",
    SK = "Slovakia"
  )
}

supplementary_us_map_ids <- function() {
  stats::setNames(c(state.abb, "DC"), c(state.name, "District of Columbia"))
}

supplementary_slug <- function(value) {
  value <- tolower(trimws(as.character(value)))
  value <- gsub("[+]", "plus", value)
  value <- gsub("[^a-z0-9]+", "-", value)
  gsub("(^-|-$)", "", value)
}

supplementary_analysis_id <- function(
  region,
  family,
  geography,
  age_group,
  sex
) {
  paste(
    supplementary_slug(region),
    supplementary_slug(family),
    supplementary_slug(geography),
    supplementary_slug(age_group),
    supplementary_slug(sex),
    sep = "__"
  )
}

normalize_europe_age <- function(age) {
  unname(c(
    `Y20-39` = "20-39",
    `Y40-59` = "40-59",
    `Y60-79` = "60-79",
    Y_GE80 = "GE80"
  )[as.character(age)])
}

normalize_supplementary_sex <- function(sex) {
  key <- as.character(sex)
  mapping <- c(
    T = "total",
    `TRUE` = "total",
    total = "total",
    F = "female",
    female = "female",
    Females = "female",
    M = "male",
    male = "male",
    Males = "male"
  )
  unname(mapping[key])
}

supplementary_model_status <- function(path, root, declared_failure = FALSE) {
  if (isTRUE(declared_failure)) return("model_failed")
  if (file.exists(file.path(root, path))) "available" else "missing_source"
}

supplementary_registry_frame <- function(...) {
  data.frame(..., stringsAsFactors = FALSE, check.names = FALSE)
}

build_europe_supplementary_registry <- function(project_root) {
  manifest_path <- file.path(
    project_root,
    "output",
    "results",
    "europe_corrected_psd_prior_20260830",
    "manifests",
    "model_manifest.csv"
  )
  manifest <- utils::read.csv(manifest_path, stringsAsFactors = FALSE)
  labels <- supplementary_europe_labels()
  age_group <- normalize_europe_age(manifest$age)
  sex <- normalize_supplementary_sex(manifest$sex)
  relative_paths <- file.path(
    "output",
    "results",
    "europe_corrected_psd_prior_20260830",
    "fitted_model",
    paste0(manifest$model_id, ".rda")
  )
  status <- vapply(
    relative_paths,
    supplementary_model_status,
    character(1L),
    root = project_root
  )
  supplementary_registry_frame(
    analysis_id = vapply(seq_len(nrow(manifest)), function(index) {
      supplementary_analysis_id(
        "europe", "europe", manifest$geo[[index]], age_group[[index]], sex[[index]]
      )
    }, character(1L)),
    analysis_family = "europe",
    region = "europe",
    geography = manifest$geo,
    geography_label = unname(labels[manifest$geo]),
    map_id = manifest$geo,
    age_group = age_group,
    sex = sex,
    frequency = "weekly",
    analysis_end = NA_character_,
    source_root = "project",
    model_path = relative_paths,
    observed_source_id = "eurostat_weekly",
    source_kind = "corrected_compact_prediction",
    status = status,
    error_message = ifelse(status == "available", NA_character_, "Missing compact prediction")
  )
}

us_historical_model_path <- function(region, geography, age_group, sex) {
  if (identical(region, "us_non_sex")) {
    source_age <- unname(c(
      `20-39` = "20-39",
      `40-59` = "40-59",
      `60-79` = "60-79",
      GE80 = "Over 80"
    )[age_group])
    return(file.path(
      "North_America", "non-stratified", "USA", "fitted_model",
      paste0(geography, "_age_", source_age, ".rda")
    ))
  }
  source_age <- unname(c(
    `0-44` = "0-44",
    `45-64` = "45-64",
    `65-84` = "65-84",
    GE85 = "Over 85"
  )[age_group])
  source_sex <- unname(c(female = "F", male = "M")[sex])
  file.path(
    "North_America", "sex-stratified", "USA", "fitted_model",
    paste0(geography, "_age_", source_age, "_sex_", source_sex, ".rda")
  )
}

build_us_supplementary_registry <- function(project_root, legacy_root) {
  cohort <- utils::read.csv(
    file.path(project_root, "config", "cohorts.csv"),
    stringsAsFactors = FALSE
  )
  relative_paths <- mapply(
    us_historical_model_path,
    cohort$region,
    cohort$jurisdiction,
    cohort$age_group,
    cohort$sex,
    USE.NAMES = FALSE
  )
  declared_failure <- cohort$analysis_id ==
    "us__us-sex__vermont__0-44__female"
  status <- mapply(
    supplementary_model_status,
    relative_paths,
    MoreArgs = list(root = legacy_root),
    declared_failure = declared_failure,
    USE.NAMES = FALSE
  )
  map_ids <- supplementary_us_map_ids()
  supplementary_registry_frame(
    analysis_id = cohort$analysis_id,
    analysis_family = cohort$region,
    region = "north_america",
    geography = cohort$jurisdiction,
    geography_label = cohort$jurisdiction,
    map_id = unname(map_ids[cohort$jurisdiction]),
    age_group = cohort$age_group,
    sex = cohort$sex,
    frequency = "monthly",
    analysis_end = "2023-08-31",
    source_root = "legacy",
    model_path = relative_paths,
    observed_source_id = ifelse(
      cohort$region == "us_sex", "cdc_wonder_sex", "cdc_wonder_non_sex"
    ),
    source_kind = "historical_compact_prediction",
    status = status,
    error_message = ifelse(
      declared_failure,
      "Registered historical model failure",
      ifelse(status == "available", NA_character_, "Missing compact prediction")
    )
  )
}

canada_historical_model_path <- function(family, province, age_group, sex) {
  source_age <- ifelse(age_group == "85+", "over 85", age_group)
  if (identical(family, "canada_non_sex")) {
    return(file.path(
      "North_America", "non-stratified", "Canada", "fitted_model",
      paste0(province, "_age_", source_age, ".rda")
    ))
  }
  source_sex <- unname(c(female = "Females", male = "Males")[sex])
  file.path(
    "North_America", "sex-stratified", "Canada", "fitted_model",
    paste0(province, "_age_", source_age, "_sex_", source_sex, ".rda")
  )
}

build_canada_supplementary_registry <- function(project_root, legacy_root) {
  data_path <- file.path(project_root, "data", "raw", "statcan", "13100768.csv")
  data <- rbind(
    read_canada_model_input(data_path, stratified_by_sex = TRUE),
    read_canada_model_input(data_path, stratified_by_sex = FALSE)
  )
  manifest <- canada_model_manifest(data)
  family <- ifelse(
    manifest$analysis_path == "sex_stratified",
    "canada_sex",
    "canada_non_sex"
  )
  sex <- normalize_supplementary_sex(manifest$sex)
  relative_paths <- mapply(
    canada_historical_model_path,
    family,
    manifest$province,
    manifest$age_group,
    sex,
    USE.NAMES = FALSE
  )
  status <- vapply(
    relative_paths,
    supplementary_model_status,
    character(1L),
    root = legacy_root
  )
  supplementary_registry_frame(
    analysis_id = mapply(
      supplementary_analysis_id,
      "canada",
      family,
      manifest$province,
      manifest$age_group,
      sex,
      USE.NAMES = FALSE
    ),
    analysis_family = family,
    region = "north_america",
    geography = manifest$province,
    geography_label = manifest$province,
    map_id = manifest$province,
    age_group = manifest$age_group,
    sex = sex,
    frequency = "weekly",
    analysis_end = NA_character_,
    source_root = "legacy",
    model_path = relative_paths,
    observed_source_id = "statcan_weekly",
    source_kind = "historical_compact_prediction",
    status = status,
    error_message = ifelse(status == "available", NA_character_, "Missing compact prediction")
  )
}

build_england_wales_supplementary_registry <- function(project_root) {
  manifest <- utils::read.csv(file.path(
    project_root,
    "output",
    "results",
    "england_wales_corrected_20260830",
    "manifests",
    "model_manifest.csv"
  ), stringsAsFactors = FALSE)
  relative_paths <- file.path(
    "output", "results", "england_wales_corrected_20260830", "fitted_model",
    paste0(manifest$model_id, ".rda")
  )
  status <- vapply(
    relative_paths,
    supplementary_model_status,
    character(1L),
    root = project_root
  )
  supplementary_registry_frame(
    analysis_id = vapply(seq_len(nrow(manifest)), function(index) {
      supplementary_analysis_id(
        "europe", "england-wales", "england-wales",
        manifest$age_group[[index]], "total"
      )
    }, character(1L)),
    analysis_family = "england_wales",
    region = "europe",
    geography = "UK",
    geography_label = "England and Wales",
    map_id = "UK",
    age_group = manifest$age_group,
    sex = "total",
    frequency = "weekly",
    analysis_end = NA_character_,
    source_root = "project",
    model_path = relative_paths,
    observed_source_id = "ons_england_wales",
    source_kind = "corrected_compact_prediction",
    status = status,
    error_message = ifelse(status == "available", NA_character_, "Missing compact prediction")
  )
}

build_ireland_supplementary_registry <- function(project_root) {
  manifest <- utils::read.csv(file.path(
    project_root,
    "output",
    "results",
    "ireland_corrected_20260830",
    "manifests",
    "model_manifest.csv"
  ), stringsAsFactors = FALSE)
  relative_paths <- file.path(
    "output", "results", "ireland_corrected_20260830", "fitted_model",
    paste0(manifest$model_id, ".rda")
  )
  status <- vapply(
    relative_paths,
    supplementary_model_status,
    character(1L),
    root = project_root
  )
  supplementary_registry_frame(
    analysis_id = vapply(seq_len(nrow(manifest)), function(index) {
      supplementary_analysis_id(
        "europe", "ireland", "ireland", manifest$age_group[[index]], "total"
      )
    }, character(1L)),
    analysis_family = "ireland",
    region = "europe",
    geography = "IE",
    geography_label = "Republic of Ireland",
    map_id = "IE",
    age_group = manifest$age_group,
    sex = "total",
    frequency = "quarterly",
    analysis_end = NA_character_,
    source_root = "project",
    model_path = relative_paths,
    observed_source_id = "cso_ireland_quarterly",
    source_kind = "corrected_compact_prediction",
    status = status,
    error_message = ifelse(status == "available", NA_character_, "Missing compact prediction")
  )
}

build_supplementary_registry <- function(
  project_root = ".",
  legacy_root = Sys.getenv("COVID_EXCESS_LEGACY_ROOT", "")
) {
  project_root <- normalizePath(project_root, winslash = "/", mustWork = TRUE)
  if (!nzchar(legacy_root)) {
    legacy_root <- file.path(dirname(project_root), "covid_excess")
  }
  legacy_root <- normalizePath(legacy_root, winslash = "/", mustWork = TRUE)
  registry <- do.call(rbind, list(
    build_europe_supplementary_registry(project_root),
    build_us_supplementary_registry(project_root, legacy_root),
    build_canada_supplementary_registry(project_root, legacy_root),
    build_england_wales_supplementary_registry(project_root),
    build_ireland_supplementary_registry(project_root)
  ))
  rownames(registry) <- NULL
  family_order <- match(registry$analysis_family, supplementary_family_levels())
  registry <- registry[order(
    family_order,
    registry$geography_label,
    registry$age_group,
    registry$sex
  ), , drop = FALSE]
  rownames(registry) <- NULL
  expected <- supplementary_expected_family_counts()
  observed <- table(factor(registry$analysis_family, levels = names(expected)))
  if (!identical(as.integer(observed), unname(expected))) {
    stop("The supplementary registry does not match the frozen family counts.")
  }
  if (anyDuplicated(registry$analysis_id)) {
    stop("The supplementary registry contains duplicate analysis IDs.")
  }
  unexpected_missing <- registry$status == "missing_source"
  if (any(unexpected_missing)) {
    stop(
      "Unexpected compact prediction files are missing: ",
      paste(registry$analysis_id[unexpected_missing], collapse = ", "),
      "."
    )
  }
  registry
}

supplementary_observed_source_inventory <- function(project_root = ".") {
  project_root <- normalizePath(project_root, winslash = "/", mustWork = TRUE)
  sources <- rbind(
    data.frame(
      observed_source_id = "eurostat_weekly",
      source_path = file.path(
        "data", "raw", "eurostat", "demo_r_mwk_20_linear.csv"
      ),
      stringsAsFactors = FALSE
    ),
    data.frame(
      observed_source_id = "cdc_wonder_sex",
      source_path = us_sex_source_paths(),
      stringsAsFactors = FALSE
    ),
    data.frame(
      observed_source_id = "cdc_wonder_non_sex",
      source_path = us_non_sex_source_paths(),
      stringsAsFactors = FALSE
    ),
    data.frame(
      observed_source_id = "statcan_weekly",
      source_path = file.path("data", "raw", "statcan", "13100768.csv"),
      stringsAsFactors = FALSE
    ),
    data.frame(
      observed_source_id = "ons_england_wales",
      source_path = file.path(
        "data",
        "raw",
        "ons",
        c(
          "daily_deaths_occurrences_1981_2020.xlsx",
          "weekly_deaths_2021_week52.xlsx",
          "weekly_deaths_2022_week52.xlsx",
          "weekly_deaths_2023_week35.xlsx"
        )
      ),
      stringsAsFactors = FALSE
    ),
    data.frame(
      observed_source_id = "cso_ireland_quarterly",
      source_path = file.path(
        "data", "raw", "cso", "ireland_quarterly_deaths.csv"
      ),
      stringsAsFactors = FALSE
    )
  )
  paths <- file.path(project_root, sources$source_path)
  if (any(!file.exists(paths))) {
    stop(
      "Canonical observed-data snapshots are missing: ",
      paste(sources$source_path[!file.exists(paths)], collapse = ", "),
      "."
    )
  }
  sources$sha256 <- vapply(
    paths,
    digest::digest,
    character(1L),
    algo = "sha256",
    file = TRUE,
    serialize = FALSE
  )
  sources$bytes <- as.numeric(file.info(paths)$size)
  rownames(sources) <- NULL
  sources
}

validate_supplementary_observed_source_inventory <- function(
  registry,
  observed_source_inventory
) {
  required <- c("observed_source_id", "source_path", "sha256", "bytes")
  if (!all(required %in% names(observed_source_inventory))) {
    stop("The observed-source inventory is missing required columns.")
  }
  if (anyDuplicated(observed_source_inventory$source_path)) {
    stop("The observed-source inventory contains duplicate file paths.")
  }
  if (!setequal(
    unique(registry$observed_source_id),
    unique(observed_source_inventory$observed_source_id)
  )) {
    stop("The observed-source inventory does not cover every registry source.")
  }
  if (any(grepl("^/", observed_source_inventory$source_path)) ||
      any(!grepl("^[a-f0-9]{64}$", observed_source_inventory$sha256)) ||
      any(!is.finite(observed_source_inventory$bytes)) ||
      any(observed_source_inventory$bytes <= 0)) {
    stop("The observed-source inventory contains an invalid public record.")
  }
  invisible(TRUE)
}

read_supplementary_observed_data <- function(project_root, config) {
  project_root <- normalizePath(project_root, winslash = "/", mustWork = TRUE)
  us_sex <- us_model_input(
    standardize_us_wonder(
      file.path(project_root, us_sex_source_paths()),
      stratified_by_sex = TRUE
    ),
    config$regions$us_sex$analysis_end
  )
  us_non_sex <- us_model_input(
    standardize_us_wonder(
      file.path(project_root, us_non_sex_source_paths()),
      stratified_by_sex = FALSE
    ),
    config$regions$us_non_sex$analysis_end
  )
  canada <- rbind(
    read_canada_model_input(
      file.path(project_root, "data", "raw", "statcan", "13100768.csv"),
      stratified_by_sex = TRUE
    ),
    read_canada_model_input(
      file.path(project_root, "data", "raw", "statcan", "13100768.csv"),
      stratified_by_sex = FALSE
    )
  )
  canada$analysis_family <- ifelse(
    canada$analysis_path == "sex_stratified", "canada_sex", "canada_non_sex"
  )
  canada$sex <- normalize_supplementary_sex(canada$sex)
  names(canada)[names(canada) == "province"] <- "geography"
  names(canada)[names(canada) == "age"] <- "age_group"
  names(canada)[names(canada) == "death"] <- "observed_deaths"

  europe <- read_corrected_europe_observed(file.path(
    project_root, "data", "raw", "eurostat", "demo_r_mwk_20_linear.csv"
  ))
  europe$age_group <- normalize_europe_age(europe$age_group)
  europe$sex <- normalize_supplementary_sex(europe$sex)

  england_wales <- read_england_wales_model_input(file.path(
    project_root, "data", "raw", "ons"
  ))
  england_wales$geography <- "UK"
  ireland <- read_ireland_model_input(file.path(
    project_root, "data", "raw", "cso", "ireland_quarterly_deaths.csv"
  ))
  ireland$geography <- "IE"

  list(
    europe = europe,
    us_non_sex = us_non_sex,
    us_sex = us_sex,
    canada_non_sex = canada[canada$analysis_family == "canada_non_sex", ],
    canada_sex = canada[canada$analysis_family == "canada_sex", ],
    england_wales = england_wales,
    ireland = ireland
  )
}

resolve_supplementary_model_path <- function(row, project_root, legacy_root) {
  root <- switch(
    row$source_root[[1]],
    project = project_root,
    legacy = legacy_root,
    stop("Unknown supplementary source root: ", row$source_root[[1]], ".")
  )
  file.path(root, row$model_path[[1]])
}

load_supplementary_model_pred <- function(path) {
  if (!file.exists(path)) stop("Compact prediction does not exist: ", path, ".")
  environment <- new.env(parent = emptyenv())
  loaded <- load(path, envir = environment)
  if (!identical(loaded, "model_pred")) {
    stop("Compact prediction must contain only model_pred: ", path, ".")
  }
  model_pred <- environment$model_pred
  if (!is.list(model_pred) ||
      !all(c("samples", "summary") %in% names(model_pred)) ||
      !is.matrix(model_pred$samples)) {
    stop("Compact prediction has an invalid structure: ", path, ".")
  }
  required_summary <- c("mean", "lower", "upper", "time")
  if (!all(required_summary %in% names(model_pred$summary))) {
    stop("Compact prediction summary is incomplete: ", path, ".")
  }
  if (nrow(model_pred$samples) != nrow(model_pred$summary) ||
      ncol(model_pred$samples) != 3000L) {
    stop("Compact prediction dimensions are invalid: ", path, ".")
  }
  model_pred
}

load_supplementary_prediction <- function(
  registry_row,
  observed_data,
  project_root,
  legacy_root
) {
  if (nrow(registry_row) != 1L) stop("Exactly one registry row is required.")
  if (!identical(registry_row$status[[1]], "available")) {
    stop("Cannot load a non-available supplementary cohort.")
  }
  path <- resolve_supplementary_model_path(
    registry_row, project_root, legacy_root
  )
  model_pred <- load_supplementary_model_pred(path)
  dates <- as.Date(model_pred$summary$time)
  keep <- rep(TRUE, length(dates))
  if (!is.na(registry_row$analysis_end[[1]])) {
    keep <- dates <= as.Date(registry_row$analysis_end[[1]])
  }
  dates <- dates[keep]
  samples <- model_pred$samples[keep, , drop = FALSE]
  summary <- model_pred$summary[keep, , drop = FALSE]

  family <- registry_row$analysis_family[[1]]
  observed <- observed_data[[family]]
  selected <- observed[
    observed$geography == registry_row$geography[[1]] &
      observed$age_group == registry_row$age_group[[1]] &
      observed$sex == registry_row$sex[[1]],
    ,
    drop = FALSE
  ]
  observed_index <- match(dates, as.Date(selected$date))
  if (anyNA(observed_index)) {
    stop(
      "Observed counts do not align with prediction dates for ",
      registry_row$analysis_id[[1]], "."
    )
  }
  list(
    analysis_id = registry_row$analysis_id[[1]],
    analysis_family = family,
    geography = registry_row$geography[[1]],
    geography_label = registry_row$geography_label[[1]],
    map_id = registry_row$map_id[[1]],
    age_group = registry_row$age_group[[1]],
    sex = registry_row$sex[[1]],
    frequency = registry_row$frequency[[1]],
    dates = dates,
    observed_deaths = as.numeric(selected$observed_deaths[observed_index]),
    samples = samples,
    expected_mean = as.numeric(summary$mean),
    expected_lower = as.numeric(summary$lower),
    expected_upper = as.numeric(summary$upper),
    model_path = path
  )
}

supplementary_row_quantiles <- function(matrix, probabilities) {
  if (requireNamespace("matrixStats", quietly = TRUE)) {
    output <- matrixStats::rowQuantiles(
      matrix,
      probs = probabilities,
      na.rm = TRUE,
      drop = FALSE
    )
  } else {
    output <- t(apply(
      matrix,
      1L,
      stats::quantile,
      probs = probabilities,
      na.rm = TRUE,
      names = FALSE
    ))
  }
  output[!is.finite(output)] <- NA_real_
  output
}

supplementary_safe_quantiles <- function(values, probabilities) {
  values <- values[is.finite(values)]
  if (length(values) == 0L) return(rep(NA_real_, length(probabilities)))
  unname(stats::quantile(values, probs = probabilities, names = FALSE))
}

summarize_supplementary_prediction <- function(
  prediction,
  wave_definitions,
  interval = c(0.025, 0.975)
) {
  samples <- prediction$samples
  observed_matrix <- matrix(
    prediction$observed_deaths,
    nrow = nrow(samples),
    ncol = ncol(samples)
  )
  pscore <- (observed_matrix - samples) / samples
  pscore[samples == 0] <- NA_real_
  probabilities <- c(interval[[1]], 0.5, interval[[2]])
  p_quantiles <- supplementary_row_quantiles(pscore, probabilities)
  p_mean <- rowMeans(pscore, na.rm = TRUE)
  p_mean[!is.finite(p_mean)] <- NA_real_
  p_variance <- if (requireNamespace("matrixStats", quietly = TRUE)) {
    matrixStats::rowVars(pscore, na.rm = TRUE)
  } else {
    apply(pscore, 1L, stats::var, na.rm = TRUE)
  }
  p_variance[!is.finite(p_variance)] <- NA_real_
  expected_median <- if (requireNamespace("matrixStats", quietly = TRUE)) {
    matrixStats::rowMedians(samples, na.rm = TRUE)
  } else {
    apply(samples, 1L, stats::median, na.rm = TRUE)
  }

  common <- list(
    analysis_id = prediction$analysis_id,
    analysis_family = prediction$analysis_family,
    geography = prediction$geography,
    geography_label = prediction$geography_label,
    map_id = prediction$map_id,
    age_group = prediction$age_group,
    sex = prediction$sex,
    frequency = prediction$frequency
  )
  pointwise <- data.frame(
    common,
    date = as.Date(prediction$dates),
    observed_deaths = prediction$observed_deaths,
    expected_mean = prediction$expected_mean,
    expected_median = expected_median,
    expected_lower = prediction$expected_lower,
    expected_upper = prediction$expected_upper,
    p_mean = p_mean,
    p_variance = p_variance,
    p_lower = p_quantiles[, 1L],
    p_median = p_quantiles[, 2L],
    p_upper = p_quantiles[, 3L],
    posterior_draws = ncol(samples),
    status = "success",
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  wave_rows <- lapply(seq_len(nrow(wave_definitions)), function(index) {
    selected <- prediction$dates >= wave_definitions$start[[index]] &
      prediction$dates < wave_definitions$end_exclusive[[index]]
    if (!any(selected)) {
      return(data.frame(
        common,
        wave = wave_definitions$wave[[index]],
        start = as.Date(wave_definitions$start[[index]]),
        end_exclusive = as.Date(wave_definitions$end_exclusive[[index]]),
        observed_periods = 0L,
        observed_deaths = NA_real_,
        expected_lower = NA_real_,
        expected_median = NA_real_,
        expected_upper = NA_real_,
        delta_lower = NA_real_,
        delta_median = NA_real_,
        delta_upper = NA_real_,
        p_lower = NA_real_,
        p_median = NA_real_,
        p_upper = NA_real_,
        posterior_draws = ncol(samples),
        status = "no_observed_periods",
        stringsAsFactors = FALSE,
        check.names = FALSE
      ))
    }
    expected <- colSums(samples[selected, , drop = FALSE])
    observed <- sum(prediction$observed_deaths[selected])
    excess <- observed - expected
    wave_pscore <- excess / expected
    wave_pscore[expected == 0] <- NA_real_
    expected_quantiles <- supplementary_safe_quantiles(expected, probabilities)
    excess_quantiles <- supplementary_safe_quantiles(excess, probabilities)
    pscore_quantiles <- supplementary_safe_quantiles(wave_pscore, probabilities)
    data.frame(
      common,
      wave = wave_definitions$wave[[index]],
      start = as.Date(wave_definitions$start[[index]]),
      end_exclusive = as.Date(wave_definitions$end_exclusive[[index]]),
      observed_periods = sum(selected),
      observed_deaths = observed,
      expected_lower = expected_quantiles[[1]],
      expected_median = expected_quantiles[[2]],
      expected_upper = expected_quantiles[[3]],
      delta_lower = excess_quantiles[[1]],
      delta_median = excess_quantiles[[2]],
      delta_upper = excess_quantiles[[3]],
      p_lower = pscore_quantiles[[1]],
      p_median = pscore_quantiles[[2]],
      p_upper = pscore_quantiles[[3]],
      posterior_draws = ncol(samples),
      status = ifelse(
        all(is.na(wave_pscore)), "undefined_expected_zero", "success"
      ),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  })
  list(pointwise = pointwise, wave = do.call(rbind, wave_rows))
}

supplementary_failed_wave_rows <- function(registry_row, wave_definitions) {
  data.frame(
    analysis_id = registry_row$analysis_id[[1]],
    analysis_family = registry_row$analysis_family[[1]],
    geography = registry_row$geography[[1]],
    geography_label = registry_row$geography_label[[1]],
    map_id = registry_row$map_id[[1]],
    age_group = registry_row$age_group[[1]],
    sex = registry_row$sex[[1]],
    frequency = registry_row$frequency[[1]],
    wave = wave_definitions$wave,
    start = as.Date(wave_definitions$start),
    end_exclusive = as.Date(wave_definitions$end_exclusive),
    observed_periods = NA_integer_,
    observed_deaths = NA_real_,
    expected_lower = NA_real_,
    expected_median = NA_real_,
    expected_upper = NA_real_,
    delta_lower = NA_real_,
    delta_median = NA_real_,
    delta_upper = NA_real_,
    p_lower = NA_real_,
    p_median = NA_real_,
    p_upper = NA_real_,
    posterior_draws = NA_integer_,
    status = "model_failed",
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

supplementary_web_shard <- function(registry, pointwise) {
  if (nrow(registry) == 0L) stop("A web shard requires registry rows.")
  available <- registry[registry$status == "available", , drop = FALSE]
  series <- lapply(available$analysis_id, function(analysis_id) {
    selected <- pointwise[pointwise$analysis_id == analysis_id, , drop = FALSE]
    list(
      analysis_id = analysis_id,
      age_group = selected$age_group[[1]],
      sex = selected$sex[[1]],
      frequency = selected$frequency[[1]],
      date = format(as.Date(selected$date), "%Y-%m-%d"),
      observed = selected$observed_deaths,
      expected_mean = selected$expected_mean,
      expected_median = selected$expected_median,
      expected_lower = selected$expected_lower,
      expected_upper = selected$expected_upper,
      p_mean = selected$p_mean,
      p_lower = selected$p_lower,
      p_median = selected$p_median,
      p_upper = selected$p_upper,
      status = selected$status
    )
  })
  failed <- registry[registry$status != "available", c(
    "analysis_id", "age_group", "sex", "status", "error_message"
  ), drop = FALSE]
  list(
    schema_version = "1.0.0",
    analysis_family = unique(registry$analysis_family),
    geography = unique(registry$geography),
    geography_label = unique(registry$geography_label),
    map_id = unique(registry$map_id),
    series = unname(series),
    unavailable = failed
  )
}

validate_interval_columns <- function(data, lower, middle, upper, label) {
  complete <- stats::complete.cases(data[c(lower, middle, upper)])
  valid <- data[[lower]][complete] <= data[[middle]][complete] &
    data[[middle]][complete] <= data[[upper]][complete]
  if (!all(valid)) stop(label, " intervals are not ordered.")
  invisible(TRUE)
}

validate_bound_columns <- function(data, lower, upper, label) {
  complete <- stats::complete.cases(data[c(lower, upper)])
  if (!all(data[[lower]][complete] <= data[[upper]][complete])) {
    stop(label, " lower bounds exceed upper bounds.")
  }
  invisible(TRUE)
}

validate_supplementary_bundle_tables <- function(
  registry,
  pointwise,
  wave_summary,
  source_inventory
) {
  if (anyDuplicated(registry$analysis_id)) {
    stop("Registry analysis IDs are not unique.")
  }
  available_ids <- registry$analysis_id[registry$status == "available"]
  failed_ids <- registry$analysis_id[registry$status == "model_failed"]
  if (!setequal(unique(pointwise$analysis_id), available_ids)) {
    stop("Pointwise summaries do not cover every available cohort.")
  }
  if (anyDuplicated(pointwise[c("analysis_id", "date")])) {
    stop("Pointwise summary keys are not unique.")
  }
  if (!setequal(unique(wave_summary$analysis_id), registry$analysis_id)) {
    stop("Wave summaries do not cover every registered cohort.")
  }
  wave_counts <- table(wave_summary$analysis_id)
  if (any(wave_counts != 4L)) {
    stop("Every registered cohort must have four wave-status rows.")
  }
  expected_waves <- c("initial", "alpha", "delta", "omicron")
  observed_waves <- split(wave_summary$wave, wave_summary$analysis_id)
  if (!all(vapply(
    observed_waves,
    function(value) identical(as.character(value), expected_waves),
    logical(1L)
  ))) {
    stop("Wave summaries are not in canonical order.")
  }
  if (length(failed_ids) > 0L) {
    failed_status <- wave_summary$status[wave_summary$analysis_id %in% failed_ids]
    if (!all(failed_status == "model_failed")) {
      stop("Failed cohorts must retain explicit failed wave rows.")
    }
  }
  if (!setequal(source_inventory$analysis_id, available_ids)) {
    stop("The source inventory does not cover every available cohort.")
  }
  if (anyDuplicated(source_inventory$analysis_id) ||
      any(!grepl("^[a-f0-9]{64}$", source_inventory$sha256))) {
    stop("The source inventory hashes are invalid.")
  }
  numeric_pointwise <- unlist(pointwise[vapply(pointwise, is.numeric, logical(1L))])
  numeric_waves <- unlist(wave_summary[vapply(wave_summary, is.numeric, logical(1L))])
  if (any(is.infinite(c(numeric_pointwise, numeric_waves)))) {
    stop("Supplementary summaries contain infinite values.")
  }
  validate_bound_columns(pointwise, "expected_lower", "expected_upper", "Expected")
  validate_interval_columns(
    pointwise,
    "expected_lower",
    "expected_median",
    "expected_upper",
    "Expected median"
  )
  validate_interval_columns(
    pointwise, "p_lower", "p_median", "p_upper", "Pointwise p-score"
  )
  validate_interval_columns(
    wave_summary, "expected_lower", "expected_median", "expected_upper",
    "Wave expected"
  )
  validate_interval_columns(
    wave_summary, "delta_lower", "delta_median", "delta_upper", "Wave excess"
  )
  validate_interval_columns(
    wave_summary, "p_lower", "p_median", "p_upper", "Wave p-score"
  )
  invisible(TRUE)
}
