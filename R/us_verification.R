verify_us_completion <- function(root = ".") {
  required_paths <- file.path(
    root,
    c(
      "data/raw/manifest.csv",
      "config/cohorts.csv",
      "artifacts/manifests/us_model_status.csv",
      "artifacts/results/us/wave_summary.csv",
      "artifacts/results/us/pointwise_summary.csv",
      "artifacts/results/us/sex_contrast_summary.csv",
      "artifacts/data/us/vaccination_membership.csv"
    )
  )
  require_files(required_paths, "US completion")

  raw_manifest <- readr::read_csv(required_paths[[1]], show_col_types = FALSE)
  us_source_ids <- c(
    "cdc_wonder_sex",
    "cdc_wonder_non_sex",
    "owid_vaccination",
    "us_census_boundary"
  )
  us_raw <- raw_manifest[raw_manifest$source_id %in% us_source_ids, ]
  if (nrow(us_raw) != 16L) {
    stop("The US raw manifest must contain exactly 16 files.")
  }
  validate_file_manifest(us_raw, root = root)

  cohorts <- readr::read_csv(required_paths[[2]], show_col_types = FALSE)
  us_cohorts <- cohorts[cohorts$region %in% c("us_sex", "us_non_sex"), ]
  if (nrow(us_cohorts) != 612L || anyDuplicated(us_cohorts$analysis_id)) {
    stop("The US cohort registry must contain 612 unique analysis identifiers.")
  }

  status <- readr::read_csv(required_paths[[3]], show_col_types = FALSE)
  if (nrow(status) != 612L || !setequal(status$analysis_id, us_cohorts$analysis_id)) {
    stop("US model status does not cover the complete cohort registry.")
  }
  allowed_failure <- "us__us-sex__vermont__0-44__female"
  failures <- status$analysis_id[status$status != "success"]
  unexpected_failures <- setdiff(failures, allowed_failure)
  if (length(unexpected_failures) > 0L) {
    stop(
      "Unexpected US model failures: ",
      paste(unexpected_failures, collapse = ", ")
    )
  }

  wave <- readr::read_csv(required_paths[[4]], show_col_types = FALSE)
  if (nrow(wave) != 612L * 4L) {
    stop("US wave summary must retain four rows for every model branch.")
  }
  if (!setequal(unique(wave$wave), c("initial", "alpha", "delta", "omicron"))) {
    stop("US wave summary contains non-canonical wave labels.")
  }
  wave_counts <- table(wave$analysis_id)
  if (any(wave_counts != 4L)) {
    stop("Every US analysis identifier must have exactly four wave rows.")
  }

  pointwise <- readr::read_csv(required_paths[[5]], show_col_types = FALSE)
  if (nrow(pointwise) == 0L || anyDuplicated(pointwise[c("analysis_id", "date")])) {
    stop("US pointwise summaries are empty or contain duplicate dates.")
  }

  contrasts <- readr::read_csv(required_paths[[6]], show_col_types = FALSE)
  if (nrow(contrasts) == 0L ||
      !identical(unique(contrasts$contrast), "female_minus_male")) {
    stop("US sex contrasts are absent or use the wrong contrast direction.")
  }

  vaccination <- readr::read_csv(required_paths[[7]], show_col_types = FALSE)
  disputed <- vaccination[
    vaccination$geography %in% c("Texas", "Arkansas", "California", "New York"),
  ]
  if (nrow(disputed) != 4L || any(disputed$vaccination_group != "neither")) {
    stop("Disputed US vaccination classifications do not match the fixed rule.")
  }

  model_files <- list.files(
    file.path(root, "artifacts", "models", "us"),
    pattern = "[.]rds$",
    full.names = TRUE
  )
  prediction_files <- list.files(
    file.path(root, "artifacts", "results", "us", "predictions"),
    pattern = "[.]rds$",
    full.names = TRUE
  )
  if (length(model_files) != 612L || length(prediction_files) != 612L) {
    stop("US model and prediction artifact counts must both equal 612.")
  }

  list(
    cohorts = nrow(us_cohorts),
    successful_models = sum(status$status == "success"),
    failed_models = length(failures),
    wave_rows = nrow(wave),
    pointwise_rows = nrow(pointwise),
    sex_contrast_rows = nrow(contrasts)
  )
}
