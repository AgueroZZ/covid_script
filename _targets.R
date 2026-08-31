library(targets)

tar_option_set(
  packages = c(
    "BayesGP",
    "digest",
    "dplyr",
    "here",
    "lubridate",
    "readr",
    "tibble",
    "yaml"
  ),
  format = "rds",
  error = "stop"
)

invisible(lapply(
  list.files("R", pattern = "[.]R$", full.names = TRUE),
  source
))

list(
  tar_target(
    analysis_config_file,
    "config/analysis.yml",
    format = "file"
  ),
  tar_target(
    analysis_config,
    read_analysis_config(analysis_config_file)
  ),
  tar_target(
    reporting_output_registry_file,
    "config/reporting_outputs.csv",
    format = "file"
  ),
  tar_target(
    reporting_panel_registry_file,
    "config/reporting_panels.csv",
    format = "file"
  ),
  tar_target(
    wave_definitions,
    wave_table(analysis_config)
  ),
  tar_target(
    foundation_manifest,
    {
      output <- artifact_path(
        analysis_config,
        "manifests",
        "foundation.csv"
      )
      readr::write_csv(wave_definitions, output)
      output
    },
    format = "file"
  ),
  tar_target(
    us_sex_raw_files,
    us_sex_source_paths(),
    format = "file"
  ),
  tar_target(
    us_non_sex_raw_files,
    us_non_sex_source_paths(),
    format = "file"
  ),
  tar_target(
    us_sex_standardized,
    standardize_us_wonder(us_sex_raw_files, stratified_by_sex = TRUE)
  ),
  tar_target(
    us_non_sex_standardized,
    standardize_us_wonder(us_non_sex_raw_files, stratified_by_sex = FALSE)
  ),
  tar_target(
    us_sex_model_input,
    us_model_input(
      us_sex_standardized,
      analysis_config$regions$us_sex$analysis_end
    )
  ),
  tar_target(
    us_non_sex_model_input,
    us_model_input(
      us_non_sex_standardized,
      analysis_config$regions$us_non_sex$analysis_end
    )
  ),
  tar_target(
    us_sex_cohorts,
    build_us_cohort_inventory(us_sex_model_input, "us_sex", analysis_config)
  ),
  tar_target(
    us_non_sex_cohorts,
    build_us_cohort_inventory(
      us_non_sex_model_input,
      "us_non_sex",
      analysis_config
    )
  ),
  tar_target(
    us_cohort_inventory_file,
    write_csv_artifact(
      dplyr::bind_rows(us_sex_cohorts, us_non_sex_cohorts),
      artifact_path(analysis_config, "data", "us", "cohort_inventory.csv")
    ),
    format = "file"
  ),
  tar_target(
    us_vaccination_file,
    "data/raw/owid/us_state_vaccination_snapshot.rda",
    format = "file"
  ),
  tar_target(
    us_vaccination_membership,
    prepare_us_vaccination(us_vaccination_file, analysis_config)
  ),
  tar_target(
    us_historical_reporting_cohorts,
    historical_us_reporting_cohorts(us_vaccination_membership$geography)
  ),
  tar_target(
    us_vaccination_membership_file,
    write_csv_artifact(
      us_vaccination_membership,
      artifact_path(
        analysis_config,
        "data",
        "us",
        "vaccination_membership.csv"
      )
    ),
    format = "file"
  ),
  tar_target(
    us_sex_branches,
    split_us_model_branches(us_sex_model_input, "us_sex"),
    iteration = "list"
  ),
  tar_target(
    us_non_sex_branches,
    split_us_model_branches(us_non_sex_model_input, "us_non_sex"),
    iteration = "list"
  ),
  tar_target(
    us_model_smoke_branch,
    select_us_model_branch(
      us_non_sex_branches,
      "us__us-non-sex__alabama__60-79__total"
    )
  ),
  tar_target(
    us_model_smoke,
    run_us_model_smoke(us_model_smoke_branch, analysis_config)
  ),
  tar_target(
    us_sex_model_run,
    run_us_model_branch(us_sex_branches, analysis_config),
    pattern = map(us_sex_branches),
    iteration = "list"
  ),
  tar_target(
    us_non_sex_model_run,
    run_us_model_branch(us_non_sex_branches, analysis_config),
    pattern = map(us_non_sex_branches),
    iteration = "list"
  ),
  tar_target(
    us_sex_model_file,
    write_us_fit_artifact(us_sex_model_run, analysis_config),
    pattern = map(us_sex_model_run),
    format = "file"
  ),
  tar_target(
    us_non_sex_model_file,
    write_us_fit_artifact(us_non_sex_model_run, analysis_config),
    pattern = map(us_non_sex_model_run),
    format = "file"
  ),
  tar_target(
    us_sex_prediction_file,
    write_us_prediction_artifact(us_sex_model_run, analysis_config),
    pattern = map(us_sex_model_run),
    format = "file"
  ),
  tar_target(
    us_non_sex_prediction_file,
    write_us_prediction_artifact(us_non_sex_model_run, analysis_config),
    pattern = map(us_non_sex_model_run),
    format = "file"
  ),
  tar_target(
    us_sex_wave_summary,
    us_wave_summary_from_run(us_sex_model_run, analysis_config),
    pattern = map(us_sex_model_run),
    iteration = "list"
  ),
  tar_target(
    us_non_sex_wave_summary,
    us_wave_summary_from_run(us_non_sex_model_run, analysis_config),
    pattern = map(us_non_sex_model_run),
    iteration = "list"
  ),
  tar_target(
    us_wave_summary_file,
    write_csv_artifact(
      dplyr::bind_rows(us_sex_wave_summary, us_non_sex_wave_summary),
      artifact_path(analysis_config, "results", "us", "wave_summary.csv")
    ),
    format = "file"
  ),
  tar_target(
    us_sex_pointwise_summary,
    us_pointwise_summary_from_run(us_sex_model_run, analysis_config),
    pattern = map(us_sex_model_run),
    iteration = "list"
  ),
  tar_target(
    us_non_sex_pointwise_summary,
    us_pointwise_summary_from_run(us_non_sex_model_run, analysis_config),
    pattern = map(us_non_sex_model_run),
    iteration = "list"
  ),
  tar_target(
    us_pointwise_summary_file,
    write_csv_artifact(
      dplyr::bind_rows(
        us_sex_pointwise_summary,
        us_non_sex_pointwise_summary
      ),
      artifact_path(analysis_config, "results", "us", "pointwise_summary.csv")
    ),
    format = "file"
  ),
  tar_target(
    us_sex_contrasts,
    build_us_sex_contrasts(us_sex_model_run),
    iteration = "list"
  ),
  tar_target(
    us_sex_contrast_summary_file,
    write_csv_artifact(
      bind_us_sex_contrast_summaries(us_sex_contrasts),
      artifact_path(
        analysis_config,
        "results",
        "us",
        "sex_contrast_summary.csv"
      )
    ),
    format = "file"
  ),
  tar_target(
    us_model_status_file,
    write_csv_artifact(
      dplyr::bind_rows(
        lapply(us_sex_model_run, us_model_run_status),
        lapply(us_non_sex_model_run, us_model_run_status)
      ),
      artifact_path(analysis_config, "manifests", "us_model_status.csv")
    ),
    format = "file"
  ),
  tar_target(
    us_sex_standardized_file,
    write_rds_artifact(
      us_sex_standardized,
      artifact_path(analysis_config, "data", "us", "sex_standardized.rds")
    ),
    format = "file"
  ),
  tar_target(
    us_non_sex_standardized_file,
    write_rds_artifact(
      us_non_sex_standardized,
      artifact_path(analysis_config, "data", "us", "non_sex_standardized.rds")
    ),
    format = "file"
  )
)
