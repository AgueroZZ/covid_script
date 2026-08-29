library(targets)

tar_option_set(
  packages = c("digest", "dplyr", "here", "readr", "tibble", "yaml"),
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
