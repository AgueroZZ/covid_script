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
  )
)
