#!/usr/bin/env Rscript

script_argument <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(script_argument) != 1L) {
  stop("Unable to determine the vaccination explorer builder path.")
}
script_path <- normalizePath(sub("^--file=", "", script_argument))
project_root <- normalizePath(file.path(dirname(script_path), "..", ".."))

suppressPackageStartupMessages({
  library(digest)
  library(dplyr)
  library(ISOweek)
  library(jsonlite)
  library(lubridate)
  library(yaml)
})

source_files <- c(
  "R/config.R",
  "R/validation.R",
  "R/waves.R",
  "R/us_data.R",
  "R/europe_reporting.R",
  "R/reporting.R",
  "R/vaccination.R",
  "R/supplementary_bundle.R",
  "R/supplementary_vaccination_groups.R"
)
invisible(lapply(file.path(project_root, source_files), source))

parse_arguments <- function(arguments) {
  defaults <- list(
    output_root = file.path(
      project_root,
      "output",
      "supplementary",
      "vaccination_groups_20260901"
    ),
    legacy_root = Sys.getenv(
      "COVID_EXCESS_LEGACY_ROOT",
      file.path(dirname(project_root), "covid_excess")
    ),
    force = "false"
  )
  if (length(arguments) == 0L) return(defaults)
  if (!all(grepl("^--[A-Za-z0-9_-]+=.+$", arguments))) {
    stop("Arguments must use --name=value syntax.")
  }
  parsed <- sub("^--", "", arguments)
  argument_names <- gsub("-", "_", sub("=.*$", "", parsed))
  values <- sub("^[^=]*=", "", parsed)
  unknown <- setdiff(argument_names, names(defaults))
  if (length(unknown) > 0L) {
    stop("Unknown arguments: ", paste(unknown, collapse = ", "), ".")
  }
  defaults[argument_names] <- as.list(values)
  defaults
}

parse_boolean <- function(value, label) {
  normalized <- tolower(value)
  if (!normalized %in% c("true", "false")) {
    stop(label, " must be true or false.")
  }
  identical(normalized, "true")
}

write_csv_atomic <- function(data, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  temporary <- tempfile(pattern = ".csv-", tmpdir = dirname(path))
  on.exit(unlink(temporary), add = TRUE)
  utils::write.csv(data, temporary, row.names = FALSE, na = "")
  if (!file.rename(temporary, path)) {
    stop("Failed to atomically install CSV: ", path, ".")
  }
  invisible(path)
}

write_csv_gz <- function(data, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  connection <- gzfile(path, open = "wb", compression = 9L)
  on.exit(close(connection), add = TRUE)
  utils::write.csv(data, connection, row.names = FALSE, na = "")
  invisible(path)
}

write_json <- function(object, path, pretty = FALSE) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(
    object,
    path,
    auto_unbox = TRUE,
    dataframe = "rows",
    na = "null",
    null = "null",
    digits = 10,
    pretty = pretty
  )
  invisible(path)
}

build_manifest <- function(root) {
  paths <- list.files(root, recursive = TRUE, full.names = TRUE)
  paths <- paths[file.info(paths)$isdir %in% FALSE]
  relative <- substring(paths, nchar(root) + 2L)
  keep <- !relative %in% c("manifest.csv", "complete.flag")
  paths <- paths[keep]
  relative <- relative[keep]
  data.frame(
    path = relative,
    bytes = as.numeric(file.info(paths)$size),
    sha256 = vapply(
      paths,
      digest::digest,
      character(1L),
      algo = "sha256",
      file = TRUE,
      serialize = FALSE
    ),
    stringsAsFactors = FALSE
  )
}

expand_manuscript_membership <- function(path) {
  input <- utils::read.csv(path, stringsAsFactors = FALSE)
  rows <- lapply(seq_len(nrow(input)), function(index) {
    figures <- strsplit(input$reporting_outputs[[index]], ";", fixed = TRUE)[[1]]
    figures <- intersect(figures, c("figure_04", "figure_05"))
    if (length(figures) == 0L) return(NULL)
    data.frame(
      figure = figures,
      region = input$region[[index]],
      geography = input$geography[[index]],
      vaccination_group = input$vaccination_group[[index]],
      stringsAsFactors = FALSE
    )
  })
  output <- dplyr::bind_rows(rows)
  output[order(output$figure, output$region, output$geography), ]
}

build_analysis_membership <- function(config, registry) {
  europe <- prepare_europe_vaccination(
    file.path(project_root, "data", "raw", "owid", "europe_vaccination_snapshot.rda"),
    config
  )
  us <- prepare_us_vaccination(
    file.path(project_root, "data", "raw", "owid", "us_state_vaccination_snapshot.rda"),
    config
  )
  europe_labels <- unique(registry[
    registry$analysis_family == "europe",
    c("geography", "geography_label")
  ])
  us_labels <- unique(registry[
    registry$analysis_family == "us_non_sex",
    c("geography", "geography_label")
  ])
  europe <- merge(europe, europe_labels, by = "geography")
  us <- merge(us, us_labels, by = "geography")
  europe$region <- "europe"
  us$region <- "us"
  output <- dplyr::bind_rows(europe, us) |>
    dplyr::transmute(
      region,
      geography,
      geography_label,
      measurement_date = as.Date(date),
      people_vaccinated_per_hundred,
      default_group = dplyr::recode(
        vaccination_group,
        neither = "excluded"
      )
    ) |>
    dplyr::arrange(region, geography_label)
  expected <- c(europe = 33L, us = 51L)
  observed <- table(factor(output$region, levels = names(expected)))
  if (!identical(as.integer(observed), unname(expected)) ||
      anyDuplicated(output[c("region", "geography")])) {
    stop("Vaccination membership does not cover the analysis geographies.")
  }
  output
}

build_figure_04_summaries <- function(pointwise_path) {
  message("Reading frozen pointwise summaries for Figure 4.")
  pointwise <- utils::read.csv(
    gzfile(pointwise_path),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  ages <- vaccination_explorer_age_groups()
  output <- pointwise[
    (
      pointwise$analysis_family == "europe" &
        pointwise$sex == "total" &
        pointwise$age_group %in% ages$europe
    ) |
      (
        pointwise$analysis_family == "us_non_sex" &
          pointwise$sex == "total" &
          pointwise$age_group %in% ages$us_figure_04
      ),
    ,
    drop = FALSE
  ]
  output <- dplyr::transmute(
    output,
    figure = "figure_04",
    region = dplyr::if_else(analysis_family == "europe", "europe", "us"),
    geography,
    geography_label,
    age_group,
    frequency,
    date = as.Date(date),
    mean = p_mean,
    variance = p_variance
  )
  rm(pointwise)
  invisible(gc(verbose = FALSE))
  output
}

read_explorer_observed_data <- function(config) {
  europe <- read_corrected_europe_observed(file.path(
    project_root,
    "data",
    "raw",
    "eurostat",
    "demo_r_mwk_20_linear.csv"
  ))
  europe$age_group <- normalize_europe_age(europe$age_group)
  europe$sex <- normalize_supplementary_sex(europe$sex)
  us <- us_model_input(
    standardize_us_wonder(
      file.path(project_root, us_sex_source_paths()),
      stratified_by_sex = TRUE
    ),
    config$regions$us_sex$analysis_end
  )
  list(europe = europe, us_sex = us)
}

build_figure_05_summaries <- function(
  registry,
  membership,
  observed_data,
  legacy_root
) {
  specifications <- list(
    europe = list(family = "europe", ages = vaccination_explorer_age_groups()$europe),
    us = list(family = "us_sex", ages = vaccination_explorer_age_groups()$us_figure_05)
  )
  summary_rows <- list()
  availability_rows <- list()
  model_ids <- character()
  completed <- 0L
  total <- sum(vapply(specifications, function(specification) {
    sum(membership$region == if (identical(specification$family, "europe")) {
      "europe"
    } else {
      "us"
    }) * length(specification$ages)
  }, integer(1L)))

  message("Deriving Figure 5 geography-level sex contrasts from paired summaries.")
  for (region in names(specifications)) {
    specification <- specifications[[region]]
    geographies <- membership$geography[membership$region == region]
    for (age_group in specification$ages) {
      for (geography in geographies) {
        selected <- registry[
          registry$analysis_family == specification$family &
            registry$geography == geography &
            registry$age_group == age_group &
            registry$sex %in% c("female", "male"),
          ,
          drop = FALSE
        ]
        available <- nrow(selected) == 2L && all(selected$status == "available")
        reason <- if (available) {
          NA_character_
        } else if (nrow(selected) != 2L) {
          "paired_registry_rows_missing"
        } else {
          paste(unique(selected$error_message[selected$status != "available"]), collapse = "; ")
        }
        availability_rows[[length(availability_rows) + 1L]] <- data.frame(
          figure = "figure_05",
          region = region,
          geography = geography,
          age_group = age_group,
          status = ifelse(available, "available", "unavailable"),
          reason = reason,
          stringsAsFactors = FALSE
        )
        if (available) {
          female_row <- selected[selected$sex == "female", , drop = FALSE]
          male_row <- selected[selected$sex == "male", , drop = FALSE]
          female <- load_supplementary_prediction(
            female_row, observed_data, project_root, legacy_root
          )
          male <- load_supplementary_prediction(
            male_row, observed_data, project_root, legacy_root
          )
          contrast <- tryCatch(
            summarize_vaccination_sex_contrast(
              female,
              male,
              denominator_epsilon = if (region == "europe") {
                .Machine$double.eps
              } else {
                0
              },
              zero_as_missing = region != "us",
              remove_missing_draws = region != "us"
            ),
            error = function(error) {
              stop(
                "Failed to summarize Figure 5 for ",
                region,
                " / ",
                geography,
                " / ",
                age_group,
                ": ",
                conditionMessage(error),
                call. = FALSE
              )
            }
          )
          summary_rows[[length(summary_rows) + 1L]] <- data.frame(
            figure = "figure_05",
            region = region,
            geography = geography,
            geography_label = female$geography_label,
            age_group = age_group,
            frequency = female$frequency,
            date = contrast$date,
            mean = contrast$mean,
            variance = contrast$variance,
            stringsAsFactors = FALSE
          )
          model_ids <- c(model_ids, female$analysis_id, male$analysis_id)
          rm(female, male, contrast)
          invisible(gc(verbose = FALSE))
        }
        completed <- completed + 1L
        if (completed %% 25L == 0L || completed == total) {
          message("Completed Figure 5 geography-age pair ", completed, " of ", total, ".")
        }
      }
    }
  }
  list(
    summaries = dplyr::bind_rows(summary_rows),
    availability = dplyr::bind_rows(availability_rows),
    model_ids = unique(model_ids)
  )
}

build_figure_04_availability <- function(summaries, membership) {
  rows <- list()
  for (region in c("europe", "us")) {
    ages <- if (region == "europe") {
      vaccination_explorer_age_groups()$europe
    } else {
      vaccination_explorer_age_groups()$us_figure_04
    }
    for (age_group in ages) {
      for (geography in membership$geography[membership$region == region]) {
        available <- any(
          summaries$region == region &
            summaries$age_group == age_group &
            summaries$geography == geography
        )
        rows[[length(rows) + 1L]] <- data.frame(
          figure = "figure_04",
          region = region,
          geography = geography,
          age_group = age_group,
          status = ifelse(available, "available", "unavailable"),
          reason = ifelse(available, NA_character_, "pointwise_summary_missing"),
          stringsAsFactors = FALSE
        )
      }
    }
  }
  dplyr::bind_rows(rows)
}

build_manuscript_validation <- function(summaries, presets, config) {
  figure_ages <- list(
    figure_04 = list(europe = c("40-59", "60-79"), us = c("40-59", "60-79")),
    figure_05 = list(europe = c("40-59", "60-79"), us = c("0-44", "45-64", "65-84"))
  )
  installed_paths <- c(
    figure_04 = file.path(
      project_root, "output", "reporting", "inputs", "figure_04_vaccination_pscore.csv"
    ),
    figure_05 = file.path(
      project_root, "output", "reporting", "inputs", "figure_05_sex_difference.csv"
    )
  )
  results <- list()
  for (figure in names(figure_ages)) {
    installed <- utils::read.csv(installed_paths[[figure]], stringsAsFactors = FALSE)
    installed$date <- as.Date(installed$date)
    for (region in names(figure_ages[[figure]])) {
      region_label <- if (region == "europe") "Europe" else "United States"
      for (age_group in figure_ages[[figure]][[region]]) {
        selected <- summaries[
          summaries$figure == figure & summaries$region == region &
            summaries$age_group == age_group,
          ,
          drop = FALSE
        ]
        membership <- presets[
          presets$figure == figure & presets$region == region,
          c("geography", "vaccination_group"),
          drop = FALSE
        ]
        reconstructed <- aggregate_vaccination_group_summary(selected, membership)
        reconstructed$region <- region_label
        reconstructed$age_group <- age_group
        reconstructed <- reconstructed[c(
          "region", "age_group", "vaccination_group", "date", "mean",
          "variance", "lower", "upper", "jurisdictions", "interval_method"
        )]
        installed_panel <- installed[
          installed$region == region_label & installed$age_group == age_group,
          ,
          drop = FALSE
        ]
        if (region == "us") {
          installed_panel <- installed_panel[
            installed_panel$date <= as.Date(config$regions$us_sex$analysis_end),
            ,
            drop = FALSE
          ]
        }
        installed_panel <- merge(
          installed_panel,
          unique(reconstructed[c(
            "region", "age_group", "vaccination_group", "date"
          )]),
          by = c("region", "age_group", "vaccination_group", "date")
        )
        comparison <- tryCatch(
          compare_vaccination_manuscript_summary(
            reconstructed,
            installed_panel,
            tolerance = 1e-10
          ),
          error = function(error) {
            stop(
              "Manuscript equivalence failed for ",
              figure,
              " / ",
              region,
              " / ",
              age_group,
              ": ",
              conditionMessage(error),
              call. = FALSE
            )
          }
        )
        comparison$figure <- figure
        comparison$region <- region
        comparison$age_group <- age_group
        results[[length(results) + 1L]] <- comparison
      }
    }
  }
  output <- dplyr::bind_rows(results)
  output[c(
    "figure", "region", "age_group", "compared_rows",
    "max_abs_numeric_difference", "tolerance", "pass"
  )]
}

arguments <- parse_arguments(commandArgs(trailingOnly = TRUE))
force <- parse_boolean(arguments$force, "--force")
output_root <- normalizePath(arguments$output_root, winslash = "/", mustWork = FALSE)
legacy_root <- normalizePath(arguments$legacy_root, winslash = "/", mustWork = TRUE)
core_root <- file.path(
  project_root, "output", "supplementary", "frozen_20260831"
)
required_inputs <- c(
  file.path(core_root, "complete.flag"),
  file.path(core_root, "registry.csv"),
  file.path(core_root, "pointwise.csv.gz"),
  file.path(core_root, "source_inventory.csv"),
  file.path(project_root, "config", "analysis.yml"),
  file.path(project_root, "config", "reporting_vaccination_groups.csv")
)
require_files(required_inputs, "vaccination explorer input")
dir.create(dirname(output_root), recursive = TRUE, showWarnings = FALSE)

if (file.exists(file.path(output_root, "complete.flag")) && !force) {
  stop(
    "A completed vaccination explorer freeze already exists at ", output_root,
    ". Use --force=true only for an intentional replacement."
  )
}
if (dir.exists(output_root)) {
  if (!force) stop("An incomplete output directory already exists: ", output_root, ".")
  backup <- paste0(
    output_root,
    ".superseded_",
    format(Sys.time(), "%Y%m%dT%H%M%S")
  )
  if (!file.rename(output_root, backup)) {
    stop("Failed to preserve the previous vaccination explorer output.")
  }
  message("Preserved the previous vaccination explorer output at ", backup, ".")
}

staging_root <- tempfile(
  pattern = paste0(basename(output_root), ".building-"),
  tmpdir = dirname(output_root)
)
dir.create(staging_root, recursive = TRUE, showWarnings = FALSE)
completed <- FALSE
on.exit({
  if (!completed && dir.exists(staging_root)) {
    unlink(staging_root, recursive = TRUE)
  }
}, add = TRUE)

config <- read_analysis_config(file.path(project_root, "config", "analysis.yml"))
waves <- wave_table(config)
coverage_start <- as.Date("2020-01-01")
minimum_coverage_fraction <- 0.95
registry <- utils::read.csv(
  file.path(core_root, "registry.csv"),
  stringsAsFactors = FALSE
)
registry$analysis_end[!nzchar(registry$analysis_end)] <- NA_character_
registry$error_message[!nzchar(registry$error_message)] <- NA_character_
membership <- build_analysis_membership(config, registry)
presets <- expand_manuscript_membership(file.path(
  project_root, "config", "reporting_vaccination_groups.csv"
))

figure_04 <- build_figure_04_summaries(file.path(core_root, "pointwise.csv.gz"))
observed_data <- read_explorer_observed_data(config)
figure_05_result <- build_figure_05_summaries(
  registry,
  membership,
  observed_data,
  legacy_root
)
summaries <- dplyr::bind_rows(figure_04, figure_05_result$summaries)
summaries <- summaries[order(
  summaries$figure,
  summaries$region,
  summaries$age_group,
  summaries$geography_label,
  summaries$date
), ]
rownames(summaries) <- NULL
validate_vaccination_geography_summaries(summaries)
coverage <- vaccination_panel_coverage(
  summaries,
  analysis_start = coverage_start,
  minimum_fraction = minimum_coverage_fraction
)
coverage_keys <- c("figure", "region", "geography", "age_group")
if (anyDuplicated(coverage[coverage_keys]) ||
    any(!is.finite(coverage$coverage_fraction)) ||
    any(coverage$coverage_fraction < 0 | coverage$coverage_fraction > 1)) {
  stop("Vaccination explorer coverage records are invalid.")
}

availability <- dplyr::bind_rows(
  build_figure_04_availability(figure_04, membership),
  figure_05_result$availability
) |>
  dplyr::left_join(
    membership[c("region", "geography", "geography_label")],
    by = c("region", "geography")
  ) |>
  dplyr::arrange(figure, region, age_group, geography_label)
unavailable_contract <- availability[availability$status == "unavailable", ]
expected_unavailable <- c(
  paste("figure_05", "europe", "DE", vaccination_explorer_age_groups()$europe, sep = "::"),
  "figure_05::us::Vermont::0-44"
)
observed_unavailable <- paste(
  unavailable_contract$figure,
  unavailable_contract$region,
  unavailable_contract$geography,
  unavailable_contract$age_group,
  sep = "::"
)
if (!setequal(observed_unavailable, expected_unavailable)) {
  stop(
    "The expected Vermont Figure 5 unavailability contract changed: ",
    paste(
      unavailable_contract$figure,
      unavailable_contract$region,
      unavailable_contract$geography,
      unavailable_contract$age_group,
      sep = "/",
      collapse = ", "
    ),
    "."
  )
}

manuscript_validation <- build_manuscript_validation(
  summaries,
  presets,
  config
)
if (!all(manuscript_validation$pass)) {
  stop("One or more manuscript-preset equivalence checks failed.")
}

browser_root <- file.path(staging_root, "browser", "vaccination_groups")
shard_root <- file.path(browser_root, "shards")
dir.create(shard_root, recursive = TRUE, showWarnings = FALSE)
panel_keys <- unique(summaries[c("figure", "region", "age_group", "frequency")])
panel_keys <- panel_keys[order(
  panel_keys$figure, panel_keys$region, panel_keys$age_group
), ]
panel_rows <- list()
for (index in seq_len(nrow(panel_keys))) {
  panel <- panel_keys[index, ]
  selected <- summaries[
    summaries$figure == panel$figure &
      summaries$region == panel$region &
      summaries$age_group == panel$age_group,
    ,
    drop = FALSE
  ]
  relative_path <- file.path(
    "shards",
    panel$figure,
    panel$region,
    paste0(supplementary_slug(panel$age_group), ".json")
  )
  write_json(
    vaccination_explorer_web_shard(
      selected,
      analysis_start = coverage_start,
      minimum_fraction = minimum_coverage_fraction
    ),
    file.path(browser_root, relative_path)
  )
  selected_availability <- availability[
    availability$figure == panel$figure &
      availability$region == panel$region &
      availability$age_group == panel$age_group,
    ,
    drop = FALSE
  ]
  selected_coverage <- coverage[
    coverage$figure == panel$figure &
      coverage$region == panel$region &
      coverage$age_group == panel$age_group,
    ,
    drop = FALSE
  ]
  panel_rows[[length(panel_rows) + 1L]] <- data.frame(
    figure = panel$figure,
    region = panel$region,
    age_group = panel$age_group,
    frequency = panel$frequency,
    shard = relative_path,
    available_geographies = sum(selected_availability$status == "available"),
    unavailable_geographies = sum(selected_availability$status != "available"),
    default_eligible_geographies = sum(selected_coverage$default_eligible),
    limited_coverage_geographies = sum(!selected_coverage$default_eligible),
    stringsAsFactors = FALSE
  )
}
panels <- dplyr::bind_rows(panel_rows)

core_inventory <- utils::read.csv(
  file.path(core_root, "source_inventory.csv"),
  stringsAsFactors = FALSE
)
figure_04_ids <- unique(registry$analysis_id[
  registry$analysis_family %in% c("europe", "us_non_sex") &
    registry$sex == "total"
])
source_inventory <- core_inventory[
  core_inventory$analysis_id %in% unique(c(
    figure_04_ids,
    figure_05_result$model_ids
  )),
  ,
  drop = FALSE
]
source_inventory <- source_inventory[order(source_inventory$analysis_id), ]

downloads_root <- file.path(browser_root, "downloads")
dir.create(downloads_root, recursive = TRUE, showWarnings = FALSE)
write_csv_atomic(
  membership,
  file.path(downloads_root, "vaccination_membership.csv")
)
write_csv_atomic(
  presets,
  file.path(downloads_root, "manuscript_membership.csv")
)
write_csv_atomic(
  availability,
  file.path(downloads_root, "availability.csv")
)
write_csv_atomic(
  coverage,
  file.path(downloads_root, "coverage.csv")
)
write_csv_atomic(
  manuscript_validation,
  file.path(downloads_root, "manuscript_equivalence.csv")
)
write_csv_atomic(
  source_inventory,
  file.path(downloads_root, "source_inventory.csv")
)
write_csv_gz(
  summaries,
  file.path(downloads_root, "geography_summaries.csv.gz")
)

metadata <- list(
  schema_version = "1.1.0",
  frozen_on = "2026-09-01",
  created_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
  upstream_supplementary_freeze = "frozen_20260831",
  upstream_complete_flag_sha256 = digest::digest(
    file.path(core_root, "complete.flag"),
    algo = "sha256",
    file = TRUE,
    serialize = FALSE
  ),
  model_refitting_performed = FALSE,
  public_bundle_contains_fitted_models = FALSE,
  public_bundle_contains_posterior_draws = FALSE,
  vaccination_metric = config$vaccination$metric,
  vaccination_reference_date = as.character(config$vaccination$reference_date),
  coverage_start = as.character(coverage_start),
  minimum_coverage_fraction = minimum_coverage_fraction,
  coverage_records = nrow(coverage),
  default_eligible_records = sum(coverage$default_eligible),
  limited_coverage_records = sum(!coverage$default_eligible),
  geography_summary_rows = nrow(summaries),
  analysis_geographies = nrow(membership),
  source_models_recorded = nrow(source_inventory),
  source_model_bytes = sum(source_inventory$bytes),
  unavailable_panels = sum(availability$status == "unavailable"),
  manuscript_equivalence_max_abs_difference = max(
    manuscript_validation$max_abs_numeric_difference
  ),
  manuscript_equivalence_tolerance = max(manuscript_validation$tolerance),
  session_info = paste(capture.output(utils::sessionInfo()), collapse = "\n")
)
yaml::write_yaml(metadata, file.path(downloads_root, "bundle_metadata.yml"))

index <- list(
  schema_version = "1.1.0",
  frozen_on = "2026-09-01",
  summary_only = TRUE,
  vaccination = list(
    metric = config$vaccination$metric,
    interpretation = config$vaccination$interpretation,
    reference_date = as.character(config$vaccination$reference_date),
    europe_date_rule = config$vaccination$europe_date_rule
  ),
  thresholds = config$vaccination$classification_rules,
  coverage = list(
    start_date = as.character(coverage_start),
    minimum_fraction = minimum_coverage_fraction,
    default_rule = "usable_fraction_at_or_above_minimum"
  ),
  waves = waves,
  smoothing = list(
    figure_05_europe = list(
      method = "centered_box_kernel",
      bandwidth_days = 14
    )
  ),
  figures = list(
    figure_04 = list(
      label = "P-score by vaccination group",
      estimand = "Pointwise P-score",
      interval_display = "ribbon"
    ),
    figure_05 = list(
      label = "Female-minus-male P-score difference",
      estimand = "Pointwise female-minus-male P-score difference",
      interval_display = "dashed"
    )
  ),
  panels = panels,
  membership = membership,
  manuscript_presets = presets,
  unavailable = availability[availability$status != "available", ],
  downloads = list(
    vaccination_membership = "downloads/vaccination_membership.csv",
    manuscript_membership = "downloads/manuscript_membership.csv",
    availability = "downloads/availability.csv",
    coverage = "downloads/coverage.csv",
    geography_summaries = "downloads/geography_summaries.csv.gz",
    source_inventory = "downloads/source_inventory.csv",
    manuscript_equivalence = "downloads/manuscript_equivalence.csv",
    metadata = "downloads/bundle_metadata.yml"
  )
)
write_json(index, file.path(browser_root, "index.json"), pretty = TRUE)

write_csv_atomic(manuscript_validation, file.path(staging_root, "manuscript_equivalence.csv"))
saveRDS(
  list(
    status = "passed",
    summary_rows = nrow(summaries),
    panels = nrow(panels),
    unavailable = sum(availability$status != "available"),
    coverage_start = coverage_start,
    minimum_coverage_fraction = minimum_coverage_fraction,
    coverage_records = nrow(coverage),
    default_eligible_records = sum(coverage$default_eligible),
    limited_coverage_records = sum(!coverage$default_eligible),
    manuscript_equivalence = manuscript_validation,
    posterior_draws_published = FALSE,
    fitted_models_published = FALSE
  ),
  file.path(staging_root, "validation_summary.rds")
)
manifest <- build_manifest(staging_root)
write_csv_atomic(manifest, file.path(staging_root, "manifest.csv"))
writeLines("complete", file.path(staging_root, "complete.flag"))

if (!file.rename(staging_root, output_root)) {
  stop("Failed to install the completed vaccination explorer freeze.")
}
completed <- TRUE
message(
  "Vaccination explorer freeze completed: ", output_root, "\n",
  "Geography summary rows: ", nrow(summaries), "\n",
  "Panels: ", nrow(panels), "\n",
  "Unavailable geography-age panels: ", sum(availability$status != "available"), "\n",
  "Default-eligible coverage records: ", sum(coverage$default_eligible), "\n",
  "Limited-coverage records: ", sum(!coverage$default_eligible), "\n",
  "Maximum manuscript equivalence difference: ",
  format(max(manuscript_validation$max_abs_numeric_difference), scientific = TRUE)
)
