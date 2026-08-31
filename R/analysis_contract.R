read_manuscript_analysis_contract <- function(
  path = here::here("config", "manuscript_analysis_contract.yml")
) {
  if (!file.exists(path)) {
    stop("Final analysis contract does not exist: ", path, ".")
  }
  contract <- yaml::read_yaml(path)
  required <- c(
    "contract_version",
    "frozen_on",
    "status",
    "scientific_contract",
    "registries"
  )
  missing <- setdiff(required, names(contract))
  if (length(missing) > 0L) {
    stop(
      "Final analysis contract is missing: ",
      paste(missing, collapse = ", "),
      "."
    )
  }
  if (!identical(contract$contract_version, "1.0.0")) {
    stop("Final analysis contract version must be 1.0.0.")
  }
  if (!identical(contract$status, "frozen_for_submission")) {
    stop("Final analysis contract status must be frozen_for_submission.")
  }
  contract
}

analysis_contract_project_path <- function(path) {
  if (grepl("^/", path)) path else here::here(path)
}

analysis_contract_sha256 <- function(path) {
  digest::digest(file = path, algo = "sha256", serialize = FALSE)
}

analysis_contract_registry_table <- function(contract) {
  rows <- lapply(contract$registries, function(registry) {
    required <- c("id", "path", "format", "sha256")
    missing <- setdiff(required, names(registry))
    if (length(missing) > 0L) {
      stop(
        "A contract registry is missing: ",
        paste(missing, collapse = ", "),
        "."
      )
    }
    path <- analysis_contract_project_path(registry$path)
    if (!file.exists(path)) {
      stop("Final analysis registry does not exist: ", registry$path, ".")
    }
    expected_rows <- registry$expected_rows
    observed_rows <- NA_integer_
    if (identical(registry$format, "csv")) {
      observed_rows <- nrow(readr::read_csv(path, show_col_types = FALSE))
      if (is.null(expected_rows)) {
        stop("CSV registry lacks expected_rows: ", registry$id, ".")
      }
      if (!identical(observed_rows, as.integer(expected_rows))) {
        stop(
          "Final analysis registry row count changed for ",
          registry$id,
          ": expected ",
          expected_rows,
          ", observed ",
          observed_rows,
          "."
        )
      }
    } else if (!identical(registry$format, "yaml")) {
      stop("Unsupported final analysis registry format: ", registry$format, ".")
    }
    observed_hash <- analysis_contract_sha256(path)
    tibble::tibble(
      id = registry$id,
      path = registry$path,
      format = registry$format,
      expected_rows = if (is.null(expected_rows)) NA_integer_ else as.integer(expected_rows),
      observed_rows = observed_rows,
      expected_sha256 = registry$sha256,
      observed_sha256 = observed_hash,
      hash_matches = identical(observed_hash, registry$sha256)
    )
  })
  table <- dplyr::bind_rows(rows)
  if (anyDuplicated(table$id) || anyDuplicated(table$path)) {
    stop("Final analysis registries must have unique IDs and paths.")
  }
  if (!all(table$hash_matches)) {
    changed <- table$id[!table$hash_matches]
    stop(
      "Final analysis registry hash changed for: ",
      paste(changed, collapse = ", "),
      ". Revalidate and deliberately re-freeze the contract."
    )
  }
  table
}

analysis_contract_assert_equal <- function(actual, expected, label) {
  actual <- as.character(unlist(actual, use.names = FALSE))
  expected <- as.character(unlist(expected, use.names = FALSE))
  if (!identical(actual, expected)) {
    stop(
      label,
      " does not match the frozen contract: expected ",
      paste(expected, collapse = ", "),
      "; observed ",
      paste(actual, collapse = ", "),
      "."
    )
  }
  invisible(TRUE)
}

analysis_contract_assert_setequal <- function(actual, expected, label) {
  if (!setequal(as.character(actual), as.character(expected))) {
    stop(label, " does not match the frozen contract.")
  }
  invisible(TRUE)
}

validate_analysis_contract_science <- function(contract) {
  scientific <- contract$scientific_contract
  required <- c(
    "training",
    "endpoints",
    "waves",
    "vaccination",
    "age_bands",
    "reporting_estimands",
    "cohorts"
  )
  missing <- setdiff(required, names(scientific))
  if (length(missing) > 0L) {
    stop(
      "Final scientific contract is missing: ",
      paste(missing, collapse = ", "),
      "."
    )
  }

  analysis <- yaml::read_yaml(here::here("config", "analysis.yml"))
  analysis_contract_assert_equal(
    scientific$training$final_date,
    analysis$training$final_date,
    "Training endpoint"
  )
  analysis_contract_assert_equal(
    scientific$endpoints$us_non_sex,
    analysis$regions$us_non_sex$analysis_end,
    "US non-sex endpoint"
  )
  analysis_contract_assert_equal(
    scientific$endpoints$us_sex,
    analysis$regions$us_sex$analysis_end,
    "US sex endpoint"
  )

  for (wave in c("initial", "alpha", "delta", "omicron")) {
    analysis_contract_assert_equal(
      scientific$waves[[wave]]$start,
      analysis$waves[[wave]]$start,
      paste(tools::toTitleCase(wave), "wave start")
    )
    analysis_contract_assert_equal(
      scientific$waves[[wave]]$end_exclusive,
      analysis$waves[[wave]]$end_exclusive,
      paste(tools::toTitleCase(wave), "wave end")
    )
  }

  vaccination <- scientific$vaccination
  analysis_contract_assert_equal(
    vaccination$metric,
    analysis$vaccination$metric,
    "Vaccination metric"
  )
  analysis_contract_assert_equal(
    vaccination$interpretation,
    analysis$vaccination$interpretation,
    "Vaccination interpretation"
  )
  analysis_contract_assert_equal(
    vaccination$reference_date,
    analysis$vaccination$reference_date,
    "Vaccination reference date"
  )
  analysis_contract_assert_equal(
    vaccination$europe_date_rule,
    analysis$vaccination$europe_date_rule,
    "European vaccination date rule"
  )
  for (region in c("us", "europe")) {
    for (threshold in c("low_below", "high_above")) {
      analysis_contract_assert_equal(
        vaccination$thresholds[[region]][[threshold]],
        analysis$vaccination$classification_rules[[region]][[threshold]],
        paste(region, threshold, "vaccination threshold")
      )
    }
  }

  for (region in names(scientific$age_bands$source_regions)) {
    analysis_contract_assert_equal(
      scientific$age_bands$source_regions[[region]],
      analysis$regions[[region]]$age_groups,
      paste(region, "source age bands")
    )
  }
  invisible(scientific)
}

analysis_contract_validate_vaccination_groups <- function(contract, europe, us) {
  groups <- readr::read_csv(
    here::here("config", "reporting_vaccination_groups.csv"),
    show_col_types = FALSE
  )
  required <- c(
    "region",
    "geography",
    "measurement_date",
    "people_vaccinated_per_hundred",
    "vaccination_group",
    "reporting_outputs"
  )
  missing <- setdiff(required, names(groups))
  if (length(missing) > 0L || nrow(groups) != 28L ||
      anyDuplicated(groups[c("region", "geography")])) {
    stop("Vaccination reporting-group registry violates its 28-row contract.")
  }
  thresholds <- contract$scientific_contract$vaccination$thresholds
  expected_group <- ifelse(
    groups$people_vaccinated_per_hundred < ifelse(
      groups$region == "us",
      thresholds$us$low_below,
      thresholds$europe$low_below
    ),
    "low",
    ifelse(
      groups$people_vaccinated_per_hundred > ifelse(
        groups$region == "us",
        thresholds$us$high_above,
        thresholds$europe$high_above
      ),
      "high",
      "neither"
    )
  )
  if (!identical(groups$vaccination_group, expected_group)) {
    stop("Vaccination reporting groups do not match the fixed threshold rule.")
  }
  europe_selected <- europe$geography[
    europe$figure_04 | europe$figure_05 | europe$table_01
  ]
  us_selected <- us$geography[us$figure_04 | us$figure_05 | us$table_01]
  analysis_contract_assert_setequal(
    groups$geography[groups$region == "europe"],
    europe_selected,
    "European vaccination reporting cohort"
  )
  analysis_contract_assert_setequal(
    groups$geography[groups$region == "us"],
    us_selected,
    "US vaccination reporting cohort"
  )
  us_joined <- dplyr::left_join(
    us[us$geography %in% us_selected, c("geography", "vaccination_group")],
    groups[groups$region == "us", c("geography", "vaccination_group")],
    by = "geography",
    suffix = c("_cohort", "_registry")
  )
  if (any(us_joined$vaccination_group_cohort !=
          us_joined$vaccination_group_registry)) {
    stop("US vaccination groups disagree across final registries.")
  }
  groups
}

analysis_contract_validate_reporting <- function(contract) {
  europe <- readr::read_csv(
    here::here("config", "europe_reporting_cohort.csv"),
    show_col_types = FALSE
  )
  uk_ie <- readr::read_csv(
    here::here("config", "uk_ie_reporting_cohort.csv"),
    show_col_types = FALSE
  )
  us <- readr::read_csv(
    here::here("config", "us_reporting_cohort.csv"),
    show_col_types = FALSE
  )
  us_table <- readr::read_csv(
    here::here("config", "us_table_01_cohort.csv"),
    show_col_types = FALSE
  )
  panels <- readr::read_csv(
    here::here("config", "reporting_panels.csv"),
    show_col_types = FALSE
  )
  outputs <- readr::read_csv(
    here::here("config", "reporting_outputs.csv"),
    show_col_types = FALSE
  )

  if (nrow(europe) != 35L || nrow(us) != 51L || nrow(uk_ie) != 8L ||
      nrow(us_table) != 11L || nrow(outputs) != 6L || nrow(panels) != 23L) {
    stop("One or more final reporting registries violate frozen row counts.")
  }
  if (!all(uk_ie$mapping_type == "approximate") ||
      any(is.na(uk_ie$disclosure)) || any(!nzchar(uk_ie$disclosure))) {
    stop("UK/Ireland approximate age mappings require explicit disclosures.")
  }
  if (any(is.na(europe$notes)) || any(!nzchar(europe$notes))) {
    stop("Every European reporting-cohort row requires an explicit note.")
  }
  analysis_contract_assert_setequal(
    us$geography[us$table_01],
    us_table$geography,
    "US Table 1 cohort"
  )

  expected_panels <- tibble::tribble(
    ~output_id, ~panel_id, ~data_age_group,
    "figure_04", "a", "40-79",
    "figure_04", "b", "40-79",
    "figure_04", "c", "40-59",
    "figure_04", "d", "40-59",
    "figure_04", "e", "60-79",
    "figure_04", "f", "60-79",
    "figure_05", "a", "40-79",
    "figure_05", "b", "0-84",
    "figure_05", "c", "40-59",
    "figure_05", "d", "0-44",
    "figure_05", "e", "60-79",
    "figure_05", "f", "65-84"
  )
  observed_panels <- panels |>
    dplyr::filter(output_id %in% c("figure_04", "figure_05")) |>
    dplyr::select(output_id, panel_id, data_age_group)
  if (!identical(
    dplyr::arrange(observed_panels, output_id, panel_id),
    dplyr::arrange(expected_panels, output_id, panel_id)
  )) {
    stop("Figure 4 or Figure 5 age estimands drifted from the frozen contract.")
  }

  groups <- analysis_contract_validate_vaccination_groups(contract, europe, us)
  list(
    europe = europe,
    uk_ie = uk_ie,
    us = us,
    us_table = us_table,
    panels = panels,
    outputs = outputs,
    vaccination_groups = groups
  )
}

analysis_contract_validate_suppression_decision <- function(contract, reporting) {
  decision <- contract$scientific_contract$cohorts$us_figure_05_missingness
  if (!identical(
    decision$rule,
    "common_available_months_within_vaccination_group"
  )) {
    stop("US Figure 5 missingness rule drifted from available-month reporting.")
  }
  if (length(decision$production_exclusions) != 0L) {
    stop("US Figure 5 production exclusions must remain empty.")
  }
  inventory <- readr::read_csv(
    here::here("config", "cohorts.csv"),
    show_col_types = FALSE
  )
  figure_05_geographies <- reporting$us$geography[reporting$us$figure_05]
  incomplete <- inventory |>
    dplyr::filter(
      region == "us_sex",
      jurisdiction %in% figure_05_geographies,
      age_group %in% c("0-44", "45-64", "65-84"),
      sex %in% c("female", "male"),
      missing_or_suppressed > 0
    )
  analysis_contract_assert_setequal(
    unique(incomplete$jurisdiction),
    decision$accepted_incomplete_geographies,
    "Accepted incomplete Figure 5 geographies"
  )
  if (nrow(incomplete) != 3L) {
    stop("Accepted Figure 5 incompleteness must contain exactly three strata.")
  }
  invisible(incomplete)
}

validate_manuscript_analysis_contract <- function(contract) {
  registry_hashes <- analysis_contract_registry_table(contract)
  validate_analysis_contract_science(contract)
  reporting <- analysis_contract_validate_reporting(contract)
  incomplete <- analysis_contract_validate_suppression_decision(contract, reporting)
  summary <- tibble::tribble(
    ~check, ~value,
    "contract_version", contract$contract_version,
    "status", contract$status,
    "training_endpoint", as.character(contract$scientific_contract$training$final_date),
    "us_endpoint", as.character(contract$scientific_contract$endpoints$us_sex),
    "waves", as.character(length(contract$scientific_contract$waves)),
    "adopted_outputs", as.character(nrow(reporting$outputs)),
    "reporting_panels", as.character(nrow(reporting$panels)),
    "europe_reporting_rows", as.character(nrow(reporting$europe)),
    "us_reporting_jurisdictions", as.character(nrow(reporting$us)),
    "vaccination_reporting_groups", as.character(nrow(reporting$vaccination_groups)),
    "accepted_incomplete_figure_05_strata", as.character(nrow(incomplete))
  )
  list(
    valid = TRUE,
    registry_hashes = registry_hashes,
    summary = summary,
    reporting = reporting,
    incomplete_figure_05 = incomplete
  )
}

flatten_manuscript_analysis_contract <- function(contract) {
  flatten_node <- function(value, path) {
    if (is.list(value)) {
      if (length(value) == 0L) {
        return(tibble::tibble(key = path, value = "[]"))
      }
      if (is.null(names(value)) || all(names(value) == "")) {
        return(tibble::tibble(
          key = path,
          value = paste(as.character(unlist(value, use.names = FALSE)), collapse = ";")
        ))
      }
      return(dplyr::bind_rows(lapply(names(value), function(name) {
        next_path <- if (nzchar(path)) paste(path, name, sep = ".") else name
        flatten_node(value[[name]], next_path)
      })))
    }
    tibble::tibble(
      key = path,
      value = paste(as.character(value), collapse = ";")
    )
  }
  flatten_node(contract$scientific_contract, "scientific_contract")
}
