manuscript_claim_statuses <- function() {
  c(
    "verified_exact",
    "verified_rounded",
    "supported_qualitatively",
    "requires_manuscript_edit",
    "unsupported_remove_or_reproduce"
  )
}

read_manuscript_claim_registry <- function(
  path = here::here("config", "manuscript_numerical_claims.csv")
) {
  claims <- readr::read_csv(path, show_col_types = FALSE, na = character())
  required <- c(
    "claim_id", "document", "section", "paragraph_index", "claim_text",
    "claim_type", "evidence_type", "evidence_path", "evidence_key",
    "reported_value", "computed_value", "status", "required_action", "notes"
  )
  missing <- setdiff(required, names(claims))
  if (length(missing) > 0L) {
    stop("Claim registry is missing columns: ", paste(missing, collapse = ", "), ".")
  }
  if (anyDuplicated(claims$claim_id)) stop("Claim IDs must be unique.")
  if (any(is.na(claims$claim_id) | claims$claim_id == "") ||
      any(is.na(claims$claim_text) | claims$claim_text == "")) {
    stop("Every claim requires an ID and source text.")
  }
  if (!all(claims$status %in% manuscript_claim_statuses())) {
    stop("Claim registry contains an unknown status.")
  }
  verified <- claims$status %in% c(
    "verified_exact", "verified_rounded", "supported_qualitatively"
  )
  if (any(verified & (is.na(claims$evidence_path) | claims$evidence_path == ""))) {
    stop("Every verified claim requires an evidence path.")
  }
  actionable <- claims$status %in% c(
    "requires_manuscript_edit", "unsupported_remove_or_reproduce"
  )
  if (any(actionable &
          (is.na(claims$required_action) | claims$required_action == ""))) {
    stop("Every actionable claim requires a concrete manuscript action.")
  }
  claims
}

manuscript_wave_table <- function(contract) {
  dplyr::bind_rows(lapply(names(contract$scientific_contract$waves), function(wave) {
    x <- contract$scientific_contract$waves[[wave]]
    tibble::tibble(
      wave = wave,
      start = as.Date(x$start),
      end_exclusive = as.Date(x$end_exclusive)
    )
  }))
}

assign_manuscript_wave <- function(date, waves) {
  date <- as.Date(date)
  output <- rep(NA_character_, length(date))
  for (i in seq_len(nrow(waves))) {
    selected <- date >= waves$start[[i]] & date < waves$end_exclusive[[i]]
    output[selected] <- waves$wave[[i]]
  }
  output
}

summarize_trajectory_by_wave <- function(path, evidence_prefix, waves) {
  x <- readr::read_csv(path, show_col_types = FALSE)
  x$wave <- assign_manuscript_wave(x$date, waves)
  x <- x[!is.na(x$wave), , drop = FALSE]
  x$region_key <- ifelse(
    tolower(x$region) == "united states",
    "us",
    tolower(x$region)
  )
  x |>
    dplyr::group_by(
      .data$region, .data$region_key, .data$age_group,
      .data$vaccination_group, .data$wave
    ) |>
    dplyr::summarise(
      n_time_points = dplyr::n(),
      min = min(.data$mean, na.rm = TRUE),
      median = stats::median(.data$mean, na.rm = TRUE),
      max = max(.data$mean, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      evidence_key = paste(
        evidence_prefix,
        .data$region_key,
        .data$age_group,
        .data$vaccination_group,
        .data$wave,
        sep = "."
      ),
      source_path = path,
      evidence_type = "wave_trajectory_summary"
    ) |>
    dplyr::select("evidence_key", "evidence_type", "source_path", dplyr::everything())
}

build_manuscript_computed_evidence <- function() {
  contract_path <- here::here("config", "manuscript_analysis_contract.yml")
  source(here::here("R", "analysis_contract.R"), local = TRUE)
  contract <- read_manuscript_analysis_contract(contract_path)
  waves <- manuscript_wave_table(contract)

  figure_02_path <- here::here("artifacts", "reporting", "inputs", "figure_02_europe_maps.rds")
  figure_03_path <- here::here("artifacts", "reporting", "inputs", "figure_03_north_america_maps.rds")
  figure_04_path <- here::here("artifacts", "reporting", "inputs", "figure_04_vaccination_pscore.csv")
  figure_05_path <- here::here("artifacts", "reporting", "inputs", "figure_05_sex_difference.csv")

  map_02 <- readRDS(figure_02_path)$map_data
  map_03 <- readRDS(figure_03_path)$map_data
  if (inherits(map_02, "sf")) map_02 <- sf::st_drop_geometry(map_02)
  if (inherits(map_03, "sf")) map_03 <- sf::st_drop_geometry(map_03)

  scope <- tibble::tribble(
    ~evidence_key, ~evidence_type, ~source_path, ~value,
    "scope.europe_reporting_geographies", "registry_count", "config/europe_reporting_cohort.csv", as.character(nrow(readr::read_csv(here::here("config", "europe_reporting_cohort.csv"), show_col_types = FALSE))),
    "scope.us_reporting_geographies", "registry_count", "config/us_reporting_cohort.csv", as.character(nrow(readr::read_csv(here::here("config", "us_reporting_cohort.csv"), show_col_types = FALSE))),
    "scope.figure03_mapped_geographies", "frozen_input_count", "artifacts/reporting/inputs/figure_03_north_america_maps.rds", as.character(dplyr::n_distinct(map_03$label)),
    "contract.training_endpoint", "contract_field", "config/manuscript_analysis_contract.yml", contract$scientific_contract$training$final_date,
    "contract.us_endpoint", "contract_field", "config/manuscript_analysis_contract.yml", contract$scientific_contract$endpoints$us_non_sex,
    "contract.vaccination_reference", "contract_field", "config/manuscript_analysis_contract.yml", contract$scientific_contract$vaccination$reference_date,
    "contract.vaccination_thresholds", "contract_field", "config/manuscript_analysis_contract.yml", "US <42/>62; Europe <41/>53",
    "contract.figure04_ages", "contract_field", "config/manuscript_analysis_contract.yml", "Europe 40-79; US 40-79",
    "contract.figure05_ages", "contract_field", "config/manuscript_analysis_contract.yml", "Europe 40-79; US 0-84; panel f US 65-84"
  )

  map_02_evidence <- map_02 |>
    dplyr::filter(.data$wave == "delta", .data$geography %in% c("RO", "BG", "SK", "RS", "PL")) |>
    dplyr::transmute(
      evidence_key = paste("figure02.delta", .data$geography, .data$age_group, sep = "."),
      evidence_type = "frozen_map_value",
      source_path = "artifacts/reporting/inputs/figure_02_europe_maps.rds",
      geography = .data$geography,
      age_group = .data$age_group,
      wave = .data$wave,
      value = as.character(signif(.data$p_median, 8))
    )

  map_03_evidence <- map_03 |>
    dplyr::filter(
      (.data$wave == "initial" & .data$label == "NY") |
        (.data$wave == "delta" & .data$label %in% c("MS", "LA", "AL", "AR", "TX"))
    ) |>
    dplyr::transmute(
      evidence_key = paste("figure03", .data$wave, .data$label, .data$age_group, sep = "."),
      evidence_type = "frozen_map_value",
      source_path = "artifacts/reporting/inputs/figure_03_north_america_maps.rds",
      geography = .data$label,
      age_group = .data$age_group,
      wave = .data$wave,
      value = as.character(signif(.data$p_median, 8))
    )

  figure_04 <- summarize_trajectory_by_wave(figure_04_path, "figure04", waves)
  figure_05 <- summarize_trajectory_by_wave(figure_05_path, "figure05", waves)
  dplyr::bind_rows(scope, map_02_evidence, map_03_evidence, figure_04, figure_05)
}
