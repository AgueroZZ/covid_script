supplementary_table_wave_levels <- function() {
  c("initial", "alpha", "delta", "omicron")
}

supplementary_table_region_contract <- function() {
  data.frame(
    analysis_family = c(
      "europe",
      "us_sex",
      "us_non_sex",
      "canada_sex",
      "canada_non_sex",
      "england_wales",
      "ireland"
    ),
    region_set = c(
      "Europe",
      "United States",
      "United States",
      "Canada",
      "Canada",
      "England and Wales",
      "Ireland"
    ),
    region_order = c(1L, 2L, 2L, 3L, 3L, 4L, 5L),
    family_order = c(1L, 1L, 2L, 1L, 2L, 1L, 1L),
    stringsAsFactors = FALSE
  )
}

supplementary_table_us_codes <- function() {
  stats::setNames(
    c(state.abb, "DC"),
    c(state.name, "District of Columbia")
  )
}

supplementary_table_canada_labels <- function() {
  c(
    AB = "Alberta",
    BC = "British Columbia",
    MB = "Manitoba",
    NB = "New Brunswick",
    NL = "Newfoundland and Labrador",
    NS = "Nova Scotia",
    ON = "Ontario",
    PE = "Prince Edward Island",
    QC = "Quebec",
    SK = "Saskatchewan"
  )
}

supplementary_table_geography_display <- function(
  region_set,
  geography,
  geography_label
) {
  region_set <- as.character(region_set)
  geography <- as.character(geography)
  geography_label <- as.character(geography_label)
  if (length(region_set) != length(geography) ||
      length(geography) != length(geography_label)) {
    stop("Geography display inputs must have equal lengths.")
  }

  label <- geography_label
  missing_label <- is.na(label) | !nzchar(label)
  label[missing_label] <- geography[missing_label]

  canada <- region_set == "Canada"
  canada_label <- unname(supplementary_table_canada_labels()[geography])
  replace_canada <- canada & !is.na(canada_label) & nzchar(canada_label)
  label[replace_canada] <- canada_label[replace_canada]

  code <- geography
  united_states <- region_set == "United States"
  us_code <- unname(supplementary_table_us_codes()[geography])
  replace_us_code <- united_states & !is.na(us_code) & nzchar(us_code)
  code[replace_us_code] <- us_code[replace_us_code]

  show_code <- region_set %in% c("Europe", "United States", "Canada") &
    !is.na(code) & nzchar(code) & label != code
  output <- label
  output[show_code] <- paste0(
    label[show_code],
    " (",
    code[show_code],
    ")"
  )
  output
}

supplementary_table_age_rank <- function(age_group) {
  ranks <- c(
    "Under 65" = 1L,
    "0-44" = 2L,
    "20-39" = 3L,
    "25-44" = 4L,
    "40-59" = 5L,
    "45-64" = 6L,
    "60-79" = 7L,
    "65-84" = 8L,
    "GE80" = 9L,
    "GE85" = 10L,
    "85+" = 10L
  )
  output <- unname(ranks[age_group])
  output[is.na(output)] <- max(ranks) + seq_len(sum(is.na(output)))
  as.integer(output)
}

supplementary_table_component <- function(rows, sex, wave) {
  selected <- rows[rows$sex == sex & rows$wave == wave, , drop = FALSE]
  if (nrow(selected) > 1L) {
    stop("Expanded table input contains duplicated sex-by-wave results.")
  }
  if (nrow(selected) == 0L ||
      !identical(selected$status[[1]], "success") ||
      !is.finite(selected$p_median[[1]])) {
    return(list(value = "unavailable", available = FALSE))
  }
  list(
    value = formatC(selected$p_median[[1]], digits = 3L, format = "f"),
    available = TRUE
  )
}

supplementary_table_format_wave <- function(rows, wave, paired) {
  if (paired) {
    male <- supplementary_table_component(rows, "male", wave)
    female <- supplementary_table_component(rows, "female", wave)
    value <- if (male$available && female$available) {
      format_table_pscore(
        as.numeric(male$value),
        as.numeric(female$value)
      )
    } else {
      paste0("M: ", male$value, "; F: ", female$value)
    }
    return(list(
      value = value,
      available = as.integer(male$available) + as.integer(female$available),
      expected = 2L
    ))
  }

  total <- supplementary_table_component(rows, "total", wave)
  list(
    value = paste0("Total: ", total$value),
    available = as.integer(total$available),
    expected = 1L
  )
}

supplementary_table_prepare_vaccination <- function(data, region_set) {
  reporting_required_columns(
    data,
    c(
      "date",
      "geography",
      "people_vaccinated_per_hundred",
      "vaccination_group"
    ),
    paste(region_set, "vaccination membership")
  )
  reporting_require_unique(
    data,
    "geography",
    paste(region_set, "vaccination membership")
  )
  geography <- as.character(data$geography)
  if (identical(region_set, "Europe")) {
    geography[geography == "GR"] <- "EL"
  }
  data.frame(
    region_set = region_set,
    geography = geography,
    people_vaccinated_per_hundred = as.numeric(
      data$people_vaccinated_per_hundred
    ),
    vaccination_group = as.character(data$vaccination_group),
    vaccination_measurement_date = as.character(as.Date(data$date)),
    stringsAsFactors = FALSE
  )
}

supplementary_table_key <- function(data) {
  paste(
    data$region_set,
    data$geography,
    data$estimand_age_group,
    sep = "\r"
  )
}

validate_supplementary_table_against_manuscript <- function(
  expanded,
  manuscript
) {
  keys <- c("region_set", "geography", "estimand_age_group")
  values <- c(
    "people_vaccinated_per_hundred",
    "vaccination_group",
    supplementary_table_wave_levels()
  )
  reporting_required_columns(
    expanded,
    c(keys, values, "in_manuscript_table_1"),
    "expanded supplementary table"
  )
  reporting_required_columns(
    manuscript,
    c(keys, values),
    "manuscript Table 1"
  )
  reporting_require_unique(expanded, keys, "expanded supplementary table")
  reporting_require_unique(manuscript, keys, "manuscript Table 1")

  index <- match(supplementary_table_key(manuscript), supplementary_table_key(expanded))
  if (anyNA(index)) {
    missing <- manuscript[is.na(index), keys, drop = FALSE]
    labels <- apply(missing, 1L, paste, collapse = " / ")
    stop(
      "Expanded supplementary table is missing manuscript rows: ",
      paste(labels, collapse = ", "),
      "."
    )
  }
  observed <- expanded[index, , drop = FALSE]
  exact <- rep(TRUE, nrow(manuscript))
  exact <- exact & abs(
    observed$people_vaccinated_per_hundred -
      manuscript$people_vaccinated_per_hundred
  ) <= 1e-12
  character_columns <- c("vaccination_group", supplementary_table_wave_levels())
  for (column in character_columns) {
    exact <- exact & observed[[column]] == manuscript[[column]]
  }
  exact <- exact & observed$in_manuscript_table_1 == "Yes"
  if (any(!exact)) {
    failed <- manuscript[!exact, keys, drop = FALSE]
    labels <- apply(failed, 1L, paste, collapse = " / ")
    stop(
      "Expanded supplementary rows differ from manuscript Table 1: ",
      paste(labels, collapse = ", "),
      "."
    )
  }
  invisible(data.frame(
    manuscript_row = seq_len(nrow(manuscript)),
    region_set = manuscript$region_set,
    geography = manuscript$geography,
    estimand_age_group = manuscript$estimand_age_group,
    exact_match = exact,
    stringsAsFactors = FALSE
  ))
}

validate_supplementary_table_explorer <- function(data) {
  required <- c(
    "region_set",
    "analysis_family",
    "geography",
    "geography_label",
    "geography_display",
    "population_view",
    "people_vaccinated_per_hundred",
    "vaccination_group",
    "vaccination_measurement_date",
    "estimand_age_group",
    "estimand_sex_group",
    "frequency",
    supplementary_table_wave_levels(),
    "result_status",
    "in_manuscript_table_1"
  )
  reporting_required_columns(data, required, "expanded supplementary table")
  keys <- c("region_set", "geography", "estimand_age_group")
  reporting_require_unique(data, keys, "expanded supplementary table")
  if (nrow(data) == 0L) {
    stop("Expanded supplementary table contains no rows.")
  }
  if (!all(data$result_status %in% c("available", "partial", "unavailable"))) {
    stop("Expanded supplementary table contains an unsupported result status.")
  }
  if (!all(data$in_manuscript_table_1 %in% c("Yes", "No"))) {
    stop("Expanded supplementary table has invalid manuscript indicators.")
  }
  if (anyNA(data$geography_display) || any(!nzchar(data$geography_display))) {
    stop("Expanded supplementary table has empty geography displays.")
  }
  supported_sex_groups <- c("Male and female", "Total")
  if (!all(data$estimand_sex_group %in% supported_sex_groups)) {
    stop("Expanded supplementary table has unsupported estimand sex groups.")
  }
  expected_view <- ifelse(
    data$estimand_sex_group == "Male and female",
    "Sex-stratified (M/F)",
    "Total population"
  )
  if (!identical(data$population_view, expected_view)) {
    stop("Population views do not match the displayed estimand sex groups.")
  }
  wave_values <- unlist(data[supplementary_table_wave_levels()], use.names = FALSE)
  if (anyNA(wave_values) || any(!nzchar(wave_values))) {
    stop("Expanded supplementary table contains empty wave displays.")
  }
  non_vaccination_regions <- !data$region_set %in% c("Europe", "United States")
  if (any(!is.na(data$people_vaccinated_per_hundred[non_vaccination_regions])) ||
      any(!is.na(data$vaccination_group[non_vaccination_regions])) ||
      any(!is.na(data$vaccination_measurement_date[non_vaccination_regions]))) {
    stop("Vaccination fields must be NA outside Europe and the United States.")
  }
  vaccination_regions <- !non_vaccination_regions
  if (any(is.na(data$people_vaccinated_per_hundred[vaccination_regions])) ||
      any(is.na(data$vaccination_group[vaccination_regions])) ||
      any(is.na(data$vaccination_measurement_date[vaccination_regions]))) {
    stop("Europe and United States rows require vaccination metadata.")
  }
  invisible(data)
}

build_supplementary_table_explorer <- function(
  wave_summary,
  europe_vaccination,
  us_vaccination,
  manuscript_table = NULL
) {
  required <- c(
    "analysis_family",
    "geography",
    "geography_label",
    "age_group",
    "sex",
    "frequency",
    "wave",
    "p_median",
    "status"
  )
  reporting_required_columns(wave_summary, required, "frozen wave summary")
  contract <- supplementary_table_region_contract()
  selected <- merge(
    wave_summary,
    contract,
    by = "analysis_family",
    all = FALSE,
    sort = FALSE
  )
  selected <- selected[
    selected$wave %in% supplementary_table_wave_levels(),
    ,
    drop = FALSE
  ]
  if (nrow(selected) == 0L) {
    stop("Frozen wave summary contains no registered table families.")
  }

  grouping_key <- paste(
    selected$region_set,
    selected$geography,
    selected$age_group,
    sep = "\r"
  )
  groups <- split(seq_len(nrow(selected)), grouping_key)
  rows <- lapply(groups, function(indices) {
    group <- selected[indices, , drop = FALSE]
    paired <- any(group$sex %in% c("male", "female"))
    result_rows <- if (paired) {
      group[group$sex %in% c("male", "female"), , drop = FALSE]
    } else {
      group[group$sex == "total", , drop = FALSE]
    }
    if (nrow(result_rows) == 0L) {
      stop("A registered table row has no paired-sex or total result.")
    }
    if (length(unique(result_rows$analysis_family)) != 1L ||
        length(unique(result_rows$frequency)) != 1L) {
      stop("A registered table row mixes analysis families or frequencies.")
    }
    result_key <- result_rows[c("sex", "wave")]
    if (anyDuplicated(result_key)) {
      stop("A registered table row contains duplicated sex-by-wave results.")
    }

    formatted <- lapply(
      supplementary_table_wave_levels(),
      function(wave) supplementary_table_format_wave(result_rows, wave, paired)
    )
    names(formatted) <- supplementary_table_wave_levels()
    available <- sum(vapply(formatted, `[[`, integer(1L), "available"))
    expected <- sum(vapply(formatted, `[[`, integer(1L), "expected"))
    status <- if (available == expected) {
      "available"
    } else if (available == 0L) {
      "unavailable"
    } else {
      "partial"
    }
    geography_labels <- unique(group$geography_label)
    geography_labels <- geography_labels[!is.na(geography_labels) &
      nzchar(geography_labels)]
    geography_label <- if (length(geography_labels) > 0L) {
      geography_labels[[1L]]
    } else {
      as.character(group$geography[[1L]])
    }
    geography <- as.character(group$geography[[1L]])
    region_set <- group$region_set[[1L]]

    data.frame(
      region_set = region_set,
      analysis_family = result_rows$analysis_family[[1L]],
      geography = geography,
      geography_label = geography_label,
      geography_display = supplementary_table_geography_display(
        region_set,
        geography,
        geography_label
      ),
      population_view = if (paired) {
        "Sex-stratified (M/F)"
      } else {
        "Total population"
      },
      estimand_age_group = as.character(group$age_group[[1L]]),
      estimand_sex_group = if (paired) "Male and female" else "Total",
      frequency = as.character(result_rows$frequency[[1L]]),
      initial = formatted$initial$value,
      alpha = formatted$alpha$value,
      delta = formatted$delta$value,
      omicron = formatted$omicron$value,
      result_status = status,
      region_order = as.integer(group$region_order[[1L]]),
      family_order = as.integer(result_rows$family_order[[1L]]),
      stringsAsFactors = FALSE
    )
  })
  output <- do.call(rbind, rows)
  rownames(output) <- NULL

  vaccination <- rbind(
    supplementary_table_prepare_vaccination(europe_vaccination, "Europe"),
    supplementary_table_prepare_vaccination(us_vaccination, "United States")
  )
  vaccination_key <- paste(
    vaccination$region_set,
    vaccination$geography,
    sep = "\r"
  )
  output_key <- paste(output$region_set, output$geography, sep = "\r")
  vaccination_index <- match(output_key, vaccination_key)
  output$people_vaccinated_per_hundred <-
    vaccination$people_vaccinated_per_hundred[vaccination_index]
  output$vaccination_group <- vaccination$vaccination_group[vaccination_index]
  output$vaccination_measurement_date <-
    vaccination$vaccination_measurement_date[vaccination_index]

  output$in_manuscript_table_1 <- "No"
  if (!is.null(manuscript_table)) {
    manuscript_keys <- supplementary_table_key(manuscript_table)
    output$in_manuscript_table_1[
      supplementary_table_key(output) %in% manuscript_keys
    ] <- "Yes"
  }

  age_rank <- supplementary_table_age_rank(output$estimand_age_group)
  output <- output[order(
    output$region_order,
    output$geography_label,
    age_rank,
    output$family_order,
    output$estimand_age_group
  ), , drop = FALSE]
  output <- output[c(
    "region_set",
    "analysis_family",
    "geography",
    "geography_label",
    "geography_display",
    "population_view",
    "people_vaccinated_per_hundred",
    "vaccination_group",
    "vaccination_measurement_date",
    "estimand_age_group",
    "estimand_sex_group",
    "frequency",
    supplementary_table_wave_levels(),
    "result_status",
    "in_manuscript_table_1"
  )]
  rownames(output) <- NULL

  validate_supplementary_table_explorer(output)
  if (!is.null(manuscript_table)) {
    validate_supplementary_table_against_manuscript(output, manuscript_table)
  }
  output
}
