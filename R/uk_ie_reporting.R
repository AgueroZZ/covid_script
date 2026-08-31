uk_ie_mapping_required_columns <- function() {
  c(
    "geography",
    "manuscript_label",
    "source_age_group",
    "display_age_group",
    "mapping_type",
    "disclosure"
  )
}

read_uk_ie_age_mapping <- function(path) {
  if (!file.exists(path)) {
    stop("UK/Ireland age-mapping registry does not exist: ", path, ".")
  }
  registry <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  required <- uk_ie_mapping_required_columns()
  missing <- setdiff(required, names(registry))
  if (length(missing) > 0L) {
    stop("UK/Ireland age-mapping registry is missing: ", paste(missing, collapse = ", "), ".")
  }
  if (nrow(registry) != 8L) {
    stop("UK/Ireland age-mapping registry must contain eight declared rows.")
  }
  if (anyDuplicated(registry[c(
    "geography", "source_age_group", "display_age_group"
  )])) {
    stop("UK/Ireland age-mapping registry contains duplicate mappings.")
  }
  if (!all(registry$mapping_type == "approximate")) {
    stop("Every UK/Ireland comparison mapping must be declared approximate.")
  }
  if (any(!nzchar(registry$disclosure))) {
    stop("Every UK/Ireland comparison mapping requires disclosure text.")
  }
  registry
}

apply_uk_ie_age_mapping <- function(summary, registry) {
  required <- c("geography", "source_age_group")
  missing <- setdiff(required, names(summary))
  if (length(missing) > 0L) {
    stop("UK/Ireland summary is missing: ", paste(missing, collapse = ", "), ".")
  }
  unknown <- unique(summary[c("geography", "source_age_group")])
  declared <- unique(registry[c("geography", "source_age_group")])
  checked <- merge(
    unknown,
    declared,
    by = c("geography", "source_age_group"),
    all.x = TRUE,
    all.y = FALSE
  )
  if (nrow(checked) != nrow(unknown)) {
    stop("UK/Ireland summary contains an undeclared source age group.")
  }
  mapped <- merge(
    summary,
    registry,
    by = c("geography", "source_age_group"),
    all.x = TRUE,
    all.y = FALSE,
    sort = FALSE
  )
  if (anyNA(mapped$display_age_group)) {
    stop("UK/Ireland summary contains an undeclared source age group.")
  }
  mapped
}

uk_ie_mapping_caption_note <- function(registry) {
  disclosures <- unique(registry$disclosure)
  paste(disclosures, collapse = " ")
}

standardize_uk_ie_wave_summary <- function(summary, registry) {
  mapped <- apply_uk_ie_age_mapping(summary, registry)
  required <- c(
    "wave", "wave_start", "wave_end_exclusive", "p_lower", "p_med",
    "p_upper", "observed_deaths", "posterior_draws"
  )
  missing <- setdiff(required, names(mapped))
  if (length(missing) > 0L) {
    stop("UK/Ireland wave summary is missing: ", paste(missing, collapse = ", "), ".")
  }
  data.frame(
    analysis_path = ifelse(
      mapped$geography == "England and Wales",
      "england_wales",
      "ireland_quarterly"
    ),
    geography = ifelse(
      mapped$geography == "England and Wales",
      "UK",
      "IE"
    ),
    source_geography = mapped$geography,
    source_age_group = mapped$source_age_group,
    age_group = sub("^Y", "", mapped$display_age_group),
    display_age_group = mapped$display_age_group,
    sex = "total",
    wave = mapped$wave,
    wave_start = as.Date(mapped$wave_start),
    wave_end_exclusive = as.Date(mapped$wave_end_exclusive),
    p_lower = mapped$p_lower,
    p_median = mapped$p_med,
    p_upper = mapped$p_upper,
    observed_deaths = mapped$observed_deaths,
    posterior_draws = mapped$posterior_draws,
    status = "success",
    mapping_type = mapped$mapping_type,
    disclosure = mapped$disclosure,
    stringsAsFactors = FALSE
  )
}

build_extended_europe_map_input <- function(
  eurostat_wave_summary,
  uk_ie_wave_summary,
  geometry_path
) {
  if (!file.exists(geometry_path)) {
    stop("European map geometry is unavailable: ", geometry_path, ".")
  }
  eurostat <- eurostat_wave_summary |>
    dplyr::filter(
      .data$sex == "total",
      .data$age_group %in% c("40-59", "60-79"),
      .data$wave %in% c("initial", "delta")
    ) |>
    dplyr::transmute(
      geography = .data$geography,
      age_group = .data$age_group,
      wave = .data$wave,
      p_median = .data$p_median,
      mapping_type = "exact",
      disclosure = "Eurostat source age band matches the display age band."
    )
  regional <- uk_ie_wave_summary |>
    dplyr::filter(
      .data$age_group %in% c("40-59", "60-79"),
      .data$wave %in% c("initial", "delta")
    ) |>
    dplyr::select(
      "geography",
      "age_group",
      "wave",
      "p_median",
      "mapping_type",
      "disclosure"
    )
  result <- dplyr::bind_rows(eurostat, regional) |>
    dplyr::mutate(
      join_code = dplyr::recode(.data$geography, EL = "GR", UK = "GB"),
      label = dplyr::recode(.data$geography, EL = "GR")
    )
  if (nrow(result) != 35L * 2L * 2L) {
    stop("Extended Figure 2 requires 35 geographies by two ages and two waves.")
  }
  if (anyDuplicated(result[c("geography", "age_group", "wave")])) {
    stop("Extended Figure 2 contains duplicate geography-age-wave keys.")
  }

  geometry <- sf::st_read(geometry_path, quiet = TRUE) |>
    sf::st_transform(3035) |>
    dplyr::filter(!is.na(.data$ISO_A2_EH), .data$ISO_A2_EH != "-99") |>
    dplyr::select(join_code = "ISO_A2_EH", "geometry") |>
    sf::st_crop(c(
      xmin = 2700000,
      ymin = 1530000,
      xmax = 5686000,
      ymax = 4660000
    )) |>
    dplyr::group_by(.data$join_code) |>
    dplyr::summarise(geometry = sf::st_union(.data$geometry), .groups = "drop")
  inset_geometry <- sf::st_as_sf(
    data.frame(
      join_code = c("MT", "CY", "IS", "AM"),
      x = c(4910146, 5182984, 3071476, 5670000),
      y = c(1580000, 1580000, 4596126, 2200000),
      stringsAsFactors = FALSE
    ),
    coords = c("x", "y"),
    crs = 3035
  )
  geometry <- dplyr::bind_rows(
    geometry[!geometry$join_code %in% inset_geometry$join_code, ],
    inset_geometry
  )
  map_data <- dplyr::inner_join(geometry, result, by = "join_code")
  if (length(unique(map_data$label)) != 35L) {
    missing <- setdiff(unique(result$label), unique(map_data$label))
    stop("Extended Figure 2 geometry is incomplete: ", paste(missing, collapse = ", "), ".")
  }
  list(
    map_data = map_data,
    scope = list(
      source = paste(
        "verified Eurostat, England-and-Wales, and Ireland corrected-prior refits"
      ),
      geographies = sort(unique(map_data$label)),
      approximate_age_mappings = unique(regional[c(
        "geography", "age_group", "mapping_type", "disclosure"
      )])
    )
  )
}
