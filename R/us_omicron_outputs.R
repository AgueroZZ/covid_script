us_canonical_waves <- function() {
  c("initial", "alpha", "delta", "omicron")
}

us_omicron_age_groups <- function() {
  c("0-44", "45-64", "65-84", "Over 85")
}

us_age_slug <- function(age_group) {
  slugs <- c(
    `0-44` = "0_44",
    `45-64` = "45_64",
    `65-84` = "65_84",
    `Over 85` = "ge85"
  )
  unname(slugs[age_group])
}

load_us_wave_result <- function(path) {
  if (!file.exists(path)) {
    stop("US wave result does not exist: ", path, ".")
  }
  environment <- new.env(parent = emptyenv())
  loaded <- load(path, envir = environment)
  if (!identical(loaded, "model_result_all")) {
    stop("US wave RDA must contain exactly one object named model_result_all.")
  }
  environment$model_result_all
}

read_us_state_codes <- function(path) {
  mapping <- utils::read.csv(path, stringsAsFactors = FALSE)
  required <- c("state_name", "state_code")
  if (!identical(names(mapping), required)) {
    stop("US state-code mapping must contain state_name and state_code.")
  }
  if (nrow(mapping) != 51L || anyDuplicated(mapping$state_name) ||
      anyDuplicated(mapping$state_code) || any(nchar(mapping$state_code) != 2L)) {
    stop("US state-code mapping must contain 51 unique jurisdictions.")
  }
  mapping
}

standardize_us_wave_result <- function(data, state_codes) {
  required <- c(
    "wave", "delta_upper", "delta_med", "delta_lower",
    "p_upper", "p_med", "p_lower", "state", "age", "sex"
  )
  missing <- setdiff(required, names(data))
  if (length(missing) > 0L) {
    stop("US wave result is missing: ", paste(missing, collapse = ", "), ".")
  }
  state_codes <- read_us_state_codes_data(state_codes)
  standardized <- dplyr::left_join(
    data,
    state_codes,
    by = c("state" = "state_name")
  )
  if (anyNA(standardized$state_code)) {
    missing_states <- sort(unique(standardized$state[is.na(standardized$state_code)]))
    stop("State-code mapping is missing: ", paste(missing_states, collapse = ", "), ".")
  }
  standardized <- standardized |>
    dplyr::transmute(
      state_name = state,
      state_code,
      age_group = age,
      sex,
      wave,
      delta_lower,
      delta_median = delta_med,
      delta_upper,
      p_lower,
      p_median = p_med,
      p_upper
    ) |>
    dplyr::arrange(
      match(age_group, us_omicron_age_groups()),
      sex,
      match(wave, us_canonical_waves()),
      state_code
    )
  rownames(standardized) <- NULL
  standardized
}

read_us_state_codes_data <- function(state_codes) {
  if (is.character(state_codes) && length(state_codes) == 1L) {
    return(read_us_state_codes(state_codes))
  }
  required <- c("state_name", "state_code")
  if (!is.data.frame(state_codes) || !identical(names(state_codes), required)) {
    stop("state_codes must be a state-code data frame or CSV path.")
  }
  if (nrow(state_codes) != 51L || anyDuplicated(state_codes$state_name) ||
      anyDuplicated(state_codes$state_code)) {
    stop("US state-code mapping must contain 51 unique jurisdictions.")
  }
  state_codes
}

validate_us_wave_result <- function(data, version) {
  if (!version %in% c("historical", "corrected")) {
    stop("version must be historical or corrected.")
  }
  required <- c(
    "state_name", "state_code", "age_group", "sex", "wave",
    "delta_lower", "delta_median", "delta_upper",
    "p_lower", "p_median", "p_upper"
  )
  if (!identical(names(data), required)) {
    stop("Standardized US wave result has an invalid column contract.")
  }
  if (!setequal(unique(data$wave), us_canonical_waves())) {
    stop("US wave result does not contain exactly the canonical four waves.")
  }
  if (!setequal(unique(data$age_group), us_omicron_age_groups()) ||
      !setequal(unique(data$sex), c("F", "M"))) {
    stop("US wave result has unexpected age or sex strata.")
  }
  keys <- c("state_code", "age_group", "sex", "wave")
  if (anyDuplicated(data[keys])) {
    stop("US wave result contains duplicated state-age-sex-wave keys.")
  }
  numeric_columns <- c(
    "delta_lower", "delta_median", "delta_upper",
    "p_lower", "p_median", "p_upper"
  )
  numeric_matrix <- as.matrix(data[numeric_columns])
  if (any(is.na(numeric_matrix) | is.nan(numeric_matrix))) {
    stop("US wave result contains missing or NaN estimates.")
  }
  median_columns <- c("delta_median", "p_median")
  if (any(!is.finite(as.matrix(data[median_columns])))) {
    stop("US wave result contains a non-finite posterior median.")
  }
  if (any(data$delta_lower > data$delta_median) ||
      any(data$delta_median > data$delta_upper) ||
      any(data$p_lower > data$p_median) ||
      any(data$p_median > data$p_upper)) {
    stop("US wave result contains unordered posterior intervals.")
  }

  expected_counts <- if (identical(version, "corrected")) {
    c(initial = 406L, alpha = 405L, delta = 407L, omicron = 406L)
  } else {
    c(initial = 406L, alpha = 405L, delta = 407L, omicron = 407L)
  }
  observed_counts <- table(factor(data$wave, levels = names(expected_counts)))
  if (!identical(as.integer(observed_counts), unname(expected_counts))) {
    stop(version, " US wave counts do not match the audited contract.")
  }
  if (nrow(data) != sum(expected_counts)) {
    stop(version, " US row count does not match the audited contract.")
  }
  if (any(data$state_code == "VT" & data$age_group == "0-44" & data$sex == "F")) {
    stop("The missing Vermont 0-44 female model unexpectedly produced rows.")
  }
  wyoming_omicron <- data$state_code == "WY" & data$age_group == "0-44" &
    data$sex == "F" & data$wave == "omicron"
  if (identical(version, "corrected") && any(wyoming_omicron)) {
    stop("Corrected data must exclude the unsupported Wyoming Omicron row.")
  }
  if (identical(version, "historical") && !any(wyoming_omicron)) {
    stop("Historical data must preserve the spurious Wyoming Omicron row.")
  }
  invisible(data)
}

us_wave_value_columns <- function() {
  c(
    "delta_lower", "delta_median", "delta_upper",
    "p_lower", "p_median", "p_upper"
  )
}

compare_us_omicron_results <- function(historical, corrected) {
  keys <- c("state_name", "state_code", "age_group", "sex", "wave")
  values <- us_wave_value_columns()
  non_omicron_historical <- historical[historical$wave != "omicron", c(keys, values)]
  non_omicron_corrected <- corrected[corrected$wave != "omicron", c(keys, values)]
  non_omicron_historical <- non_omicron_historical[do.call(order, non_omicron_historical[keys]), ]
  non_omicron_corrected <- non_omicron_corrected[do.call(order, non_omicron_corrected[keys]), ]
  non_omicron_exact <- isTRUE(all.equal(
    non_omicron_historical[keys],
    non_omicron_corrected[keys],
    tolerance = 0,
    check.attributes = FALSE
  )) &&
    isTRUE(all.equal(
      non_omicron_historical[values],
      non_omicron_corrected[values],
      tolerance = 0,
      check.attributes = FALSE
    ))

  delta <- historical[historical$wave == "delta", c(keys[-5], values)]
  historical_omicron <- historical[
    historical$wave == "omicron",
    c(keys[-5], values)
  ]
  comparable_old <- dplyr::inner_join(
    delta,
    historical_omicron,
    by = keys[-5],
    suffix = c("_delta", "_omicron")
  )
  historical_duplicates_delta <- all(vapply(values, function(column) {
    identical(
      comparable_old[[paste0(column, "_delta")]],
      comparable_old[[paste0(column, "_omicron")]]
    )
  }, logical(1)))

  historical_selected <- historical_omicron |>
    dplyr::rename_with(
      ~ paste0(.x, "_historical"),
      dplyr::all_of(values)
    )
  corrected_selected <- corrected[corrected$wave == "omicron", c(keys[-5], values)] |>
    dplyr::rename_with(
      ~ paste0(.x, "_corrected"),
      dplyr::all_of(values)
    )
  rowwise <- dplyr::full_join(
    historical_selected,
    corrected_selected,
    by = keys[-5]
  ) |>
    dplyr::mutate(
      key_status = dplyr::case_when(
        is.na(p_median_historical) ~ "corrected_only",
        is.na(p_median_corrected) ~ "historical_only",
        TRUE ~ "shared"
      ),
      p_median_change = p_median_corrected - p_median_historical,
      sign_changed = dplyr::if_else(
        key_status == "shared",
        sign(p_median_corrected) != sign(p_median_historical),
        NA
      )
    ) |>
    dplyr::group_by(age_group, sex) |>
    dplyr::mutate(
      historical_rank = rank(-p_median_historical, ties.method = "min", na.last = "keep"),
      corrected_rank = rank(-p_median_corrected, ties.method = "min", na.last = "keep"),
      rank_change = corrected_rank - historical_rank
    ) |>
    dplyr::ungroup() |>
    dplyr::arrange(age_group, sex, state_code)

  shared_change <- rowwise$p_median_change[rowwise$key_status == "shared"]
  summary <- data.frame(
    non_omicron_exact = non_omicron_exact,
    historical_omicron_duplicates_delta = historical_duplicates_delta,
    historical_omicron_rows = nrow(historical_omicron),
    corrected_omicron_rows = sum(corrected$wave == "omicron"),
    shared_rows = sum(rowwise$key_status == "shared"),
    changed_rows = sum(shared_change != 0),
    decreased_rows = sum(shared_change < 0),
    increased_rows = sum(shared_change > 0),
    sign_changes = sum(rowwise$sign_changed, na.rm = TRUE),
    median_change = stats::median(shared_change),
    historical_only_rows = sum(rowwise$key_status == "historical_only"),
    corrected_only_rows = sum(rowwise$key_status == "corrected_only")
  )
  list(rowwise = rowwise, summary = summary)
}

as_reporting_us_wave_summary <- function(data) {
  data |>
    dplyr::transmute(
      analysis_path = "us_sex",
      geography = state_name,
      age_group,
      sex = dplyr::recode(sex, F = "female", M = "male"),
      wave,
      p_lower,
      p_median,
      p_upper,
      status = "success"
    )
}

expected_us_omicron_output_inventory <- function() {
  versions <- c("historical", "corrected")
  formats <- c("pdf", "png")
  bar_rows <- lapply(versions, function(version) {
    do.call(rbind, lapply(us_omicron_age_groups(), function(age_group) {
      do.call(rbind, lapply(c("F", "M"), function(sex) {
        data.frame(
          result_version = version,
          output_family = "pscore_bar",
          age_group,
          sex,
          wave = NA_character_,
          format = formats,
          path = file.path(
            version,
            "pscore_bars",
            paste0("pscore_bar_age_", us_age_slug(age_group), "_sex_", sex, ".", formats)
          ),
          stringsAsFactors = FALSE
        )
      }))
    }))
  })
  map_rows <- lapply(versions, function(version) {
    do.call(rbind, lapply(us_omicron_age_groups(), function(age_group) {
      do.call(rbind, lapply(c("F", "M"), function(sex) {
        do.call(rbind, lapply(us_canonical_waves(), function(wave) {
          data.frame(
            result_version = version,
            output_family = "state_map",
            age_group,
            sex,
            wave,
            format = formats,
            path = file.path(
              version,
              "maps",
              paste0(
                "state_map_age_", us_age_slug(age_group),
                "_sex_", sex, "_wave_", wave, ".", formats
              )
            ),
            stringsAsFactors = FALSE
          )
        }))
      }))
    }))
  })
  table_rows <- lapply(versions, function(version) {
    data.frame(
      result_version = version,
      output_family = "table_01",
      age_group = "65-84",
      sex = "F+M",
      wave = "all",
      format = c("csv", "html"),
      path = file.path(version, "tables", paste0("table_01_wave_pscores.", c("csv", "html"))),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, c(bar_rows, map_rows, table_rows))
}

render_us_wave_bar <- function(
  data,
  age_group,
  sex,
  state_codes,
  output_pdf
) {
  selected <- data[
    data$age_group == age_group & data$sex == sex &
      data$state_code %in% state_codes,
  ]
  if (nrow(selected) == 0L) {
    stop("No rows are available for bar panel ", age_group, " ", sex, ".")
  }
  selected$state_code <- factor(selected$state_code, levels = state_codes)
  selected$wave <- factor(selected$wave, levels = us_canonical_waves())
  palette <- c(
    initial = "#D55E00",
    alpha = "#009E73",
    delta = "#0072B2",
    omicron = "#CC79A7"
  )
  plot <- ggplot2::ggplot(
    selected,
    ggplot2::aes(x = state_code, y = p_median, color = wave)
  ) +
    ggplot2::geom_hline(yintercept = 0, linewidth = 0.3, color = "grey35") +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = p_lower, ymax = p_upper),
      position = ggplot2::position_dodge(width = 0.65),
      width = 0.18,
      linewidth = 0.35
    ) +
    ggplot2::geom_point(
      position = ggplot2::position_dodge(width = 0.65),
      size = 1.8
    ) +
    ggplot2::coord_cartesian(ylim = c(-1, 1)) +
    ggplot2::scale_color_manual(values = palette, drop = FALSE) +
    ggplot2::labs(
      title = paste0("US P-scores: age ", age_group, ", sex ", sex),
      x = "State",
      y = "P-score",
      color = "Wave"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      legend.position = "top",
      panel.grid.minor = ggplot2::element_blank()
    )
  dir.create(dirname(output_pdf), recursive = TRUE, showWarnings = FALSE)
  output_png <- sub("[.]pdf$", ".png", output_pdf)
  ggplot2::ggsave(output_pdf, plot, width = 8.5, height = 6.5, device = "pdf")
  ggplot2::ggsave(
    output_png,
    plot,
    width = 8.5,
    height = 6.5,
    units = "in",
    dpi = 300
  )
  c(pdf = output_pdf, png = output_png)
}

load_us_map_geometry <- function(zip_path) {
  if (!file.exists(zip_path)) {
    stop("US map geometry ZIP does not exist: ", zip_path, ".")
  }
  extraction_root <- tempfile("us-state-geometry-")
  dir.create(extraction_root, recursive = TRUE)
  utils::unzip(zip_path, exdir = extraction_root)
  shapefiles <- list.files(
    extraction_root,
    pattern = "[.]shp$",
    recursive = TRUE,
    full.names = TRUE
  )
  if (length(shapefiles) != 1L) {
    stop("Expected exactly one shapefile in the US geometry ZIP.")
  }
  geometry <- suppressWarnings(sf::st_read(shapefiles, quiet = TRUE))
  required <- c("STUSPS", "NAME")
  if (!all(required %in% names(geometry))) {
    stop("US geometry is missing STUSPS or NAME.")
  }
  geometry[!geometry$STUSPS %in% c("AK", "HI", "AS", "PR", "MP", "VI", "GU"), ]
}

render_us_wave_map <- function(
  data,
  geometry,
  age_group,
  sex,
  wave,
  output_pdf
) {
  selected <- data[
    data$age_group == age_group & data$sex == sex & data$wave == wave,
    c("state_code", "p_median")
  ]
  if (anyDuplicated(selected$state_code)) {
    stop("Map input contains duplicated state codes.")
  }
  joined <- dplyr::left_join(geometry, selected, by = c("STUSPS" = "state_code"))
  breaks <- c(-Inf, 0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.5, 1, Inf)
  labels <- c("<0", "0-0.05", "0.05-0.10", "0.10-0.15", "0.15-0.20",
              "0.20-0.25", "0.25-0.50", "0.50-1.00", ">=1")
  joined$pscore_band <- cut(
    joined$p_median,
    breaks = breaks,
    labels = labels,
    right = FALSE
  )
  colors <- rev(RColorBrewer::brewer.pal(9, "RdYlGn"))
  joined$pscore_band_display <- factor(
    ifelse(is.na(joined$pscore_band), "Missing", as.character(joined$pscore_band)),
    levels = c(labels, "Missing")
  )
  label_geometry <- suppressWarnings(
    sf::st_point_on_surface(sf::st_geometry(joined))
  )
  label_data <- sf::st_sf(
    STUSPS = joined$STUSPS,
    geometry = label_geometry
  )
  bounding_box <- sf::st_bbox(joined)
  legend_data <- data.frame(
    x = rep(mean(c(bounding_box[["xmin"]], bounding_box[["xmax"]])), 10L),
    y = rep(mean(c(bounding_box[["ymin"]], bounding_box[["ymax"]])), 10L),
    pscore_band_display = factor(
      c(labels, "Missing"),
      levels = c(labels, "Missing")
    )
  )
  plot <- ggplot2::ggplot(joined) +
    ggplot2::geom_sf(
      ggplot2::aes(fill = pscore_band_display),
      color = "white",
      linewidth = 0.2,
      show.legend = FALSE
    ) +
    ggplot2::geom_sf_text(
      data = label_data,
      ggplot2::aes(label = STUSPS),
      fun.geometry = function(geometry) geometry,
      size = 2.1,
      color = "grey15"
    ) +
    ggplot2::geom_point(
      data = legend_data,
      ggplot2::aes(x = x, y = y, fill = pscore_band_display),
      shape = 22,
      size = 0.01,
      alpha = 0,
      inherit.aes = FALSE,
      show.legend = TRUE
    ) +
    ggplot2::scale_fill_manual(
      values = c(stats::setNames(colors, labels), Missing = "grey80"),
      limits = c(labels, "Missing"),
      drop = FALSE,
      na.value = "grey80"
    ) +
    ggplot2::guides(
      fill = ggplot2::guide_legend(
        override.aes = list(
          fill = c(colors, "grey80"),
          color = NA,
          alpha = 1,
          shape = 22,
          size = 5
        )
      )
    ) +
    ggplot2::coord_sf(datum = NA) +
    ggplot2::labs(
      title = paste0("US P-score: ", age_group, ", ", sex, ", ", wave),
      fill = "P-score"
    ) +
    ggplot2::theme_void(base_size = 10) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5),
      legend.position = "right",
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      legend.background = ggplot2::element_rect(fill = "white", color = NA)
    )
  dir.create(dirname(output_pdf), recursive = TRUE, showWarnings = FALSE)
  output_png <- sub("[.]pdf$", ".png", output_pdf)
  ggplot2::ggsave(
    output_pdf,
    plot,
    width = 9,
    height = 6.5,
    device = "pdf",
    bg = "white"
  )
  ggplot2::ggsave(
    output_png,
    plot,
    width = 9,
    height = 6.5,
    units = "in",
    dpi = 300,
    bg = "white"
  )
  c(pdf = output_pdf, png = output_png)
}
