reporting_required_columns <- function(data, columns, label = "reporting data") {
  missing <- setdiff(columns, names(data))
  if (length(missing) > 0L) {
    stop(
      label,
      " are missing required columns: ",
      paste(missing, collapse = ", "),
      "."
    )
  }
  invisible(data)
}

reporting_require_unique <- function(data, keys, label = "reporting data") {
  reporting_required_columns(data, keys, label)
  duplicated_keys <- duplicated(data[keys])
  if (any(duplicated_keys)) {
    stop(label, " contain duplicated keys: ", paste(keys, collapse = ", "), ".")
  }
  invisible(data)
}

reporting_require_files <- function(paths, label = "reporting input") {
  missing <- paths[!file.exists(paths)]
  if (length(missing) > 0L) {
    stop(
      "Missing ",
      label,
      if (length(missing) == 1L) ": " else "s: ",
      paste(missing, collapse = ", "),
      ". Run the corresponding upstream targets before rendering this output."
    )
  }
  invisible(paths)
}

reporting_arguments <- function(defaults, arguments = commandArgs(trailingOnly = TRUE)) {
  if (length(arguments) == 0L) {
    return(defaults)
  }
  valid <- grepl("^--[A-Za-z0-9_-]+=.+$", arguments)
  if (!all(valid)) {
    stop("Arguments must use --name=value syntax.")
  }
  parsed <- sub("^--", "", arguments)
  names_parsed <- sub("=.*$", "", parsed)
  values <- sub("^[^=]*=", "", parsed)
  unknown <- setdiff(names_parsed, names(defaults))
  if (length(unknown) > 0L) {
    stop("Unknown arguments: ", paste(unknown, collapse = ", "), ".")
  }
  defaults[names_parsed] <- as.list(values)
  defaults
}

reporting_output_pair <- function(primary_pdf) {
  extension <- tolower(tools::file_ext(primary_pdf))
  if (!identical(extension, "pdf")) {
    stop("The primary figure artifact must have a .pdf extension.")
  }
  c(
    pdf = primary_pdf,
    png = sub("\\.pdf$", ".png", primary_pdf, ignore.case = TRUE)
  )
}

render_submission_figure <- function(
  draw,
  primary_pdf,
  width,
  height,
  png_resolution = 300L
) {
  outputs <- reporting_output_pair(primary_pdf)
  dir.create(dirname(outputs[["pdf"]]), recursive = TRUE, showWarnings = FALSE)

  grDevices::pdf(outputs[["pdf"]], width = width, height = height, onefile = FALSE)
  tryCatch(draw(), finally = grDevices::dev.off())

  grDevices::png(
    outputs[["png"]],
    width = width,
    height = height,
    units = "in",
    res = png_resolution,
    type = "cairo-png"
  )
  tryCatch(draw(), finally = grDevices::dev.off())

  if (any(file.info(outputs)$size <= 0L)) {
    stop("One or more rendered figure artifacts are empty.")
  }
  unname(outputs)
}

validate_interval_summary <- function(
  data,
  mean_column = "mean",
  lower_column = "lower",
  upper_column = "upper",
  label = "interval summary"
) {
  reporting_required_columns(
    data,
    c(mean_column, lower_column, upper_column),
    label
  )
  finite <- is.finite(data[[mean_column]]) &
    is.finite(data[[lower_column]]) &
    is.finite(data[[upper_column]])
  if (any(data[[lower_column]][finite] > data[[mean_column]][finite]) ||
      any(data[[upper_column]][finite] < data[[mean_column]][finite])) {
    stop(label, " contain intervals that do not enclose the mean.")
  }
  invisible(data)
}

aggregate_fixed_effect_summary <- function(
  data,
  grouping_columns,
  mean_column = "mean",
  variance_column = "variance"
) {
  required <- unique(c(grouping_columns, mean_column, variance_column))
  reporting_required_columns(data, required, "fixed-effect input")
  if (nrow(data) == 0L) {
    stop("Fixed-effect input contains no rows.")
  }

  grouped <- split(
    data,
    interaction(data[grouping_columns], drop = TRUE, lex.order = TRUE)
  )
  rows <- lapply(grouped, function(group) {
    variances <- group[[variance_column]]
    usable <- is.finite(variances) & variances > 0 &
      is.finite(group[[mean_column]])
    if (!any(usable)) {
      stop("A fixed-effect group has no finite positive variances.")
    }
    weights <- 1 / variances[usable]
    weights <- weights / sum(weights)
    mean_value <- sum(weights * group[[mean_column]][usable])
    variance_value <- sum(weights^2 * variances[usable])
    key <- group[1, grouping_columns, drop = FALSE]
    key$mean <- mean_value
    key$variance <- variance_value
    key$lower <- mean_value - 1.96 * sqrt(variance_value)
    key$upper <- mean_value + 1.96 * sqrt(variance_value)
    key$jurisdictions <- sum(usable)
    key$interval_method <- "fixed_effect_normal_approximation"
    key
  })
  result <- dplyr::bind_rows(rows)
  rownames(result) <- NULL
  result
}

join_reporting_vaccination <- function(results, vaccination) {
  reporting_required_columns(
    results,
    "geography",
    "reporting results"
  )
  reporting_required_columns(
    vaccination,
    c("geography", "vaccination_group"),
    "vaccination membership"
  )
  reporting_require_unique(
    vaccination,
    "geography",
    "vaccination membership"
  )
  joined <- dplyr::left_join(results, vaccination, by = "geography")
  if (any(is.na(joined$vaccination_group))) {
    missing <- sort(unique(joined$geography[is.na(joined$vaccination_group)]))
    stop(
      "Vaccination membership is missing for: ",
      paste(missing, collapse = ", "),
      "."
    )
  }
  joined
}

read_reporting_registry <- function(
  output_path = here::here("config", "reporting_outputs.csv"),
  panel_path = here::here("config", "reporting_panels.csv")
) {
  reporting_require_files(c(output_path, panel_path), "registry file")
  list(
    outputs = readr::read_csv(output_path, show_col_types = FALSE),
    panels = readr::read_csv(panel_path, show_col_types = FALSE)
  )
}
