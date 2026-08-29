draw_interval_polygon <- function(date, lower, upper, color) {
  graphics::polygon(
    c(date, rev(date)),
    c(lower, rev(upper)),
    border = NA,
    col = grDevices::adjustcolor(color, alpha.f = 0.18)
  )
}

draw_model_component <- function(
  data,
  y_label,
  training_boundary,
  observed = NULL,
  y_limits = NULL
) {
  validate_interval_summary(data, label = y_label)
  reporting_required_columns(data, "date", y_label)
  date <- as.Date(data$date)
  if (is.null(y_limits)) {
    y_limits <- range(c(data$lower, data$upper, observed), na.rm = TRUE)
  }
  graphics::plot(
    date,
    data$mean,
    type = "n",
    xlab = "",
    ylab = y_label,
    ylim = y_limits,
    bty = "l"
  )
  draw_interval_polygon(date, data$lower, data$upper, "grey35")
  graphics::lines(date, data$mean, col = "#4C5FFF", lwd = 1)
  if (!is.null(observed)) {
    graphics::points(date, observed, pch = 1, cex = 0.45, col = "grey20")
  }
  graphics::abline(v = as.Date(training_boundary), col = "#C77CFF", lty = 3)
}

validate_figure_01_input <- function(input) {
  required_components <- c("overall", "trend", "seasonal", "training_boundary")
  missing <- setdiff(required_components, names(input))
  if (length(missing) > 0L) {
    stop("Figure 1 input is missing: ", paste(missing, collapse = ", "), ".")
  }
  reporting_required_columns(
    input$overall,
    c("date", "observed", "mean", "lower", "upper"),
    "Figure 1 overall component"
  )
  for (component in c("trend", "seasonal")) {
    reporting_required_columns(
      input[[component]],
      c("date", "mean", "lower", "upper"),
      paste("Figure 1", component, "component")
    )
  }
  invisible(input)
}

render_figure_01 <- function(input, primary_pdf) {
  validate_figure_01_input(input)
  draw <- function() {
    old <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(old), add = TRUE)
    graphics::layout(
      matrix(c(1, 1, 2, 1, 1, 3), nrow = 2, byrow = TRUE),
      widths = c(1, 1, 1.15)
    )
    graphics::par(mar = c(4.4, 4.4, 1.2, 1.2), las = 1)
    draw_model_component(
      input$overall,
      "weekly deaths",
      input$training_boundary,
      observed = input$overall$observed
    )
    graphics::mtext("(a): Overall", side = 1, line = 2.8, font = 2)
    draw_model_component(
      input$trend,
      "seasonally adjusted deaths",
      input$training_boundary
    )
    graphics::mtext("(b): Trend", side = 1, line = 2.8, font = 2)
    draw_model_component(
      input$seasonal,
      "(log) relative rate",
      input$training_boundary
    )
    graphics::mtext("(c): Seasonal", side = 1, line = 2.8, font = 2)
  }
  render_submission_figure(draw, primary_pdf, width = 11.5, height = 6.2)
}

map_palette <- function(number_of_bins) {
  if (number_of_bins < 3L || number_of_bins > 9L) {
    stop("Map palettes support between 3 and 9 bins.")
  }
  rev(RColorBrewer::brewer.pal(number_of_bins, "RdYlGn"))
}

draw_pscore_map_panel <- function(
  map_data,
  age_group,
  wave,
  breaks,
  legend_labels,
  title,
  label_column = "label"
) {
  if (!inherits(map_data, "sf")) {
    stop("Map reporting input must be an sf object.")
  }
  reporting_required_columns(
    map_data,
    c("age_group", "wave", "p_median", label_column),
    "map reporting input"
  )
  selected <- map_data[
    map_data$age_group == age_group & map_data$wave == wave,
  ]
  if (nrow(selected) == 0L) {
    stop("No map rows were found for age ", age_group, " and wave ", wave, ".")
  }
  bins <- cut(selected$p_median, breaks = breaks, include.lowest = TRUE)
  colors <- map_palette(length(breaks) - 1L)
  fill <- colors[as.integer(bins)]
  fill[is.na(fill)] <- "white"
  graphics::plot(sf::st_geometry(selected), col = fill, border = "grey20", reset = FALSE)
  points <- suppressWarnings(sf::st_point_on_surface(selected))
  coordinates <- sf::st_coordinates(points)
  graphics::text(
    coordinates[, 1],
    coordinates[, 2],
    labels = selected[[label_column]],
    cex = 0.55
  )
  graphics::legend(
    "bottomleft",
    legend = legend_labels,
    fill = colors,
    cex = 0.55,
    bty = "o",
    bg = "white"
  )
  graphics::mtext(title, side = 1, line = 1.5, cex = 0.9)
}

render_four_panel_map <- function(input, primary_pdf, region = c("europe", "north_america")) {
  region <- match.arg(region)
  if (!is.list(input) || is.null(input$map_data)) {
    stop("Map figure input must be a list containing map_data.")
  }
  defaults <- if (region == "europe") {
    list(
      breaks = c(-Inf, 0, 0.05, 0.10, 0.15, 0.20, 0.25, 0.50, 0.75, Inf),
      legend_labels = c("0", "5", "10", "15", "20", "25", "50", "75", "100"),
      ages = c("40-59", "60-79")
    )
  } else {
    list(
      breaks = c(-Inf, 0, 0.05, 0.10, 0.15, 0.20, 0.25, 0.50, 1, Inf),
      legend_labels = c("0", "5", "10", "15", "20", "25", "50", "100", "200"),
      ages = c("40-59", "60-79")
    )
  }
  breaks <- input$breaks %||% defaults$breaks
  legend_labels <- input$legend_labels %||% defaults$legend_labels
  panels <- list(
    list(age = defaults$ages[[1]], wave = "initial", label = "(a) Ages 40-59, Initial Wave"),
    list(age = defaults$ages[[1]], wave = "delta", label = "(b) Ages 40-59, Delta Wave"),
    list(age = defaults$ages[[2]], wave = "initial", label = "(c) Ages 60-79, Initial Wave"),
    list(age = defaults$ages[[2]], wave = "delta", label = "(d) Ages 60-79, Delta Wave")
  )
  draw <- function() {
    old <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(old), add = TRUE)
    graphics::par(mfrow = c(2, 2), mar = c(2.8, 0.8, 0.8, 0.8))
    for (panel in panels) {
      draw_pscore_map_panel(
        input$map_data,
        panel$age,
        panel$wave,
        breaks,
        legend_labels,
        panel$label
      )
    }
  }
  render_submission_figure(draw, primary_pdf, width = 10.5, height = 9.2)
}

`%||%` <- function(left, right) {
  if (is.null(left)) right else left
}

draw_wave_annotations <- function(config, y_position) {
  definitions <- wave_table(config)
  plot_limits <- graphics::par("usr")[1:2]
  for (index in seq_len(nrow(definitions))) {
    wave_start <- as.numeric(definitions$start[[index]])
    wave_end <- as.numeric(definitions$end_exclusive[[index]])
    if (wave_end < plot_limits[[1]] || wave_start > plot_limits[[2]]) {
      next
    }
    graphics::abline(v = definitions$start[[index]], lty = 3, col = "grey30")
    visible_start <- max(wave_start, plot_limits[[1]])
    visible_end <- min(wave_end, plot_limits[[2]])
    midpoint <- as.Date((visible_start + visible_end) / 2, origin = "1970-01-01")
    graphics::text(
      midpoint,
      y_position,
      labels = tools::toTitleCase(definitions$wave[[index]]),
      srt = 45,
      cex = 0.7
    )
  }
}

smooth_reporting_trajectory <- function(
  data,
  region,
  bandwidth_days = 14
) {
  reporting_required_columns(
    data,
    c("region", "age_group", "vaccination_group", "date", "mean", "variance"),
    "trajectory smoothing input"
  )
  selected <- data[data$region == region, ]
  other <- data[data$region != region, ]
  groups <- split(
    selected,
    interaction(
      selected[c("age_group", "vaccination_group")],
      drop = TRUE,
      lex.order = TRUE
    )
  )
  smoothed <- lapply(groups, function(group) {
    group <- group[order(as.Date(group$date)), ]
    numeric_date <- as.numeric(as.Date(group$date))
    group$mean <- stats::ksmooth(
      numeric_date,
      group$mean,
      kernel = "box",
      bandwidth = bandwidth_days,
      x.points = numeric_date
    )$y
    group$variance <- stats::ksmooth(
      numeric_date,
      group$variance,
      kernel = "box",
      bandwidth = bandwidth_days,
      x.points = numeric_date
    )$y
    group$lower <- group$mean - 1.96 * sqrt(group$variance)
    group$upper <- group$mean + 1.96 * sqrt(group$variance)
    group
  })
  dplyr::bind_rows(other, dplyr::bind_rows(smoothed))
}

draw_trajectory_panel <- function(
  data,
  region,
  age_group,
  title,
  config,
  y_limits,
  y_label,
  interval = c("ribbon", "dashed"),
  include_legend = FALSE
) {
  interval <- match.arg(interval)
  selected <- data[data$region == region & data$age_group == age_group, ]
  groups <- c("high", "low")
  if (!all(groups %in% selected$vaccination_group)) {
    stop("Trajectory panel ", title, " requires high and low vaccination groups.")
  }
  colors <- c(high = "#4C5FFF", low = "#FF4D4D")
  first <- selected[selected$vaccination_group == groups[[1]], ]
  first <- first[order(first$date), ]
  graphics::plot(
    as.Date(first$date),
    first$mean,
    type = "n",
    xlab = "",
    ylab = y_label,
    ylim = y_limits,
    bty = "l"
  )
  for (group in groups) {
    current <- selected[selected$vaccination_group == group, ]
    current <- current[order(current$date), ]
    dates <- as.Date(current$date)
    if (interval == "ribbon") {
      draw_interval_polygon(dates, current$lower, current$upper, colors[[group]])
    } else {
      graphics::lines(dates, current$lower, col = colors[[group]], lty = 3, lwd = 0.6)
      graphics::lines(dates, current$upper, col = colors[[group]], lty = 3, lwd = 0.6)
    }
    graphics::lines(dates, current$mean, col = colors[[group]], lwd = 1.6)
  }
  graphics::abline(h = 0, lty = 2, col = "grey35")
  draw_wave_annotations(config, y_limits[[2]] - diff(y_limits) * 0.09)
  graphics::mtext(title, side = 1, line = 2.4, cex = 0.9)
  if (include_legend) {
    graphics::legend(
      "topright",
      legend = c("high vac", "low vac"),
      col = colors,
      lty = 1,
      lwd = 1.6,
      bty = "n",
      cex = 0.75
    )
  }
}

validate_trajectory_input <- function(data, label) {
  reporting_required_columns(
    data,
    c(
      "region",
      "age_group",
      "vaccination_group",
      "date",
      "mean",
      "lower",
      "upper"
    ),
    label
  )
  if (!all(unique(data$vaccination_group) %in% c("high", "low"))) {
    stop(label, " contain unsupported vaccination groups.")
  }
  validate_interval_summary(data, label = label)
  invisible(data)
}

render_six_panel_trajectory <- function(
  data,
  primary_pdf,
  config,
  panel_specification,
  y_limits,
  y_label,
  interval
) {
  validate_trajectory_input(data, "trajectory reporting input")
  if (any(panel_specification$scientific_status == "blocked_estimand_definition")) {
    blocked <- panel_specification$panel_id[
      panel_specification$scientific_status == "blocked_estimand_definition"
    ]
    stop(
      "Rendering is blocked by unresolved estimand definitions in panel(s): ",
      paste(blocked, collapse = ", "),
      "."
    )
  }
  draw <- function() {
    old <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(old), add = TRUE)
    graphics::par(mfrow = c(3, 2), mar = c(4, 4, 1.2, 0.8), las = 1)
    for (index in seq_len(nrow(panel_specification))) {
      panel <- panel_specification[index, ]
      draw_trajectory_panel(
        data,
        panel$region,
        panel$data_age_group,
        paste0("(", panel$panel_id, ") ", panel$display_label),
        config,
        y_limits,
        y_label,
        interval = interval,
        include_legend = index == 1L
      )
    }
  }
  render_submission_figure(draw, primary_pdf, width = 10.2, height = 11.5)
}
