source(here::here("R", "config.R"))
source(here::here("R", "waves.R"))
source(here::here("R", "reporting.R"))
source(here::here("R", "figures.R"))
source(here::here("R", "tables.R"))

test_that("fixed-effect reporting summaries use named groups", {
  input <- tibble::tibble(
    region = "Europe",
    age_group = "40-59",
    vaccination_group = "high",
    date = rep(as.Date("2021-01-01"), 2),
    geography = c("AA", "BB"),
    mean = c(0.1, 0.3),
    variance = c(1, 4)
  )
  result <- aggregate_fixed_effect_summary(
    input,
    c("region", "age_group", "vaccination_group", "date")
  )

  expect_equal(result$mean, 0.14)
  expect_equal(result$jurisdictions, 2L)
  expect_identical(
    result$interval_method,
    "fixed_effect_normal_approximation"
  )
})

test_that("Figure 1 renderer creates vector and raster outputs", {
  dates <- seq.Date(as.Date("2019-01-01"), by = "month", length.out = 24)
  component <- tibble::tibble(
    date = dates,
    mean = seq(10, 20, length.out = length(dates)),
    lower = seq(8, 18, length.out = length(dates)),
    upper = seq(12, 22, length.out = length(dates))
  )
  input <- list(
    overall = dplyr::mutate(component, observed = mean + sin(seq_along(mean))),
    trend = component,
    seasonal = dplyr::mutate(
      component,
      mean = sin(seq_along(mean)) / 10,
      lower = mean - 0.05,
      upper = mean + 0.05
    ),
    training_boundary = as.Date("2020-01-01")
  )
  output <- tempfile(fileext = ".pdf")
  rendered <- render_figure_01(input, output)

  expect_true(all(file.exists(rendered)))
  expect_true(all(file.info(rendered)$size > 0L))
})

test_that("four-panel map renderer uses explicit age-wave keys", {
  skip_if_not_installed("sf")
  skip_if_not_installed("RColorBrewer")
  first <- sf::st_polygon(list(matrix(
    c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0),
    ncol = 2,
    byrow = TRUE
  )))
  second <- sf::st_polygon(list(matrix(
    c(1, 0, 2, 0, 2, 1, 1, 1, 1, 0),
    ncol = 2,
    byrow = TRUE
  )))
  combinations <- tidyr::crossing(
    age_group = c("40-59", "60-79"),
    wave = c("initial", "delta"),
    label = c("AA", "BB")
  )
  combinations$p_median <- seq(0.05, 0.4, length.out = nrow(combinations))
  combinations$geometry <- sf::st_sfc(rep(list(first, second), 4), crs = 4326)
  map_data <- sf::st_as_sf(combinations)
  output <- tempfile(fileext = ".pdf")
  rendered <- render_four_panel_map(
    list(map_data = map_data),
    output,
    region = "europe"
  )

  expect_true(all(file.exists(rendered)))
  expect_true(all(file.info(rendered)$size > 0L))
})

test_that("six-panel trajectories render recorded estimand statuses", {
  config <- read_analysis_config(here::here("config", "analysis.yml"))
  registry <- read_reporting_registry()
  panels <- registry$panels[registry$panels$output_id == "figure_04", ]
  panels$scientific_status <- "confirmed"
  dates <- seq.Date(as.Date("2020-03-01"), by = "month", length.out = 18)
  input <- tidyr::crossing(
    region = c("Europe", "United States"),
    age_group = c("40-79", "40-59", "60-79"),
    vaccination_group = c("high", "low"),
    date = dates
  )
  input$mean <- ifelse(input$vaccination_group == "high", 0.1, 0.2)
  input$lower <- input$mean - 0.03
  input$upper <- input$mean + 0.03
  output <- tempfile(fileext = ".pdf")
  rendered <- render_six_panel_trajectory(
    input,
    output,
    config,
    panels,
    y_limits = c(-0.3, 1),
    y_label = "P-score",
    interval = "ribbon"
  )

  expect_true(all(file.exists(rendered)))
  expect_true(all(file.info(rendered)$size > 0L))

  figure_05_panels <- registry$panels[
    registry$panels$output_id == "figure_05",
  ]
  figure_05_input <- dplyr::bind_rows(
    tidyr::crossing(
      region = "Europe",
      age_group = c("40-79", "40-59", "60-79"),
      vaccination_group = c("high", "low"),
      date = dates
    ),
    tidyr::crossing(
      region = "United States",
      age_group = c("0-84", "0-44", "65-84"),
      vaccination_group = c("high", "low"),
      date = dates
    )
  )
  figure_05_input$mean <- 0.05
  figure_05_input$lower <- 0.01
  figure_05_input$upper <- 0.09
  figure_05_output <- tempfile(fileext = ".pdf")
  figure_05_rendered <- render_six_panel_trajectory(
    figure_05_input,
    figure_05_output,
    config,
    figure_05_panels,
    y_limits = c(-0.2, 0.5),
    y_label = "P-score difference (F-M)",
    interval = "dashed"
  )
  expect_true(all(file.exists(figure_05_rendered)))
})

test_that("European trajectory smoothing preserves explicit groups", {
  input <- tidyr::crossing(
    region = c("Europe", "United States"),
    age_group = "40-59",
    vaccination_group = c("high", "low"),
    date = seq.Date(as.Date("2020-01-01"), by = "week", length.out = 8)
  )
  input$mean <- seq_len(nrow(input)) / 100
  input$variance <- 0.01
  input$lower <- input$mean - 1.96 * sqrt(input$variance)
  input$upper <- input$mean + 1.96 * sqrt(input$variance)
  output <- smooth_reporting_trajectory(input, "Europe")

  expect_equal(nrow(output), nrow(input))
  expect_setequal(unique(output$region), c("Europe", "United States"))
  expect_true(all(output$lower <= output$mean))
  expect_true(all(output$upper >= output$mean))
})

synthetic_table_wave_summary <- function(analysis_path, age_group) {
  grid <- tidyr::crossing(
    geography = c("High", "Low"),
    sex = c("female", "male"),
    wave = c("initial", "alpha", "delta", "omicron")
  )
  dplyr::mutate(
    grid,
    analysis_path = analysis_path,
    age_group = age_group,
    p_median = ifelse(sex == "female", 0.2, 0.1),
    status = "success"
  )
}

test_that("Table 1 is built from fixed vaccination membership", {
  vaccination <- tibble::tibble(
    geography = c("High", "Low", "Middle"),
    people_vaccinated_per_hundred = c(70, 30, 50),
    vaccination_group = c("high", "low", "neither")
  )
  output <- build_table_01(
    synthetic_table_wave_summary("europe_sex", "60-79"),
    synthetic_table_wave_summary("us_sex", "65-84"),
    vaccination,
    vaccination
  )

  expect_equal(nrow(output), 4L)
  expect_false(any(output$geography == "Middle"))
  expect_true(all(grepl("M: 0.100; F: 0.200", output$initial, fixed = TRUE)))
  expect_setequal(output$estimand_age_group, c("60-79", "65-84"))
})
