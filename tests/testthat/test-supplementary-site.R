test_that("workflowr registers all supplementary explorers", {
  site <- readLines(here::here("analysis", "_site.yml"), warn = FALSE)
  validator <- readLines(
    here::here("scripts", "publication", "validate_site_inputs.R"),
    warn = FALSE
  )
  builder <- readLines(
    here::here("scripts", "publication", "build_site.R"),
    warn = FALSE
  )
  builder_text <- paste(builder, collapse = "\n")

  expect_true(any(grepl("supplementary_timeseries.html", site, fixed = TRUE)))
  expect_true(any(grepl("supplementary_wave_maps.html", site, fixed = TRUE)))
  expect_true(any(grepl("supplementary_vaccination_groups.html", site, fixed = TRUE)))
  expect_true(any(grepl("supplementary_table_explorer.html", site, fixed = TRUE)))
  expect_true(any(grepl("supplementary_timeseries.Rmd", validator, fixed = TRUE)))
  expect_true(any(grepl("supplementary_wave_maps.Rmd", validator, fixed = TRUE)))
  expect_true(any(grepl("supplementary_vaccination_groups.Rmd", validator, fixed = TRUE)))
  expect_true(any(grepl("supplementary_table_explorer.Rmd", validator, fixed = TRUE)))
  expect_true(any(grepl("vaccination_groups_20260901", validator, fixed = TRUE)))
  expect_true(any(grepl("table_explorer_20260901", validator, fixed = TRUE)))
  expect_match(builder_text, "supplementary_roots", fixed = TRUE)
  expect_match(builder_text, "copy_supplementary_assets", fixed = TRUE)
})

test_that("supplementary pages use local interactive assets", {
  required <- c(
    "supplementary.Rmd",
    "supplementary_timeseries.Rmd",
    "supplementary_wave_maps.Rmd",
    "supplementary_vaccination_groups.Rmd",
    "supplementary_table_explorer.Rmd",
    "supplementary_app.js",
    "supplementary_app.css",
    "supplementary_vaccination_app.js",
    "supplementary_vaccination_app.css",
    "supplementary_table_app.js",
    "supplementary_table_app.css"
  )
  paths <- here::here("analysis", required)
  expect_true(all(file.exists(paths)))

  text <- paste(unlist(lapply(paths, readLines, warn = FALSE)), collapse = "\n")
  expect_match(text, "data-root=\"assets/supplementary\"", fixed = TRUE)
  expect_match(text, 'rootPath(app, "index.json")', fixed = TRUE)
  expect_match(text, "supplementary_app.js", fixed = TRUE)
  expect_match(text, "supplementary_app.css", fixed = TRUE)
  expect_false(grepl("https://cdn", text, fixed = TRUE))
  expect_false(grepl("/Users/", text, fixed = TRUE))
})

test_that("time-series explorer exposes display windows and canonical waves", {
  rmd <- paste(readLines(
    here::here("analysis", "supplementary_timeseries.Rmd"),
    warn = FALSE
  ), collapse = "\n")
  javascript <- paste(readLines(
    here::here("analysis", "supplementary_app.js"),
    warn = FALSE
  ), collapse = "\n")

  expect_match(rmd, 'data-control="window"', fixed = TRUE)
  expect_match(rmd, 'value="2019"', fixed = TRUE)
  expect_match(rmd, 'value="full"', fixed = TRUE)
  expect_match(javascript, "WAVE_COLORS", fixed = TRUE)
  expect_match(javascript, "filterDisplayedSeries", fixed = TRUE)
  expect_match(javascript, "renderWaveBands", fixed = TRUE)
})

test_that("vaccination explorer exposes thresholds and manual membership", {
  rmd <- paste(readLines(
    here::here("analysis", "supplementary_vaccination_groups.Rmd"),
    warn = FALSE
  ), collapse = "\n")
  landing <- paste(readLines(
    here::here("analysis", "supplementary.Rmd"),
    warn = FALSE
  ), collapse = "\n")
  javascript <- paste(readLines(
    here::here("analysis", "supplementary_vaccination_app.js"),
    warn = FALSE
  ), collapse = "\n")
  builder <- paste(readLines(
    here::here("scripts", "supplementary", "build_vaccination_groups.R"),
    warn = FALSE
  ), collapse = "\n")

  expect_match(rmd, 'data-control="low-threshold"', fixed = TRUE)
  expect_match(rmd, 'data-control="high-threshold"', fixed = TRUE)
  expect_match(rmd, 'data-action="apply-thresholds"', fixed = TRUE)
  expect_match(rmd, 'data-action="manuscript-preset"', fixed = TRUE)
  expect_match(rmd, 'data-role="membership-body"', fixed = TRUE)
  expect_match(javascript, "thresholdGroup", fixed = TRUE)
  expect_match(javascript, "aggregateFixedEffect", fixed = TRUE)
  expect_match(javascript, "contributing_jurisdictions", fixed = TRUE)
  expect_match(javascript, "renderMembershipTable", fixed = TRUE)
  expect_match(javascript, "default_eligible", fixed = TRUE)
  expect_match(javascript, "usable_observations", fixed = TRUE)
  expect_match(javascript, "missing_observations", fixed = TRUE)
  expect_match(javascript, "coverage_fraction", fixed = TRUE)
  expect_match(javascript, "panelKey(figureSelect.value", fixed = TRUE)
  expect_match(builder, "minimum_coverage_fraction <- 0.95", fixed = TRUE)
  expect_match(builder, 'coverage = "downloads/coverage.csv"', fixed = TRUE)
  expect_match(rmd, "Usable / expected", fixed = TRUE)
  expect_match(rmd, "Missing observations", fixed = TRUE)
  expect_match(rmd, "Coverage (%)", fixed = TRUE)
  expect_false(grepl("Figure 4|Figure 5", paste(rmd, landing)))
  expect_false(grepl("https://cdn", paste(rmd, javascript), fixed = TRUE))
})

test_that("table explorer exposes column filters, sorting, and pagination", {
  rmd <- paste(readLines(
    here::here("analysis", "supplementary_table_explorer.Rmd"),
    warn = FALSE
  ), collapse = "\n")
  landing <- paste(readLines(
    here::here("analysis", "supplementary.Rmd"),
    warn = FALSE
  ), collapse = "\n")
  javascript <- paste(readLines(
    here::here("analysis", "supplementary_table_app.js"),
    warn = FALSE
  ), collapse = "\n")
  builder <- paste(readLines(
    here::here("scripts", "supplementary", "build_table_explorer.R"),
    warn = FALSE
  ), collapse = "\n")

  expect_match(rmd, 'data-control="global-search"', fixed = TRUE)
  expect_match(rmd, 'data-control="page-size"', fixed = TRUE)
  expect_match(rmd, 'data-action="manuscript"', fixed = TRUE)
  expect_match(rmd, 'data-action="clear"', fixed = TRUE)
  expect_match(rmd, 'data-action="download"', fixed = TRUE)
  expect_match(rmd, 'data-role="pagination"', fixed = TRUE)
  expect_match(javascript, "tablex-column-filter", fixed = TRUE)
  expect_match(javascript, "compareRows", fixed = TRUE)
  expect_match(javascript, "paginationItems", fixed = TRUE)
  expect_match(javascript, "supplementary_table_explorer_filtered.csv", fixed = TRUE)
  expect_match(builder, "manuscript_rows_exact", fixed = TRUE)
  expect_match(builder, "public_bundle_contains_posterior_draws", fixed = TRUE)
  expect_match(landing, "expanded_table_01.csv", fixed = TRUE)
  expect_false(grepl("https://cdn", paste(rmd, javascript), fixed = TRUE))
})
