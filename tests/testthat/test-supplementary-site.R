test_that("workflowr registers both supplementary explorers", {
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
  expect_true(any(grepl("supplementary_timeseries.Rmd", validator, fixed = TRUE)))
  expect_true(any(grepl("supplementary_wave_maps.Rmd", validator, fixed = TRUE)))
  expect_match(builder_text, "copy_supplementary_assets", fixed = TRUE)
})

test_that("supplementary pages use local interactive assets", {
  required <- c(
    "supplementary.Rmd",
    "supplementary_timeseries.Rmd",
    "supplementary_wave_maps.Rmd",
    "supplementary_app.js",
    "supplementary_app.css"
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
