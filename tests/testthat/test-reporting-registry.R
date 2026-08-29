test_that("submission output registry covers the authoritative document", {
  outputs <- readr::read_csv(
    here::here("config", "reporting_outputs.csv"),
    show_col_types = FALSE
  )
  panels <- readr::read_csv(
    here::here("config", "reporting_panels.csv"),
    show_col_types = FALSE
  )

  expect_equal(nrow(outputs), 6L)
  expect_setequal(
    outputs$output_id,
    c(
      "figure_01",
      "figure_02",
      "figure_03",
      "figure_04",
      "figure_05",
      "table_01"
    )
  )
  expect_equal(anyDuplicated(outputs$output_id), 0L)
  expect_true(all(file.exists(here::here(outputs$script_path))))
  expect_true(all(outputs$scientific_status %in% c(
    "confirmed",
    "caption_review_required",
    "blocked_estimand_definition"
  )))

  expect_setequal(unique(panels$output_id), outputs$output_id[1:5])
  expect_equal(anyDuplicated(panels[c("output_id", "panel_id")]), 0L)
  expect_equal(nrow(panels[panels$output_id == "figure_01", ]), 3L)
  expect_equal(nrow(panels[panels$output_id %in% c("figure_02", "figure_03"), ]), 8L)
  expect_equal(nrow(panels[panels$output_id %in% c("figure_04", "figure_05"), ]), 12L)
  expect_equal(
    panels$scientific_status[
      panels$output_id == "figure_05" & panels$panel_id == "a"
    ],
    "blocked_estimand_definition"
  )
})
