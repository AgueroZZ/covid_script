source(here::here("R", "manuscript_claims.R"))

test_that("manuscript numerical claim registry has a complete terminal schema", {
  claims <- read_manuscript_claim_registry()
  expect_gt(nrow(claims), 25L)
  expect_equal(anyDuplicated(claims$claim_id), 0L)
  expect_true(all(claims$status %in% manuscript_claim_statuses()))
  expect_true(all(claims$document %in% c("manuscript", "appendix")))
})

test_that("computed evidence locks the frozen scope and map values", {
  evidence <- build_manuscript_computed_evidence()
  value <- function(key) evidence$value[evidence$evidence_key == key]
  expect_equal(value("scope.europe_reporting_geographies"), "35")
  expect_equal(value("scope.us_reporting_geographies"), "51")
  expect_equal(value("scope.figure03_mapped_geographies"), "59")
  expect_equal(as.numeric(value("figure02.delta.BG.40-59")), 0.6371173, tolerance = 1e-6)
  expect_equal(as.numeric(value("figure03.initial.NY.40-59")), 0.5100181, tolerance = 1e-6)
})

test_that("trajectory evidence preserves required manuscript wave summaries", {
  evidence <- build_manuscript_computed_evidence()
  f4 <- evidence[evidence$evidence_key == "figure04.us.40-79.high.delta", ]
  f5 <- evidence[evidence$evidence_key == "figure05.us.0-44.low.delta", ]
  expect_equal(nrow(f4), 1L)
  expect_gt(f4$median, 0.20)
  expect_equal(nrow(f5), 1L)
  expect_gt(f5$median, 0)
})
