test_that("tracked manuscript artifacts match their publication manifests", {
  manifests <- c(
    here::here("figures", "manuscript", "manifest.csv"),
    here::here("tables", "manuscript", "manifest.csv")
  )
  expect_true(all(file.exists(manifests)))

  publication <- do.call(
    rbind,
    lapply(manifests, utils::read.csv, stringsAsFactors = FALSE, check.names = FALSE)
  )
  expect_equal(nrow(publication), 12L)
  expect_setequal(unique(publication$artifact_id), sprintf("figure_%02d", 1:5) |> c("table_01"))

  public_paths <- here::here(publication$public_path)
  expect_true(all(file.exists(public_paths)))
  expect_true(all(file.info(public_paths)$size > 0L))

  observed_hashes <- vapply(
    public_paths,
    digest::digest,
    character(1L),
    algo = "sha256",
    file = TRUE,
    serialize = FALSE
  )
  expect_identical(unname(observed_hashes), unname(publication$sha256))
  expect_equal(sum(publication$format == "pdf"), 5L)
  expect_equal(sum(publication$format == "png"), 5L)
  expect_equal(sum(publication$format == "csv"), 1L)
  expect_equal(sum(publication$format == "html"), 1L)
})

test_that("reporting scripts write only to the ignored output tree", {
  registry <- utils::read.csv(
    here::here("config", "reporting_outputs.csv"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  expect_true(all(startsWith(registry$primary_artifact, "output/")))
  expect_false(any(startsWith(registry$primary_artifact, "figures/manuscript/")))
  expect_false(any(startsWith(registry$primary_artifact, "tables/manuscript/")))

  scripts <- lapply(here::here(registry$script_path), readLines, warn = FALSE)
  script_text <- paste(unlist(scripts, use.names = FALSE), collapse = "\n")
  expect_false(grepl("figures/manuscript|tables/manuscript", script_text, fixed = FALSE))
})
