#!/usr/bin/env Rscript

store <- tempfile("covid-targets-smoke-")
on.exit(unlink(store, recursive = TRUE, force = TRUE), add = TRUE)

targets::tar_make(
  names = "foundation_manifest",
  callr_function = NULL,
  store = store
)
manifest <- targets::tar_read(foundation_manifest, store = store)

if (!file.exists(manifest)) {
  stop("Foundation smoke target did not create its manifest.")
}

cat("Foundation smoke pipeline passed.\n")
