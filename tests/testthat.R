if (requireNamespace("renv", quietly = TRUE)) {
  .libPaths(c(renv::paths$library(project = normalizePath(".")), .libPaths()))
}

library(testthat)
library(targets)

test_dir("tests/testthat")
