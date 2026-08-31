#!/usr/bin/env Rscript

source("R/validation.R")
source("R/raw_manifest.R")
source("R/us_verification.R")

result <- verify_us_completion()
cat("US pipeline verification passed.\n")
print(result)
