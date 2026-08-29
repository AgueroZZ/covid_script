# Local Smoke Test

The local smoke test validates the package environment, analysis configuration, wave definitions, and target graph without fitting the full historical model set. A deterministic one-stratum model smoke target will be added when the first regional model pipeline is migrated.

## Standard local environment

From the repository root:

```bash
Rscript -e 'renv::restore()'
Rscript tests/testthat.R
Rscript scripts/run_smoke_test.R
```

To run selected targets in the current R process:

```bash
COVID_PIPELINE_IN_PROCESS=true Rscript scripts/run_pipeline.R foundation_manifest
```

The smoke script uses a unique temporary target store, a fixed analysis configuration, and no persistent model artifact.

## Codex macOS sandbox

The managed Codex sandbox can deny a `processx` system call specifically when an R file is passed directly to `Rscript`. Use these equivalent in-process commands only in that sandbox:

```bash
Rscript -e 'source("scripts/run_smoke_test.R")'
Rscript -e 'library(targets); source("tests/testthat.R")'
```

An `Operation not permitted` error from `processx` in this environment is a sandbox execution restriction, not evidence that the target graph is invalid. The same graph must still pass the standard commands on Darjeeling before a full run is accepted.
