# Darjeeling Full-Run Runbook

Darjeeling Linux is the reference environment for production model fitting. Run from a clean checkout of the intended commit and retain all commands and terminal output in the run record.

## Preflight

Record the Git commit and start time:

```bash
git status --short
git rev-parse HEAD
date --iso-8601=seconds
```

The working tree must contain no unexplained source changes. Restore packages and run the complete test suite:

```bash
Rscript -e 'renv::restore()'
Rscript tests/testthat.R
```

Save R and platform provenance:

```bash
Rscript -e 'sessionInfo()'
```

## Build and verify

Run the complete graph and generate the output manifest:

```bash
Rscript scripts/run_pipeline.R
Rscript scripts/verify_artifacts.R
```

Record the end time and inspect the manifest:

```bash
date --iso-8601=seconds
wc -l artifacts/manifests/artifacts.csv
```

The run record must include the Git commit, start and end timestamps, `sessionInfo()`, target outcome, artifact-manifest path, and any warnings. A run is incomplete if any regional target fails, an expected final figure or table is absent, or the output manifest is not generated.

Until all regional migration plans are complete, this command builds only the targets present on the branch and must not be described as a full manuscript reproduction.
