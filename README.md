# Reproducible COVID-19 Excess Mortality Analysis

This repository contains the data snapshots, analysis code, reporting scripts,
and publication artifacts for the associated COVID-19 excess mortality study.
The analysis covers Europe, England and Wales, Ireland, Canada, and the United
States.

## Reproduction model

Exact reproduction uses the versioned source snapshots under `data/raw/`.
Provider data can be revised after release, so current downloads are not used as
silent replacements. The scripts under `scripts/data_access/` retrieve current
provider versions into the ignored `provider_downloads/` directory when an
updated comparison is required.

Mortality-model fitting is computationally intensive and is intentionally
separate from website generation. Fitted objects, posterior draws, standardized
intermediate data, and regenerated outputs are written under the ignored
`output/` directory. The publication website reads the tracked figures and
tables directly and never fits a model.

## Repository layout

```text
analysis/                  workflowr analysis pages
code/regions/              source-specific model implementations
config/                    scientific and reporting contracts
data/raw/                  versioned analysis snapshots and hash manifest
documentation/             methods and reproduction documentation
figures/manuscript/        tracked manuscript figures in PDF and PNG
R/                         reusable analysis modules
scripts/data_access/       optional current-provider retrieval
scripts/model_fitting/     explicit computationally intensive entry points
scripts/reporting/         figure and table construction
scripts/publication/       tracked-artifact synchronization and site checks
tables/manuscript/         tracked manuscript tables
tests/                     analysis and reporting tests
output/                    ignored local fits and regenerated products
```

See [Repository structure](documentation/repository-structure.md) and
[Pipeline architecture](documentation/pipeline.md) for the dependency graph and
the boundary between tracked and locally generated files.

## Environment and validation

Restore the pinned R environment from the repository root:

```bash
Rscript -e 'renv::restore()'
```

Verify the versioned source snapshots and run the tests:

```bash
Rscript scripts/data_access/verify_snapshots.R
Rscript tests/testthat.R
```

Run the lightweight target smoke test without model fitting:

```bash
Rscript scripts/pipeline/run_smoke_test.R
```

The regional fitting commands are documented in
[Reproducing the analysis](documentation/reproduction.md). Each runner defaults
to one worker and supports a manifest-only mode that performs no model fitting.

## Reporting

The five manuscript figures and Table 1 have dedicated entry points under
`scripts/reporting/`. Each figure renderer writes an editable vector PDF and a
300-dpi PNG; the table renderer writes CSV and HTML. The complete reporting
registry can be checked with:

```bash
Rscript scripts/reporting/run_all.R \
  --include_caption_review=true --strict=true
```

To refresh the tracked publication copies from a completed local submission
freeze:

```bash
Rscript scripts/publication/sync_submission_artifacts.R \
  output/submission_freeze/local_20260831
```

The selected figures and tables are also available through the workflowr site.
Zenodo packaging is deferred until manuscript submission.
