# Reproducing the analysis

The rendered workflowr documentation is available at the
[publication website](https://aguerozz.github.io/covid_script/). Website
generation reads tracked publication artifacts and does not refit models.

## 1. Restore the environment and verify inputs

```bash
Rscript -e 'renv::restore()'
Rscript scripts/data_access/verify_snapshots.R
Rscript tests/testthat.R
```

Exact reproduction uses the tracked snapshots. Optional provider-access scripts
write current downloads to `provider_downloads/`; providers may revise those
data after the repository snapshots were created.

## 2. Inspect the model inventories

The following commands validate inputs and write manifests without fitting any
model:

```bash
Rscript scripts/model_fitting/europe/refit_eurostat.R \
  --manifest-only=true
Rscript scripts/model_fitting/england_wales/refit.R \
  --manifest-only=true
Rscript scripts/model_fitting/ireland/refit.R \
  --manifest-only=true
Rscript scripts/model_fitting/canada/refit.R \
  --manifest-only=true
```

The United States model inventory is defined by `_targets.R`. A lightweight
pipeline check is available through:

```bash
Rscript scripts/pipeline/run_smoke_test.R
```

## 3. Fit models when required

Omit `--manifest-only=true` to run a regional batch. Each runner defaults to one
worker, writes only below `output/`, records input and code hashes, and creates a
completion flag only after all selected models finish. The United States target
graph is run with:

```bash
Rscript scripts/pipeline/run_pipeline.R
```

These computations can require substantial memory and wall time. They are not
performed during website generation.

## 4. Render manuscript outputs

After the required standardized summaries are present under `output/`, render
all registered outputs with:

```bash
Rscript scripts/reporting/run_all.R \
  --include_caption_review=true --strict=true
```

The figure scripts write PDF and PNG files. Table 1 is written as CSV and HTML.
The resulting artifacts can be synchronized to the tracked publication folders
only from a completed local submission freeze.

## 5. Use or prepare the fitted-results deposit

The computationally intensive fitted objects are distributed separately from
Git. To prepare the checksum-verified Zenodo package from a completed local
analysis:

```bash
Rscript scripts/submission/prepare_zenodo_deposit.R
```

The package includes canonical production fits and the final reporting inputs;
it excludes validation duplicates, compiler products, failed runs, stale
freezes, and private manuscript files. See
[Zenodo fitted-results deposit](zenodo-deposit.md) for the complete boundary.
