# Repository structure

The repository separates scientific inputs, computation, reporting, and public
artifacts.

## Versioned inputs and contracts

- `data/raw/` contains exact source snapshots used by the analysis.
- `data/raw/manifest.csv` records provider, byte count, SHA-256 hash, temporal
  coverage, and reproduction role for every snapshot.
- `config/` contains wave definitions, cohort registries, age mappings,
  vaccination thresholds, and reporting contracts.

## Analysis code

- `R/` contains reusable readers, model contracts, estimands, summaries, and
  validation functions.
- `code/regions/` contains source-specific model implementations that cannot be
  represented by a single common module.
- `scripts/model_fitting/` contains the only public entry points that perform
  computationally intensive model fitting.
- `scripts/reporting/` constructs manuscript figures and tables from completed
  standardized results.

## Generated and publication artifacts

- `output/` contains local fitted models, posterior draws, intermediate data,
  validation products, and regenerated figures and tables. It is ignored by
  Git.
- `figures/manuscript/` and `tables/manuscript/` contain small selected
  publication artifacts and their manifests. These files are tracked.
- `analysis/` contains workflowr pages. Website generation reads tracked
  publication artifacts and executes only lightweight display or validation
  code.

## Repository hygiene

Manuscript authoring files, local run logs, planning documents, editor state,
agent metadata, caches, and provider refresh downloads are excluded from the
public repository. Historical development files remain recoverable from Git
history but are not part of the publication-facing tree.
