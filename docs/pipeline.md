# Pipeline Architecture

The canonical dependency order is:

```text
source snapshots
  -> standardized regional observations
  -> explicit cohort eligibility
  -> stratum-specific model fits
  -> posterior predictive draws
  -> pointwise P-scores and sex contrasts
  -> wave-level ratio-of-totals summaries
  -> vaccination-group summaries
  -> figures, maps, and tables
  -> artifact manifest and manuscript audit
```

## Current migration boundary

The foundation graph currently validates `config/analysis.yml`, materializes the canonical wave table, and writes a smoke manifest. Historical regional scripts remain available as provenance, but they are not yet treated as independently reproducible target stages.

Migration proceeds region by region in this order:

1. United States, including sex-stratified and non-sex-stratified analyses.
2. Canada.
3. Europe.
4. England and Wales, then Ireland.
5. Cross-region reporting, manuscript tables, figures, and claim validation.

Each migration must identify immutable raw inputs, replace positional selection with named cohort rules, preserve posterior draws, compare regenerated summaries to historical artifacts, and add regression tests before its targets become part of the default graph.

## Artifact policy

Pipeline outputs are written below `artifacts/`:

```text
artifacts/
  data/
  models/
  results/
  figures/
  tables/
  manifests/
```

These outputs are reproducible build products and are ignored by Git. `scripts/verify_artifacts.R` hashes all generated artifacts except existing manifests and writes `artifacts/manifests/artifacts.csv`.
