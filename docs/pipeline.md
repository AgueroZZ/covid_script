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

The graph validates the global configuration and implements the complete United States path through standardized observations, named cohort eligibility, vaccination membership, sex-stratified and non-sex model branches, posterior predictions, pointwise P-scores, sex contrasts, wave summaries, completion checks, and historical comparisons. Full US model production remains a Darjeeling run; the local test suite validates the transformation and contract layers.

The reporting layer is also structurally complete. `config/reporting_outputs.csv` records the five adopted figures and one adopted table from `figures.docx`, while `config/reporting_panels.csv` makes every panel estimand explicit. Dedicated scripts under `scripts/reporting/` render standardized artifact inputs without accessing the historical archive.

Migration proceeds region by region in this order:

1. United States, including sex-stratified and non-sex-stratified analyses.
2. Canada.
3. Europe.
4. England and Wales, then Ireland.
5. Connect the already-defined cross-region reporting inputs, then run manuscript output and claim validation.

Each migration must identify immutable raw inputs, replace positional selection with named cohort rules, preserve posterior draws, compare regenerated summaries to historical artifacts, and add regression tests before its targets become part of the default graph.

## Reporting entry points

The reporting scripts use deterministic default paths:

```text
scripts/reporting/figure_01_model_illustration.R
scripts/reporting/figure_02_europe_maps.R
scripts/reporting/figure_03_north_america_maps.R
scripts/reporting/figure_04_vaccination_pscore.R
scripts/reporting/figure_05_sex_difference.R
scripts/reporting/table_01_wave_pscores.R
scripts/reporting/run_all.R
```

Each figure script writes a vector PDF and 300-dpi PNG. The table script writes CSV and HTML. Named `--input=value` and `--output=value` arguments may override the defaults for validation, but the production target graph uses paths registered in `config/reporting_outputs.csv`.

The reporting runner does not treat a missing artifact as permission to search old directories. It reports the exact missing upstream files. It also refuses to render outputs marked `blocked_estimand_definition`.

Two historical labels require manuscript review:

- Figure 4 “All Ages” represents ages 40-79 in both Europe and the US.
- Figure 5 European “All Age” historically combined observed ages 20-79 with expected ages 40-79 and is therefore blocked until a coherent age range is chosen.

These are downstream calculation and labeling issues. If compatible posterior prediction artifacts are available, correcting them does not require fitting the underlying mortality models again.

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
