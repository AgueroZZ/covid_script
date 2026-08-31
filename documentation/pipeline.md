# Pipeline architecture

The analysis follows a fixed dependency order:

```text
versioned source snapshots
  -> standardized regional observations
  -> explicit analysis cohorts
  -> stratum-specific mortality models
  -> posterior predictive counts
  -> pointwise and wave-level P-scores
  -> vaccination-group and sex contrasts
  -> manuscript figures and tables
  -> tracked publication artifacts
```

## Analysis layers

Reusable transformations and statistical contracts are implemented under
`R/`. Region-specific model implementations are retained under `code/regions/`,
and production fitting entry points are under `scripts/model_fitting/`.

The United States target graph in `_targets.R` connects raw CDC WONDER exports
to standardized observations, cohort eligibility, model branches, posterior
summaries, sex contrasts, and wave summaries. Europe, England and Wales,
Ireland, and Canada use explicit regional batch runners because their source
frequencies and model implementations differ.

All regional runners support `--manifest-only=true`. This mode validates inputs
and writes the expected model inventory without fitting models. Production fits
write only to `output/` and use completion flags and batch manifests.

## Reporting layer

The reporting registry comprises five figures and one table:

```text
scripts/reporting/figure_01_model_illustration.R
scripts/reporting/figure_02_europe_maps.R
scripts/reporting/figure_03_north_america_maps.R
scripts/reporting/figure_04_vaccination_pscore.R
scripts/reporting/figure_05_sex_difference.R
scripts/reporting/table_01_wave_pscores.R
scripts/reporting/run_all.R
```

Each renderer reads named standardized inputs under `output/` and fails with an
explicit missing-input message. It does not search historical directories or
fit mortality models. Figure renderers produce vector PDF and 300-dpi PNG
files. The table renderer produces CSV and HTML.

The publication copies under `figures/manuscript/` and `tables/manuscript/` are
small, versioned artifacts synchronized from a completed local freeze. Their
manifests record file sizes and SHA-256 hashes.

## Output policy

Large or regenerable products are local:

```text
output/
  data/
  models/
  results/
  reporting/
  validation/
  submission_freeze/
  manifests/
```

The entire directory is ignored by Git. `scripts/validation/verify_outputs.R`
can inventory and hash generated products after a local run. Website builds
read only tracked inputs and therefore do not depend on the presence of
`output/`.
