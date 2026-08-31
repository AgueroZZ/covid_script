# Reproducible COVID-19 Excess Mortality Analysis

This repository is being converted from a historical script collection into an end-to-end reproducible pipeline for the associated manuscript. The intended workflow covers immutable source snapshots, standardized regional observations, stratum-specific model fitting, posterior-derived analyses, and final figures and tables.

The current branch contains the validated foundation and the complete US data, cohort, model, posterior-summary, vaccination, and historical-validation graph. Canada and Europe are the next regional migrations. The manuscript-facing reporting inventory is now fixed by `figures.docx`: five figures and one table, each with a dedicated R entry point. See [Pipeline architecture](docs/pipeline.md) for the current boundary and migration order.

## Reference environment

Darjeeling Linux is the reference platform for full model fitting. The R package environment is pinned by `renv.lock`, including the archived BayesGP source package in `vendor/`. Large fitted models and generated outputs are written below `output/` and are intentionally ignored by Git.

Restore the environment from the repository root:

```bash
Rscript -e 'renv::restore()'
```

Run the foundation smoke pipeline:

```bash
Rscript scripts/run_smoke_test.R
```

Run the complete target graph available on the current branch:

```bash
Rscript scripts/run_pipeline.R
```

Run the tests:

```bash
Rscript tests/testthat.R
```

After the required upstream artifacts exist, render one manuscript output directly:

```bash
Rscript scripts/reporting/figure_01_model_illustration.R
Rscript scripts/reporting/figure_02_europe_maps.R
Rscript scripts/reporting/figure_03_north_america_maps.R
Rscript scripts/reporting/figure_04_vaccination_pscore.R
Rscript scripts/reporting/table_01_wave_pscores.R
```

Inspect or run the complete reporting registry:

```bash
Rscript scripts/reporting/run_all.R
```

Figure 5 now uses the corrected European ages 40-79 sex-contrast estimand and US ages 65-84 in panel (f). Figure 4 uses ages 40-79 in both regions. Full provenance and recalculation triggers are documented in [Submission figures and tables](docs/submission-outputs.md).

After corrected fitted results and reporting inputs are available, rebuild every
downstream reporting consumer, run the complete test suite, and create the local
submission freeze with:

```bash
Rscript --vanilla scripts/submission/freeze_local_submission.R \
  --output-root=output/submission_freeze/local_20260831
```

The freeze contains reporting inputs, Figures 1-5, Table 1, selected validation
figures, hashes, command logs, and runtime provenance. It does not refit models,
copy manuscript authoring files, or build a Zenodo upload bundle. After visually
reviewing the rendered PDFs, finalize it with:

```bash
Rscript --vanilla scripts/submission/finalize_local_submission_freeze.R \
  --output-root=output/submission_freeze/local_20260831 \
  --visual-qa=pass
```

The Codex macOS sandbox may prohibit `processx` system calls when an R file is passed directly to `Rscript`. That sandbox-only case can use the equivalent command documented in [Local smoke testing](docs/runbooks/local-smoke.md). It is not required on Darjeeling.

## Inputs and provenance

- `config/data_sources.csv` registers every currently identified source and historical local snapshot.
- `data/raw/manifest.csv` is the canonical schema for immutable raw-file hashes and snapshot dates. Regional migration stages will populate it as source files are normalized under `data/raw/`.
- `docs/provenance/` records the baseline repository and historical archive audit.
- `config/analysis.yml` is the canonical analysis contract for wave dates, vaccination thresholds, regional frequencies, age groups, and primary model settings.
- `config/reporting_outputs.csv` and `config/reporting_panels.csv` define the authoritative submission-output and panel contracts.

The historical `covid_excess` directory is a read-only provenance source. It is not part of the repository and must not be modified by pipeline code.

## Contributor documentation

- [Pipeline architecture](docs/pipeline.md)
- [Canonical data dictionary](docs/data-dictionary.md)
- [Primary model contract](docs/model-specification.md)
- [Submission figures and tables](docs/submission-outputs.md)
- [Local smoke testing](docs/runbooks/local-smoke.md)
- [Darjeeling full run](docs/runbooks/darjeeling-full-run.md)

Do not commit generated fitted models, posterior draws, figures, tables, target stores, or artifact manifests. Commit source code, configuration, tests, documentation, and intentionally versioned source snapshots only.
