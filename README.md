# Reproducible COVID-19 Excess Mortality Analysis

This repository is being converted from a historical script collection into an end-to-end reproducible pipeline for the associated manuscript. The intended workflow covers immutable source snapshots, standardized regional observations, stratum-specific model fitting, posterior-derived analyses, and final figures and tables.

The current branch contains the validated pipeline foundation. Regional processing and model targets are being migrated in explicit follow-on stages; see [Pipeline architecture](docs/pipeline.md) for the current boundary and migration order.

## Reference environment

Darjeeling Linux is the reference platform for full model fitting. The R package environment is pinned by `renv.lock`, including the archived BayesGP source package in `vendor/`. Large fitted models and generated outputs are written below `artifacts/` and are intentionally ignored by Git.

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

The Codex macOS sandbox may prohibit `processx` system calls when an R file is passed directly to `Rscript`. That sandbox-only case can use the equivalent command documented in [Local smoke testing](docs/runbooks/local-smoke.md). It is not required on Darjeeling.

## Inputs and provenance

- `config/data_sources.csv` registers every currently identified source and historical local snapshot.
- `data/raw/manifest.csv` is the canonical schema for immutable raw-file hashes and snapshot dates. Regional migration stages will populate it as source files are normalized under `data/raw/`.
- `docs/provenance/` records the baseline repository and historical archive audit.
- `config/analysis.yml` is the canonical analysis contract for wave dates, vaccination thresholds, regional frequencies, age groups, and primary model settings.

The historical `covid_excess` directory is a read-only provenance source. It is not part of the repository and must not be modified by pipeline code.

## Contributor documentation

- [Pipeline architecture](docs/pipeline.md)
- [Canonical data dictionary](docs/data-dictionary.md)
- [Primary model contract](docs/model-specification.md)
- [Local smoke testing](docs/runbooks/local-smoke.md)
- [Darjeeling full run](docs/runbooks/darjeeling-full-run.md)

Do not commit generated fitted models, posterior draws, figures, tables, target stores, or artifact manifests. Commit source code, configuration, tests, documentation, and intentionally versioned source snapshots only.
