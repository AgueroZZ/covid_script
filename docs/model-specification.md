# Primary Model Contract

This file records the model implemented by the reproducible pipeline. Regional migration plans must document any frequency-specific construction without changing the primary estimand or silently introducing a different model.

## Outcome and likelihood

- Primary outcome: observed death counts.
- Likelihood: Poisson with a log link and an IID overdispersion component.
- Trend: second-order integrated Wiener process (IWP2).
- Seasonality: aggregated seasonal Gaussian process with periods 12, 6, 4, and 3 months for monthly data. Frequency-specific equivalents must be documented by each regional pipeline.
- Training period: observations strictly before 2020-01-01.
- US exposure offset: log calendar days per month.
- Population offset: none in the implemented primary model.
- Jurisdiction pooling: none. Each jurisdiction-age-sex series is fitted independently.

## Posterior quantities

Posterior predictive draws are retained rather than replacing uncertainty with fitted means. Pointwise summaries and wave-level estimands are distinct outputs. Wave-level excess-mortality summaries use the ratio of total observed deaths to total posterior-predicted baseline deaths over the configured interval; they are not averages of pointwise ratios.

Wave intervals are start-inclusive and end-exclusive and are read from `config/analysis.yml`. The canonical assignment implementation is `R/waves.R`.

## Reproducibility requirements

- Model targets derive random-number seeds deterministically from the base seed in `config/analysis.yml` and the stratum identifier.
- Fitted models, posterior draws, figures, and tables are generated artifacts and are not committed to Git.
- Every final artifact must be associated with the Git commit, input hashes, package lockfile, and a SHA-256 output manifest.
- Any departure from this contract requires an explicit configuration change, regression test, and manuscript-to-code audit update.
