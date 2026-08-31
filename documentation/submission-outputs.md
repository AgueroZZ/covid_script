# Manuscript figures and tables

The reporting contract contains five figures and one table. Machine-readable
definitions are stored in `config/reporting_outputs.csv` and
`config/reporting_panels.csv`.

| Output | Content | Entry point |
|---|---|---|
| Figure 1 | Netherlands age 80+ model illustration | `scripts/reporting/figure_01_model_illustration.R` |
| Figure 2 | European Initial- and Delta-wave P-score maps | `scripts/reporting/figure_02_europe_maps.R` |
| Figure 3 | North American Initial- and Delta-wave P-score maps | `scripts/reporting/figure_03_north_america_maps.R` |
| Figure 4 | Vaccination-group P-score trajectories | `scripts/reporting/figure_04_vaccination_pscore.R` |
| Figure 5 | Female-minus-male P-score differences | `scripts/reporting/figure_05_sex_difference.R` |
| Table 1 | Sex-specific wave P-scores for selected cohorts | `scripts/reporting/table_01_wave_pscores.R` |

## Estimand conventions

- Figures 2 and 3 report wave P-scores after summing observed and posterior
  expected deaths over the relevant half-open wave interval.
- Figure 4 reports inverse-variance-pooled pointwise P-score trajectories. Its
  combined-age panels represent ages 40--79 in both regions.
- Figure 5 reports pooled pointwise female-minus-male P-score differences.
  Positive values indicate higher female P-scores. The European combined-age
  panel represents ages 40--79, and the United States combined-age panel
  represents ages 0--84.
- Table 1 reports wave-level P-scores from the fixed output-specific cohorts.

England-and-Wales and Ireland age bands do not coincide exactly with the
Eurostat bands. Figure 2 uses the approximate mappings declared in
`config/uk_ie_reporting_cohort.csv`; the source bands remain explicit in the
analysis inputs and captions.

## Artifact policy

Canonical local renderings are written under `output/figures/` and
`output/tables/`. The tracked publication copies are under
`figures/manuscript/` and `tables/manuscript/` and include editable PDF figures,
PNG previews, CSV tables, and file manifests.

Reporting scripts do not fit mortality models. They require completed upstream
summaries and return an explicit error if an input is absent. The workflowr site
uses the tracked publication copies and likewise performs no model fitting.
