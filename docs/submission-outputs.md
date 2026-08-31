# Submission figures and tables

`figures.docx` is the authoritative inventory of manuscript-facing outputs. The repository may contain additional exploratory graphics, but only the five figures and one table below are part of the submission reporting contract.

The historical archive at `/Users/ziangzhang/Desktop/covid_mortality/covid_excess` was used only to establish provenance. It is not a runtime dependency of this repository.

## Output registry

| Output | Adopted content | Historical source | New entry point | Current scientific status |
|---|---|---|---|---|
| Figure 1 | Netherlands age 80+ SAMM overall, trend, and seasonal components | `Europe/non_stratified/Figure1.R` and `NL_Fig.pdf` | `scripts/reporting/figure_01_model_illustration.R` | Rebuilt from corrected Eurostat seasonal prior |
| Figure 2 | European total-sex initial/Delta P-score maps, ages 40-59 and 60-79 | `Europe/stratified/script/plot_map.R` | `scripts/reporting/figure_02_europe_maps.R` | Rebuilt for 33 corrected Eurostat geographies plus corrected England-and-Wales and Ireland approximations |
| Figure 3 | North American non-sex initial/Delta P-score maps, ages 40-59 and 60-79 | `North_America/non-stratified/plot_map_all.R` | `scripts/reporting/figure_03_north_america_maps.R` | Confirmed |
| Figure 4 | European and US high/low-vaccination P-score trajectories | European and US non-sex aggregated P-score scripts | `scripts/reporting/figure_04_vaccination_pscore.R` | Europe rebuilt from corrected Eurostat fits; caption age label review still required |
| Figure 5 | European and US high/low-vaccination female-minus-male trajectories | European and US sex-contrast scripts | `scripts/reporting/figure_05_sex_difference.R` | Europe rebuilt from corrected Eurostat fits; combined-age panel uses ages 40-79 |
| Table 1 | Sex-specific wave P-scores for selected vaccination groups | European and US `prepare_table.R` scripts | `scripts/reporting/table_01_wave_pscores.R` | Europe rebuilt from corrected Eurostat fits; US corrected Omicron rows retained |

Machine-readable details are in `config/reporting_outputs.csv` and `config/reporting_panels.csv`.

## Historical figure-to-code verification

- Figure 1 in the Word file is visually identical to `covid_excess/Europe/non_stratified/NL_Fig.pdf`. The archive retains the three component PDFs and the R code that generated them.
- Figure 2 matches the total-sex European map panels `EU_map_Y40-59_T_initial.pdf`, `EU_map_Y40-59_T_delta.pdf`, `EU_map_Y60-79_T_initial.pdf`, and `EU_map_Y60-79_T_delta.pdf`.
- Figure 3 matches the combined US/Canada panels `all_map_40-59_initial.pdf`, `all_map_40-59_delta.pdf`, `all_map_60-79_initial.pdf`, and `all_map_60-79_delta.pdf`.
- Figures 4 and 5 were manually assembled from six separately generated trajectory panels. The new scripts own both panel generation and assembly so that no Keynote or copy/paste step is required.
- Table 1 was manually inserted as an image. The new table script produces machine-readable CSV and submission-ready HTML from named columns.

## Corrections and recalculation impact

| Change | Figure 1 | Figure 2 | Figure 3 | Figure 4 | Figure 5 | Table 1 |
|---|---:|---:|---:|---:|---:|---:|
| Fixed vaccination thresholds or membership | No | No | No | Yes | Yes | Yes |
| European combined-age sex-contrast correction | No | No | No | No | Yes, panel (a) | No |
| US sex-stratified Omicron wave aggregation | No | No | No | No | No for trajectories | Yes |
| US non-sex fitted predictions | No | No | Yes | Yes | No | No |
| US sex-stratified fitted predictions | No | No | No | No | Yes | Yes |
| European fitted predictions | Yes for the Netherlands branch | Yes | No | Yes | Yes | Yes |
| Canadian non-sex fitted predictions | No | No | Yes | No | No | No |
| Wave-boundary changes | No | Yes | Yes | Plot annotations | Plot annotations | Yes |

The known US Omicron correction affects wave aggregation, not the pointwise trajectories used in Figures 4 and 5. Therefore it requires recalculation of the affected Table 1 values but does not by itself require refitting the mortality models.

### Completed corrected-Eurostat rebuild

The verified 388-model Eurostat corrected-prior refit now supplies Figures 1, 2, 4, and 5 and the European rows of Table 1. Historical and corrected artifacts, pointwise comparisons, hashes, and provenance are stored under `artifacts/reporting/validation/europe_corrected_psd_prior_20260830/`.

Figure 2 now contains the 33 verified Eurostat geographies plus separately corrected England-and-Wales and Ireland results. The source age bands remain explicit: England and Wales use Under 65, 65-84, and 85+; Ireland uses 25-44, 45-64, 65-84, and 85+. The figure retains the historical approximate comparison mappings declared in `config/uk_ie_reporting_cohort.csv`. Figure 3 is unaffected and its PDF and PNG are byte-identical. The US trajectory rows in Figures 4 and 5 and the 11 US Table 1 rows were preserved.

### England-and-Wales and Ireland corrected extension

Three England-and-Wales weekly models and four Ireland quarterly models were refitted with the corrected seasonal predictive-SD prior. England-and-Wales weekly dates now use the same ISO-week Monday convention as Eurostat. Ireland uses one annual harmonic and samples quarterly posterior predictive counts directly from the aggregated quarterly Poisson mean.

The extended Figure 2 contains 35 geographies. The 132 existing Eurostat map cells agree with the previously installed corrected input to within `1e-12`; only the newly restored UK and Ireland cells were added. For Figure 2, the largest England-and-Wales changes relative to the historical artifacts are Under-65 Initial `0.1463 -> 0.1619` and Delta `0.1878 -> 0.2081`. Ireland Figure 2 changes are all below `0.008` P-score units. Across all 28 UK/Ireland age-wave comparisons, the only sign change is England-and-Wales ages 85+ during Omicron (`-0.0024 -> 0.0170`), a near-zero result outside Figure 2.

The full regional comparisons, mapping provenance, preserved 33-geography baseline, output hashes, and completion flag are under `artifacts/validation/uk_ie_corrected_20260830/`.

The largest corrected European differences were 0.05033 P-score units in Figure 4, 0.03325 in Figure 5, and 0.02955 in the Table 1 wave medians. Visual inspection and numerical comparisons found no reversal of the main qualitative vaccination-group or sex-contrast patterns. Exact numerical Results statements and all affected manuscript images/table cells must nevertheless be updated from the corrected artifacts.

### Completed US Omicron rebuild

The complete historical-versus-corrected rebuild is stored locally under `artifacts/validation/us_omicron_all_outputs_20260830/`. It contains standardized source summaries, eight age-sex bar panels and 32 state-wave maps per result version in PDF and PNG, controlled Table 1 versions, row-level comparisons, immutable manuscript baselines, SHA-256 manifests, and a completion flag.

The corrected calculation uses the configured US analysis end of August 31, 2023. The previous manuscript-bundle builder did not enforce this boundary and included fitted-prediction dates through November 2023 in its Omicron aggregate; November is a visibly incomplete source month. `prepare_manuscript_bundle.R` now enforces `config$regions$us_sex$analysis_end` when constructing the US wave summary.

All 1,218 non-Omicron historical rows are reproduced exactly. All 406 shared Omicron P-score medians change, with 400 decreases, 6 increases, and 72 sign changes. The historical-only Wyoming 0-44 female Omicron row is removed because the observed series does not reach Omicron; Vermont 0-44 female remains missing because the fitted model is absent.

The five manuscript figure PNGs are bit-for-bit unchanged after rerendering, confirming that this correction does not flow into their pointwise inputs. Table 1 changes for all 11 adopted US jurisdictions and has been regenerated at `artifacts/tables/table_01_wave_pscores.csv` and `.html`.

The manuscript states that US CDC WONDER data extend through October 31, 2023, while the analysis code and cohort registry end on August 31, 2023. The Methods text must be reconciled with the frozen analysis interval before submission.

## Age-estimand decisions

### Figure 4

Both historical panels labeled “All Ages” combine only the 40-59 and 60-79 models and observed counts. They estimate ages 40-79, not all ages. Before submission, either:

1. relabel both panels as “Ages 40-79”; or
2. define and regenerate a genuinely broader combined-age estimand.

The reporting registry preserves the historical 40-79 definition and marks the caption for review.

### Figure 5, European combined-age panel

The canonical European panel now aggregates ages 40-59 and 60-79 for both observed and expected deaths, yielding a coherent ages 40-79 estimand. This choice follows the manuscript's 40-79 sex-difference claim, Appendix Eq. (3), and the intended expected-count construction in the historical script.

The incompatible historical panel remains available at `artifacts/reporting/validation/figure_05_europe_historical_incoherent.csv`. It preserves the original observed ages 20-79 versus expected ages 40-79 calculation for provenance and impact comparison, but it is not used by the Figure 5 renderer.

The US all-age sex-contrast panel is internally consistent and combines 0-44, 45-64, and 65-84, yielding ages 0-84.

Figure 5 panel (f) now uses US ages 65-84, matching the manuscript's older-adult result. The standardized input retains ages 45-64, and `scripts/reporting/compare_figure_05_us_panel_f.R` preserves both controlled six-panel versions for provenance.

## Runtime contract

Every entry-point script reads only standardized files under `artifacts/` and writes generated outputs under `artifacts/figures/` or `artifacts/tables/`. Missing inputs produce an explicit error listing the required upstream files. No reporting script searches the historical archive or depends on the current working directory.
