# Submission figures and tables

`figures.docx` is the authoritative inventory of manuscript-facing outputs. The repository may contain additional exploratory graphics, but only the five figures and one table below are part of the submission reporting contract.

The historical archive at `/Users/ziangzhang/Desktop/covid_mortality/covid_excess` was used only to establish provenance. It is not a runtime dependency of this repository.

## Output registry

| Output | Adopted content | Historical source | New entry point | Current scientific status |
|---|---|---|---|---|
| Figure 1 | Netherlands age 80+ SAMM overall, trend, and seasonal components | `Europe/non_stratified/Figure1.R` and `NL_Fig.pdf` | `scripts/reporting/figure_01_model_illustration.R` | Confirmed |
| Figure 2 | European total-sex initial/Delta P-score maps, ages 40-59 and 60-79 | `Europe/stratified/script/plot_map.R` | `scripts/reporting/figure_02_europe_maps.R` | Confirmed; UK/Ireland age harmonization still requires a named upstream mapping |
| Figure 3 | North American non-sex initial/Delta P-score maps, ages 40-59 and 60-79 | `North_America/non-stratified/plot_map_all.R` | `scripts/reporting/figure_03_north_america_maps.R` | Confirmed |
| Figure 4 | European and US high/low-vaccination P-score trajectories | European and US non-sex aggregated P-score scripts | `scripts/reporting/figure_04_vaccination_pscore.R` | Caption review required |
| Figure 5 | European and US high/low-vaccination female-minus-male trajectories | European and US sex-contrast scripts | `scripts/reporting/figure_05_sex_difference.R` | One estimand is blocked |
| Table 1 | Sex-specific wave P-scores for selected vaccination groups | European and US `prepare_table.R` scripts | `scripts/reporting/table_01_wave_pscores.R` | Confirmed |

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
| US sex-stratified Omicron wave aggregation | No | No | No | No | No for trajectories | Yes |
| US non-sex fitted predictions | No | No | Yes | Yes | No | No |
| US sex-stratified fitted predictions | No | No | No | No | Yes | Yes |
| European fitted predictions | Yes for the Netherlands branch | Yes | No | Yes | Yes | Yes |
| Canadian non-sex fitted predictions | No | No | Yes | No | No | No |
| Wave-boundary changes | No | Yes | Yes | Plot annotations | Plot annotations | Yes |

The known US Omicron correction affects wave aggregation, not the pointwise trajectories used in Figures 4 and 5. Therefore it requires recalculation of the affected Table 1 values but does not by itself require refitting the mortality models.

## Unresolved age-estimand issues

### Figure 4

Both historical panels labeled “All Ages” combine only the 40-59 and 60-79 models and observed counts. They estimate ages 40-79, not all ages. Before submission, either:

1. relabel both panels as “Ages 40-79”; or
2. define and regenerate a genuinely broader combined-age estimand.

The reporting registry preserves the historical 40-79 definition and marks the caption for review.

### Figure 5, European all-age panel

The historical European script constructs observed deaths by summing ages 20-39, 40-59, and 60-79, but constructs expected deaths by summing only ages 40-59 and 60-79. The resulting P-score difference does not have a coherent age estimand. This panel is marked `blocked_estimand_definition`; the new reporting runner must not generate it until observed and expected age groups are identical.

The US all-age sex-contrast panel is internally consistent and combines 0-44, 45-64, and 65-84, yielding ages 0-84.

## Runtime contract

Every entry-point script reads only standardized files under `artifacts/` and writes generated outputs under `artifacts/figures/` or `artifacts/tables/`. Missing inputs produce an explicit error listing the required upstream files. No reporting script searches the historical archive or depends on the current working directory.
