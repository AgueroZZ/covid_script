# Manuscript Numerical Claim Ledger

This ledger audits the accepted view of the tracked manuscript and appendix.
It uses the frozen analysis contract and frozen reporting inputs; it does not
modify either Word source.

## Status summary

| Status | Claims |
|---|---:|
| requires_manuscript_edit | 14 |
| supported_qualitatively | 4 |
| unsupported_remove_or_reproduce | 4 |
| verified_exact | 7 |
| verified_rounded | 3 |

## Point-by-point ledger

### ABS-001 - Abstract Methods

- Current wording: weekly or monthly mortality over forty European countries, fifty US states, and Canadian provinces (2000-2024)
- Verified value/evidence: 35 European reporting geographies; 50 states plus DC; 10 mapped Canadian provinces; source-specific periods
- Status: `requires_manuscript_edit`
- Action: Replace the broad count and date wording with exact source-specific scope; distinguish the Figure 3 map scope from the complete US cohort.
- Evidence: `config/manuscript_analysis_contract.yml;config/europe_reporting_cohort.csv;config/us_reporting_cohort.csv`

### ABS-002 - Abstract Methods

- Current wording: initial, Alpha, Delta, and Omicron wave date ranges
- Verified value/evidence: initial [2020-03-01,2020-11-01); alpha [2020-11-01,2021-07-01); delta [2021-07-01,2022-01-01); omicron [2022-01-01,regional endpoint]
- Status: `verified_exact`
- Action: No manuscript change required.
- Evidence: `config/manuscript_analysis_contract.yml`

### ABS-003 - Abstract Findings

- Current wording: P-scores frequently exceeded 40% among individuals aged 80 years or older
- Verified value/evidence:
- Status: `unsupported_remove_or_reproduce`
- Action: Remove or regenerate a frozen GE80/GE85 summary supporting this threshold; current adopted Figures 2-5 and Table 1 do not estimate this statement.
- Evidence: ``

### ABS-004 - Abstract Findings

- Current wording: high-vaccination areas generally maintained near-zero P-scores during Alpha and Delta
- Verified value/evidence: Europe high 40-79 medians: Alpha 0.080 and Delta 0.080; US high 40-79 Delta median 0.212
- Status: `requires_manuscript_edit`
- Action: Replace near-zero with a directionally accurate statement and report region-, age-, and wave-specific values only.
- Evidence: `artifacts/reporting/inputs/figure_04_vaccination_pscore.csv`

### ABS-005 - Abstract Findings

- Current wording: low-vaccination areas experienced increases of approximately 50%
- Verified value/evidence: Delta maxima range from 0.613 to 1.160 across displayed US ages and 0.660 to 0.862 across displayed European combined ages/waves
- Status: `requires_manuscript_edit`
- Action: Specify the panel, wave, age group, and whether the value is a pointwise maximum or median; do not retain a single approximate 50% summary.
- Evidence: `artifacts/reporting/inputs/figure_04_vaccination_pscore.csv`

### ABS-006 - Abstract Findings

- Current wording: male P-scores were generally 5-15 percentage points higher
- Verified value/evidence: many pooled medians are within about -6 to +10 percentage points and some Delta contrasts reverse sign
- Status: `requires_manuscript_edit`
- Action: Replace the universal 5-15 percentage-point range with wave-, age-, and vaccination-group-specific female-minus-male contrasts.
- Evidence: `artifacts/reporting/inputs/figure_05_sex_difference.csv`

### ABS-007 - Abstract Findings

- Current wording: female P-scores aged 40-79 were comparable to or higher than male P-scores in Eastern Europe but weaker in the US
- Verified value/evidence: European low-vaccination Delta median female-minus-male is positive; US patterns vary by age and wave
- Status: `supported_qualitatively`
- Action: No manuscript change required.
- Evidence: `artifacts/reporting/inputs/figure_05_sex_difference.csv`

### MTH-001 - Methods Vaccination groups

- Current wording: European high group omitted Portugal and low group included Poland; US high group included California and New York and low group included Arkansas and Texas
- Verified value/evidence: Portugal is high and Poland is neither in Europe; California, New York, Arkansas, and Texas are neither in the US
- Status: `requires_manuscript_edit`
- Action: Use the exact output-specific frozen cohorts and exclude all jurisdictions classified as neither from pooled high-versus-low comparisons.
- Evidence: `config/europe_reporting_cohort.csv;config/us_reporting_cohort.csv`

### RES-001 - Results Geographic patterns

- Current wording: Delta wave (July-December 2021)
- Verified value/evidence: [2021-07-01,2022-01-01)
- Status: `verified_exact`
- Action: No manuscript change required.
- Evidence: `config/manuscript_analysis_contract.yml`

### RES-002 - Results Geographic patterns

- Current wording: Romania, Bulgaria, Slovakia and Serbia experienced increases in both 40-59 and 60-79 age groups
- Verified value/evidence: Delta P-scores: BG 0.637/0.616; RS 0.460/0.524; RO 0.333/0.477; SK 0.334/0.331 for ages 40-59/60-79
- Status: `verified_rounded`
- Action: No manuscript change required.
- Evidence: `artifacts/reporting/inputs/figure_02_europe_maps.rds`

### RES-003 - Results Geographic patterns

- Current wording: New York showed the largest initial-wave peak
- Verified value/evidence: NY is highest among mapped geographies in both 40-59 (0.510) and 60-79 (0.446)
- Status: `verified_rounded`
- Action: No manuscript change required.
- Evidence: `artifacts/reporting/inputs/figure_03_north_america_maps.rds`

### RES-004 - Results Geographic patterns

- Current wording: low-vaccination states including Mississippi, Louisiana, Alabama, and Arkansas
- Verified value/evidence: MS, LA and AL are low; AR is neither
- Status: `requires_manuscript_edit`
- Action: Remove Arkansas from the low-vaccination examples or describe it separately as neither under the fixed rule.
- Evidence: `config/us_reporting_cohort.csv`

### RES-005 - Results Vaccination patterns

- Current wording: low-vaccination Europe included Romania, Bulgaria and Poland; low-vaccination US included Mississippi, Louisiana, Tennessee and Texas
- Verified value/evidence: Romania, Bulgaria, Mississippi, Louisiana and Tennessee are low; Poland and Texas are neither
- Status: `requires_manuscript_edit`
- Action: Remove Poland and Texas from low-vaccination examples and use only locations classified by the frozen registry.
- Evidence: `config/europe_reporting_cohort.csv;config/us_reporting_cohort.csv`

### RES-006 - Results Vaccination patterns

- Current wording: Delta peaks often exceeded 50% in low-vaccination settings
- Verified value/evidence: pointwise maxima exceed 0.50 in displayed low-vaccination Delta trajectories, but pooled medians are lower
- Status: `supported_qualitatively`
- Action: No manuscript change required.
- Evidence: `artifacts/reporting/inputs/figure_04_vaccination_pscore.csv`

### RES-007 - Results Age patterns

- Current wording: oldest age groups
- Verified value/evidence: Europe GE80; US sex and Canada GE85
- Status: `requires_manuscript_edit`
- Action: Name source-specific oldest-age bands rather than using a single implicit cutoff.
- Evidence: `config/manuscript_analysis_contract.yml`

### RES-008 - Results Age patterns

- Current wording: ages 60-79 maintained near-zero P-scores in high-vaccination areas
- Verified value/evidence: high-vaccination wave medians are region- and wave-dependent and are not uniformly near zero
- Status: `requires_manuscript_edit`
- Action: Replace with exact regional wave summaries or soften to lower than low-vaccination groups where supported.
- Evidence: `artifacts/reporting/inputs/figure_04_vaccination_pscore.csv`

### RES-009 - Results Age patterns

- Current wording: middle-aged adults (40-64 years)
- Verified value/evidence: Europe/non-sex US 40-59; sex-stratified US/Canada 45-64
- Status: `requires_manuscript_edit`
- Action: Replace 40-64 with source-specific bands or explicitly call them approximately aligned middle-aged bands.
- Evidence: `config/manuscript_analysis_contract.yml`

### RES-010 - Results Sex differences

- Current wording: male excess mortality was generally 5-15 percentage points higher
- Verified value/evidence: pooled medians vary substantially and reverse sign in some Delta strata
- Status: `requires_manuscript_edit`
- Action: Use the female-minus-male sign convention and quote only exact wave/age/group summaries from Figure 5.
- Evidence: `artifacts/reporting/inputs/figure_05_sex_difference.csv`

### RES-011 - Results Sex differences

- Current wording: Romania, Bulgaria and Serbia showed comparable or higher female mortality among ages 40-79
- Verified value/evidence: Table 1 supports female-higher Delta P-scores at ages 60-79; pooled Figure 5 supports the group-level reversal
- Status: `supported_qualitatively`
- Action: No manuscript change required.
- Evidence: `artifacts/tables/table_01_wave_pscores.csv`

### RES-012 - Results Sex differences

- Current wording: US ages 0-44 showed dramatically elevated male excess mortality across all waves in both vaccination groups
- Verified value/evidence: Delta median female-minus-male is positive in both groups (high 0.014; low 0.058) and all wave ranges cross or approach zero
- Status: `requires_manuscript_edit`
- Action: Delete across all waves and dramatically; describe the mixed wave-specific 0-44 contrasts.
- Evidence: `artifacts/reporting/inputs/figure_05_sex_difference.csv`

### RES-013 - Results Sex differences

- Current wording: older adults (65+)
- Verified value/evidence: 65-84 in US Figure 5 panel f
- Status: `requires_manuscript_edit`
- Action: Replace 65+ with 65-84 when describing Figure 5 panel f.
- Evidence: `config/manuscript_analysis_contract.yml`

### RES-014 - Results Sex differences

- Current wording: low-vaccination states showed higher female P-scores during Delta in older adults
- Verified value/evidence: US 65-84 low-vaccination Delta median female-minus-male is approximately 0.047
- Status: `verified_rounded`
- Action: No manuscript change required.
- Evidence: `artifacts/reporting/inputs/figure_05_sex_difference.csv`

### RES-015 - Results Supplementary analysis

- Current wording: supplementary maps, time series and all-age analyses
- Verified value/evidence:
- Status: `unsupported_remove_or_reproduce`
- Action: Inventory and freeze each cited supplementary output before submission, or remove unsupported references.
- Evidence: ``

### RES-016 - Results Supplementary analysis

- Current wording: interactive web application
- Verified value/evidence:
- Status: `unsupported_remove_or_reproduce`
- Action: Supply and archive the application with a stable URL or remove the claim and placeholder.
- Evidence: ``

### RES-017 - Results Supplementary analysis

- Current wording: New York initial-wave P-score exceeded 200%
- Verified value/evidence:
- Status: `unsupported_remove_or_reproduce`
- Action: Remove this pointwise claim or freeze the underlying full New York trajectory and a reproducible calculation.
- Evidence: ``

### APP-001 - Appendix A Model

- Current wording: second-order integrated Wiener process and four seasonal harmonics
- Verified value/evidence: IWP2; m=4
- Status: `verified_exact`
- Action: No manuscript change required.
- Evidence: `R/europe_model.R;config/manuscript_analysis_contract.yml`

### APP-002 - Appendix A Model

- Current wording: pre-2020 data
- Verified value/evidence: through 2019-12-31
- Status: `verified_exact`
- Action: No manuscript change required.
- Evidence: `config/manuscript_analysis_contract.yml`

### APP-003 - Appendix A Model

- Current wording: three components and priors h=5 u=0.1 alpha=0.01; seasonal h=1
- Verified value/evidence: matches corrected Europe model implementation
- Status: `supported_qualitatively`
- Action: No manuscript change required.
- Evidence: `R/europe_model.R`

### APP-004 - Appendix A Offset

- Current wording: monthly US calendar-day offset and weekly zero offset
- Verified value/evidence: monthly calendar-month length adjustment; weekly no offset
- Status: `verified_exact`
- Action: No manuscript change required.
- Evidence: `R/figures.R;config/manuscript_analysis_contract.yml`

### APP-005 - Appendix B P-score

- Current wording: P-score equals 100 times observed minus expected divided by expected
- Verified value/evidence: 100*(observed-expected)/expected
- Status: `verified_exact`
- Action: No manuscript change required.
- Evidence: `appendices_tracked.docx`

### APP-006 - Appendix B Combined ages

- Current wording: US 0-44, 45-64 and 65-84 combine to 0-84; other regions 40-59 and 60-79 combine to 40-79
- Verified value/evidence: Figure 5 US 0-84 and Europe 40-79; Figure 4 US and Europe 40-79
- Status: `requires_manuscript_edit`
- Action: Qualify the equation by figure: Figure 4 US combines 40-59 and 60-79, whereas Figure 5 US combines 0-44, 45-64 and 65-84.
- Evidence: `config/manuscript_analysis_contract.yml`

### APP-007 - Appendix B Pooling

- Current wording: inverse-variance pooling
- Verified value/evidence: inverse-variance fixed-effect pooling with normal-approximation intervals
- Status: `verified_exact`
- Action: No manuscript change required.
- Evidence: `R/figures.R`
