# United States Historical Comparison

This validation separates immutable historical artifacts from regenerated and corrected pipeline outputs. The external archive is read-only and is never required for a normal pipeline run.

## Current historical inventory

The audited archive under `North_America/` contains:

| Artifact | Observed count |
|---|---:|
| Sex-stratified grouped-age prediction files | 407 |
| Sex-stratified all-age prediction files | 102 |
| Non-sex prediction files | 204 |
| Historical sex wave-summary rows | 1,625 |
| Historical non-sex wave-summary rows | 816 |

The missing grouped-age sex prediction is Vermont, age 0-44, female. The 102 historical all-age predictions represent 51 jurisdictions by two sexes and are separate from the primary grouped-age loop.

## Standardized data comparison

The named-column preprocessing in `R/us_data.R` reproduces both historical `USA_monthly.rda` analytic datasets exactly after canonical renaming, removal of CDC notes/footer rows, exclusion of non-analytic ages, and conversion of `Over 80`/`Over 85` to `GE80`/`GE85`.

- Sex-stratified standardized analytic rows: 118,190.
- Non-sex standardized analytic rows: 58,787.
- Both contain 51 jurisdictions.
- Model inputs through 2023-08-31 contain 117,079 and 58,459 rows, respectively.

## Corrected Omicron status

The historical sex result contains 1,625 rows because its Omicron branch reused the Delta window. A separately named corrected validation artifact contains 1,624 rows and preserves all 1,218 non-Omicron historical rows exactly. The corrected wave counts are 406 initial, 405 Alpha, 407 Delta, and 406 Omicron rows. Corrected production summaries are not allowed to overwrite the historical RDA.

After the Darjeeling full US refit, this document must be extended with numerical comparisons between regenerated predictions, the 816-row non-sex historical summary, and the separately named corrected sex summary.

## Reproduction command

From the repository root, supply the `North_America` archive directory explicitly:

```bash
Rscript scripts/validate_us_historical.R /path/to/covid_excess/North_America
```

The command writes ignored comparison files under `artifacts/results/us/validation/` and fails if standardized data or inventory counts differ.
