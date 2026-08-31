# Final Manuscript Analysis Contract

## Purpose

`config/manuscript_analysis_contract.yml` is the single submission-facing
snapshot of the scientific choices used by the manuscript outputs. It freezes:

- the pre-pandemic training endpoint;
- the US non-sex and sex-stratified analysis endpoint;
- the four pandemic-wave intervals;
- the vaccination metric, reference-date rule, and fixed high/low thresholds;
- source-region age bands and adopted Figure 4 and Figure 5 combined-age
  estimands;
- reporting cohorts, exclusions, approximate UK/Ireland mappings, and the US
  Figure 5 available-month rule.

The YAML does not replace the detailed CSV registries. It references them by
path, expected row count, and SHA-256 so that any silent change fails the
contract validator.

## Vaccination-group registry

`config/reporting_vaccination_groups.csv` freezes the 28 high/low geography
classifications used by at least one adopted vaccination comparison:

- 15 European geographies;
- 13 US states, including Maine and New Hampshire in Figure 4 only;
- 11 US states in Figure 5 and Table 1.

Every row records the source measurement date, coverage value, fixed-threshold
group, and adopted output membership. Classification is validated against the
thresholds in `config/analysis.yml`.

## US suppression decision

The production Figure 5 cohort remains unchanged. Idaho and New Mexico are
retained, and each vaccination-group trajectory uses dates commonly available
across its included states. Three incomplete source strata are recorded in the
contract validation snapshot. The controlled three-method comparison found the
effect trivial during the manuscript display period, so suppression is accepted
as disclosed missingness rather than treated as a production exclusion.

## Validation and change policy

Run:

```bash
Rscript scripts/validation/validate_analysis_contract.R
```

Successful validation writes a flattened contract snapshot, source-registry
hash table, semantic summary, accepted incomplete-strata table, R session
information, and `complete.flag` under
`output/validation/final_analysis_contract_20260831/`.

Any deliberate change to a scientific choice or referenced registry must be
reviewed, tested, and re-frozen by updating the contract version or hash. A hash
failure must never be bypassed by automatically updating the YAML.

This contract validation performs no model fitting and changes no canonical
figure, table, or manuscript file.
