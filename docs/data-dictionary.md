# Canonical Data Dictionary

This document defines the standardized observation schema that every regional preprocessing pipeline must produce. Source-specific columns may be retained in intermediate tables, but downstream model targets must use these canonical fields.

## Observation fields

| Field | Type | Definition |
|---|---|---|
| `date` | `Date` | Start date of the source observation interval. Its interpretation must be paired with `source_frequency`. |
| `geography` | character | Canonical jurisdiction name used in reporting and cohort selection. |
| `age_group` | character | Mutually exclusive analysis age band. Open-ended bands must use `GE80` or `GE85`; labels such as `Over 85` are prohibited. |
| `sex` | character | Canonical value: `female`, `male`, or `total`. |
| `observed_deaths` | non-negative integer | Reported deaths for the geography, age group, sex, and interval. Suppressed values must not be silently converted to zero. |
| `suppression_status` | character | One of `observed`, `suppressed`, `missing`, or `not_applicable`. |
| `count_definition` | character | Source-specific definition of the death count, including occurrence versus registration where relevant. |
| `source_frequency` | character | One of `weekly`, `monthly`, `quarterly`, or `daily`. |
| `source_id` | character | Stable identifier present in `config/data_sources.csv`. |

## Required provenance fields

Each immutable raw snapshot must have a row in `data/raw/manifest.csv` containing its source identifier, repository-relative path, byte size, SHA-256 hash, declared snapshot date, and whether the file is tracked in Git. Regional migration code must preserve source labels before mapping them to canonical values.

## Validation rules

- A canonical observation is unique by `date`, `geography`, `age_group`, `sex`, and `source_id` unless a regional contract explicitly documents an additional key.
- `observed_deaths` cannot be negative or fractional.
- Missing or suppressed counts remain explicit through cohort eligibility checks.
- Frequencies are not mixed within a model series.
- Geography, age, and sex inclusion is determined from values, never from positional row or column indices.
