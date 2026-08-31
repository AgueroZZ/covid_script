# Data sources and versioned snapshots

## Reproduction policy

The analysis uses the versioned files under `data/raw/`. These files are the
canonical inputs for exact reproduction and are verified against
`data/raw/manifest.csv`. Provider-hosted data are subject to revision, including
changes to provisional counts, historical corrections, schemas, and download
URLs.

The scripts under `scripts/data_access/` retrieve current provider versions into
`provider_downloads/` by default. That directory is ignored by Git. Current
downloads are appropriate for sensitivity checks and planned data updates, but
they must not silently replace the canonical snapshots.

Run the following command before analysis:

```bash
Rscript --vanilla scripts/data_access/verify_snapshots.R
```

## Registered providers

### Eurostat

Weekly deaths are from Eurostat dataset `DEMO_R_MWK_20`. The repository snapshot
contains observations from 2000-W01 through 2024-W19 and records the provider's
22 May 2024 update. Eurostat provides current data through its
[dissemination API](https://ec.europa.eu/eurostat/api/dissemination/sdmx/2.1/data/demo_r_mwk_20).

### Statistics Canada

Canadian weekly deaths are from Table `13-10-0768-01`, *Provisional weekly death
counts, by age group and sex*. The repository snapshot ends on 5 August 2023.
The [current table](https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=1310076801)
may contain subsequent corrections and releases.

### Office for National Statistics

England-and-Wales inputs combine two source definitions. Data through 2020 are
daily occurrence counts among usual residents from ONS reference 14173. Data
for 2021--2023 are weekly registration counts that include non-residents, from
the archived weekly provisional workbooks. The change in count definition is
preserved in the analysis metadata and is not treated as a continuous source
definition.

The 2023 snapshot is the archived week-35 workbook published on 19 September
2023. ONS retains [previous dataset versions](https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/datasets/weeklyprovisionalfiguresondeathsregisteredinenglandandwales/2023).

### Central Statistics Office Ireland

Quarterly registered deaths are an extract of PxStat table `VSQ20`, restricted
to total deaths. The repository snapshot covers 2010Q1 through 2023Q1. The
[current table](https://data.cso.ie/table/VSQ20) extends beyond this period and
may be revised.

### CDC WONDER

United States monthly death counts are saved CDC WONDER exports grouped by
residence state, age, year, and month, with a separate sex-stratified export.
The repository retains the Notes column and footer rows used to validate each
export. CDC final and provisional mortality databases are updated on different
schedules, and provisional observations may be revised. Current exports require
acceptance of the [CDC WONDER data-use terms](https://wonder.cdc.gov/mcd.html).

### Our World in Data

The adopted vaccination classifications use compact R snapshots derived from
the archived Our World in Data vaccination tables at the 1 July 2021 reference
date. The repository records both the European latest-on-or-before-date extract
and the United States state-level extract. The source repository is archived;
its [vaccination documentation](https://github.com/owid/covid-19-data/blob/master/public/data/vaccinations/README.md)
describes the variables and original upstream sources.

### United States Census Bureau

United States map geometry uses the 2018 national state cartographic boundary
file at 1:500,000 resolution. The exact ZIP archive is versioned under
`data/raw/us_census/`; the provider copy is available from the
[Census Bureau](https://www2.census.gov/geo/tiger/GENZ2018/shp/cb_2018_us_state_500k.zip).
