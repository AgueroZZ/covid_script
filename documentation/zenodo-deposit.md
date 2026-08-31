# Zenodo fitted-results deposit

The Git repository contains the complete analysis code, exact raw-data
snapshots, and final publication figures and tables. Computationally expensive
fitted objects are maintained separately under the ignored `output/` tree and
are prepared as a versioned Zenodo dataset.

## Deposit boundary

The deposit includes:

- the final 2026-08-30 corrected European production fits and diagnostics;
- the final England and Wales and Ireland fits, diagnostics, and wave summaries;
- the US posterior-prediction objects and Canadian reporting object used by the
  adopted North American outputs;
- map geometry that is not duplicated by the tracked raw-data snapshots;
- the exact reporting inputs, configuration, manifests, figures, and table from
  the completed submission freeze.

The deposit excludes validation-only copies, historical or superseded fits,
compiler products, failed runs, stale freezes, logs, private manuscript files,
and derivatives that can be regenerated quickly. The tracked raw-data snapshots
are not duplicated in the deposit.

## Build and verify

From the repository root:

```bash
Rscript scripts/submission/prepare_zenodo_deposit.R
```

The command creates a separately named directory and ZIP archive below
`output/zenodo/`. It never modifies the source fits. Every deposited file is
listed in `manifests/deposit_inventory.csv` and verified by `SHA256SUMS`.

Inspect the selection without copying data:

```bash
Rscript scripts/submission/prepare_zenodo_deposit.R \
  --dry-run=true --archive=false
```

## Metadata confirmation

`config/zenodo_metadata.yml` is a formal metadata draft. Creator order, ORCIDs,
affiliations, publication date, data license, and the accepted article DOI must
be confirmed before the Zenodo record is published. A DOI is not inserted into
the repository before Zenodo assigns or reserves it.
