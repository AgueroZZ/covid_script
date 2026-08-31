# Canonical source snapshots

This directory contains the exact source snapshots used by the analysis. Each
file is tracked in Git and registered with its byte count and SHA-256 hash in
`manifest.csv`.

Data providers may revise historical observations, provisional values, file
schemas, or download endpoints. Exact reproduction therefore uses the files in
this directory. The scripts under `scripts/data_access/` retrieve current
provider versions into the ignored `provider_downloads/` directory for
comparison or future updates; they never replace these snapshots.

The two files under `owid/` are provider-derived R snapshots at the adopted
vaccination reference date. Their role is recorded explicitly in the manifest.
