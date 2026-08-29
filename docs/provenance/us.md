# United States Source Provenance

The United States analyses use tracked CDC WONDER tab-delimited snapshots, a tracked Our World in Data vaccination snapshot, and a tracked US Census Bureau boundary archive. `data/raw/manifest.csv` records each file's repository-relative path, size, and SHA-256 hash.

The original retrieval dates were not preserved with the collected repository. The manifest therefore uses 2025-11-18, the Git import date for these files, as a declared fallback snapshot date. This date identifies the repository snapshot, not the date on which CDC WONDER, Our World in Data, or the Census Bureau originally produced or downloaded the data.

The historical preprocessing and fitted artifacts remain available for validation under the external read-only `covid_excess` archive. The reproducible target graph reads the tracked text snapshots directly and does not require historical RDA model artifacts.
