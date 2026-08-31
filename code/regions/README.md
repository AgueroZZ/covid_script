# Region-specific model code

This directory retains source-specific model implementations used by the
regional production runners.

- `canada/` contains the Canada BayesGP model functions.
- `europe/` contains the Eurostat model functions and TMB source used for
  equivalence validation.
- `england_wales/` contains the England-and-Wales model implementation.
- `ireland/` contains the quarterly Ireland model functions and TMB source.
- `united_states/` contains the historical sex-stratified and non-sex model
  functions used by regression tests; the production United States pipeline is
  implemented by the reusable modules under `R/` and `_targets.R`.

Users should invoke the entry points under `scripts/model_fitting/` rather than
source these files interactively. All generated objects are written under the
ignored `output/` directory.
