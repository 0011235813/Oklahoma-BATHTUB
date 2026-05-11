# okBATHTUB 0.1.1

This is a remediation release addressing seven issues identified in a
v0.1.0 forensic review.

## Major scientific correction

* **TP retention default is now Walker BATHTUB Model 1 (second-order
  available-P)**, the canonical default of the BATHTUB program.
  In v0.1.0, the default was the Vollenweider (1976) / Larsen-Mercier
  (1976) first-order hydraulic-residence form, mislabelled as the
  "Walker BATHTUB default." This was incorrect: Walker's BATHTUB
  documentation identifies Model 1 as the calibrated default, and
  treats the Vollenweider / Larsen-Mercier form as Walker Model 5
  ("Northern Lakes"), which is explicitly *not* calibrated to U.S. Army
  Corps of Engineers reservoir data.

  For a typical Cross Timbers reservoir, the corrected Walker Model 1
  default predicts roughly 40% lower in-lake TP than v0.1.0's
  mislabelled default. Users who want to reproduce v0.1.0 results
  exactly can pass `coefficients = "vollenweider"` to `ok_load()`.

* **TN retention default is now Walker BATHTUB Model 1** (second-order),
  matching the TP form for consistency. The fixed apparent-settling-
  velocity form is retained as the TN companion for
  `coefficients = "vollenweider"`.

## Other fixes

* **`data/ok_reservoirs.rda`** is now populated. v0.1.0 shipped this
  file as a 334-byte empty data frame despite documentation referring
  to 123 lakes. v0.1.1 contains 40 Oklahoma reservoirs across seven EPA
  Level III ecoregions, compiled from publicly available USACE, BOR,
  NID, and OWRB sources. Source CSV is in `data-raw/` for transparency
  and reproducibility.

* **AWQMS connector removed**. The `ok_from_awqms()` family of
  functions hardcoded a private database server and credentials and
  has been removed from the public package. Users with AWQMS access
  should pull data using `DBI` directly in their own workflows.

* **`ok_project_path()` removed**. The function exported a Windows
  OneDrive path via `Sys.getenv("USERPROFILE")` and would fail on
  every non-Windows CRAN check machine.

* **`ok_load_multi()` now passes `ecoregion` through to `ok_load()`**.
  Previously, the argument was silently dropped from multi-tributary
  workflows.

* **Author / copyright cleanup**. v0.1.1 is released under the MIT
  license with Jordon Henderson as the sole copyright holder. v0.1.0
  contained contradictory authorship statements across `DESCRIPTION`,
  `LICENSE`, `README`, and plot captions.

## Minor improvements

* `ok_tsi()` now reports `tsi_n` (number of components averaged) and
  emits a message when fewer than three TSI components are available.

* Carlson TSI(Chl-a) intercept uses 9.81 (the value in Carlson 1977),
  not 9.84 (Walker's BATHTUB documentation rounding).

* Plot functions now check for `ggplot2`, `dplyr`, and `tidyr`
  availability at call time (these are now in `Suggests`, not
  `Imports`).

* Documentation for `ok_hydraulics()` now discusses the volume =
  `mean_depth * surface_area` approximation explicitly.

* Documentation for `ok_inlake()` discusses non-algal turbidity as a
  caveat for Secchi predictions in central and western Oklahoma
  reservoirs.

* New test file `test-reservoirs.R` includes a regression test ensuring
  `ok_reservoirs` is never silently empty.

# okBATHTUB 0.1.0

Initial release.
