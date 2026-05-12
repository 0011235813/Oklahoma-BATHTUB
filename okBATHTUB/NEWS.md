# okBATHTUB 0.1.6

* Restored `URL:` and `BugReports:` fields in `DESCRIPTION`. The GitHub
  repository (https://github.com/0011235813/okBATHTUB) is now public.

# okBATHTUB 0.1.5

Addresses two of the four NOTE items returned by win-builder R-release
(R 4.6.0) for the v0.1.4 pre-submission check.

* **Removed broken GitHub URLs from DESCRIPTION.** `URL:` and
  `BugReports:` previously pointed to a not-yet-public GitHub
  repository, causing 404 responses in CRAN's URL-validity check.
  These will be re-added once the repository is made public.

* **Updated broken EPA URL in `hawqs-linkage.Rmd`** from the deprecated
  `epa.gov/cwsrf/clean-water-state-revolving-fund-cwsrf` subpath to the
  current canonical landing page for the 604(b) Water Quality
  Management Planning Grants program:
  `epa.gov/nps/water-quality-management-planning-grants`.

The remaining two NOTE items ("possibly misspelled words" and "new
submission") are not actionable on the package side and are explained
to CRAN reviewers in `cran-comments.md`.

# okBATHTUB 0.1.4

Three small polish fixes prior to CRAN submission.

* **Removed R 4.2+ native pipe placeholder syntax from a vignette.**
  `vignettes/hawqs-linkage.Rmd` previously used `do.call(rbind, args = _)`,
  which requires R >= 4.2.0 but `DESCRIPTION` declares R >= 4.1.0.
  Replaced with the equivalent `do.call(rbind, annual_list)` two-step
  pattern that works on any supported R.

* **Namespaced `readr::read_fwf()` and `readr::fwf_cols()` calls** in the
  `hawqs-linkage.Rmd` SWAT-input example chunk. The chunk is `eval=FALSE`
  so this is cosmetic, but the namespaced form is clearer to readers and
  removes a potential `no visible global function` reviewer note.

* **Generalized "Arcadia Lake" examples** in `getting-started.Rmd` and
  `hawqs-linkage.Rmd` to "a representative Cross Timbers reservoir" where
  the example uses morphometry (890 ha / 4.2 m) that doesn't match the
  bundled `ok_reservoirs` entry for Arcadia Lake (716 ha / 4.6 m). The
  890/4.2 figures remain the canonical Walker Model 1 numerical reference
  case used throughout tests and docstring examples; only the labeling
  was changed.

* **Strengthened cross-dataset consistency test** to validate that
  `ok_reservoirs` and `ok_lake_ecoregions` agree on both the EPA L3
  *code* and *name* for shared lakes, and that all rows within
  `ok_reservoirs` sharing the same `eco_l3_name` also share the same
  `eco_l3_code`. Catches a class of bug that a name-only check would
  miss.

# okBATHTUB 0.1.3

Pre-CRAN forensic review pass addressing 14 findings from the v0.1.2
review. `R CMD check --as-cran` continues to pass cleanly (0 errors,
0 warnings, 0 notes).

## Critical fixes

* **`ok_reservoirs` and `ok_lake_ecoregions` ecoregion assignments now
  agree.** v0.1.2 had 18 of 26 shared lakes disagreeing on EPA Level III
  ecoregion (e.g. Hugo Lake was "South Central Plains" in `ok_reservoirs`
  but "Ouachita Mountains" in `ok_lake_ecoregions`). The `eco_l3_name`
  and `eco_l3_code` columns in `ok_reservoirs.csv` have been regenerated
  by joining against the authoritative `lake_ecoregion_lookup.csv`,
  with manual fallback for lakes not in the lookup (Optima Lake -> 
  Southwestern Tablelands). A new regression test
  (`test-reservoirs.R: ok_reservoirs and ok_lake_ecoregions agree on
  ecoregion for shared lakes`) guards against future drift.
  37 of 40 reservoirs received corrected ecoregion assignments.

* **`R/plot.R` source-file comment header no longer claims "Oklahoma
  Water Resources Board - Water Quality Division" institutional
  attribution.** The earlier cleanup pass scrubbed plot captions and
  visible titles but missed this source-file comment.

* **`vignettes/hawqs-linkage.Rmd` reference list no longer cites a
  current OWRB grant project as a reference.** Replaced with a generic
  EPA Clean Water Act Section 604(b) program reference.

## Major fixes

* **DESCRIPTION now leads with Walker BATHTUB Model 1 as the default**
  retention model, with Vollenweider/Larsen-Mercier as the alternative.
  The v0.1.2 description had this inverted, which would have given a
  CRAN reviewer a misleading first impression about model fidelity.

* **`ok_lake_ecoregion()` now has a stable return type.** Always
  returns a data frame, regardless of how many lakes match. The
  `simplify` argument is deprecated (retained for backward compatibility
  with a deprecation warning). Use `$eco_l3_name` to extract the
  ecoregion name. v0.1.2's behavior of returning character on single
  match and data.frame otherwise was a recurring source of brittle
  caller code.

## Minor fixes

* `ok_load()` now emits a warning when `tp_inflow_ugl < 1 ug/L` to
  flag unphysically low TP values that would produce unreliable Chl-a
  and Secchi predictions.

* Removed empty `comment = c(ORCID = "")` from Authors@R in DESCRIPTION.

* Fixed encoding mojibake `(0?1)` -> `(0-1)` in `R/scenarios.R`
  docstring.

* Walker Model 1 retention now documents the implicit units of the
  A1 coefficient (m^3/(mg*yr)) and explicitly notes the defensive
  guard branch is unreachable under normal operation.

* `data-raw/CALIBRATION_README.md` documents a newly discovered
  discrepancy between `ok_ecoregion_assignment.R` and the bundled
  `lake_ecoregion_lookup.csv`. The CSV is treated as authoritative
  because it is what was fed into the calibration regressions; the
  script-CSV reconciliation is queued as a future recalibration task.

# okBATHTUB 0.1.2

This is a metadata-correction release. **Coefficient values are
unchanged.** Only the calibration metadata (sample sizes, ecoregion
fallback flags) has been corrected to match the source-of-truth
calibration report (`data-raw/ok_calibration_report.xlsx`).

## New bundled dataset and lookup helper

* `ok_lake_ecoregions` — a data frame of 214 Oklahoma (and several
  border) lakes with their EPA Level III ecoregion assignment and
  monitoring coverage statistics (number of monitoring stations,
  maximum years of TP/Chl-a/Secchi/TN coverage in the 2000-2024 window).
* `ok_lake_ecoregion()` — a convenience lookup function returning the
  ecoregion name for a given lake. Usable directly with
  `coefficients = "oklahoma"`:

  ```r
  eco <- ok_lake_ecoregion("Tenkiller", exact = FALSE)
  ok_load(inflow_m3yr = 1e9, tp_inflow_ugl = 60,
          coefficients = "oklahoma", ecoregion = eco)
  ```

  Monitoring coverage columns are a 2000-2024 snapshot and are not
  updated automatically; treat them as a useful starting point, not a
  current inventory.

## Calibration metadata corrections

Following an audit against the calibration report XLSX, the following
sample-size values in `.oklahoma_coefficients()` were corrected:

| Fit | v0.1.1 metadata | v0.1.2 (correct) |
|---|---|---|
| Cross Timbers Chl-a | n=181, lakes=40 | **n=169, lakes=36** |
| Cross Timbers Secchi | n=265, lakes=36 | **n=169, lakes=36** |
| Central OK/TX Plains Chl-a | n=37, lakes=10 | **n=24, lakes=10** |
| Central OK/TX Plains Secchi | n=26, lakes=12 | **n=24, lakes=10** |
| Ozark Highlands Chl-a | n=20, lakes=14 | n=20, lakes=14 (unchanged) |

The Cross Timbers Secchi value of n=265 in v0.1.1 (which had exceeded
the statewide pooled n=250 — mathematically impossible if Cross Timbers
were a subset of the pooled fit) was the original trigger that prompted
this audit. The corrected values are consistent with the joint
TP+Chl-a+Secchi filter described in `data-raw/CALIBRATION_README.md`:
the same filtered records feed both regressions, so per-ecoregion n
values are identical across the Chl-a and Secchi fits.

## Calibration documentation added

The full calibration provenance is now bundled in `data-raw/`:

* `CALIBRATION_README.md` — methodology, R^2 threshold rationale,
  ecoregion-by-ecoregion outcomes, known limitations, recalibration
  instructions
* `ok_calibration.R` — the script that fit the regressions
* `ok_calibration_report.xlsx` — full coefficient table, raw ecoregion
  fits, statewide pooled fits, and the calibration dataset
* `ok_calibration_plots/` — diagnostic plots per ecoregion
* `ok_ecoregion_assignment.R` and `lake_ecoregion_lookup.csv` —
  ecoregion assignments for 214 Oklahoma lakes

These materials are not run at install or load time; they document how
the bundled `"oklahoma"` coefficient values were derived.

## Other clarifications

* Ozark Highlands Secchi correctly falls back to statewide pooled
  (R^2=0.228 below 0.25 threshold). v0.1.1 implemented this correctly
  by code path (since Ozark Highlands was never in the per-ecoregion
  Secchi list) but the comment was incomplete. v0.1.2 documents the
  fallback explicitly.

* `.oklahoma_coefficients()` docstring now references the calibration
  README and the source-of-truth XLSX, and clarifies why the per-
  ecoregion n values do not sum to the pooled total (the pooled fit
  includes records from ecoregions whose per-ecoregion fits did not
  meet thresholds).

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
