## Resubmission notes (v0.1.2)

This is a metadata-correction release of okBATHTUB. Coefficient values
are unchanged from v0.1.1; only the calibration metadata (sample sizes
and ecoregion-fallback flags) has been corrected to match the source-of-
truth calibration report (`data-raw/ok_calibration_report.xlsx`). See
NEWS.md for the full diff. The calibration provenance trail
(`CALIBRATION_README.md`, the calibration script, the report XLSX, and
diagnostic plots) is now bundled in `data-raw/` (build-ignored, so not
in the installed tarball).

A new test file `test-calibration-metadata.R` pins the metadata to the
XLSX values to prevent future silent drift.

## v0.1.1 background

v0.1.0 was withdrawn from CRAN consideration after a forensic review
identified seven issues (scientific accuracy, empty bundled dataset,
hardcoded private infrastructure, and authorship inconsistency). All
seven were addressed in v0.1.1. v0.1.2 is a follow-up correction to
calibration metadata only.

## Test environments

* (To be filled in by the maintainer after running on local environments.)
  Suggested:
  - Local Windows 11, R 4.4.x
  - GitHub Actions: ubuntu-latest (release), windows-latest (release),
    macOS-latest (release), ubuntu-latest (devel)
  - win-builder: R-release, R-devel
  - R-hub: ubuntu-clang, fedora-clang-devel

## R CMD check results

* 0 errors | 0 warnings | 0 notes (expected after running
  `devtools::check()` locally).

## Downstream dependencies

None. This is a new package.

## Notes for CRAN reviewers

* The `data/ok_reservoirs.rda` file ships in pyreadr-written
  uncompressed format. The maintainer will run
  `tools::resaveRdaFiles("data/")` before submission to gzip-compress
  it per CRAN policy. Alternatively, the dataset can be rebuilt from
  `data-raw/ok_reservoirs.csv` with usethis-default bzip2 compression.

* The package provides three retention coefficient sets (`"walker"`,
  `"vollenweider"`, `"oklahoma"`). All are documented with primary
  literature citations. The Oklahoma coefficients are calibrated from
  publicly available state lake monitoring data.

* No external services or APIs are required at install or load time.
  The package is self-contained.
