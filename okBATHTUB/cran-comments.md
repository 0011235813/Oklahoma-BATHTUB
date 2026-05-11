## Resubmission notes

This is a remediation release of okBATHTUB. v0.1.0 was withdrawn from
CRAN consideration after a forensic review identified seven issues
(scientific accuracy, empty bundled dataset, hardcoded private
infrastructure, and authorship inconsistency). All seven have been
addressed in v0.1.1. See NEWS.md for details.

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
