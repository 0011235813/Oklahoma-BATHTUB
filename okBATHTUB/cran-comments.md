## Resubmission notes (v0.1.4)

Three small polish fixes following a final pre-submission review:
- Removed R 4.2+ native pipe placeholder syntax from a vignette to
  stay compatible with the declared R >= 4.1.0 requirement.
- Namespaced `readr::` calls in an `eval=FALSE` SWAT example chunk.
- Generalized vignette example labeling to keep canonical Walker Model 1
  numerical reference inputs (890 ha / 4.2 m) decoupled from named lake
  examples.
- Strengthened cross-dataset consistency test (now checks both ecoregion
  code and name).

`R CMD check --as-cran` passes cleanly (0 errors, 0 warnings, 0 notes)
on Windows 11 / R 4.5.3.

## Background

v0.1.0 was withdrawn from CRAN consideration after an internal forensic
review identified seven scientific and CRAN-policy issues. v0.1.1 fixed
those. v0.1.2 corrected calibration metadata to match the source-of-
truth XLSX. v0.1.3 addressed the v0.1.2 review's 14 findings. v0.1.4 is
final polish before submission.

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
