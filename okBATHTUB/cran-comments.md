## Resubmission notes (v0.1.3)

Pre-CRAN forensic review release addressing 14 findings from a v0.1.2
review. `R CMD check --as-cran` passes cleanly (0 errors, 0 warnings,
0 notes) on Windows 11 / R 4.5.3.

Highlights:
- `ok_reservoirs` and `ok_lake_ecoregions` ecoregion assignments now
  cross-validate (cross-dataset regression test added).
- DESCRIPTION reframed: Walker BATHTUB Model 1 (the default) is now
  identified as such; Vollenweider/Larsen-Mercier is described as the
  alternative.
- `ok_lake_ecoregion()` return type is now stable (always data frame);
  the `simplify` argument is deprecated.
- Stray institutional attribution strings removed from a source-file
  header and a vignette reference list to bring the package fully in
  line with its personal-capacity, MIT-licensed authorship.

See NEWS.md for the complete list.

## Background

v0.1.0 was withdrawn from CRAN consideration after an internal forensic
review identified seven scientific and CRAN-policy issues; v0.1.1 fixed
those; v0.1.2 corrected calibration metadata to match the source-of-
truth XLSX; v0.1.3 addresses remaining items from a final pre-submission
review.

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
