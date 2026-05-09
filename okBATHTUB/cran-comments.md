# CRAN Submission Comments — okBATHTUB 0.1.0

## Test environments

- Local: Windows 11 x64, R 4.5.3
- win-builder: R-devel, R-release
- rhub: windows-x86_64-release, ubuntu-gcc-release, macos-highsierra-release

## R CMD check results

0 errors | 0 warnings | 0 notes

## Notes on suggested packages

The following packages are in `Suggests` and are only used in the
`ok_from_awqms()` function (database connection) and `ok_plot_*()` functions
(visualization). All examples using these packages are wrapped in
`\dontrun{}`. The package installs and all core functions work without them.

- `ggplot2` — visualization functions only
- `DBI`, `odbc`, `keyring` — AWQMS database connection only (OWRB-internal)
- `openxlsx` — optional Excel export

## First submission

This is the first submission of okBATHTUB to CRAN.

The package implements Walker's (1996) steady-state BATHTUB reservoir
eutrophication model with Oklahoma-specific calibration from the OWRB
Lake Monitoring Program. It is designed for water quality practitioners
and researchers modelling nutrient loading in Oklahoma reservoirs.

## Package context

- Developed by the Oklahoma Water Resources Board, Water Quality Division
- Complements watershed-scale models (SWAT/OK-HAWQS) in a two-model
  nutrient management workflow
- No reverse dependencies (new package)
