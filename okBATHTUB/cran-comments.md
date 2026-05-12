## Submission notes (v0.1.7)

First-time CRAN submission. Pre-submission win-builder R-release run on
v0.1.4 returned 1 NOTE with four items. Three have been addressed
across v0.1.5-v0.1.7; the remaining one is the expected "new submission"
tag plus the misspelled-words item, which are not actionable on the
package side.

## Changes since the v0.1.4 win-builder check

- v0.1.5: updated `vignettes/hawqs-linkage.Rmd` EPA URL from the
  deprecated `epa.gov/cwsrf/clean-water-state-revolving-fund-cwsrf`
  path to the current canonical
  `epa.gov/nps/water-quality-management-planning-grants`.
- v0.1.6: re-enabled `URL:` and `BugReports:` in `DESCRIPTION` after
  the GitHub repository was made public.
- v0.1.7: corrected the GitHub URL. The repository is at
  `0011235813/Oklahoma-BATHTUB` (capital O, hyphenated; the package
  source lives in the `okBATHTUB/` subdirectory). Added the pkgdown
  documentation site as the primary URL.

## Test environments

* Local: Windows 11, R 4.5.3 - 0 errors, 0 warnings, 0 notes
* win-builder R-release (R 4.6.0) on v0.1.4 - 0 errors, 0 warnings,
  1 NOTE (URL/spelling items addressed in v0.1.5/v0.1.6)

## R CMD check results

Expected: 0 errors | 0 warnings | 1 NOTE (new-submission tag plus
the spelling words noted below).

## Notes for CRAN reviewers

This is a first-time submission. The remaining NOTE items are:

1. **"Possibly misspelled words: HAWQS, Mercier, Secchi, Trophic,
   Vollenweider."** All five are intentional and correct:
   - *Vollenweider* (Richard Vollenweider) and *Mercier* (Henri
     Mercier) are scientist surnames cited in the package's primary
     references for lake eutrophication modelling.
   - *Secchi* is from Pietro Angelo Secchi, the 19th-century Italian
     astronomer whose name labels the standard water-clarity
     measurement.
   - *HAWQS* is the U.S. EPA Hydrologic and Water Quality System
     watershed modelling platform.
   - *Trophic* is a standard ecology term (oligotrophic, eutrophic,
     hypereutrophic).
   These words are also listed in `inst/WORDLIST` for the `spelling`
   package's checks.

2. **"New submission"**: expected and not actionable for a first-time
   package.

## Package overview

okBATHTUB implements empirical reservoir eutrophication modelling using
Walker's BATHTUB Model 1 (second-order available-phosphorus
sedimentation; Walker 1985, 1996) as the default retention model. The
Vollenweider (1976) / Larsen-Mercier (1976) hydraulic-residence form is
available as an alternative. The package provides three retention
coefficient sets (`"walker"`, `"vollenweider"`, `"oklahoma"`). All are
documented with primary literature citations. The Oklahoma coefficients
are calibrated from publicly available state lake monitoring data; the
full calibration provenance is bundled in `data-raw/` (build-ignored).

No external services or APIs are required at install or load time.
The package is self-contained.

## Downstream dependencies

None. This is a new package.

## Version history

- v0.1.0: initial package construction (not submitted).
- v0.1.1: internal forensic review fixed seven scientific and CRAN-policy issues.
- v0.1.2: calibration metadata corrected to match source-of-truth XLSX.
- v0.1.3: addressed 14 findings from a pre-submission review.
- v0.1.4: R version compatibility, namespace cleanup, vignette labeling
  consistency, strengthened cross-dataset consistency test.
- v0.1.5: updated deprecated EPA URL.
- v0.1.6: re-enabled `URL:`/`BugReports:` after public GitHub release.
- v0.1.7: corrected GitHub repo URL (`Oklahoma-BATHTUB`, not
  `okBATHTUB`); added pkgdown site as primary URL.
