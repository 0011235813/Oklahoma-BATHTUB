# okBATHTUB Coefficient Calibration — Methodology Notes

**Oklahoma Water Resources Board | Water Quality Division**
**Generated:** See ok_coefficients.R header for calibration date
**Calibration script:** ok_calibration.R

---

## Overview

okBATHTUB uses empirical regression coefficients to predict in-lake
chlorophyll-a from total phosphorus (TP) and Secchi depth from
chlorophyll-a. These relationships are fitted from OWRB Lake Monitoring
Program (LMP) data pulled from AWQMS and are applied per EPA Level III
ecoregion. TP retention uses Walker (1996) Larsen-Mercier defaults
throughout; retention calibration requires paired tributary inflow load
data and is deferred to a future phase.

---

## Regression Equations

**Chlorophyll-a from in-lake TP (log-log OLS):**

    log10(Chl-a) = a + b * log10(TP)

**Secchi depth from chlorophyll-a (log-log OLS):**

    log10(Secchi) = a + b * log10(Chl-a)

Walker (1996) defaults for reference:
- Chl-a: a = -1.136, b = 1.449
- Secchi: a = 0.616, b = -0.473

---

## Calibration Data

- **Source:** OWRB Lake Monitoring Program (AWQMS, ext.results_standard_vw)
- **Period:** 2000–2024
- **Lakes used:** 82
- **Observations:** 250 lake-station-years
- **Season:** Growing season (May–October), surface grab samples only
- **Minimum samples per lake-year:** 3 per parameter
- **Chlorophyll:** Pheophytin-corrected preferred; uncorrected used only
  where corrected is unavailable
- **Ecoregion assignment:** EPA Level III (Griffith et al. 2004),
  hardcoded geographic lookup (ok_ecoregion_assignment_v2.R)

---

## Coefficient Cascade Logic

For each ecoregion and each regression, coefficients are selected using
the following cascade:

1. **Oklahoma ecoregion-specific fit** — used when:
   - n_obs >= 15 AND
   - n_lakes >= 5 AND
   - R² >= 0.25 (minimum acceptable predictive power)

2. **Oklahoma statewide pooled fit** — used when the ecoregion-specific
   fit does not meet all three criteria above

3. **Walker (1996) defaults** — used only if the statewide pooled fit
   also fails (not triggered in current calibration)

---

## R² Minimum Threshold — Scientific Justification

A minimum R² of 0.25 was applied to ecoregion-specific fits. This
threshold was established based on the following reasoning:

An ecoregion-specific regression must demonstrate meaningful predictive
power to be preferred over the statewide pooled model. An R² below 0.25
indicates that less than 25% of chlorophyll-a or Secchi depth variance
is explained by the predictor — in practical terms, the ecoregion-specific
line is adding noise rather than signal compared to the pooled Oklahoma
relationship.

**Arkansas Valley triggered this threshold for both regressions:**

| Regression | Ecoregion R² | Action |
|---|---|---|
| Chl-a from TP (Arkansas Valley) | 0.120 | Rejected → statewide pooled |
| Secchi from Chl-a (Arkansas Valley) | 0.018 | Rejected → statewide pooled |

The Arkansas Valley R² values are consistent with the peer-reviewed
literature on turbid midcontinent reservoirs. Jones & Knowlton (2005,
Lake Reserv. Manage.) and Dzialowski et al. (2011, Lake Reserv. Manage.)
demonstrated that in flood-dominated, turbid Plains reservoirs, the
TP→Chl-a relationship is suppressed by light limitation from inorganic
suspended sediment (non-algal turbidity, NAT). When NAT exceeds
approximately 2.0 m⁻¹, algal biomass per unit phosphorus collapses
because light — not nutrients — controls primary production. Oklahoma's
Arkansas Valley reservoirs (Eufaula, Robert S. Kerr, Webbers Falls,
Wister) are among the most turbid in the state and exhibit exactly this
pattern.

The practical consequence is that predicting chlorophyll-a from TP alone
in Arkansas Valley lakes using an ecoregion-specific regression would
produce less reliable estimates than the statewide pooled model. A future
improvement for these reservoirs is to implement Walker's Model 2 Chl-a
submodel (which includes a non-algal turbidity covariate), requiring
paired Secchi/Chl-a observations to back-calculate NAT per reservoir.

---

## Ecoregion Summary

| Ecoregion | Chl-a Source | Chl-a R² | Secchi Source | Secchi R² |
|---|---|---|---|---|
| Cross Timbers | oklahoma_ecoregion | 0.391 | oklahoma_ecoregion | 0.359 |
| Central OK/TX Plains | oklahoma_ecoregion | 0.614 | oklahoma_ecoregion | 0.394 |
| Ozark Highlands | oklahoma_ecoregion | 0.609 | oklahoma_ecoregion | 0.228 |
| Arkansas Valley | oklahoma_statewide | 0.442 | oklahoma_statewide | 0.364 |
| Ouachita Mountains | oklahoma_statewide | 0.442 | oklahoma_statewide | 0.364 |
| Central Great Plains | oklahoma_statewide | 0.442 | oklahoma_statewide | 0.364 |
| South Central Plains | oklahoma_statewide | 0.442 | oklahoma_statewide | 0.364 |
| Flint Hills | oklahoma_statewide | 0.442 | oklahoma_statewide | 0.364 |

---

## Comparison to Walker (1996)

Oklahoma Chl-a slopes (0.62–0.80) are consistently shallower than
Walker's national slope of 1.449. This is ecologically expected:
Walker's dataset includes many clear glacial lakes where TP→Chl-a is
tightly coupled. Oklahoma's warm, turbid, flood-dominated reservoirs
show a more muted algal response per unit phosphorus — particularly
in the western and Arkansas Valley systems.

---

## Known Limitations

1. **TP retention uses Walker defaults.** Retention calibration requires
   paired tributary grab samples and USGS daily discharge data to compute
   annual TP loads via FLUX-style regression. This work is deferred to a
   future phase of the project.

2. **Arkansas Valley turbidity.** The statewide pooled Chl-a and Secchi
   coefficients applied to Arkansas Valley lakes do not account for the
   suppressive effect of inorganic turbidity. Users should interpret
   chlorophyll-a predictions for highly turbid Arkansas Valley reservoirs
   (Eufaula, RSK, Webbers Falls, Wister) with appropriate caution and
   consider sensitivity analysis across plausible NAT values per Walker
   (1996, p. 4-29).

3. **Ouachita Mountains sample size.** With 10 paired TP+Chl-a
   observations across 6 lakes, the Ouachita ecoregion did not meet the
   minimum threshold for an independent regression. The statewide pooled
   model is applied. More LMP data from McCurtain, Pushmataha, and Le
   Flore county lakes would strengthen this ecoregion in future
   recalibrations.

4. **Central Great Plains thin data.** Only 1 observation met all quality
   filters (Canton Lake). Fort Supply, Lake Etling, and other western
   lakes likely have sparse growing-season surface grab sample coverage
   relative to the min_samples = 3 threshold.

5. **Temporal scope.** Calibration covers 2000–2024. Oklahoma experienced
   exceptional drought in 2011–2013 which may have altered trophic
   dynamics during those years. A future sensitivity analysis stratifying
   by wet/dry periods is recommended.

---

## Recalibration

To recalibrate with updated data, run ok_calibration.R in an R session
where all_lakes is already loaded from ok_from_awqms(). The script will
regenerate ok_coefficients.R, ok_calibration_report.xlsx, and all
diagnostic plots automatically.

Suggested recalibration trigger: when 5 or more new lake-years of LMP
data have been added to AWQMS since the last calibration date shown in
ok_coefficients.R.

---

## References

- Walker, W.W. (1996). Simplified Procedures for Eutrophication
  Assessment and Prediction: User Manual. U.S. Army Corps of Engineers,
  Instruction Report W-96-2.
- Jones, J.R. and Knowlton, M.F. (2005). Chlorophyll response to
  nutrients and non-algal seston in Missouri reservoirs and oxbow lakes.
  Lake and Reservoir Management 21(3): 361–371.
- Dzialowski, A.R. et al. (2011). Effects of non-algal turbidity on
  cyanobacterial biomass in seven turbid Kansas reservoirs. Lake and
  Reservoir Management 27(1): 6–14.
- Jones, J.R. and Hubbart, J.A. (2011). Empirical estimation of
  non-chlorophyll light attenuation in Missouri reservoirs. Lake and
  Reservoir Management 27(2): 103–107.
- Griffith, G.E. et al. (2004). Ecoregions of Oklahoma. U.S. Geological
  Survey, Reston, Virginia.
