# okBATHTUB Oklahoma Calibration — Methodology Notes

**Calibration script:** `ok_calibration.R`
**Source of truth for coefficient values:** `ok_calibration_report.xlsx`

---

## Overview

okBATHTUB's `"oklahoma"` coefficient set uses empirical regression
coefficients to predict in-lake chlorophyll-a from total phosphorus
(TP) and Secchi depth from chlorophyll-a. These relationships are
fitted from publicly available Oklahoma lake monitoring data and are
applied per EPA Level III ecoregion. TP and TN retention use Walker
BATHTUB Model 1 (second-order available-P sedimentation) throughout;
retention calibration would require paired tributary inflow load data
and is out of scope for the empirical Chl-a / Secchi regressions
documented here.

---

## Regression equations

**Chlorophyll-a from in-lake TP (log-log OLS):**

    log10(Chl-a) = a + b * log10(TP)

**Secchi depth from chlorophyll-a (log-log OLS):**

    log10(Secchi) = a + b * log10(Chl-a)

Walker (1985, 1996) national reference values for comparison:
- Chl-a: a = -1.136, b = 1.449
- Secchi: a = 0.616, b = -0.473

---

## Calibration data

- **Source:** Publicly available Oklahoma lake monitoring data
  (Beneficial Use Monitoring Program / Lake Monitoring Program records,
  retrieved from AWQMS)
- **Period:** 2000-2024
- **Lakes used (statewide pooled):** 82
- **Observations (statewide pooled):** 250 lake-station-years
- **Aggregation unit:** lake-station-year (each row is the arithmetic
  mean of growing-season surface grab samples at one monitoring station
  for one calendar year)
- **Season:** Growing season (May-October), surface grab samples only
- **Minimum samples per lake-station-year:** 3 per parameter
- **Joint filter:** Records must have valid TP, chlorophyll-a, AND
  Secchi values to be included in either regression. Consequently, the
  per-ecoregion sample sizes are identical across the Chl-a and Secchi
  fits.
- **Chlorophyll:** Pheophytin-corrected preferred; uncorrected used
  only where corrected is unavailable
- **Ecoregion assignment:** EPA Level III (Griffith et al. 2004)

---

## Coefficient cascade logic

For each ecoregion and each regression, coefficients are selected using
the following cascade:

1. **Oklahoma ecoregion-specific fit** — used when:
   - n_obs >= 15 AND
   - n_lakes >= 5 AND
   - R^2 >= 0.25 (minimum acceptable predictive power)

2. **Oklahoma statewide pooled fit** — used when the ecoregion-specific
   fit does not meet all three criteria above

3. **Walker (1985) defaults** — used only if the statewide pooled fit
   also fails (not triggered in current calibration)

---

## R^2 minimum threshold — scientific justification

A minimum R^2 of 0.25 was applied to ecoregion-specific fits. The
reasoning: an ecoregion-specific regression must demonstrate meaningful
predictive power to be preferred over the statewide pooled model. An
R^2 below 0.25 indicates that less than 25% of chlorophyll-a or Secchi
depth variance is explained by the predictor — in practical terms, the
ecoregion-specific line is adding noise rather than signal compared to
the pooled Oklahoma relationship.

**Two ecoregions triggered this threshold:**

| Regression | Ecoregion R^2 | Action |
|---|---|---|
| Chl-a from TP (Arkansas Valley) | 0.120 | Rejected -> statewide pooled |
| Secchi from Chl-a (Arkansas Valley) | 0.018 | Rejected -> statewide pooled |
| Secchi from Chl-a (Ozark Highlands) | 0.228 | Rejected -> statewide pooled |

The Arkansas Valley R^2 values are consistent with the peer-reviewed
literature on turbid midcontinent reservoirs. Jones & Knowlton (2005,
Lake Reserv. Manage.) and Dzialowski et al. (2011, Lake Reserv. Manage.)
demonstrated that in flood-dominated, turbid Plains reservoirs, the
TP -> Chl-a relationship is suppressed by light limitation from inorganic
suspended sediment (non-algal turbidity, NAT). When NAT exceeds
approximately 2.0 m^-1, algal biomass per unit phosphorus collapses
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

## Final coefficient table

Source of truth: `ok_calibration_report.xlsx`, sheet `Final_Coefficients`.

| Ecoregion | Chl-a a | Chl-a b | Chl-a source | Chl-a R^2 | Chl-a n / lakes |
|---|---|---|---|---|---|
| Cross Timbers | 0.2823 | 0.6171 | oklahoma_ecoregion | 0.391 | 169 / 36 |
| Central OK/TX Plains | 0.0485 | 0.7462 | oklahoma_ecoregion | 0.614 | 24 / 10 |
| Ozark Highlands | -0.1684 | 0.8021 | oklahoma_ecoregion | 0.609 | 20 / 14 |
| Arkansas Valley | 0.1505 | 0.6715 | oklahoma_statewide | 0.442 | 250 / 82 |
| Ouachita Mountains | 0.1505 | 0.6715 | oklahoma_statewide | 0.442 | 250 / 82 |
| Central Great Plains | 0.1505 | 0.6715 | oklahoma_statewide | 0.442 | 250 / 82 |
| South Central Plains | 0.1505 | 0.6715 | oklahoma_statewide | 0.442 | 250 / 82 |
| Flint Hills | 0.1505 | 0.6715 | oklahoma_statewide | 0.442 | 250 / 82 |

| Ecoregion | Secchi a | Secchi b | Secchi source | Secchi R^2 | Secchi n / lakes |
|---|---|---|---|---|---|
| Cross Timbers | 0.4334 | -0.5235 | oklahoma_ecoregion | 0.359 | 169 / 36 |
| Central OK/TX Plains | 0.6489 | -0.5743 | oklahoma_ecoregion | 0.394 | 24 / 10 |
| Ozark Highlands | 0.4730 | -0.5330 | oklahoma_statewide | 0.364 | 250 / 82 |
| Arkansas Valley | 0.4730 | -0.5330 | oklahoma_statewide | 0.364 | 250 / 82 |
| Ouachita Mountains | 0.4730 | -0.5330 | oklahoma_statewide | 0.364 | 250 / 82 |
| Central Great Plains | 0.4730 | -0.5330 | oklahoma_statewide | 0.364 | 250 / 82 |
| South Central Plains | 0.4730 | -0.5330 | oklahoma_statewide | 0.364 | 250 / 82 |
| Flint Hills | 0.4730 | -0.5330 | oklahoma_statewide | 0.364 | 250 / 82 |

Note: The statewide pooled fit (n=250, lakes=82) includes records from
all ecoregions, including those whose per-ecoregion fits were rejected
(Ouachita Mountains, Central Great Plains, Arkansas Valley Chl-a, etc.).
This is why the pooled total exceeds the sum of the ecoregion-specific
fit sample sizes.

---

## Comparison to Walker (1985, 1996)

Oklahoma Chl-a slopes (0.62-0.80) are consistently shallower than
Walker's national slope of 1.449. This is ecologically expected:
Walker's dataset includes many clear glacial lakes where TP -> Chl-a is
tightly coupled. Oklahoma's warm, turbid, flood-dominated reservoirs
show a more muted algal response per unit phosphorus — particularly
in the western and Arkansas Valley systems.

---

## Known limitations

1. **Retention uses Walker defaults.** Retention calibration would
   require paired tributary grab samples and USGS daily discharge data
   to compute annual TP loads via FLUX-style regression. This is out of
   scope for this empirical Chl-a / Secchi calibration.

2. **Arkansas Valley turbidity.** The statewide pooled Chl-a and Secchi
   coefficients applied to Arkansas Valley lakes do not account for the
   suppressive effect of inorganic turbidity. Users should interpret
   chlorophyll-a predictions for highly turbid Arkansas Valley reservoirs
   (Eufaula, Robert S. Kerr, Webbers Falls, Wister) with appropriate
   caution and consider sensitivity analysis across plausible NAT values
   per Walker (1996, p. 4-29).

3. **Ouachita Mountains sample size.** With 10 paired TP+Chl-a
   observations across 6 lakes, the Ouachita ecoregion did not meet the
   minimum threshold for an independent regression. The statewide pooled
   model is applied. More monitoring data from McCurtain, Pushmataha,
   and Le Flore county lakes would strengthen this ecoregion in future
   recalibrations.

4. **Central Great Plains thin data.** Only 1 observation met all
   quality filters (Canton Lake). Fort Supply, Lake Etling, and other
   western lakes likely have sparse growing-season surface grab sample
   coverage relative to the min_samples = 3 threshold.

5. **Temporal scope.** Calibration covers 2000-2024. Oklahoma
   experienced exceptional drought in 2011-2013 which may have altered
   trophic dynamics during those years. A future sensitivity analysis
   stratifying by wet/dry periods is recommended.

---

## Recalibration

To recalibrate with updated data, the user must:

1. Pull updated lake monitoring data into a data frame `all_lakes`
   with the columns expected by `ok_calibration.R`:
   `lake_name`, `monitoring_location_id`, `eco_l3_name`, `eco_l3_code`,
   `sample_year`, `tp_ugl`, `tp_n`, `chla_ugl`, `chla_n`,
   `chla_corrected`, `secchi_m`, `secchi_n`.
2. Source `ok_calibration.R` in the same R session.
3. The script will regenerate `ok_calibration_report.xlsx` and all
   diagnostic plots. The R coefficient values can then be transcribed
   into `.oklahoma_coefficients()` in `R/okBATHTUB-package.R`.

This package does not bundle a data pull connector. Users with access
to AWQMS, BUMP, or other lake monitoring data sources should use their
preferred database tooling.

---

## References

- Walker, W.W. (1985). Empirical methods for predicting eutrophication
  in impoundments; Report 3, Phase III: Model refinements. Technical
  Report E-81-9, U.S. Army Engineer Waterways Experiment Station.
- Walker, W.W. (1996). Simplified Procedures for Eutrophication
  Assessment and Prediction: User Manual. U.S. Army Corps of Engineers,
  Instruction Report W-96-2.
- Jones, J.R. and Knowlton, M.F. (2005). Chlorophyll response to
  nutrients and non-algal seston in Missouri reservoirs and oxbow lakes.
  Lake and Reservoir Management 21(3): 361-371.
- Dzialowski, A.R. et al. (2011). Effects of non-algal turbidity on
  cyanobacterial biomass in seven turbid Kansas reservoirs. Lake and
  Reservoir Management 27(1): 6-14.
- Jones, J.R. and Hubbart, J.A. (2011). Empirical estimation of
  non-chlorophyll light attenuation in Missouri reservoirs. Lake and
  Reservoir Management 27(2): 103-107.
- Griffith, G.E. et al. (2004). Ecoregions of Oklahoma. U.S. Geological
  Survey, Reston, Virginia.
