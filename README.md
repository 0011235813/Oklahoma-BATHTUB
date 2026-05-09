# okBATHTUB

## Overview

**okBATHTUB** is an R implementation of Walker's (1996) steady-state empirical
reservoir eutrophication model (BATHTUB), extended with Oklahoma-specific
regression coefficients calibrated from the Oklahoma Water Resources Board
(OWRB) Lake Monitoring Program (LMP). The package predicts in-lake total
phosphorus (TP), total nitrogen (TN), chlorophyll-*a*, and Secchi depth from
tributary hydraulic and nutrient loading inputs, and computes Carlson (1977)
Trophic State Indices (TSI) from those predictions.

okBATHTUB is designed to complement watershed-scale loading models (e.g.,
OK-HAWQS/SWAT) in a two-model nutrient management workflow: the watershed model
estimates tributary loads under baseline and conservation practice scenarios;
okBATHTUB translates those loads into predicted in-lake water quality responses.
This approach directly supports Oklahoma's Sensitive Water Supply (SWS) reservoir
protection program and 604(b) water quality planning grants.

---

## Scientific Basis

### Core Model

The BATHTUB framework (Walker 1996) is a steady-state mass-balance model for
reservoir eutrophication. For a single well-mixed segment, the in-lake TP
concentration is:

```
C_lake = C_in × (1 - R_TP)
```

where *C_in* is the flow-weighted mean inflow TP concentration (µg/L) and
*R_TP* is the TP retention coefficient, estimated using the Larsen-Mercier
(1976) hydraulic residence time model:

```
R_TP = 1 / (1 + τ^(-0.5))
```

where *τ* is hydraulic residence time (years). TN retention uses the
Vollenweider settling-velocity formulation with Walker's default apparent
settling velocity of 10 m/yr.

### Oklahoma-Calibrated Coefficients

Chlorophyll-*a* and Secchi depth are predicted from in-lake TP and
chlorophyll-*a*, respectively, using log-log ordinary least squares regression:

```
log10(Chl-a)  = a + b × log10(TP_lake)
log10(Secchi) = a + b × log10(Chl-a)
```

Coefficients are calibrated from OWRB LMP grab sample data (2000–2024, 82
lakes, ~250 lake-station-year observations; growing season May–October, surface
samples only, minimum 3 samples per parameter per lake-year). Ecoregion-specific
fits are applied where data are sufficient (n ≥ 15 obs, n ≥ 5 lakes, R² ≥ 0.25);
otherwise the package falls back to a statewide pooled model.

| Ecoregion | Chl-*a* source | Chl-*a* R² | Secchi source | Secchi R² |
|---|---|---|---|---|
| Cross Timbers | Oklahoma ecoregion | 0.391 | Oklahoma ecoregion | 0.359 |
| Central OK/TX Plains | Oklahoma ecoregion | 0.614 | Oklahoma ecoregion | 0.394 |
| Ozark Highlands | Oklahoma ecoregion | 0.609 | Oklahoma statewide | 0.364 |
| Arkansas Valley | Oklahoma statewide | 0.442 | Oklahoma statewide | 0.364 |
| All others | Oklahoma statewide | 0.442 | Oklahoma statewide | 0.364 |

Oklahoma Chl-*a* slopes (0.62–0.80) are consistently shallower than Walker's
nationally-derived slope of 1.449, consistent with the peer-reviewed literature
on warm, turbid, flood-dominated mid-continent reservoirs where non-algal
turbidity suppresses the TP–Chl-*a* relationship (Jones & Knowlton 2005;
Dzialowski et al. 2011).

---

## Installation

```r
# From CRAN (once accepted):
install.packages("okBATHTUB")

# Development version from GitHub:
# install.packages("remotes")
remotes::install_github("0011235813/Oklahoma-BATHTUB")
```

---

## Core Pipeline

The standard single-segment workflow pipes five functions in sequence:

```r
library(okBATHTUB)

result <- ok_load(
    inflow_m3yr   = 45e6,        # annual inflow volume (m³/yr)
    tp_inflow_ugl = 120,         # flow-weighted mean inflow TP (µg/L)
    tn_inflow_ugl = 1800,        # flow-weighted mean inflow TN (µg/L)
    coefficients  = "oklahoma",  # use Oklahoma LMP-calibrated coefficients
    ecoregion     = "Cross Timbers"
  ) |>
  ok_hydraulics(
    surface_area_ha = 890,       # reservoir surface area at normal pool (ha)
    mean_depth_m    = 4.2        # mean depth at normal pool (m)
  ) |>
  ok_retention()  |>            # estimates TP and TN retention coefficients
  ok_inlake()     |>            # predicts in-lake TP, TN, Chl-a, Secchi
  ok_tsi()                      # computes Carlson TSI and trophic state

summary(result)
```

```
========================================
  okBATHTUB Water Quality Summary
========================================

  Segment      : main
  Coefficients : oklahoma
  Pipeline     : tsi

  -- Hydraulics --
  Inflow           : 4.500e+07 m3/yr
  Surface area     : 890.0 ha
  Mean depth       : 4.20 m
  Residence time   : 0.831 yr
  Areal water load : 5.06 m/yr

  -- Nutrient Retention --
  TP retention     : 0.477
  TN retention     : 0.664

  -- In-Lake Predictions --
  TP               : 62.8 ug/L
  TN               : 604.5 ug/L
  Chlorophyll-a    : 24.65 ug/L
  Secchi depth     : 0.51 m

  -- Carlson Trophic State Index --
  TSI(TP)          : 63.8
  TSI(Chl-a)       : 62.5
  TSI(Secchi)      : 69.2
  TSI(mean)        : 65.2
  Trophic state    : Eutrophic

========================================
```

---

## Key Features

### Load Reduction Scenario Analysis

```r
baseline <- ok_load(
    inflow_m3yr   = 45e6,
    tp_inflow_ugl = 120,
    coefficients  = "oklahoma",
    ecoregion     = "Cross Timbers"
  ) |>
  ok_hydraulics(surface_area_ha = 890, mean_depth_m = 4.2)

# Sweep TP reductions from 5% to 70% in 5% steps
sweep <- ok_scenario_sweep(baseline, target_class = "mesotrophic")
print(sweep[, c("scenario", "tp_inflow_ugl", "tsi_mean",
                "trophic_state", "meets_target")])
```

### Multi-Tributary Loading

```r
tribs <- data.frame(
  inflow_m3yr   = c(30e6, 15e6),
  tp_inflow_ugl = c(100,  160),
  tn_inflow_ugl = c(1500, 2400)
)
result <- ok_load_multi(tribs) |>
  ok_hydraulics(surface_area_ha = 890, mean_depth_m = 4.2) |>
  ok_retention() |> ok_inlake() |> ok_tsi()
```

### Multi-Segment Reservoirs

```r
# Riverine zone
riverine <- ok_load(
    inflow_m3yr   = 45e6,
    tp_inflow_ugl = 150,
    segment_label = "riverine"
  ) |>
  ok_hydraulics(surface_area_ha = 280, mean_depth_m = 3.1) |>
  ok_retention() |> ok_inlake() |> ok_tsi()

# Lacustrine zone — receives riverine outflow as inflow
lacustrine <- ok_segment(riverine, segment_label = "lacustrine") |>
  ok_hydraulics(surface_area_ha = 610, mean_depth_m = 5.8) |>
  ok_retention() |> ok_inlake() |> ok_tsi()
```

### Observed TSI from Monitoring Data

```r
# Compute Carlson TSI directly from AWQMS or LMP grab sample data
ok_tsi_observed(tp_ugl = 85, chla_ugl = 22, secchi_m = 0.8)
```

### Built-in Oklahoma Reservoir Dataset

```r
# Morphometry for 123 OWRB LMP reservoirs
ok_reservoir("Arcadia Lake")
ok_reservoir(ecoregion = "Cross Timbers", data_quality = "A")
head(ok_reservoirs)
```

---

## Built-in Data

`ok_reservoirs` — morphometric and geographic characteristics for 123 Oklahoma
LMP reservoirs, sourced from USACE Tulsa District design memoranda, OWRB
bathymetric surveys, BOR/USBR design data, and the National Inventory of Dams.
Data quality codes: **A** = measured/design data; **B** = estimated via Oklahoma
regional depth regression (log(mean depth) = 0.28 × log(area_ha) − 0.34).

---

## Function Reference

| Function | Description |
|---|---|
| `ok_load()` | Assemble and validate tributary loading inputs |
| `ok_load_multi()` | Flow-weighted aggregation of multiple tributaries |
| `ok_hydraulics()` | Compute HRT and areal water load from morphometry |
| `ok_retention()` | Estimate TP and TN retention coefficients |
| `ok_inlake()` | Predict in-lake TP, TN, Chl-*a*, and Secchi depth |
| `ok_tsi()` | Compute Carlson TSI and trophic state classification |
| `ok_tsi_observed()` | Carlson TSI from observed grab sample data |
| `ok_scenario()` | Run named load reduction scenarios |
| `ok_scenario_sweep()` | Sweep TP reductions across a range |
| `ok_segment()` | Chain two reservoir segments in series |
| `ok_segment_chain()` | Chain multiple segments from a list |
| `ok_segment_summary()` | Summarise a segment chain as a data frame |
| `ok_reservoir()` | Look up reservoir morphometry from `ok_reservoirs` |
| `ok_from_awqms()` | Pull and format AWQMS data for calibration (OWRB) |
| `ok_plot_response()` | Load-response curve (TP → Chl-*a* or TSI) |
| `ok_plot_scenario()` | Scenario comparison chart |
| `ok_plot_segments()` | Longitudinal segment profile |
| `ok_plot_tsi()` | Carlson TSI deviation diagram |

---

## References

Carlson, R.E. (1977). A trophic state index for lakes. *Limnology and
Oceanography*, 22(2), 361–369.
<https://doi.org/10.4319/lo.1977.22.2.0361>

Dzialowski, A.R., Smith, V.H., Huggins, D.G., deNoyelles, F., Lim, N.C.,
Baker, D.S., and Beury, J.H. (2011). Effects of non-algal turbidity on
cyanobacterial biomass in seven turbid Kansas reservoirs. *Lake and Reservoir
Management*, 27(1), 6–14.
<https://doi.org/10.1080/07438141.2011.551027>

Jones, J.R. and Knowlton, M.F. (2005). Chlorophyll response to nutrients and
non-algal seston in Missouri reservoirs and oxbow lakes. *Lake and Reservoir
Management*, 21(3), 361–371.
<https://doi.org/10.1080/07438140509354439>

Larsen, D.P. and Mercier, H.T. (1976). Phosphorus retention capacity of lakes.
*Journal of the Fisheries Research Board of Canada*, 33(8), 1742–1750.
<https://doi.org/10.1139/f76-221>

Walker, W.W. Jr. (1996). *Simplified Procedures for Eutrophication Assessment
and Prediction: User Manual*. Instruction Report W-96-2. U.S. Army Engineer
Waterways Experiment Station, Vicksburg, MS.

---

## Citation

```r
citation("okBATHTUB")
```

```
Henderson, J. (2026). okBATHTUB: Reservoir Eutrophication Modeling for
Oklahoma Lakes. R package version 0.1.0.
https://github.com/0011235813/Oklahoma-BATHTUB
```

---

## License

MIT © Jordon Henderson

---

## Development

okBATHTUB is an independent open-source contribution to the water quality
modeling community, developed by Jordon Henderson in a personal capacity.
The calibration data underlying the Oklahoma-specific coefficients derives
from the OWRB Lake Monitoring Program, which is publicly available.

Coefficient recalibration is recommended when five or more new lake-years
of LMP data have been added to AWQMS since the calibration date recorded in
`data-raw/ok_coefficients.R`.

Bug reports and feature requests:
<https://github.com/0011235813/Oklahoma-BATHTUB/issues>
