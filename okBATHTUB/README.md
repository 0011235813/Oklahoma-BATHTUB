# okBATHTUB

<!-- badges: start -->
<!-- badges: end -->

Empirical reservoir eutrophication modelling in R.

`okBATHTUB` implements steady-state water quality predictions for
reservoirs using the **Walker BATHTUB Model 1** (second-order
available-phosphorus sedimentation) as the default retention model,
with the simpler Vollenweider (1976) / Larsen-Mercier (1976) form
available as an alternative. The package predicts in-lake total
phosphorus, total nitrogen, chlorophyll-a, and Secchi depth from
tributary nutrient and hydraulic loading inputs, and computes
Carlson (1977) Trophic State Indices.

Optional Oklahoma-specific chlorophyll-a and Secchi regression
coefficients are bundled, calibrated from publicly available state lake
monitoring data, but the package is fully usable for reservoirs anywhere.

`okBATHTUB` is designed to complement watershed loading models such as
SWAT and HAWQS in a two-model nutrient management workflow.

## Installation

```r
# install.packages("remotes")
remotes::install_github("0011235813/okBATHTUB")
```

## Quick start

```r
library(okBATHTUB)

result <- ok_load(
    inflow_m3yr   = 45e6,      # tributary inflow
    tp_inflow_ugl = 120,       # flow-weighted mean TP
    tn_inflow_ugl = 1800       # flow-weighted mean TN
  ) |>
  ok_hydraulics(
    surface_area_ha = 890,     # normal pool surface area
    mean_depth_m    = 4.2      # mean depth at normal pool
  ) |>
  ok_retention() |>            # Walker BATHTUB Model 1
  ok_inlake()    |>            # Mass balance + Chl-a + Secchi
  ok_tsi()                     # Carlson Trophic State Index

summary(result)
```

## Three retention model families

| Set            | Retention                       | Chl-a / Secchi             |
|----------------|---------------------------------|----------------------------|
| `"walker"` (default) | Walker BATHTUB Model 1     | Walker (1985) national     |
| `"vollenweider"`     | Vollenweider/Larsen-Mercier (= BATHTUB Model 5) | Walker (1985) national |
| `"oklahoma"`         | Walker BATHTUB Model 1     | Oklahoma ecoregion-specific|

The default is Walker Model 1 because it is the canonical default of
the BATHTUB program and is calibrated to U.S. Army Corps of Engineers
reservoir data, which matches the management context for most U.S.
reservoirs. Vollenweider / Larsen-Mercier is provided for users who
want a parsimonious single-parameter retention model when
ortho-P / total-P partitioning is unknown.

## Pipeline functions

| Function                | Purpose                                              |
|-------------------------|------------------------------------------------------|
| `ok_load()`             | Assemble tributary load inputs                        |
| `ok_load_multi()`       | Aggregate multiple tributaries automatically          |
| `ok_hydraulics()`       | Add reservoir morphometry; compute residence time     |
| `ok_retention()`        | TP / TN retention coefficients                        |
| `ok_inlake()`           | Mass balance, chlorophyll-a, Secchi depth             |
| `ok_tsi()`              | Carlson Trophic State Indices                         |
| `ok_segment()` /        | Multi-segment reservoir modelling                     |
| `ok_segment_chain()`    |                                                      |
| `ok_scenario()` /       | Load reduction scenario analysis                      |
| `ok_scenario_sweep()`   |                                                      |
| `ok_reservoir()` /      | Look up Oklahoma reservoir morphometry                |
| `ok_reservoirs`         | Bundled dataset (40 reservoirs, 7 ecoregions)         |
| `ok_plot_response()` /  | Visualizations (require `ggplot2`)                    |
| `ok_plot_scenario()` /  |                                                      |
| `ok_plot_segments()` /  |                                                      |
| `ok_plot_tsi()`         |                                                      |

## Vignettes

- `vignette("getting-started",   package = "okBATHTUB")`
- `vignette("oklahoma-workflow", package = "okBATHTUB")`
- `vignette("hawqs-linkage",     package = "okBATHTUB")`

## Caveats

- All predictions are **steady-state**. Inputs should represent
  long-term annual or seasonal averages, not single-event values.
- The bundled `ok_reservoirs` dataset is a compilation of public-domain
  morphometric data; for decision-relevant applications, verify against
  the most current authoritative source for the specific reservoir.
- The Oklahoma chlorophyll-a and Secchi regressions are empirical and
  show substantial residual scatter (typical $R^2$ values of 0.35-0.60).
  Treat point predictions as central estimates within a $\pm$
  factor-of-2 uncertainty range.

## Origin and independence

`okBATHTUB` was developed independently by Jordon Henderson on personal
time and personal equipment. It is released under the MIT license as
personal-capacity open-source research software. It is not a product
of, sponsored by, or affiliated with any employer or government agency,
and does not represent or imply endorsement by any agency. All data
used to calibrate the bundled Oklahoma coefficients are publicly
available state monitoring data.

## References

- Carlson, R.E. (1977). A trophic state index for lakes.
  *Limnology and Oceanography*, 22(2), 361-369.
- Larsen, D.P. & Mercier, H.T. (1976). Phosphorus retention capacity
  of lakes. *Journal of the Fisheries Research Board of Canada*, 33(8),
  1742-1750.
- Vollenweider, R.A. (1976). Advances in defining critical loading
  levels for phosphorus in lake eutrophication.
  *Memorie dell'Istituto Italiano di Idrobiologia*, 33, 53-83.
- Walker, W.W. (1985). Empirical methods for predicting eutrophication
  in impoundments; Report 3, Phase III: Model refinements.
  Technical Report E-81-9, U.S. Army Engineer Waterways Experiment Station.
- Walker, W.W. (1996). *Simplified Procedures for Eutrophication
  Assessment and Prediction: User Manual*. Instruction Report W-96-2,
  U.S. Army Engineer Waterways Experiment Station.

## License

MIT &copy; 2026 Jordon Henderson.
