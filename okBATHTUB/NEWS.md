# okBATHTUB 0.1.0

## Initial release

* Core BATHTUB pipeline: `ok_load()`, `ok_hydraulics()`, `ok_retention()`,
  `ok_inlake()`, `ok_tsi()`
* Oklahoma LMP-calibrated regression coefficients for three EPA Level III
  ecoregions (Cross Timbers, Central OK/TX Plains, Ozark Highlands) with
  statewide pooled fallback
* Multi-segment reservoir support via `ok_segment()` and `ok_segment_chain()`
* Load reduction scenario analysis via `ok_scenario()` and
  `ok_scenario_sweep()`
* Visualization functions: `ok_plot_response()`, `ok_plot_scenario()`,
  `ok_plot_segments()`, `ok_plot_tsi()`
* AWQMS database integration via `ok_from_awqms()` (OWRB-internal)
* Carlson TSI computation from observed data via `ok_tsi_observed()`
* Built-in dataset `ok_reservoirs` with morphometry for 123 Oklahoma LMP lakes
