## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse  = TRUE,
  comment   = "#>",
  fig.width = 7,
  fig.height = 5,
  dpi       = 120
)

## ----load---------------------------------------------------------------------
library(okBATHTUB)
library(dplyr)
library(readr)

## ----read_hawqs, eval=FALSE---------------------------------------------------
# # Read SWAT output.rch file
# # (Adjust column positions for your SWAT version)
# read_swat_rch <- function(rch_path) {
#   # Read fixed-width format
#   raw <- read_fwf(
#     rch_path,
#     fwf_cols(
#       rch_id    = c(1, 4),
#       year      = c(5, 9),
#       month     = c(10, 12),
#       day       = c(13, 16),
#       area_km2  = c(17, 26),
#       flow_cms  = c(27, 38),   # FLOW_OUTcms
#       no3_kgha  = c(83, 94),   # NO3_OUT
#       orgn_kgha = c(95, 106),  # ORGN_OUT
#       solp_kgha = c(107, 118), # SOLP_OUT
#       orgp_kgha = c(119, 130)  # ORGP_OUT
#     ),
#     skip = 9,
#     col_types = "iiiiidddddd"
#   )
#   raw
# }

## ----simulate_hawqs-----------------------------------------------------------
# Simulated OK-HAWQS annual output at reservoir inlet
# Replace with actual output.rch data in production use
hawqs_annual <- data.frame(
  year         = 2010:2022,
  flow_m3yr    = c(38.2, 52.1, 29.4, 44.7, 61.3, 35.8, 41.9,
                   48.2, 33.6, 57.4, 42.1, 36.8, 49.3) * 1e6,
  tp_load_kgyr = c(4584, 7314, 2823, 5811, 9195, 3938, 5447,
                   6253, 3427, 8296, 5473, 3938, 6657),
  tn_load_kgyr = c(69120, 98990, 47100, 84780, 127280, 57890, 79860,
                   91580, 53420, 114800, 80020, 58920, 95280)
)

# Compute flow-weighted mean concentrations
# Concentration (µg/L) = load (kg/yr) / volume (m³/yr) * 1e6
hawqs_annual <- hawqs_annual |>
  dplyr::mutate(
    tp_conc_ugl = tp_load_kgyr / flow_m3yr * 1e6,
    tn_conc_ugl = tn_load_kgyr / flow_m3yr * 1e6
  )

cat("--- OK-HAWQS Annual Load Summary ---\n")
cat(sprintf("Mean annual flow:    %.1f million m³/yr\n",
            mean(hawqs_annual$flow_m3yr) / 1e6))
cat(sprintf("Mean annual TP load: %.0f kg/yr\n",
            mean(hawqs_annual$tp_load_kgyr)))
cat(sprintf("FWM TP conc:         %.1f µg/L\n",
            mean(hawqs_annual$tp_conc_ugl)))
cat(sprintf("FWM TN conc:         %.1f µg/L\n",
            mean(hawqs_annual$tn_conc_ugl)))

## ----hawqs_to_bathtub---------------------------------------------------------
# Use long-term mean conditions for steady-state BATHTUB
mean_flow_m3yr <- mean(hawqs_annual$flow_m3yr)
mean_tp_ugl    <- mean(hawqs_annual$tp_conc_ugl)
mean_tn_ugl    <- mean(hawqs_annual$tn_conc_ugl)

cat(sprintf("BATHTUB inputs:\n"))
cat(sprintf("  Inflow volume: %.2f million m³/yr\n", mean_flow_m3yr / 1e6))
cat(sprintf("  TP inflow:     %.1f µg/L\n", mean_tp_ugl))
cat(sprintf("  TN inflow:     %.1f µg/L\n", mean_tn_ugl))

## ----baseline_run-------------------------------------------------------------
# Reservoir morphometry: Arcadia Lake (Cross Timbers ecoregion)
# surface_area_ha = 890, mean_depth_m = 4.2 (hardcoded for reproducibility)

baseline <- ok_load(
    inflow_m3yr   = mean_flow_m3yr,
    tp_inflow_ugl = mean_tp_ugl,
    tn_inflow_ugl = mean_tn_ugl,
    coefficients  = "oklahoma",
    ecoregion     = "Cross Timbers",
    segment_label = "baseline"
  ) |>
  ok_hydraulics(
    surface_area_ha = 890,
    mean_depth_m    = 4.2
  )

result_baseline <- baseline |>
  ok_retention() |>
  ok_inlake()    |>
  ok_tsi()

summary(result_baseline)

## ----bmp_scenarios------------------------------------------------------------
# Simulated HAWQS BMP scenario outputs
# Each represents a different watershed management alternative
hawqs_scenarios <- list(
  list(
    label        = "Baseline (current conditions)",
    flow_m3yr    = mean_flow_m3yr,
    tp_load_kgyr = mean(hawqs_annual$tp_load_kgyr),
    tn_load_kgyr = mean(hawqs_annual$tn_load_kgyr)
  ),
  list(
    label        = "10% cropland cover crops",
    flow_m3yr    = mean_flow_m3yr * 0.97,     # slight flow reduction
    tp_load_kgyr = mean(hawqs_annual$tp_load_kgyr) * 0.88,
    tn_load_kgyr = mean(hawqs_annual$tn_load_kgyr) * 0.92
  ),
  list(
    label        = "30% cropland cover crops + buffer strips",
    flow_m3yr    = mean_flow_m3yr * 0.94,
    tp_load_kgyr = mean(hawqs_annual$tp_load_kgyr) * 0.72,
    tn_load_kgyr = mean(hawqs_annual$tn_load_kgyr) * 0.78
  ),
  list(
    label        = "Full BMP suite (TMDL alternative)",
    flow_m3yr    = mean_flow_m3yr * 0.91,
    tp_load_kgyr = mean(hawqs_annual$tp_load_kgyr) * 0.55,
    tn_load_kgyr = mean(hawqs_annual$tn_load_kgyr) * 0.62
  )
)

# Convert each HAWQS scenario to okBATHTUB concentrations and run pipeline
scenario_results <- lapply(hawqs_scenarios, function(sc) {
  tp_ugl <- sc$tp_load_kgyr / sc$flow_m3yr * 1e6
  tn_ugl <- sc$tn_load_kgyr / sc$flow_m3yr * 1e6

  r <- ok_load(
      inflow_m3yr   = sc$flow_m3yr,
      tp_inflow_ugl = tp_ugl,
      tn_inflow_ugl = tn_ugl,
      coefficients  = "oklahoma",
      ecoregion     = "Cross Timbers"
    ) |>
    ok_hydraulics(
      surface_area_ha = 890,
      mean_depth_m    = 4.2
    ) |>
    ok_retention() |>
    ok_inlake()    |>
    ok_tsi()

  data.frame(
    scenario         = sc$label,
    tp_inflow_ugl    = round(tp_ugl, 1),
    tp_reduction_pct = round(100 * (1 - tp_ugl / mean_tp_ugl), 1),
    tp_inlake_ugl    = round(r$data$tp_inlake_ugl, 1),
    chla_ugl         = round(r$data$chla_ugl,      2),
    secchi_m         = round(r$data$secchi_m,      2),
    tsi_mean         = round(r$data$tsi_mean,      1),
    trophic_state    = r$data$trophic_state,
    stringsAsFactors = FALSE
  )
})

scenario_df <- do.call(rbind, scenario_results)
print(scenario_df)

## ----interannual--------------------------------------------------------------
# Run BATHTUB for each year in the HAWQS record
annual_results <- lapply(seq_len(nrow(hawqs_annual)), function(i) {
  yr   <- hawqs_annual[i, ]
  tp_ugl <- yr$tp_conc_ugl
  tn_ugl <- yr$tn_conc_ugl

  r <- ok_load(
      inflow_m3yr   = yr$flow_m3yr,
      tp_inflow_ugl = tp_ugl,
      tn_inflow_ugl = tn_ugl,
      coefficients  = "oklahoma",
      ecoregion     = "Cross Timbers"
    ) |>
    ok_hydraulics(
      surface_area_ha = 890,
      mean_depth_m    = 4.2
    ) |>
    ok_retention() |>
    ok_inlake()    |>
    ok_tsi()

  data.frame(
    year          = yr$year,
    flow_m3yr     = yr$flow_m3yr / 1e6,
    tp_inflow_ugl = round(tp_ugl, 1),
    tp_inlake_ugl = round(r$data$tp_inlake_ugl, 1),
    chla_ugl      = round(r$data$chla_ugl, 2),
    secchi_m      = round(r$data$secchi_m, 2),
    tsi_mean      = round(r$data$tsi_mean, 1),
    trophic_state = r$data$trophic_state
  )
}) |>
  do.call(rbind, args = _)

cat("--- Interannual Range ---\n")
cat(sprintf("TSI range:    %.1f - %.1f\n",
            min(annual_results$tsi_mean), max(annual_results$tsi_mean)))
cat(sprintf("Chl-a range:  %.2f - %.2f µg/L\n",
            min(annual_results$chla_ugl), max(annual_results$chla_ugl)))
cat(sprintf("Secchi range: %.2f - %.2f m\n",
            min(annual_results$secchi_m), max(annual_results$secchi_m)))

## ----response_plot, fig.height=5.5, eval=requireNamespace("ggplot2", quietly=TRUE)----
ok_plot_response(
  baseline,
  response     = "tsi",
  target_class = "mesotrophic",
  current_tp   = mean_tp_ugl,
  lake_name    = "Arcadia Lake -- OK-HAWQS/okBATHTUB Linkage"
)

