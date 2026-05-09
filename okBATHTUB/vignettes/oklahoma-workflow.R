## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse   = TRUE,
  comment    = "#>",
  fig.width  = 7,
  fig.height = 5,
  dpi        = 120,
)


## ----load---------------------------------------------------------------------
library(okBATHTUB)
library(dplyr)


## ----awqms_pull---------------------------------------------------------------
# Pull 10 years of data for Arcadia Lake
arcadia_data <- ok_from_awqms(
  lake_names            = "Arcadia Lake",
  year_start            = 2015,
  year_end              = 2024,
  ecoregion_lookup_path = ok_path("lake_ecoregion_lookup.csv")
)

glimpse(arcadia_data)


## ----observed_tsi-------------------------------------------------------------
obs_tsi <- arcadia_data |>
  dplyr::filter(!is.na(tp_ugl), !is.na(chla_ugl), !is.na(secchi_m)) |>
  rowwise() |>
  dplyr::mutate(
    tsi_tp     = 14.42 * log(tp_ugl)   + 4.15,
    tsi_chla   = 9.81  * log(chla_ugl) + 30.6,
    tsi_secchi = 60.0  - 14.41 * log(secchi_m),
    tsi_mean   = mean(c(tsi_tp, tsi_chla, tsi_secchi))
  ) |>
  ungroup()

# Annual mean TSI by site
obs_tsi |>
  dplyr::group_by(sample_year) |>
  dplyr::summarise(
    tsi_tp     = round(mean(tsi_tp,     na.rm = TRUE), 1),
    tsi_chla   = round(mean(tsi_chla,   na.rm = TRUE), 1),
    tsi_secchi = round(mean(tsi_secchi, na.rm = TRUE), 1),
    tsi_mean   = round(mean(tsi_mean,   na.rm = TRUE), 1),
    .groups    = "drop"
  ) |>
  print()


## ----single_segment-----------------------------------------------------------
# Look up reservoir morphometry
res <- ok_reservoir("Arcadia Lake")
cat("Surface area:", res$surface_area_ha, "ha | Mean depth:", res$mean_depth_m, "m\n")
cat("Ecoregion:   ", res$eco_l3_name, "\n")

# Estimated inflow TP from watershed monitoring
# (Replace with USGS NWIS/FLUX-derived annual load estimate)
tp_inflow_est  <- 120   # µg/L flow-weighted mean
tn_inflow_est  <- 1800  # µg/L
inflow_vol_est <- 45e6  # m³/yr (~1.43 m³/s mean)

result <- ok_load(
    inflow_m3yr   = inflow_vol_est,
    tp_inflow_ugl = tp_inflow_est,
    tn_inflow_ugl = tn_inflow_est,
    coefficients  = "oklahoma",
    ecoregion     = res$eco_l3_name
  ) |>
  ok_hydraulics(
    surface_area_ha = res$surface_area_ha,
    mean_depth_m    = res$mean_depth_m
  ) |>
  ok_retention() |>
  ok_inlake()    |>
  ok_tsi()

summary(result)


## ----compare------------------------------------------------------------------
cat("--- Predicted (okBATHTUB) vs Observed (LMP) ---\n")
cat(sprintf("In-lake TP:   predicted = %.1f µg/L | observed = %.1f µg/L\n",
            result$data$tp_inlake_ugl,
            mean(arcadia_data$tp_ugl, na.rm = TRUE)))
cat(sprintf("Chl-a:        predicted = %.2f µg/L | observed = %.2f µg/L\n",
            result$data$chla_ugl,
            mean(arcadia_data$chla_ugl, na.rm = TRUE)))
cat(sprintf("Secchi depth: predicted = %.2f m    | observed = %.2f m\n",
            result$data$secchi_m,
            mean(arcadia_data$secchi_m, na.rm = TRUE)))


## ----multi_segment------------------------------------------------------------
eufaula <- ok_reservoir("Eufaula Lake")

# Three segments representing the longitudinal gradient
# Segment proportions estimated from USACE bathymetric data
segments <- list(
  list(
    label           = "Riverine",
    surface_area_ha = eufaula$surface_area_ha * 0.25,
    mean_depth_m    = 3.2
  ),
  list(
    label           = "Transitional",
    surface_area_ha = eufaula$surface_area_ha * 0.35,
    mean_depth_m    = 4.8
  ),
  list(
    label           = "Lacustrine",
    surface_area_ha = eufaula$surface_area_ha * 0.40,
    mean_depth_m    = 7.1
  )
)

chain <- ok_segment_chain(
  inflow_m3yr   = 2200e6,   # Eufaula: large watershed, high inflow
  tp_inflow_ugl = 145,
  tn_inflow_ugl = 2100,
  segments      = segments,
  coefficients  = "oklahoma",
  ecoregion     = "Arkansas Valley"   # statewide pooled applied (low R2)
)

ok_segment_summary(chain) |>
  dplyr::select(segment, tp_inflow_ugl, tp_inlake_ugl, tp_retention,
                chla_ugl, secchi_m, tsi_mean, trophic_state) |>
  print()


## ----segment_plot, fig.height=5.5, eval=requireNamespace("ggplot2", quietly=TRUE) && exists("chain") && is.list(chain)----
# ok_plot_segments(chain, lake_name = "Eufaula Lake")


## ----scenarios----------------------------------------------------------------
baseline <- ok_load(
    inflow_m3yr   = inflow_vol_est,
    tp_inflow_ugl = tp_inflow_est,
    tn_inflow_ugl = tn_inflow_est,
    coefficients  = "oklahoma",
    ecoregion     = res$eco_l3_name
  ) |>
  ok_hydraulics(
    surface_area_ha = res$surface_area_ha,
    mean_depth_m    = res$mean_depth_m
  )

# Sweep 10% through 70% TP reduction, target: mesotrophic (TSI < 50)
sweep <- ok_scenario_sweep(
  baseline,
  max_reduction_pct = 70,
  step_pct          = 10,
  target_class      = "mesotrophic"
)

sweep |>
  dplyr::select(scenario, tp_inflow_ugl, tp_inlake_ugl,
         chla_ugl, secchi_m, tsi_mean, trophic_state, meets_target) |>
  print()


## ----min_reduction------------------------------------------------------------
min_met <- sweep |>
  dplyr::filter(meets_target == TRUE) |>
  dplyr::slice(1)

if (nrow(min_met) > 0) {
  cat(sprintf(
    "Minimum TP reduction to achieve mesotrophic (TSI < 50): %s\n",
    min_met$scenario
  ))
  cat(sprintf(
    "  Inflow TP: %.0f µg/L → In-lake TP: %.1f µg/L → TSI: %.1f\n",
    min_met$tp_inflow_ugl, min_met$tp_inlake_ugl, min_met$tsi_mean
  ))
} else {
  cat("No scenario in the sweep achieves mesotrophic status.\n")
  cat("Consider evaluating reductions beyond", max(sweep$tp_reduction_pct, na.rm=TRUE), "%\n")
}


## ----custom_scenarios---------------------------------------------------------
custom <- ok_scenario(
  baseline  = baseline,
  scenarios = list(
    list(label = "Agricultural BMPs (25% TP)",
         tp_reduction = 0.25),
    list(label = "Wastewater upgrade (40% TP, 30% TN)",
         tp_reduction = 0.40, tn_reduction = 0.30),
    list(label = "Drought year (20% flow reduction)",
         flow_change  = -0.20),
    list(label = "Wet year + BMPs",
         tp_reduction = 0.25, flow_change = 0.15),
    list(label = "TMDL target TP = 60 µg/L",
         tp_inflow_ugl = 60)
  ),
  target_class = "mesotrophic"
)

custom |>
  dplyr::select(scenario, tp_inflow_ugl, tsi_mean, trophic_state, meets_target) |>
  print()


## ----response_plot, fig.height=5.5, eval=requireNamespace("ggplot2", quietly=TRUE) && exists("baseline") && exists("tp_inflow_est")----
# ok_plot_response(
#   baseline,
#   response     = "tsi",
#   target_class = "mesotrophic",
#   current_tp   = tp_inflow_est,
#   lake_name    = "Arcadia Lake"
# )


## ----scenario_plot, fig.height=5, eval=requireNamespace("ggplot2", quietly=TRUE) && exists("sweep") && is.data.frame(sweep)----
# ok_plot_scenario(sweep, lake_name = "Arcadia Lake")


## ----tsi_diagram, eval=FALSE--------------------------------------------------
# # Pull all LMP lakes for statewide TSI diagram
# all_lakes <- ok_from_awqms(
#   year_start            = 2018,
#   year_end              = 2024,
#   ecoregion_lookup_path = ok_path("lake_ecoregion_lookup.csv")
# )
# 
# # Compute observed TSI
# all_tsi <- all_lakes |>
#   dplyr::filter(!is.na(tp_ugl), !is.na(chla_ugl)) |>
#   dplyr::mutate(
#     tsi_tp   = 14.42 * log(tp_ugl)   + 4.15,
#     tsi_chla = 9.81  * log(chla_ugl) + 30.6
#   )
# 
# ok_plot_tsi(all_tsi, color_by = "eco_l3_name")


## ----full_workflow, eval=requireNamespace("ggplot2", quietly=TRUE)------------
# 1. Look up reservoir morphometry
res <- ok_reservoir("Arcadia Lake")

# 2. Define loading inputs (from USGS/FLUX load estimates)
loading <- list(
  inflow_m3yr   = 45e6,
  tp_inflow_ugl = 120,
  tn_inflow_ugl = 1800
)

# 3. Run pipeline with Oklahoma coefficients
baseline <- ok_load(
    inflow_m3yr   = loading$inflow_m3yr,
    tp_inflow_ugl = loading$tp_inflow_ugl,
    tn_inflow_ugl = loading$tn_inflow_ugl,
    coefficients  = "oklahoma",
    ecoregion     = res$eco_l3_name
  ) |>
  ok_hydraulics(
    surface_area_ha = res$surface_area_ha,
    mean_depth_m    = res$mean_depth_m
  )

result <- baseline |>
  ok_retention() |>
  ok_inlake()    |>
  ok_tsi()

# 4. Scenario analysis
sweep <- ok_scenario_sweep(baseline, max_reduction_pct = 60,
                            step_pct = 10, target_class = "mesotrophic")

# 5. Visualise
ok_plot_response(baseline, response = "tsi",
                  target_class = "mesotrophic",
                  current_tp = loading$tp_inflow_ugl,
                  lake_name = res$lake_name)

