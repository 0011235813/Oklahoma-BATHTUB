## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse  = TRUE,
  comment   = "#>",
  fig.width = 7,
  fig.height = 4.5,
  dpi       = 120
)

## ----install, eval=FALSE------------------------------------------------------
# # Install from GitHub (until CRAN submission)
# # install.packages("remotes")
# remotes::install_github("0011235813/okBATHTUB")

## ----load---------------------------------------------------------------------
library(okBATHTUB)

## ----load_step----------------------------------------------------------------
result <- ok_load(
  inflow_m3yr   = 45e6,    # 45 million m³/yr (~1.43 m³/s mean flow)
  tp_inflow_ugl = 120,     # 120 µg/L flow-weighted mean TP
  tn_inflow_ugl = 1800,    # 1800 µg/L flow-weighted mean TN
  segment_label = "lacustrine"
)

print(result)

## ----hydraulics_step----------------------------------------------------------
result <- result |>
  ok_hydraulics(
    surface_area_ha = 890,   # normal pool surface area
    mean_depth_m    = 4.2    # mean depth at normal pool
  )

cat("Hydraulic residence time:", round(result$data$hydraulic_residence_time_yr, 2), "yr\n")
cat("Areal water load (qs):   ", round(result$data$areal_water_load_myr, 2), "m/yr\n")

## ----retention_step-----------------------------------------------------------
result <- result |> ok_retention()

cat("TP retention coefficient:", round(result$data$tp_retention_coeff, 3), "\n")
cat("TN retention coefficient:", round(result$data$tn_retention_coeff, 3), "\n")

## ----inlake_step--------------------------------------------------------------
result <- result |> ok_inlake()

cat("In-lake TP:    ", round(result$data$tp_inlake_ugl, 1), "µg/L\n")
cat("Chlorophyll-a: ", round(result$data$chla_ugl, 2),    "µg/L\n")
cat("Secchi depth:  ", round(result$data$secchi_m, 2),    "m\n")

## ----tsi_step-----------------------------------------------------------------
result <- result |> ok_tsi()

summary(result)

## ----full_pipeline------------------------------------------------------------
result <- ok_load(
    inflow_m3yr   = 45e6,
    tp_inflow_ugl = 120,
    tn_inflow_ugl = 1800
  ) |>
  ok_hydraulics(surface_area_ha = 890, mean_depth_m = 4.2) |>
  ok_retention() |>
  ok_inlake()    |>
  ok_tsi()

summary(result)

## ----reservoir_lookup---------------------------------------------------------
# Look up Arcadia Lake from the built-in dataset
res <- ok_reservoir("Arcadia")
if (nrow(res) == 0) {
  # Fallback: use known Arcadia Lake morphometry if not in local dataset
  res <- data.frame(
    lake_name       = "Arcadia Lake",
    surface_area_ha = 890,
    mean_depth_m    = 4.2,
    eco_l3_name     = "Cross Timbers",
    data_quality    = "A",
    notes           = "Hardcoded fallback"
  )
}
cat("Surface area:", res$surface_area_ha, "ha\n")
cat("Mean depth:  ", res$mean_depth_m, "m\n")

## ----reservoir_pipeline-------------------------------------------------------
# Use known Arcadia Lake morphometry for a reproducible example
result <- ok_load(
    inflow_m3yr   = 45e6,
    tp_inflow_ugl = 120,
    tn_inflow_ugl = 1800
  ) |>
  ok_hydraulics(
    surface_area_ha = 890,
    mean_depth_m    = 4.2
  ) |>
  ok_retention() |>
  ok_inlake()    |>
  ok_tsi()

cat("Trophic state:", result$data$trophic_state, "\n")
cat("Mean TSI:     ", round(result$data$tsi_mean, 1), "\n")

## ----multi_trib---------------------------------------------------------------
tributaries <- data.frame(
  inflow_m3yr   = c(30e6, 15e6),
  tp_inflow_ugl = c(100,  160),
  tn_inflow_ugl = c(1500, 2400)
)

result_multi <- ok_load_multi(tributaries) |>
  ok_hydraulics(surface_area_ha = 890, mean_depth_m = 4.2) |>
  ok_retention() |>
  ok_inlake()    |>
  ok_tsi()

# Flow-weighted mean TP: (100*30e6 + 160*15e6) / 45e6 = 120 µg/L
cat("FW mean TP inflow:", round(result_multi$data$tp_inflow_ugl, 1), "µg/L\n")

## ----tsi_observed-------------------------------------------------------------
obs <- ok_tsi_observed(
  tp_ugl    = 85,
  chla_ugl  = 22,
  secchi_m  = 0.8
)

cat("TSI(TP):    ", round(obs$tsi_tp,     1), "\n")
cat("TSI(Chl-a): ", round(obs$tsi_chla,   1), "\n")
cat("TSI(Secchi):", round(obs$tsi_secchi, 1), "\n")
cat("Mean TSI:   ", round(obs$tsi_mean,   1), "\n")
cat("Class:      ", obs$trophic_state,         "\n")

## ----coeff_comparison---------------------------------------------------------
# Walker coefficients (national defaults)
r_walker <- ok_load(
    inflow_m3yr   = 45e6,
    tp_inflow_ugl = 120,
    coefficients  = "walker"
  ) |>
  ok_hydraulics(surface_area_ha = 890, mean_depth_m = 4.2) |>
  ok_retention() |>
  ok_inlake()    |>
  ok_tsi()

# Oklahoma ecoregion-specific coefficients
r_oklahoma <- ok_load(
    inflow_m3yr   = 45e6,
    tp_inflow_ugl = 120,
    coefficients  = "oklahoma",
    ecoregion     = "Cross Timbers"
  ) |>
  ok_hydraulics(surface_area_ha = 890, mean_depth_m = 4.2) |>
  ok_retention() |>
  ok_inlake()    |>
  ok_tsi()

cat("Chlorophyll-a (Walker):   ", round(r_walker$data$chla_ugl, 2),   "µg/L\n")
cat("Chlorophyll-a (Oklahoma): ", round(r_oklahoma$data$chla_ugl, 2), "µg/L\n\n")
cat("Mean TSI (Walker):        ", round(r_walker$data$tsi_mean, 1),   "\n")
cat("Mean TSI (Oklahoma):      ", round(r_oklahoma$data$tsi_mean, 1), "\n")

## ----plot_response, fig.height=5, eval=requireNamespace("ggplot2", quietly=TRUE)----
baseline <- ok_load(
    inflow_m3yr   = 45e6,
    tp_inflow_ugl = 120,
    coefficients  = "oklahoma",
    ecoregion     = "Cross Timbers"
  ) |>
  ok_hydraulics(surface_area_ha = 890, mean_depth_m = 4.2)

ok_plot_response(
  baseline,
  response     = "tsi",
  target_class = "mesotrophic",
  current_tp   = 120,
  lake_name    = "Arcadia Lake"
)

