# =============================================================================
# okBATHTUB — Oklahoma Coefficient Calibration
# Oklahoma Water Resources Board — Water Quality Division
#
# Purpose: Fit Oklahoma-specific empirical regression coefficients for the
#          okBATHTUB reservoir eutrophication model using OWRB Lake Monitoring
#          Program data pulled from AWQMS.
#
# Two regressions are fitted per ecoregion (log-log OLS):
#   1. Chlorophyll-a from in-lake TP:
#        log10(Chl-a) = a + b * log10(TP)
#   2. Secchi depth from chlorophyll-a:
#        log10(Secchi) = a + b * log10(Chl-a)
#
# TP retention uses Walker (1996) defaults — see ok_retention() in the package.
# Retention calibration requires paired inflow load data and is deferred to a
# future phase.
#
# Ecoregion calibration strategy (based on sample size assessment):
#   Cross Timbers (28)            — fit independently (n=181 / 265)
#   Central OK/TX Plains (27)     — fit independently (n=37 / 26)
#   Arkansas Valley (37)          — fit independently (n=30 / 27)
#   Ozark Highlands (30)          — fit independently (n=20 / 26)
#   Ouachita Mountains (29)       — Secchi: fit independently (n=21)
#                                   Chl-a:  use statewide pooled (n=10, below threshold)
#   Central Great Plains (25)     — fall back to Walker (n=1)
#
# Minimum thresholds for ecoregion-specific fit: n >= 15 obs, >= 5 lakes.
# Below threshold: use statewide pooled fit.
# If statewide pooled also inadequate: use Walker defaults.
#
# Outputs:
#   ok_coefficients.R      — R object ready to bundle into package as internal data
#   ok_calibration_report.xlsx — full coefficient table with diagnostics
#   ok_calibration_plots/  — diagnostic plots per ecoregion
#
# Dependencies: dplyr, tidyr, ggplot2, openxlsx, stringr, readr
# Requires: all_lakes object from ok_from_awqms() in session
# =============================================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(openxlsx)
library(stringr)
library(readr)

source(ok_path("ok_from_awqms.R"))   # for ok_path()

# =============================================================================
# 1. CONFIGURATION
# =============================================================================

# Minimum observations for ecoregion-specific fit
MIN_OBS_ECO   <- 15L
MIN_LAKES_ECO <- 5L

# Walker (1996) default coefficients — used as fallback
WALKER <- list(
  chla = list(intercept = -1.136, slope = 1.449),
  secchi = list(intercept = 0.616, slope = -0.473)
)

# Output paths
out_dir        <- ok_path()
plots_dir      <- ok_path("ok_calibration_plots")
report_path    <- ok_path("ok_calibration_report.xlsx")
coeff_r_path   <- ok_path("ok_coefficients.R")

dir.create(plots_dir, showWarnings = FALSE, recursive = TRUE)

# =============================================================================
# 2. PREPARE CALIBRATION DATASET
# =============================================================================

message("Preparing calibration dataset...")

# Use all_lakes object already in session (from ok_from_awqms full pull)
# Filter to quality observations: min 3 samples per parameter, valid values
calib_raw <- all_lakes %>%
  filter(
    !is.na(eco_l3_code),
    !is.na(tp_ugl),    tp_ugl    > 0,  tp_n    >= 3,
    !is.na(chla_ugl),  chla_ugl  > 0,  chla_n  >= 3,
    !is.na(secchi_m),  secchi_m  > 0
  ) %>%
  mutate(
    log_tp     = log10(tp_ugl),
    log_chla   = log10(chla_ugl),
    log_secchi = log10(secchi_m)
  )

message(sprintf("Calibration records (TP + Chl-a + Secchi): %d obs across %d lakes",
                nrow(calib_raw), n_distinct(calib_raw$lake_name)))

# Subset for each regression
# TP -> Chl-a: needs TP and Chl-a
df_tp_chla <- calib_raw %>%
  filter(!is.na(log_tp), !is.na(log_chla))

# Chl-a -> Secchi: needs Chl-a and Secchi
df_chla_secchi <- calib_raw %>%
  filter(!is.na(log_chla), !is.na(log_secchi))

message(sprintf("TP -> Chl-a pairs:    %d", nrow(df_tp_chla)))
message(sprintf("Chl-a -> Secchi pairs: %d", nrow(df_chla_secchi)))

# =============================================================================
# 3. REGRESSION FITTING FUNCTIONS
# =============================================================================

#' Fit a log-log OLS regression and return a tidy coefficient record
#'
#' @param df Data frame with predictor and response columns
#' @param x_col Name of log-transformed predictor column (character)
#' @param y_col Name of log-transformed response column (character)
#' @param label Label for this regression (e.g. "chla_from_tp")
#' @param ecoregion Ecoregion name
#' @param walker_default Named list with intercept and slope from Walker
#' @return Named list of fit statistics
fit_loglog <- function(df, x_col, y_col, label, ecoregion,
                        walker_default, min_obs, min_lakes) {

  n_obs   <- nrow(df)
  n_lakes <- n_distinct(df$lake_name)

  # Check if we meet the threshold for an ecoregion-specific fit
  if (n_obs < min_obs || n_lakes < min_lakes) {
    return(list(
      ecoregion  = ecoregion,
      regression = label,
      source     = "insufficient — see statewide or Walker",
      n_obs      = n_obs,
      n_lakes    = n_lakes,
      intercept  = NA_real_,
      slope      = NA_real_,
      r_squared  = NA_real_,
      rmse       = NA_real_,
      intercept_se = NA_real_,
      slope_se     = NA_real_,
      p_value      = NA_real_,
      walker_intercept = walker_default$intercept,
      walker_slope     = walker_default$slope
    ))
  }

  # Fit OLS
  formula_str <- paste(y_col, "~", x_col)
  fit <- lm(as.formula(formula_str), data = df)
  sm  <- summary(fit)
  cf  <- coef(sm)

  resid_sd <- sqrt(mean(residuals(fit)^2))

  list(
    ecoregion        = ecoregion,
    regression       = label,
    source           = "oklahoma_lmp",
    n_obs            = n_obs,
    n_lakes          = n_lakes,
    intercept        = unname(coef(fit)[1]),
    slope            = unname(coef(fit)[2]),
    r_squared        = sm$r.squared,
    rmse             = resid_sd,
    intercept_se     = cf[1, "Std. Error"],
    slope_se         = cf[2, "Std. Error"],
    p_value          = cf[2, "Pr(>|t|)"],
    walker_intercept = walker_default$intercept,
    walker_slope     = walker_default$slope
  )
}

# =============================================================================
# 4. FIT REGRESSIONS PER ECOREGION
# =============================================================================

message("\nFitting ecoregion-specific regressions...")

ecoregions <- sort(unique(calib_raw$eco_l3_name))

chla_results   <- list()
secchi_results <- list()

for (eco in ecoregions) {

  message(sprintf("  Ecoregion: %s", eco))

  df_eco_tp_chla     <- df_tp_chla     %>% filter(eco_l3_name == eco)
  df_eco_chla_secchi <- df_chla_secchi %>% filter(eco_l3_name == eco)

  chla_results[[eco]] <- fit_loglog(
    df          = df_eco_tp_chla,
    x_col       = "log_tp",
    y_col       = "log_chla",
    label       = "chla_from_tp",
    ecoregion   = eco,
    walker_default = WALKER$chla,
    min_obs     = MIN_OBS_ECO,
    min_lakes   = MIN_LAKES_ECO
  )

  secchi_results[[eco]] <- fit_loglog(
    df          = df_eco_chla_secchi,
    x_col       = "log_chla",
    y_col       = "log_secchi",
    label       = "secchi_from_chla",
    ecoregion   = eco,
    walker_default = WALKER$secchi,
    min_obs     = MIN_OBS_ECO,
    min_lakes   = MIN_LAKES_ECO
  )
}

# =============================================================================
# 5. FIT STATEWIDE POOLED REGRESSIONS
# Used as fallback for ecoregions below the minimum threshold
# =============================================================================

message("\nFitting statewide pooled regressions...")

chla_pooled <- fit_loglog(
  df          = df_tp_chla,
  x_col       = "log_tp",
  y_col       = "log_chla",
  label       = "chla_from_tp",
  ecoregion   = "Statewide (pooled)",
  walker_default = WALKER$chla,
  min_obs     = 1L,   # always fit pooled
  min_lakes   = 1L
)

secchi_pooled <- fit_loglog(
  df          = df_chla_secchi,
  x_col       = "log_chla",
  y_col       = "log_secchi",
  label       = "secchi_from_chla",
  ecoregion   = "Statewide (pooled)",
  walker_default = WALKER$secchi,
  min_obs     = 1L,
  min_lakes   = 1L
)

message(sprintf("  Statewide Chl-a: R2=%.3f, n=%d",
                chla_pooled$r_squared, chla_pooled$n_obs))
message(sprintf("  Statewide Secchi: R2=%.3f, n=%d",
                secchi_pooled$r_squared, secchi_pooled$n_obs))

# =============================================================================
# 6. RESOLVE FINAL COEFFICIENTS
# For each ecoregion, apply the cascade:
#   1. Ecoregion-specific fit (if n >= threshold)
#   2. Statewide pooled fit
#   3. Walker defaults
# =============================================================================

message("\nResolving final coefficient cascade...")

all_ecoregions <- c(
  "Central Great Plains",
  "Central Oklahoma/Texas Plains",
  "Cross Timbers",
  "Ouachita Mountains",
  "Ozark Highlands",
  "Arkansas Valley",
  "South Central Plains",    # no data — will use Walker
  "Flint Hills"              # no data — will use Walker
)

# Minimum R2 for ecoregion fit acceptance.
# Fits below this fall back to statewide pooled.
# Rationale: see CALIBRATION_README.md — Arkansas Valley triggered this
# threshold (Chl-a R2=0.120, Secchi R2=0.018) due to inorganic turbidity
# decoupling TP->Chl-a and Chl-a->Secchi in flood-dominated reservoirs.
MIN_R2_ECO <- 0.25

resolve_coeff <- function(eco_result, pooled_result, walker_default,
                          regression_name, min_r2 = MIN_R2_ECO) {

  eco_fit_accepted <- (
    !is.na(eco_result$intercept) &&
    !is.na(eco_result$r_squared) &&
    eco_result$r_squared >= min_r2
  )

  if (eco_fit_accepted) {
    # Use ecoregion-specific fit — meets sample size AND R2 threshold
    list(
      intercept = eco_result$intercept,
      slope     = eco_result$slope,
      source    = "oklahoma_ecoregion",
      r_squared = eco_result$r_squared,
      n_obs     = eco_result$n_obs,
      n_lakes   = eco_result$n_lakes
    )
  } else if (!is.na(pooled_result$intercept)) {
    # Fall back to statewide pooled
    list(
      intercept = pooled_result$intercept,
      slope     = pooled_result$slope,
      source    = "oklahoma_statewide",
      r_squared = pooled_result$r_squared,
      n_obs     = pooled_result$n_obs,
      n_lakes   = pooled_result$n_lakes
    )
  } else {
    # Fall back to Walker defaults
    list(
      intercept = walker_default$intercept,
      slope     = walker_default$slope,
      source    = "walker_1996",
      r_squared = NA_real_,
      n_obs     = 0L,
      n_lakes   = 0L
    )
  }
}

ok_coefficients <- list()

for (eco in all_ecoregions) {

  eco_chla   <- chla_results[[eco]]   %||% list(intercept = NA_real_,
                                                 slope = NA_real_)
  eco_secchi <- secchi_results[[eco]] %||% list(intercept = NA_real_,
                                                 slope = NA_real_)

  ok_coefficients[[eco]] <- list(
    ecoregion      = eco,
    tp_retention   = list(
      form   = "larsen_mercier",
      source = "walker_1996",
      note   = "Retention calibration deferred — requires paired inflow load data"
    ),
    tn_settling_velocity = list(
      value  = 10.0,
      source = "walker_1996",
      note   = "TN settling velocity calibration deferred"
    ),
    chla = resolve_coeff(
      eco_chla, chla_pooled, WALKER$chla, "chla_from_tp"
    ),
    secchi = resolve_coeff(
      eco_secchi, secchi_pooled, WALKER$secchi, "secchi_from_chla"
    )
  )

  message(sprintf(
    "  %-35s  Chl-a: %-22s (R2=%s)  Secchi: %-22s (R2=%s)",
    eco,
    ok_coefficients[[eco]]$chla$source,
    ifelse(is.na(ok_coefficients[[eco]]$chla$r_squared), " NA ",
           sprintf("%.3f", ok_coefficients[[eco]]$chla$r_squared)),
    ok_coefficients[[eco]]$secchi$source,
    ifelse(is.na(ok_coefficients[[eco]]$secchi$r_squared), " NA ",
           sprintf("%.3f", ok_coefficients[[eco]]$secchi$r_squared))
  ))
}

# Null coalescing operator (if not already loaded from ok_from_awqms.R)
`%||%` <- function(a, b) if (!is.null(a)) a else b

# =============================================================================
# 7. DIAGNOSTIC PLOTS
# =============================================================================

message("\nGenerating diagnostic plots...")

plot_loglog <- function(df, x_col, y_col, x_label, y_label,
                         eco_name, fit_result, walker_default,
                         filename) {

  # Back-transform for axis labels
  x_range <- range(df[[x_col]], na.rm = TRUE)
  x_seq   <- seq(x_range[1], x_range[2], length.out = 100)

  # Oklahoma fit line
  ok_line <- if (!is.na(fit_result$intercept)) {
    data.frame(
      x    = x_seq,
      y    = fit_result$intercept + fit_result$slope * x_seq,
      type = sprintf("Oklahoma LMP (R²=%.2f, n=%d)",
                     fit_result$r_squared, fit_result$n_obs)
    )
  } else NULL

  # Walker line
  walker_line <- data.frame(
    x    = x_seq,
    y    = walker_default$intercept + walker_default$slope * x_seq,
    type = "Walker (1996) default"
  )

  lines_df <- if (!is.null(ok_line)) {
    dplyr::bind_rows(ok_line, walker_line)
  } else {
    walker_line
  }

  p <- ggplot(df, aes(x = .data[[x_col]], y = .data[[y_col]])) +
    geom_point(aes(color = lake_name), alpha = 0.6, size = 2,
               show.legend = nrow(df) <= 100) +
    geom_line(data = lines_df,
              aes(x = x, y = y, linetype = type, color = type),
              linewidth = 0.9,
              inherit.aes = FALSE) +
    scale_color_manual(
      values = c(
        "Oklahoma LMP (R²=NA, n=NA)" = "#E63946",
        "Walker (1996) default"       = "#457B9D",
        setNames(
          scales::hue_pal()(n_distinct(df$lake_name)),
          unique(df$lake_name)
        )
      ),
      guide = guide_legend(
        override.aes = list(size = 3, alpha = 1)
      )
    ) +
    scale_linetype_manual(
      values = c(
        "Walker (1996) default" = "dashed"
      ),
      na.value = "solid"
    ) +
    labs(
      title    = sprintf("%s — %s", eco_name, y_label),
      subtitle = sprintf("log10(%s) = %.3f + %.3f × log10(%s)",
                         y_label,
                         ifelse(is.na(fit_result$intercept),
                                walker_default$intercept,
                                fit_result$intercept),
                         ifelse(is.na(fit_result$slope),
                                walker_default$slope,
                                fit_result$slope),
                         x_label),
      x        = sprintf("log10(%s)", x_label),
      y        = sprintf("log10(%s)", y_label),
      color    = NULL,
      linetype = NULL,
      caption  = "OWRB Lake Monitoring Program | okBATHTUB"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title    = element_text(face = "bold"),
      legend.position = if (n_distinct(df$lake_name) > 15) "none" else "right"
    )

  ggsave(
    filename = file.path(plots_dir, filename),
    plot     = p,
    width    = 8, height = 5.5, dpi = 150
  )

  invisible(p)
}

# Generate plots for each ecoregion
for (eco in ecoregions) {

  eco_safe <- str_replace_all(eco, "[^A-Za-z0-9]", "_")

  # Chl-a from TP
  df_eco <- df_tp_chla %>% filter(eco_l3_name == eco)
  if (nrow(df_eco) > 0) {
    plot_loglog(
      df             = df_eco,
      x_col          = "log_tp",
      y_col          = "log_chla",
      x_label        = "TP (µg/L)",
      y_label        = "Chl-a (µg/L)",
      eco_name       = eco,
      fit_result     = chla_results[[eco]],
      walker_default = WALKER$chla,
      filename       = sprintf("%s_chla_from_tp.png", eco_safe)
    )
  }

  # Secchi from Chl-a
  df_eco2 <- df_chla_secchi %>% filter(eco_l3_name == eco)
  if (nrow(df_eco2) > 0) {
    plot_loglog(
      df             = df_eco2,
      x_col          = "log_chla",
      y_col          = "log_secchi",
      x_label        = "Chl-a (µg/L)",
      y_label        = "Secchi (m)",
      eco_name       = eco,
      fit_result     = secchi_results[[eco]],
      walker_default = WALKER$secchi,
      filename       = sprintf("%s_secchi_from_chla.png", eco_safe)
    )
  }
}

# Statewide pooled plots
plot_loglog(
  df             = df_tp_chla,
  x_col          = "log_tp",
  y_col          = "log_chla",
  x_label        = "TP (µg/L)",
  y_label        = "Chl-a (µg/L)",
  eco_name       = "Statewide (all ecoregions)",
  fit_result     = chla_pooled,
  walker_default = WALKER$chla,
  filename       = "statewide_chla_from_tp.png"
)

plot_loglog(
  df             = df_chla_secchi,
  x_col          = "log_chla",
  y_col          = "log_secchi",
  x_label        = "Chl-a (µg/L)",
  y_label        = "Secchi (m)",
  eco_name       = "Statewide (all ecoregions)",
  fit_result     = secchi_pooled,
  walker_default = WALKER$secchi,
  filename       = "statewide_secchi_from_chla.png"
)

message(sprintf("Plots saved to: %s", plots_dir))

# =============================================================================
# 8. COEFFICIENT SUMMARY TABLE
# =============================================================================

message("\nBuilding coefficient summary table...")

coeff_rows <- lapply(ok_coefficients, function(x) {
  data.frame(
    ecoregion          = x$ecoregion,
    chla_intercept     = round(x$chla$intercept, 4),
    chla_slope         = round(x$chla$slope, 4),
    chla_source        = x$chla$source,
    chla_r2            = round(x$chla$r_squared, 3),
    chla_n_obs         = x$chla$n_obs,
    chla_n_lakes       = x$chla$n_lakes,
    secchi_intercept   = round(x$secchi$intercept, 4),
    secchi_slope       = round(x$secchi$slope, 4),
    secchi_source      = x$secchi$source,
    secchi_r2          = round(x$secchi$r_squared, 3),
    secchi_n_obs       = x$secchi$n_obs,
    secchi_n_lakes     = x$secchi$n_lakes,
    walker_chla_int    = WALKER$chla$intercept,
    walker_chla_slope  = WALKER$chla$slope,
    walker_secchi_int  = WALKER$secchi$intercept,
    walker_secchi_slope= WALKER$secchi$slope,
    stringsAsFactors   = FALSE
  )
}) %>%
  dplyr::bind_rows() %>%
  dplyr::arrange(ecoregion)

message("\n=== FINAL COEFFICIENT TABLE ===")
print(coeff_rows %>%
        select(ecoregion, chla_intercept, chla_slope, chla_source,
               chla_r2, secchi_intercept, secchi_slope, secchi_source,
               secchi_r2))

# =============================================================================
# 9. WRITE CALIBRATION REPORT
# =============================================================================

message("\nWriting calibration report...")

wb <- openxlsx::createWorkbook()

# Final coefficients
openxlsx::addWorksheet(wb, "Final_Coefficients", tabColour = "#2E75B6")
openxlsx::writeDataTable(wb, "Final_Coefficients", coeff_rows,
                          tableStyle = "TableStyleMedium2")
openxlsx::setColWidths(wb, "Final_Coefficients",
                        cols = seq_len(ncol(coeff_rows)), widths = "auto")

# Raw ecoregion fits — Chl-a
chla_fits <- dplyr::bind_rows(chla_results) %>%
  dplyr::bind_rows(as.data.frame(chla_pooled))
openxlsx::addWorksheet(wb, "Chla_Fits", tabColour = "#70AD47")
openxlsx::writeDataTable(wb, "Chla_Fits", chla_fits,
                          tableStyle = "TableStyleMedium2")
openxlsx::setColWidths(wb, "Chla_Fits",
                        cols = seq_len(ncol(chla_fits)), widths = "auto")

# Raw ecoregion fits — Secchi
secchi_fits <- dplyr::bind_rows(secchi_results) %>%
  dplyr::bind_rows(as.data.frame(secchi_pooled))
openxlsx::addWorksheet(wb, "Secchi_Fits", tabColour = "#70AD47")
openxlsx::writeDataTable(wb, "Secchi_Fits", secchi_fits,
                          tableStyle = "TableStyleMedium2")
openxlsx::setColWidths(wb, "Secchi_Fits",
                        cols = seq_len(ncol(secchi_fits)), widths = "auto")

# Raw calibration data
openxlsx::addWorksheet(wb, "Calibration_Data", tabColour = "#ED7D31")
openxlsx::writeDataTable(
  wb, "Calibration_Data",
  calib_raw %>%
    select(lake_name, eco_l3_name, sample_year, monitoring_location_id,
           tp_ugl, tp_n, chla_ugl, chla_n, chla_corrected,
           secchi_m, secchi_n, log_tp, log_chla, log_secchi),
  tableStyle = "TableStyleMedium2"
)

openxlsx::saveWorkbook(wb, report_path, overwrite = TRUE)
message(sprintf("Report saved: %s", report_path))

# =============================================================================
# 10. SAVE ok_coefficients AS R OBJECT
# This file is sourced by the package to populate the "oklahoma" coefficient set
# =============================================================================

message("\nSaving ok_coefficients R object...")

coeff_lines <- c(
  "# =============================================================================",
  "# okBATHTUB — Oklahoma Empirical Coefficients",
  "# Generated by ok_calibration.R",
  sprintf("# Date: %s", format(Sys.Date(), "%Y-%m-%d")),
  sprintf("# Calibration data: OWRB Lake Monitoring Program, %d-%d",
          min(calib_raw$sample_year), max(calib_raw$sample_year)),
  sprintf("# Lakes: %d | Observations: %d",
          n_distinct(calib_raw$lake_name), nrow(calib_raw)),
  "#",
  "# TP retention uses Walker (1996) Larsen-Mercier defaults.",
  "# Chl-a and Secchi coefficients are Oklahoma-specific where n >= 15 obs",
  "# across >= 5 lakes; statewide pooled otherwise; Walker as final fallback.",
  "# =============================================================================",
  "",
  ".oklahoma_coefficients <- function() {",
  ""
)

for (eco in names(ok_coefficients)) {
  x <- ok_coefficients[[eco]]
  eco_safe_r <- str_replace_all(eco, "[^A-Za-z0-9]", "_")

  coeff_lines <- c(coeff_lines,
    sprintf("  # %s", eco),
    sprintf("  # Chl-a:  source=%s, R2=%.3f, n=%d lakes / %d obs",
            x$chla$source,
            x$chla$r_squared %||% NA,
            x$chla$n_lakes, x$chla$n_obs),
    sprintf("  # Secchi: source=%s, R2=%.3f, n=%d lakes / %d obs",
            x$secchi$source,
            x$secchi$r_squared %||% NA,
            x$secchi$n_lakes, x$secchi$n_obs)
  )
}

# Write the full list structure
sink(coeff_r_path)
cat("# okBATHTUB Oklahoma Coefficients\n")
cat(sprintf("# Generated: %s\n", Sys.time()))
cat(sprintf("# Calibration: OWRB LMP %d-%d, %d lakes, %d obs\n\n",
            min(calib_raw$sample_year), max(calib_raw$sample_year),
            n_distinct(calib_raw$lake_name), nrow(calib_raw)))
cat(".oklahoma_coefficients <- function() {\n\n")
cat("  message(\n")
cat("    'Oklahoma-specific Chl-a and Secchi coefficients applied.\\n',\n")
cat("    'TP retention uses Walker (1996) defaults.\\n',\n")
cat(sprintf("    'Calibrated from OWRB LMP data (%d-%d, %d lakes, %d obs).'\n",
            min(calib_raw$sample_year), max(calib_raw$sample_year),
            n_distinct(calib_raw$lake_name), nrow(calib_raw)))
cat("  )\n\n")
cat("  list(\n")
cat("    tp_retention_form    = 'larsen_mercier',\n")
cat("    tn_settling_velocity = 10.0,\n\n")
cat("    # Ecoregion-specific Chl-a coefficients\n")
cat("    # log10(chla) = chla_intercept + chla_slope * log10(tp_inlake)\n")
cat("    chla_coefficients = list(\n")

eco_names <- names(ok_coefficients)
for (i in seq_along(eco_names)) {
  eco <- eco_names[i]
  x   <- ok_coefficients[[eco]]
  comma <- if (i < length(eco_names)) "," else ""
  cat(sprintf("      '%s' = list(\n", eco))
  cat(sprintf("        intercept = %s,\n",
              formatC(x$chla$intercept, digits = 6, format = "f")))
  cat(sprintf("        slope     = %s,\n",
              formatC(x$chla$slope, digits = 6, format = "f")))
  cat(sprintf("        source    = '%s',\n", x$chla$source))
  cat(sprintf("        r_squared = %s,\n",
              ifelse(is.na(x$chla$r_squared), "NA",
                     formatC(x$chla$r_squared, digits = 4, format = "f"))))
  cat(sprintf("        n_obs     = %d,\n", x$chla$n_obs))
  cat(sprintf("        n_lakes   = %d\n", x$chla$n_lakes))
  cat(sprintf("      )%s\n", comma))
}

cat("    ),\n\n")
cat("    # Ecoregion-specific Secchi coefficients\n")
cat("    # log10(secchi) = secchi_intercept + secchi_slope * log10(chla)\n")
cat("    secchi_coefficients = list(\n")

for (i in seq_along(eco_names)) {
  eco <- eco_names[i]
  x   <- ok_coefficients[[eco]]
  comma <- if (i < length(eco_names)) "," else ""
  cat(sprintf("      '%s' = list(\n", eco))
  cat(sprintf("        intercept = %s,\n",
              formatC(x$secchi$intercept, digits = 6, format = "f")))
  cat(sprintf("        slope     = %s,\n",
              formatC(x$secchi$slope, digits = 6, format = "f")))
  cat(sprintf("        source    = '%s',\n", x$secchi$source))
  cat(sprintf("        r_squared = %s,\n",
              ifelse(is.na(x$secchi$r_squared), "NA",
                     formatC(x$secchi$r_squared, digits = 4, format = "f"))))
  cat(sprintf("        n_obs     = %d,\n", x$secchi$n_obs))
  cat(sprintf("        n_lakes   = %d\n", x$secchi$n_lakes))
  cat(sprintf("      )%s\n", comma))
}

cat("    )\n")
cat("  )\n")
cat("}\n")
sink()

message(sprintf("Coefficients saved: %s", coeff_r_path))

# =============================================================================
# 11. CONSOLE SUMMARY
# =============================================================================

message("\n========================================")
message("  okBATHTUB Calibration Complete")
message("========================================")
message(sprintf("  Lakes used:      %d", n_distinct(calib_raw$lake_name)))
message(sprintf("  Observations:    %d", nrow(calib_raw)))
message(sprintf("  Years covered:   %d-%d",
                min(calib_raw$sample_year), max(calib_raw$sample_year)))
message(sprintf("  Ecoregions fit:  %d", length(ecoregions)))
message(sprintf("\n  Statewide Chl-a R2:    %.3f", chla_pooled$r_squared))
message(sprintf("  Statewide Secchi R2:   %.3f", secchi_pooled$r_squared))
message(sprintf("\n  Report:  %s", report_path))
message(sprintf("  Coefficients: %s", coeff_r_path))
message(sprintf("  Plots:   %s", plots_dir))
message("========================================")
