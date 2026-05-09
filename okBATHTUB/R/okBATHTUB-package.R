#' okBATHTUB: Reservoir Eutrophication Modeling for Oklahoma Lakes
#'
#' @description
#' okBATHTUB implements steady-state empirical reservoir water quality modeling
#' based on Walker's BATHTUB framework, extended with Oklahoma-specific
#' calibration options for lakes in the OWRB Lake Monitoring Program.
#'
#' The package predicts in-lake total phosphorus (TP), total nitrogen (TN),
#' chlorophyll-a, and Secchi depth from tributary nutrient and hydraulic
#' loading inputs, and computes Carlson Trophic State Indices (TSI) from
#' those predictions.
#'
#' @section Core pipeline:
#' The standard single-segment workflow is:
#' \preformatted{
#' ok_load() |> ok_hydraulics() |> ok_retention() |> ok_inlake() |> ok_tsi()
#' }
#'
#' For multi-segment reservoirs, pass the result of one segment's
#' \code{ok_inlake()} into the next segment's \code{ok_load()} via
#' \code{ok_segment()}.
#'
#' @section Coefficient sets:
#' All model functions accept a \code{coefficients} argument. The default is
#' \code{"walker"}, which uses Walker's nationally-derived empirical
#' coefficients. Passing \code{"oklahoma"} substitutes Oklahoma Lake Monitoring
#' Program-derived coefficients where available. Users may also supply a named
#' list of custom coefficients.
#'
#' @references
#' Walker, W.W. (1996). Simplified Procedures for Eutrophication Assessment
#' and Prediction: User Manual. U.S. Army Corps of Engineers, Instruction
#' Report W-96-2.
#'
#' @docType package
#' @name okBATHTUB-package
"_PACKAGE"


# ---------------------------------------------------------------------------
# S3 class constructors and methods
# ---------------------------------------------------------------------------

#' Construct an okBATHTUB result object
#'
#' Internal constructor for the \code{okBATHTUB} S3 class. All pipeline
#' functions return and accept objects of this class, enabling pipe-based
#' workflows. Users do not call this function directly.
#'
#' @param data A named list of model state values accumulated across steps.
#' @param step Character string naming the pipeline step that produced this
#'   object (e.g., \code{"load"}, \code{"hydraulics"}, \code{"retention"},
#'   \code{"inlake"}, \code{"tsi"}).
#' @param meta A named list of metadata (segment label, coefficient set, etc.).
#'
#' @return An object of class \code{okBATHTUB}.
#' @keywords internal
new_okBATHTUB <- function(data, step, meta = list()) {
  structure(
    list(data = data, step = step, meta = meta),
    class = "okBATHTUB"
  )
}


#' Validate that an object is an okBATHTUB result
#'
#' @param x Object to check.
#' @param required_step If supplied, also checks that the pipeline has
#'   progressed at least to the step preceding \code{required_step}.
#' @keywords internal
assert_okBATHTUB <- function(x, required_step = NULL) {
  if (!inherits(x, "okBATHTUB")) {
    stop(
      "Expected an 'okBATHTUB' object. ",
      "Did you start the pipeline with ok_load()?",
      call. = FALSE
    )
  }
  if (!is.null(required_step)) {
    valid_steps  <- .pipeline_order()
    required_idx <- which(valid_steps == required_step)
    current_idx  <- which(valid_steps == x$step)
    # Reject if: step not recognised, too early, or already past this step
    # (e.g. passing a tsi result to ok_hydraulics is invalid)
    too_early <- length(current_idx) == 0 || current_idx < required_idx - 1L
    too_late  <- length(current_idx) > 0  && current_idx >= required_idx
    if (too_early || too_late) {
      stop(
        sprintf(
          "ok_%s() requires that ok_%s() has been run first.",
          required_step,
          valid_steps[required_idx - 1L]
        ),
        call. = FALSE
      )
    }
  }
  invisible(x)
}


#' Internal pipeline step order
#' @keywords internal
.pipeline_order <- function() {
  c("load", "hydraulics", "retention", "inlake", "tsi")
}


#' Print method for okBATHTUB objects
#'
#' @param x An \code{okBATHTUB} object.
#' @param ... Ignored.
#' @export
print.okBATHTUB <- function(x, ...) {
  cat("-- okBATHTUB Result --\n")
  cat(sprintf("  Pipeline step : %s\n", x$step))
  if (!is.null(x$meta$segment_label))
    cat(sprintf("  Segment       : %s\n", x$meta$segment_label))
  if (!is.null(x$meta$coefficients))
    cat(sprintf("  Coefficients  : %s\n", x$meta$coefficients))
  cat("\n")
  fields <- x$data
  if (length(fields) == 0) {
    cat("  (no data)\n")
    return(invisible(x))
  }
  max_len <- max(nchar(names(fields)))
  for (nm in names(fields)) {
    val <- fields[[nm]]
    if (is.numeric(val) && length(val) == 1L) {
      cat(sprintf("  %-*s : %.4g\n", max_len, nm, val))
    } else if (is.character(val) && length(val) == 1L) {
      cat(sprintf("  %-*s : %s\n",  max_len, nm, val))
    }
  }
  invisible(x)
}


#' Summary method for okBATHTUB objects
#'
#' Prints a formatted summary of all water quality predictions accumulated
#' through the pipeline. Most informative after \code{ok_tsi()} has been run.
#'
#' @param object An \code{okBATHTUB} object.
#' @param ... Ignored.
#' @export
summary.okBATHTUB <- function(object, ...) {
  d <- object$data

  cat("========================================\n")
  cat("  okBATHTUB Water Quality Summary\n")
  cat("========================================\n\n")

  if (!is.null(object$meta$segment_label))
    cat(sprintf("  Segment      : %s\n", object$meta$segment_label))
  cat(sprintf("  Coefficients : %s\n", object$meta$coefficients %||% "walker"))
  cat(sprintf("  Pipeline     : %s\n\n", object$step))

  if (!is.null(d$hydraulic_residence_time_yr)) {
    cat("  -- Hydraulics --\n")
    cat(sprintf("  Inflow           : %.3e m3/yr\n",  d$inflow_m3yr %||% NA))
    cat(sprintf("  Surface area     : %.1f ha\n",     d$surface_area_ha %||% NA))
    cat(sprintf("  Mean depth       : %.2f m\n",      d$mean_depth_m %||% NA))
    cat(sprintf("  Residence time   : %.3f yr\n",     d$hydraulic_residence_time_yr))
    cat(sprintf("  Areal water load : %.2f m/yr\n",   d$areal_water_load_myr %||% NA))
    cat("\n")
  }

  if (!is.null(d$tp_retention_coeff)) {
    cat("  -- Nutrient Retention --\n")
    cat(sprintf("  TP retention     : %.3f\n", d$tp_retention_coeff))
    if (!is.null(d$tn_retention_coeff))
      cat(sprintf("  TN retention     : %.3f\n", d$tn_retention_coeff))
    cat("\n")
  }

  if (!is.null(d$tp_inlake_ugl)) {
    cat("  -- In-Lake Predictions --\n")
    cat(sprintf("  TP               : %.1f ug/L\n", d$tp_inlake_ugl))
    if (!is.null(d$tn_inlake_ugl))
      cat(sprintf("  TN               : %.1f ug/L\n", d$tn_inlake_ugl))
    if (!is.null(d$chla_ugl))
      cat(sprintf("  Chlorophyll-a    : %.2f ug/L\n", d$chla_ugl))
    if (!is.null(d$secchi_m))
      cat(sprintf("  Secchi depth     : %.2f m\n", d$secchi_m))
    cat("\n")
  }

  if (!is.null(d$tsi_tp)) {
    cat("  -- Carlson Trophic State Index --\n")
    cat(sprintf("  TSI(TP)          : %.1f\n", d$tsi_tp))
    if (!is.null(d$tsi_chla))
      cat(sprintf("  TSI(Chl-a)       : %.1f\n", d$tsi_chla))
    if (!is.null(d$tsi_secchi))
      cat(sprintf("  TSI(Secchi)      : %.1f\n", d$tsi_secchi))
    if (!is.null(d$tsi_mean))
      cat(sprintf("  TSI(mean)        : %.1f\n", d$tsi_mean))
    if (!is.null(d$trophic_state))
      cat(sprintf("  Trophic state    : %s\n", d$trophic_state))
    cat("\n")
  }

  cat("========================================\n")
  invisible(object)
}


# ---------------------------------------------------------------------------
# Internal utility helpers
# ---------------------------------------------------------------------------

#' @noRd
`%||%` <- function(a, b) if (!is.null(a)) a else b

#' Assert single positive finite numeric
#' @keywords internal
.assert_positive <- function(x, name) {
  if (!is.numeric(x) || length(x) != 1L || !is.finite(x) || x <= 0)
    stop(sprintf("'%s' must be a single positive finite number.", name),
         call. = FALSE)
}

#' Assert single non-negative finite numeric
#' @keywords internal
.assert_nonneg <- function(x, name) {
  if (!is.numeric(x) || length(x) != 1L || !is.finite(x) || x < 0)
    stop(sprintf("'%s' must be a single non-negative finite number.", name),
         call. = FALSE)
}

#' Resolve coefficient set from argument
#'
#' Returns the appropriate named coefficient list given a \code{coefficients}
#' argument. Accepts \code{"walker"}, \code{"oklahoma"}, or a user-supplied
#' named list.
#'
#' @param coefficients One of \code{"walker"} (default), \code{"oklahoma"},
#'   or a named list of custom coefficients.
#' @return A named list of coefficients.
#' @keywords internal
.resolve_coefficients <- function(coefficients) {
  if (is.list(coefficients))   return(coefficients)
  if (identical(coefficients, "walker"))    return(.walker_coefficients())
  if (identical(coefficients, "oklahoma"))  return(.oklahoma_coefficients())
  stop(
    "'coefficients' must be \"walker\", \"oklahoma\", or a named list.",
    call. = FALSE
  )
}

#' Walker (1996) default coefficient set
#'
#' Nationally-derived empirical coefficients from Walker's BATHTUB model.
#' These are the default for all okBATHTUB model functions.
#'
#' @return Named list of coefficients.
#' @keywords internal
.walker_coefficients <- function() {
  list(
    # TP retention: Larsen-Mercier form
    # R_tp = 1 / (1 + 1 / sqrt(tau))  where tau = hydraulic residence time (yr)
    tp_retention_form    = "larsen_mercier",

    # TN retention: fixed settling velocity (m/yr)
    # R_tn = ks / (ks + qs)  where qs = areal water load (m/yr)
    tn_settling_velocity = 10.0,

    # Chlorophyll-a from in-lake TP (log-log linear regression)
    # log10(chla) = chla_intercept + chla_slope * log10(tp_inlake)
    chla_intercept = -1.136,
    chla_slope     =  1.449,

    # Secchi depth from chlorophyll-a (log-log linear regression)
    # log10(secchi) = secchi_intercept + secchi_slope * log10(chla)
    secchi_intercept =  0.616,
    secchi_slope     = -0.473
  )
}

#' Oklahoma Lake Monitoring Program calibrated coefficient set
#'
#' Returns Oklahoma-specific empirical coefficients calibrated from OWRB Lake
#' Monitoring Program data (2000-2024, 82 lakes, 250 observations).
#' Chlorophyll-a and Secchi depth regressions are ecoregion-specific where
#' sufficient data exist (n >= 15 obs, n >= 5 lakes, R2 >= 0.25); statewide
#' pooled otherwise. TP retention uses Walker (1996) Larsen-Mercier defaults.
#'
#' Ecoregion coefficients are selected by matching the \code{ecoregion}
#' argument to the calibrated coefficient list. If no match is found, the
#' statewide pooled coefficients are returned with a message.
#'
#' @param ecoregion Character. EPA Level III ecoregion name. If \code{NULL},
#'   returns the statewide pooled coefficients.
#' @return Named list of coefficients compatible with all okBATHTUB model
#'   functions.
#' @keywords internal
.oklahoma_coefficients <- function(ecoregion = NULL) {

  # Ecoregion-specific Chl-a coefficients
  # log10(chla) = intercept + slope * log10(tp_inlake)
  # Source: OWRB LMP calibration (ok_calibration.R)
  # Threshold: n >= 15 obs, >= 5 lakes, R2 >= 0.25
  chla_eco <- list(
    "Cross Timbers" = list(
      intercept = 0.2823, slope = 0.6171,
      source = "oklahoma_ecoregion", r_squared = 0.391,
      n_obs = 181, n_lakes = 40
    ),
    "Central Oklahoma/Texas Plains" = list(
      intercept = 0.0485, slope = 0.7462,
      source = "oklahoma_ecoregion", r_squared = 0.614,
      n_obs = 37, n_lakes = 10
    ),
    "Ozark Highlands" = list(
      intercept = -0.1684, slope = 0.8021,
      source = "oklahoma_ecoregion", r_squared = 0.609,
      n_obs = 20, n_lakes = 14
    )
    # All other ecoregions use statewide pooled (below)
  )

  # Ecoregion-specific Secchi coefficients
  # log10(secchi) = intercept + slope * log10(chla)
  secchi_eco <- list(
    "Cross Timbers" = list(
      intercept = 0.4334, slope = -0.5235,
      source = "oklahoma_ecoregion", r_squared = 0.359,
      n_obs = 265, n_lakes = 36
    ),
    "Central Oklahoma/Texas Plains" = list(
      intercept = 0.6489, slope = -0.5743,
      source = "oklahoma_ecoregion", r_squared = 0.394,
      n_obs = 26, n_lakes = 12
    )
    # Ozark Highlands Secchi R2=0.228 < 0.25 threshold - uses statewide pooled
    # Arkansas Valley Secchi R2=0.018 - uses statewide pooled
    # All other ecoregions use statewide pooled (below)
  )

  # Statewide pooled fallback
  # Fitted from all 250 observations across 82 Oklahoma lakes
  chla_pooled  <- list(intercept = 0.1505, slope = 0.6715,
                        source = "oklahoma_statewide", r_squared = 0.442,
                        n_obs = 250, n_lakes = 82)
  secchi_pooled <- list(intercept = 0.4730, slope = -0.5330,
                         source = "oklahoma_statewide", r_squared = 0.364,
                         n_obs = 250, n_lakes = 82)

  # Resolve ecoregion-specific or fall back to statewide pooled
  if (isTRUE(!is.null(ecoregion) && !is.na(ecoregion) && ecoregion %in% names(chla_eco))) {
    chla_coeff <- chla_eco[[ecoregion]]
  } else {
    if (isTRUE(!is.null(ecoregion) && !is.na(ecoregion) && !ecoregion %in% names(chla_eco))) {
      message(sprintf(
        paste0("okBATHTUB: No ecoregion-specific Chl-a coefficients for '%s'. ",
               "Oklahoma statewide pooled coefficients applied."),
        ecoregion
      ))
    }
    chla_coeff <- chla_pooled
  }

  if (isTRUE(!is.null(ecoregion) && !is.na(ecoregion) && ecoregion %in% names(secchi_eco))) {
    secchi_coeff <- secchi_eco[[ecoregion]]
  } else {
    secchi_coeff <- secchi_pooled
  }

  list(
    tp_retention_form    = "larsen_mercier",
    tn_settling_velocity = 10.0,
    chla_intercept       = chla_coeff$intercept,
    chla_slope           = chla_coeff$slope,
    chla_source          = chla_coeff$source,
    chla_r_squared       = chla_coeff$r_squared,
    secchi_intercept     = secchi_coeff$intercept,
    secchi_slope         = secchi_coeff$slope,
    secchi_source        = secchi_coeff$source,
    secchi_r_squared     = secchi_coeff$r_squared,
    ecoregion_applied    = if (is.null(ecoregion) || is.na(ecoregion)) "statewide_pooled" else ecoregion
  )
}


#' Resolve Oklahoma coefficients with ecoregion lookup
#'
#' Called internally by \code{ok_load()} when \code{coefficients = "oklahoma"}.
#' Extracts the ecoregion from the \code{meta} list if available.
#'
#' @param ecoregion Character or NULL. EPA Level III ecoregion name.
#' @return Named list of Oklahoma coefficients.
#' @keywords internal
.resolve_oklahoma <- function(ecoregion = NULL) {
  .oklahoma_coefficients(ecoregion = ecoregion)
}


# =============================================================================
# Global variable declarations
# Suppresses R CMD check "no visible binding" notes for dplyr/ggplot2 columns
# =============================================================================

utils::globalVariables(c(
  # ok_from_awqms column references
  "relative_depth", "result_depth_height", "characteristic_name",
  "result_value", "result_value_num", "detection_limit",
  "detection_condition", "param_group", "monitoring_location_id",
  "monitoring_location_name", "monitoring_location_latitude",
  "monitoring_location_longitude", "sample_date", "sample_year",
  "sample_month", "use_corrected", "chla_corrected_flag",
  "chla_corrected", "mean_value", "n", "lake_name",
  "eco_l3_code", "eco_l3_name",

  # ok_reservoir / ok_reservoirs
  "ok_reservoirs", "data_quality", "surface_area_ha", "mean_depth_m",
  "n_lakes",

  # ok_plot_* column references
  "tp_inflow", "ymin", "ymax", "xmin", "xmax", "state",
  "tsi_tp", "tsi_chla", "tsi_secchi", "tsi_type", "tsi_y",
  "metric", "metric_label", "scenario", "meets_target",
  "value", "point_color", "segment",

  # ggplot2 .data pronoun
  ".data",

  # ok_reservoir_summary
  "eco_l3_name",

  # pipe
  "%>%"
))


# =============================================================================
# Package-level imports
# Declared here so roxygen2 adds them to NAMESPACE correctly
# =============================================================================

#' @importFrom dplyr filter mutate select group_by summarise arrange
#'   left_join anti_join bind_rows distinct n n_distinct first all_of
#'   if_else recode case_when ungroup rowwise pull slice
#' @importFrom tidyr pivot_wider pivot_longer
#' @importFrom stringr str_remove str_trim str_replace_all
#' @importFrom utils head tail
NULL
