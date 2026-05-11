#' okBATHTUB: Empirical Reservoir Eutrophication Modelling
#'
#' @description
#' okBATHTUB implements steady-state empirical reservoir water quality
#' modelling. The package provides three families of phosphorus retention
#' models, all of which feed into Carlson (1977) Trophic State Index
#' predictions:
#'
#' \describe{
#'   \item{\code{"walker"} (default)}{Walker (1985, 1996) BATHTUB Model 1
#'     second-order available-phosphorus sedimentation. This is the
#'     canonical default in the BATHTUB program and is calibrated to
#'     U.S. Army Corps of Engineers reservoir data.}
#'   \item{\code{"vollenweider"}}{Vollenweider (1976) / Larsen-Mercier (1976)
#'     hydraulic-residence retention,
#'     \eqn{C_{lake} = C_{in}/(1 + \sqrt{\tau})}. Equivalent to Walker
#'     BATHTUB Model 5 (Northern Lakes). Simpler and requires only
#'     hydraulic residence time; recommended when ortho-P / total-P
#'     partitioning is unknown.}
#'   \item{\code{"oklahoma"}}{Uses Walker Model 1 for nutrient retention
#'     combined with Oklahoma-specific chlorophyll-a and Secchi depth
#'     regression coefficients calibrated from publicly available state
#'     lake monitoring data.}
#' }
#'
#' Users may also pass a fully custom named list of coefficients.
#'
#' @section Important note on retention model fidelity:
#' Earlier versions of okBATHTUB (v0.1.0) used the Vollenweider /
#' Larsen-Mercier form as the default and labelled it as the
#' "Walker BATHTUB default." This was incorrect: Walker's BATHTUB
#' documentation identifies the second-order available-P model (Model 1
#' in his Table 2) as the calibrated default, with the Vollenweider /
#' Larsen-Mercier form available as an alternative (Model 5, "Northern
#' Lakes," explicitly flagged as not calibrated to CE reservoir data).
#' Starting in v0.1.1, \code{coefficients = "walker"} correctly invokes
#' Model 1, and the previous behaviour is available via
#' \code{coefficients = "vollenweider"}.
#'
#' @section Core pipeline:
#' The standard single-segment workflow is:
#' \preformatted{
#' ok_load() |> ok_hydraulics() |> ok_retention() |> ok_inlake() |> ok_tsi()
#' }
#'
#' For multi-segment reservoirs, pass the result of one segment's
#' \code{ok_inlake()} into the next via \code{ok_segment()}.
#'
#' @references
#' Carlson, R.E. (1977). A trophic state index for lakes.
#'   Limnology and Oceanography, 22(2), 361-369.
#'
#' Larsen, D.P. and Mercier, H.T. (1976). Phosphorus retention capacity
#'   of lakes. Journal of the Fisheries Research Board of Canada, 33(8),
#'   1742-1750.
#'
#' Vollenweider, R.A. (1976). Advances in defining critical loading
#'   levels for phosphorus in lake eutrophication. Memorie dell'Istituto
#'   Italiano di Idrobiologia, 33, 53-83.
#'
#' Walker, W.W. (1985). Empirical methods for predicting eutrophication
#'   in impoundments; Report 3, Phase III: Model refinements. Technical
#'   Report E-81-9, U.S. Army Engineer Waterways Experiment Station.
#'
#' Walker, W.W. (1996). Simplified Procedures for Eutrophication
#'   Assessment and Prediction: User Manual. Instruction Report W-96-2,
#'   U.S. Army Engineer Waterways Experiment Station.
#'
#' @keywords internal
"_PACKAGE"


# ---------------------------------------------------------------------------
# S3 class constructors and methods
# ---------------------------------------------------------------------------

#' Construct an okBATHTUB result object
#'
#' Internal constructor for the `okBATHTUB` S3 class. All pipeline
#' functions return and accept objects of this class, enabling pipe-based
#' workflows. Users do not call this function directly.
#'
#' @param data A named list of model state values accumulated across steps.
#' @param step Character string naming the pipeline step that produced this
#'   object (e.g. `"load"`, `"hydraulics"`, `"retention"`, `"inlake"`,
#'   `"tsi"`).
#' @param meta A named list of metadata (segment label, coefficient set,
#'   etc.).
#'
#' @return An object of class `okBATHTUB`.
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
#'   progressed exactly to the step preceding `required_step`.
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

    if (length(required_idx) == 0) {
      stop(sprintf("Internal error: unknown required_step '%s'.",
                   required_step), call. = FALSE)
    }
    if (required_idx == 1L) {
      stop("Internal error: assert_okBATHTUB() should not be called ",
           "with required_step = 'load'.", call. = FALSE)
    }

    current_idx  <- which(valid_steps == x$step)
    too_early <- length(current_idx) == 0L || current_idx < required_idx - 1L
    too_late  <- length(current_idx) > 0L  && current_idx >= required_idx
    if (too_early || too_late) {
      prev_step <- valid_steps[required_idx - 1L]
      stop(
        sprintf(
          "ok_%s() requires that ok_%s() has been run first.",
          required_step, prev_step
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
#' @param x An `okBATHTUB` object.
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
  if (length(fields) == 0L) {
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
#' through the pipeline. Most informative after `ok_tsi()` has been run.
#'
#' @param object An `okBATHTUB` object.
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
  if (!is.null(object$meta$ecoregion))
    cat(sprintf("  Ecoregion    : %s\n", object$meta$ecoregion))
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
    cat(sprintf("  TP retention     : %.3f  (%s)\n",
                d$tp_retention_coeff,
                d$tp_retention_form %||% "walker_model1"))
    if (!is.null(d$tn_retention_coeff))
      cat(sprintf("  TN retention     : %.3f  (%s)\n",
                  d$tn_retention_coeff,
                  d$tn_retention_form %||% "walker_model1"))
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
      cat(sprintf("  TSI(mean)        : %.1f  (n = %d component%s)\n",
                  d$tsi_mean,
                  d$tsi_n %||% NA_integer_,
                  if ((d$tsi_n %||% 1L) == 1L) "" else "s"))
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


# ---------------------------------------------------------------------------
# Coefficient sets
# ---------------------------------------------------------------------------

#' Resolve coefficient set from argument
#'
#' Returns the appropriate named coefficient list given a `coefficients`
#' argument. Accepts `"walker"` (Walker BATHTUB Model 1, default),
#' `"vollenweider"` (Vollenweider/Larsen-Mercier, equivalent to Walker
#' Model 5), `"oklahoma"` (Walker Model 1 retention plus Oklahoma-specific
#' chlorophyll/Secchi regressions), or a user-supplied named list.
#'
#' @param coefficients One of `"walker"`, `"vollenweider"`, `"oklahoma"`,
#'   or a named list of custom coefficients.
#' @param ecoregion EPA Level III ecoregion name. Used only with
#'   `coefficients = "oklahoma"`.
#' @return A named list of coefficients.
#' @keywords internal
.resolve_coefficients <- function(coefficients, ecoregion = NULL) {
  if (is.list(coefficients))                    return(coefficients)
  if (identical(coefficients, "walker"))        return(.walker_coefficients())
  if (identical(coefficients, "vollenweider"))  return(.vollenweider_coefficients())
  if (identical(coefficients, "oklahoma"))      return(.oklahoma_coefficients(ecoregion))
  stop(
    "'coefficients' must be \"walker\", \"vollenweider\", \"oklahoma\", ",
    "or a named list.",
    call. = FALSE
  )
}


#' Walker (1985, 1996) BATHTUB Model 1 coefficient set
#'
#' Walker's calibrated second-order phosphorus and nitrogen sedimentation
#' models, fit to U.S. Army Corps of Engineers reservoir data. This is the
#' default option in the BATHTUB program.
#'
#' TP sedimentation rate (mg/m3-yr):
#' \deqn{W_s = K \cdot A_1 \cdot P^2}
#' where \eqn{A_1 = 0.17 \cdot Q_s / (Q_s + 13.3)} and
#' \eqn{Q_s = \max(Z/T,\,4)} m/yr. The mass balance solution for the
#' mixed-segment in-lake TP concentration is then:
#' \deqn{P = \frac{-1 + \sqrt{1 + 4 K A_1 P_i T}}{2 K A_1 T}}
#'
#' TN follows the same form with \eqn{B_1 = 0.0045 \cdot Q_s/(Q_s + 7.2)}.
#'
#' Chlorophyll-a and Secchi depth use Walker's nationally-derived log-log
#' regressions.
#'
#' @return Named list of coefficients.
#' @keywords internal
.walker_coefficients <- function() {
  list(
    # TP retention: Walker Model 1 (second-order, available-P)
    tp_retention_form  = "walker_model1",
    tp_a1_num          = 0.17,    # A1 = a1_num * Qs / (Qs + a1_denom_add)
    tp_a1_denom_add    = 13.3,
    tp_qs_min          = 4.0,     # Qs = max(Z/T, qs_min)
    tp_calib_factor    = 1.0,     # K (calibration; 1.0 default)

    # TN retention: Walker Model 1
    tn_retention_form  = "walker_model1",
    tn_b1_num          = 0.0045,
    tn_b1_denom_add    = 7.2,
    tn_qs_min          = 4.0,
    tn_calib_factor    = 1.0,

    # Chlorophyll-a from in-lake TP (log-log; Walker 1985 national)
    # log10(chla) = chla_intercept + chla_slope * log10(tp_inlake)
    chla_intercept     = -1.136,
    chla_slope         =  1.449,
    chla_source        = "walker_1985_national",

    # Secchi depth from chlorophyll-a (log-log; Walker 1985 national)
    secchi_intercept   =  0.616,
    secchi_slope       = -0.473,
    secchi_source      = "walker_1985_national"
  )
}


#' Vollenweider (1976) / Larsen-Mercier (1976) coefficient set
#'
#' First-order hydraulic-residence retention model:
#' \deqn{R_{TP} = \frac{1}{1 + 1/\sqrt{\tau}}}
#' equivalently \eqn{C_{lake} = C_{in} / (1 + \sqrt{\tau})}.
#'
#' This is mathematically equivalent to Walker (1996) BATHTUB Model 5
#' (Northern Lakes), which Walker explicitly notes is **not** calibrated to
#' Corps of Engineers reservoir data and "likely to require calibration to
#' site-specific data." It is offered here for users who want a parsimonious
#' single-parameter retention model, particularly when ortho-P / total-P
#' partitioning information is unavailable.
#'
#' @return Named list of coefficients.
#' @keywords internal
.vollenweider_coefficients <- function() {
  list(
    tp_retention_form    = "vollenweider",
    # TN: simple fixed apparent settling velocity (m/yr)
    tn_retention_form    = "settling_velocity",
    tn_settling_velocity = 10.0,

    # Walker national Chl-a / Secchi regressions
    chla_intercept       = -1.136,
    chla_slope           =  1.449,
    chla_source          = "walker_1985_national",
    secchi_intercept     =  0.616,
    secchi_slope         = -0.473,
    secchi_source        = "walker_1985_national"
  )
}


#' Oklahoma-calibrated chlorophyll/Secchi coefficient set
#'
#' Returns Oklahoma-specific empirical chlorophyll-a and Secchi depth
#' regression coefficients calibrated from publicly available state lake
#' monitoring data, layered on top of Walker (1985, 1996) BATHTUB Model 1
#' nutrient retention. Ecoregion-specific regressions are applied where
#' calibration support is sufficient (n >= 15 observations, n >= 5 lakes,
#' R^2 >= 0.25); a statewide pooled regression is used otherwise.
#'
#' @section Calibration data:
#' Calibration used 2000-2024 growing-season (May-October) surface grab
#' samples from publicly available state lake monitoring data, aggregated
#' to lake-station-year means requiring at least 3 samples per parameter,
#' then filtered to records with valid TP, chlorophyll-a, and Secchi
#' values (joint filter). The same filtered dataset feeds both the
#' Chl-a-from-TP and Secchi-from-Chl-a regressions, so per-ecoregion
#' n values are identical across the two fits. The statewide pooled
#' total (n=250 observations from 82 lakes) is larger than the sum of
#' the per-ecoregion fits because the pooled fit includes records from
#' ecoregions where the per-ecoregion fit did not meet the sample size
#' threshold (e.g. Ouachita Mountains, Central Great Plains). See
#' `data-raw/CALIBRATION_README.md` and
#' `data-raw/ok_calibration_report.xlsx` in the package source for full
#' provenance.
#'
#' @section Ecoregions with ecoregion-specific fits:
#' \itemize{
#'   \item Cross Timbers (Chl-a and Secchi)
#'   \item Central Oklahoma/Texas Plains (Chl-a and Secchi)
#'   \item Ozark Highlands (Chl-a only; Secchi fit fell below
#'     R^2 = 0.25 threshold and uses statewide pooled)
#' }
#' All other ecoregions use the statewide pooled coefficients.
#' Arkansas Valley Chl-a was rejected at R^2 = 0.120 due to inorganic
#' turbidity decoupling the TP-Chl-a relationship; see the calibration
#' README for context.
#'
#' @param ecoregion Character. EPA Level III ecoregion name. If `NULL`,
#'   returns the statewide pooled coefficients.
#' @return Named list of coefficients compatible with all okBATHTUB model
#'   functions.
#' @keywords internal
.oklahoma_coefficients <- function(ecoregion = NULL) {

  # ---------------------------------------------------------------------
  # Ecoregion-specific Chl-a coefficients
  # log10(chla) = chla_intercept + chla_slope * log10(tp_inlake)
  #
  # Source of truth: data-raw/ok_calibration_report.xlsx (Final_Coefficients).
  # These values are produced by data-raw/ok_calibration.R from publicly
  # available state lake monitoring data (2000-2024).
  # ---------------------------------------------------------------------
  chla_eco <- list(
    "Cross Timbers" = list(
      intercept = 0.2823, slope = 0.6171,
      source = "oklahoma_ecoregion_crosstimbers", r_squared = 0.391,
      n_obs_in_fit = 169, n_lakes_in_fit = 36
    ),
    "Central Oklahoma/Texas Plains" = list(
      intercept = 0.0485, slope = 0.7462,
      source = "oklahoma_ecoregion_centralplains", r_squared = 0.614,
      n_obs_in_fit = 24, n_lakes_in_fit = 10
    ),
    "Ozark Highlands" = list(
      intercept = -0.1684, slope = 0.8021,
      source = "oklahoma_ecoregion_ozark", r_squared = 0.609,
      n_obs_in_fit = 20, n_lakes_in_fit = 14
    )
  )

  # ---------------------------------------------------------------------
  # Ecoregion-specific Secchi coefficients
  # log10(secchi) = secchi_intercept + secchi_slope * log10(chla)
  #
  # Note: Ozark Highlands Secchi fit was R^2 = 0.228, below the
  # 0.25 acceptance threshold, so it is NOT included here and falls
  # back to the statewide pooled coefficients by code path.
  # ---------------------------------------------------------------------
  secchi_eco <- list(
    "Cross Timbers" = list(
      intercept = 0.4334, slope = -0.5235,
      source = "oklahoma_ecoregion_crosstimbers", r_squared = 0.359,
      n_obs_in_fit = 169, n_lakes_in_fit = 36
    ),
    "Central Oklahoma/Texas Plains" = list(
      intercept = 0.6489, slope = -0.5743,
      source = "oklahoma_ecoregion_centralplains", r_squared = 0.394,
      n_obs_in_fit = 24, n_lakes_in_fit = 10
    )
  )

  # Statewide pooled fallback
  chla_pooled <- list(intercept = 0.1505, slope = 0.6715,
                      source = "oklahoma_statewide", r_squared = 0.442,
                      n_obs_in_fit = 250, n_lakes_in_fit = 82)
  secchi_pooled <- list(intercept = 0.4730, slope = -0.5330,
                        source = "oklahoma_statewide", r_squared = 0.364,
                        n_obs_in_fit = 250, n_lakes_in_fit = 82)

  # Resolve ecoregion-specific or fall back
  eco_norm <- if (!is.null(ecoregion) && !is.na(ecoregion)) ecoregion else NULL

  if (!is.null(eco_norm) && eco_norm %in% names(chla_eco)) {
    chla_coeff <- chla_eco[[eco_norm]]
  } else {
    if (!is.null(eco_norm) && !(eco_norm %in% names(chla_eco))) {
      message(sprintf(
        paste0("okBATHTUB: no ecoregion-specific Chl-a coefficients for ",
               "'%s'; using Oklahoma statewide pooled."),
        eco_norm
      ))
    }
    chla_coeff <- chla_pooled
  }

  if (!is.null(eco_norm) && eco_norm %in% names(secchi_eco)) {
    secchi_coeff <- secchi_eco[[eco_norm]]
  } else {
    secchi_coeff <- secchi_pooled
  }

  # Start from Walker Model 1 retention defaults, then override Chl-a / Secchi
  base <- .walker_coefficients()
  base$chla_intercept   <- chla_coeff$intercept
  base$chla_slope       <- chla_coeff$slope
  base$chla_source      <- chla_coeff$source
  base$chla_r_squared   <- chla_coeff$r_squared
  base$chla_n_obs       <- chla_coeff$n_obs_in_fit
  base$chla_n_lakes     <- chla_coeff$n_lakes_in_fit
  base$secchi_intercept <- secchi_coeff$intercept
  base$secchi_slope     <- secchi_coeff$slope
  base$secchi_source    <- secchi_coeff$source
  base$secchi_r_squared <- secchi_coeff$r_squared
  base$secchi_n_obs     <- secchi_coeff$n_obs_in_fit
  base$secchi_n_lakes   <- secchi_coeff$n_lakes_in_fit
  base$ecoregion_applied <- if (is.null(eco_norm)) "statewide_pooled" else eco_norm

  base
}


# =============================================================================
# Global variable declarations
# =============================================================================

utils::globalVariables(c(
  # ggplot2 .data pronoun
  ".data",
  # plot column references
  "tp_inflow", "ymin", "ymax", "state",
  "tsi_y", "metric_label", "scenario", "meets_target",
  "value", "segment", "tsi_type",
  # dataset columns referenced in ok_reservoir / ok_reservoir_summary
  "ok_reservoirs", "data_quality", "surface_area_ha", "mean_depth_m",
  "eco_l3_name",
  # ok_lake_ecoregions dataset
  "ok_lake_ecoregions"
))
