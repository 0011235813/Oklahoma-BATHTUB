#' Assemble tributary load inputs
#'
#' @description
#' `ok_load()` is the entry point for the okBATHTUB pipeline. It accepts
#' tributary hydraulic and nutrient loading data, validates inputs,
#' resolves the coefficient set, and returns an `okBATHTUB` object ready
#' to pass into [ok_hydraulics()].
#'
#' All concentration inputs are volume-flow-weighted means representing
#' the period of analysis (typically an annual or seasonal average). If
#' multiple tributaries contribute to the reservoir, either aggregate
#' them manually before calling `ok_load()`, or use [ok_load_multi()] to
#' supply tributary data as a data frame.
#'
#' @param inflow_m3yr Numeric. Total annual tributary inflow volume
#'   (m^3/yr). Must be positive.
#' @param tp_inflow_ugl Numeric. Flow-weighted mean total phosphorus
#'   concentration of tributary inflow (ug/L). Must be non-negative.
#' @param tn_inflow_ugl Numeric. Flow-weighted mean total nitrogen
#'   concentration of tributary inflow (ug/L). Optional. Default `NULL`.
#' @param tss_inflow_mgl Numeric. Flow-weighted mean total suspended
#'   solids concentration (mg/L). Optional. Default `NULL`.
#' @param segment_label Character. Optional label for this reservoir
#'   segment (e.g. `"riverine"`, `"lacustrine"`). Default `"main"`.
#' @param coefficients One of `"walker"` (default, Walker BATHTUB Model 1),
#'   `"vollenweider"` (Vollenweider 1976 / Larsen-Mercier 1976),
#'   `"oklahoma"` (Walker Model 1 retention plus Oklahoma-specific Chl-a /
#'   Secchi regressions), or a named list of custom coefficients.
#' @param ecoregion Character. EPA Level III ecoregion name
#'   (e.g. `"Cross Timbers"`). Used only when `coefficients = "oklahoma"`;
#'   silently ignored otherwise (with a message if the combination is
#'   suspicious). Default `NULL`.
#'
#' @return An `okBATHTUB` object at pipeline step `"load"`.
#'
#' @examples
#' # Minimum required inputs (TP only)
#' result <- ok_load(inflow_m3yr = 45e6, tp_inflow_ugl = 120)
#' print(result)
#'
#' # Full inputs with TN and TSS
#' result <- ok_load(
#'   inflow_m3yr    = 45e6,
#'   tp_inflow_ugl  = 120,
#'   tn_inflow_ugl  = 1800,
#'   tss_inflow_mgl = 35,
#'   segment_label  = "lacustrine"
#' )
#'
#' # Oklahoma ecoregion-specific coefficients
#' result <- ok_load(
#'   inflow_m3yr   = 45e6,
#'   tp_inflow_ugl = 120,
#'   coefficients  = "oklahoma",
#'   ecoregion     = "Cross Timbers"
#' )
#'
#' @seealso [ok_load_multi()], [ok_hydraulics()]
#' @export
ok_load <- function(inflow_m3yr,
                    tp_inflow_ugl,
                    tn_inflow_ugl  = NULL,
                    tss_inflow_mgl = NULL,
                    segment_label  = "main",
                    coefficients   = "walker",
                    ecoregion      = NULL) {

  # --- Input validation ---
  .assert_positive(inflow_m3yr,   "inflow_m3yr")
  .assert_nonneg(tp_inflow_ugl,   "tp_inflow_ugl")

  # TP = 0 is mathematically allowed but unphysical (no surface water on
  # Earth has zero phosphorus); warn the user without failing.
  if (tp_inflow_ugl < 1) {
    warning(sprintf(
      paste0("'tp_inflow_ugl' = %.3g ug/L is unusually low; ",
             "downstream Chl-a and Secchi predictions may be unreliable."),
      tp_inflow_ugl
    ), call. = FALSE)
  }

  if (!is.null(tn_inflow_ugl))
    .assert_nonneg(tn_inflow_ugl, "tn_inflow_ugl")
  if (!is.null(tss_inflow_mgl))
    .assert_nonneg(tss_inflow_mgl, "tss_inflow_mgl")

  if (!is.character(segment_label) || length(segment_label) != 1L)
    stop("'segment_label' must be a single character string.", call. = FALSE)

  # Warn on suspicious ecoregion + non-Oklahoma coefficient combinations
  if (!is.null(ecoregion) && is.character(coefficients) &&
      coefficients != "oklahoma") {
    message(sprintf(
      "okBATHTUB: 'ecoregion' is ignored when coefficients = '%s'. ",
      coefficients
    ))
  }

  # Resolve coefficient set
  coeff <- .resolve_coefficients(coefficients, ecoregion = ecoregion)

  # --- Derived load quantities ---
  # 1 ug/L = 1 mg/m3; ug/L * m3/yr = mg/yr; / 1e6 -> kg/yr
  tp_load_kgyr <- tp_inflow_ugl * inflow_m3yr / 1e6
  tn_load_kgyr <- if (!is.null(tn_inflow_ugl))
    tn_inflow_ugl * inflow_m3yr / 1e6 else NULL
  tss_load_kgyr <- if (!is.null(tss_inflow_mgl))
    tss_inflow_mgl * inflow_m3yr / 1e3 else NULL

  data <- list(
    inflow_m3yr    = inflow_m3yr,
    tp_inflow_ugl  = tp_inflow_ugl,
    tp_load_kgyr   = tp_load_kgyr,
    tn_inflow_ugl  = tn_inflow_ugl,
    tn_load_kgyr   = tn_load_kgyr,
    tss_inflow_mgl = tss_inflow_mgl,
    tss_load_kgyr  = tss_load_kgyr
  )

  new_okBATHTUB(
    data = data,
    step = "load",
    meta = list(
      segment_label = segment_label,
      coefficients  = if (is.character(coefficients)) coefficients else "custom",
      ecoregion     = ecoregion,
      coeff         = coeff
    )
  )
}


#' Assemble tributary loads from multiple tributaries
#'
#' @description
#' A convenience wrapper around [ok_load()] for reservoirs with more than
#' one tributary. Accepts a data frame of tributary data, computes
#' flow-weighted mean concentrations, sums inflow volumes, and calls
#' [ok_load()] with the aggregated values.
#'
#' @param tributaries A data frame with one row per tributary and these
#'   columns:
#'   \describe{
#'     \item{`inflow_m3yr`}{Annual inflow volume (m^3/yr). Required.}
#'     \item{`tp_inflow_ugl`}{Flow-weighted mean TP (ug/L). Required.}
#'     \item{`tn_inflow_ugl`}{Flow-weighted mean TN (ug/L). Optional.}
#'     \item{`tss_inflow_mgl`}{Flow-weighted mean TSS (mg/L). Optional.}
#'   }
#' @param segment_label Character. Segment label passed to [ok_load()].
#'   Default `"main"`.
#' @param coefficients Coefficient set. Default `"walker"`.
#' @param ecoregion EPA Level III ecoregion name. Default `NULL`.
#'   Passed through to [ok_load()].
#'
#' @return An `okBATHTUB` object at pipeline step `"load"`.
#'
#' @examples
#' tribs <- data.frame(
#'   inflow_m3yr   = c(30e6, 15e6),
#'   tp_inflow_ugl = c(110,  145),
#'   tn_inflow_ugl = c(1600, 2100)
#' )
#' result <- ok_load_multi(tribs)
#'
#' @seealso [ok_load()]
#' @export
ok_load_multi <- function(tributaries,
                          segment_label = "main",
                          coefficients  = "walker",
                          ecoregion     = NULL) {

  if (!is.data.frame(tributaries))
    stop("'tributaries' must be a data frame.", call. = FALSE)

  required_cols <- c("inflow_m3yr", "tp_inflow_ugl")
  missing_cols  <- setdiff(required_cols, names(tributaries))
  if (length(missing_cols) > 0L)
    stop(sprintf("'tributaries' is missing required columns: %s",
                 paste(missing_cols, collapse = ", ")),
         call. = FALSE)

  total_inflow <- sum(tributaries$inflow_m3yr, na.rm = TRUE)
  if (!is.finite(total_inflow) || total_inflow <= 0)
    stop("Sum of tributary inflows must be a positive finite number.",
         call. = FALSE)

  # Flow-weighted means
  fwm_tp <- sum(tributaries$tp_inflow_ugl * tributaries$inflow_m3yr,
                na.rm = TRUE) / total_inflow

  fwm_tn <- if ("tn_inflow_ugl" %in% names(tributaries) &&
                any(!is.na(tributaries$tn_inflow_ugl))) {
    sum(tributaries$tn_inflow_ugl * tributaries$inflow_m3yr,
        na.rm = TRUE) / total_inflow
  } else NULL

  fwm_tss <- if ("tss_inflow_mgl" %in% names(tributaries) &&
                 any(!is.na(tributaries$tss_inflow_mgl))) {
    sum(tributaries$tss_inflow_mgl * tributaries$inflow_m3yr,
        na.rm = TRUE) / total_inflow
  } else NULL

  ok_load(
    inflow_m3yr    = total_inflow,
    tp_inflow_ugl  = fwm_tp,
    tn_inflow_ugl  = fwm_tn,
    tss_inflow_mgl = fwm_tss,
    segment_label  = segment_label,
    coefficients   = coefficients,
    ecoregion      = ecoregion
  )
}
