#' Assemble tributary load inputs
#'
#' @description
#' \code{ok_load()} is the entry point for the okBATHTUB pipeline. It
#' accepts tributary hydraulic and nutrient loading data, validates all
#' inputs, converts to consistent internal units, and returns an
#' \code{okBATHTUB} object ready to pass into \code{ok_hydraulics()}.
#'
#' All concentration inputs are volume-flow-weighted means representing the
#' period of analysis (typically an annual or seasonal average). If multiple
#' tributaries contribute to the reservoir, aggregate them to a single
#' combined inflow before calling \code{ok_load()}, or use
#' \code{ok_load_multi()} to supply tributary data as a data frame.
#'
#' @param inflow_m3yr Numeric. Total annual tributary inflow volume (m\eqn{^3}/yr).
#'   Must be positive. To convert from acre-feet/yr multiply by 1233.48.
#'   To convert from cfs annual mean multiply by 28.3168 * 86400 * 365.
#' @param tp_inflow_ugl Numeric. Flow-weighted mean total phosphorus
#'   concentration of tributary inflow (ug/L). Must be non-negative.
#' @param tn_inflow_ugl Numeric. Flow-weighted mean total nitrogen
#'   concentration of tributary inflow (ug/L). Must be non-negative.
#'   Pass \code{NULL} (default) if TN data are unavailable; TN predictions
#'   will be skipped downstream.
#' @param tss_inflow_mgl Numeric. Flow-weighted mean total suspended solids
#'   concentration of tributary inflow (mg/L). Optional. Used in future
#'   turbidity-corrected chlorophyll-a predictions. Default \code{NULL}.
#' @param segment_label Character. Optional label for this reservoir segment
#'   (e.g., \code{"riverine"}, \code{"lacustrine"}, \code{"segment_1"}).
#'   Useful when running multi-segment workflows. Default \code{"main"}.
#' @param coefficients One of \code{"walker"} (default), \code{"oklahoma"},
#'   or a named list of custom coefficients. The coefficient set is set here
#'   and propagated through the entire pipeline.
#' @param ecoregion Character. EPA Level III ecoregion name (e.g.
#'   \code{"Cross Timbers"}, \code{"Central Oklahoma/Texas Plains"}).
#'   Used when \code{coefficients = "oklahoma"}. Default \code{NULL}.
#'
#' @return An \code{okBATHTUB} object at pipeline step \code{"load"}.
#'
#' @examples
#' # Minimum required inputs (TP only, no TN)
#' result <- ok_load(
#'   inflow_m3yr   = 45e6,
#'   tp_inflow_ugl = 120
#' )
#' print(result)
#'
#' # Full inputs with TN and TSS
#' result <- ok_load(
#'   inflow_m3yr   = 45e6,
#'   tp_inflow_ugl = 120,
#'   tn_inflow_ugl = 1800,
#'   tss_inflow_mgl = 35,
#'   segment_label = "lacustrine"
#' )
#'
#' @seealso \code{\link{ok_load_multi}}, \code{\link{ok_hydraulics}}
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

  if (!is.null(tn_inflow_ugl))
    .assert_nonneg(tn_inflow_ugl, "tn_inflow_ugl")

  if (!is.null(tss_inflow_mgl))
    .assert_nonneg(tss_inflow_mgl, "tss_inflow_mgl")

  if (!is.character(segment_label) || length(segment_label) != 1L)
    stop("'segment_label' must be a single character string.", call. = FALSE)

  # Resolve and validate coefficient set early so errors surface at ok_load()
  # For Oklahoma coefficients, pass ecoregion for region-specific lookup
  if (identical(coefficients, "oklahoma")) {
    coeff <- .oklahoma_coefficients(ecoregion = ecoregion)
  } else {
    coeff <- .resolve_coefficients(coefficients)
  }

  # --- Compute derived load quantities ---
  # TP load: concentration (ug/L) * volume (m3/yr) -> convert to kg/yr
  # 1 ug/L = 1 mg/m3, so ug/L * m3/yr = mg/yr; divide by 1e6 for kg/yr
  tp_load_kgyr <- tp_inflow_ugl * inflow_m3yr / 1e6

  tn_load_kgyr <- if (!is.null(tn_inflow_ugl)) {
    tn_inflow_ugl * inflow_m3yr / 1e6
  } else NULL

  tss_load_kgyr <- if (!is.null(tss_inflow_mgl)) {
    # mg/L = g/m3, so g/m3 * m3/yr = g/yr; divide by 1000 for kg/yr
    tss_inflow_mgl * inflow_m3yr / 1e3
  } else NULL

  # --- Assemble data list ---
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
#' A convenience wrapper around \code{ok_load()} for reservoirs with more than
#' one tributary. Accepts a data frame of tributary data, computes
#' flow-weighted mean concentrations, sums inflow volumes, and calls
#' \code{ok_load()} with the aggregated values.
#'
#' @param tributaries A data frame with one row per tributary and the following
#'   columns:
#'   \describe{
#'     \item{\code{inflow_m3yr}}{Annual inflow volume (m\eqn{^3}/yr). Required.}
#'     \item{\code{tp_inflow_ugl}}{Flow-weighted mean TP (ug/L). Required.}
#'     \item{\code{tn_inflow_ugl}}{Flow-weighted mean TN (ug/L). Optional.}
#'     \item{\code{tss_inflow_mgl}}{Flow-weighted mean TSS (mg/L). Optional.}
#'   }
#' @param segment_label Character. Segment label passed to \code{ok_load()}.
#'   Default \code{"main"}.
#' @param coefficients Coefficient set. Default \code{"walker"}.
#'
#' @return An \code{okBATHTUB} object at pipeline step \code{"load"}.
#'
#' @examples
#' tribs <- data.frame(
#'   inflow_m3yr   = c(30e6, 15e6),
#'   tp_inflow_ugl = c(110,  145),
#'   tn_inflow_ugl = c(1600, 2100)
#' )
#' result <- ok_load_multi(tribs)
#'
#' @seealso \code{\link{ok_load}}
#' @export
ok_load_multi <- function(tributaries,
                          segment_label = "main",
                          coefficients  = "walker") {

  if (!is.data.frame(tributaries))
    stop("'tributaries' must be a data frame.", call. = FALSE)

  required_cols <- c("inflow_m3yr", "tp_inflow_ugl")
  missing_cols  <- setdiff(required_cols, names(tributaries))
  if (length(missing_cols) > 0)
    stop(sprintf("'tributaries' is missing required columns: %s",
                 paste(missing_cols, collapse = ", ")),
         call. = FALSE)

  total_inflow <- sum(tributaries$inflow_m3yr, na.rm = TRUE)

  # Flow-weighted mean TP
  fwm_tp <- sum(tributaries$tp_inflow_ugl * tributaries$inflow_m3yr,
                na.rm = TRUE) / total_inflow

  # Flow-weighted mean TN (if present)
  fwm_tn <- if ("tn_inflow_ugl" %in% names(tributaries)) {
    sum(tributaries$tn_inflow_ugl * tributaries$inflow_m3yr,
        na.rm = TRUE) / total_inflow
  } else NULL

  # Flow-weighted mean TSS (if present)
  fwm_tss <- if ("tss_inflow_mgl" %in% names(tributaries)) {
    sum(tributaries$tss_inflow_mgl * tributaries$inflow_m3yr,
        na.rm = TRUE) / total_inflow
  } else NULL

  ok_load(
    inflow_m3yr    = total_inflow,
    tp_inflow_ugl  = fwm_tp,
    tn_inflow_ugl  = fwm_tn,
    tss_inflow_mgl = fwm_tss,
    segment_label  = segment_label,
    coefficients   = coefficients
  )
}
