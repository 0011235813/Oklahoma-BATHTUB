#' Estimate nutrient retention coefficients
#'
#' @description
#' \code{ok_retention()} computes the fraction of incoming total phosphorus
#' (TP) and total nitrogen (TN) that is retained within the reservoir through
#' sedimentation, biological uptake, and denitrification. These retention
#' coefficients are then applied in \code{ok_inlake()} to predict in-lake
#' nutrient concentrations.
#'
#' @section TP retention:
#' Two formulations are available via the coefficient set:
#'
#' \strong{Larsen-Mercier (default in Walker):}
#' \deqn{R_{TP} = \frac{1}{1 + 1/\sqrt{\tau}}}
#' where \eqn{\tau} is hydraulic residence time (yr). This form is appropriate
#' for most Oklahoma reservoirs and is the Walker BATHTUB default.
#'
#' \strong{Vollenweider settling velocity:}
#' \deqn{R_{TP} = \frac{v_s}{v_s + q_s}}
#' where \eqn{v_s} is an empirical phosphorus settling velocity (m/yr) and
#' \eqn{q_s} is the areal water load (m/yr). Used when
#' \code{tp_retention_form = "vollenweider"} in a custom coefficient list.
#'
#' @section TN retention:
#' TN retention uses the settling velocity form:
#' \deqn{R_{TN} = \frac{k_{TN}}{k_{TN} + q_s}}
#' where \eqn{k_{TN}} is the TN apparent settling velocity (m/yr, default 10
#' from Walker). This captures both sedimentation and denitrification losses.
#'
#' @param x An \code{okBATHTUB} object produced by \code{ok_hydraulics()}.
#' @param tp_retention_override Numeric between 0 and 1. If supplied, bypasses the
#'   equation and uses this value directly as the TP retention coefficient.
#'   Useful when observed retention is available from paired inflow/outflow
#'   monitoring. Default \code{NULL}.
#' @param tn_retention_override Numeric between 0 and 1. Same as above for TN.
#'   Default \code{NULL}.
#'
#' @return An \code{okBATHTUB} object at pipeline step \code{"retention"},
#'   with the following fields added to \code{$data}:
#'   \describe{
#'     \item{\code{tp_retention_coeff}}{TP retention coefficient (dimensionless,
#'       0--1). The fraction of incoming TP retained in the reservoir.}
#'     \item{\code{tn_retention_coeff}}{TN retention coefficient (0--1), or
#'       \code{NULL} if TN was not supplied in \code{ok_load()}.}
#'     \item{\code{tp_retention_form}}{Character. Which retention equation was
#'       applied.}
#'   }
#'
#' @examples
#' result <- ok_load(
#'   inflow_m3yr   = 45e6,
#'   tp_inflow_ugl = 120,
#'   tn_inflow_ugl = 1800
#' ) |>
#' ok_hydraulics(surface_area_ha = 890, mean_depth_m = 4.2) |>
#' ok_retention()
#' print(result)
#'
#' # Supply an observed TP retention coefficient directly
#' result2 <- ok_load(inflow_m3yr = 45e6, tp_inflow_ugl = 120) |>
#'   ok_hydraulics(surface_area_ha = 890, mean_depth_m = 4.2) |>
#'   ok_retention(tp_retention_override = 0.42)
#'
#' @seealso \code{\link{ok_hydraulics}}, \code{\link{ok_inlake}}
#' @export
ok_retention <- function(x,
                         tp_retention_override = NULL,
                         tn_retention_override = NULL) {

  assert_okBATHTUB(x, required_step = "retention")

  d     <- x$data
  coeff <- x$meta$coeff

  tau <- d$hydraulic_residence_time_yr
  qs  <- d$areal_water_load_myr

  # --- TP retention ---
  if (!is.null(tp_retention_override)) {
    if (!is.numeric(tp_retention_override) ||
        tp_retention_override < 0 ||
        tp_retention_override > 1)
      stop("'tp_retention_override' must be numeric and between 0 and 1.",
           call. = FALSE)
    r_tp   <- tp_retention_override
    r_form <- "observed_override"

  } else {
    r_form <- coeff$tp_retention_form %||% "larsen_mercier"

    r_tp <- switch(r_form,

      # Larsen-Mercier (Walker BATHTUB default)
      larsen_mercier = {
        1 / (1 + 1 / sqrt(tau))
      },

      # Vollenweider settling velocity form
      vollenweider = {
        vs <- coeff$tp_settling_velocity %||% {
          stop(
            "tp_settling_velocity must be specified in the coefficient list ",
            "when tp_retention_form = 'vollenweider'.",
            call. = FALSE
          )
        }
        vs / (vs + qs)
      },

      stop(sprintf(
        "Unknown tp_retention_form '%s'. Use 'larsen_mercier' or 'vollenweider'.",
        r_form
      ), call. = FALSE)
    )
  }

  # Clamp retention to [0, 1] as a safety guard
  r_tp <- min(max(r_tp, 0), 1)

  # --- TN retention ---
  r_tn <- NULL

  if (!is.null(d$tn_inflow_ugl)) {
    if (!is.null(tn_retention_override)) {
      if (!is.numeric(tn_retention_override) ||
          tn_retention_override < 0 ||
          tn_retention_override > 1)
        stop("'tn_retention_override' must be numeric and between 0 and 1.",
             call. = FALSE)
      r_tn <- tn_retention_override
    } else {
      k_tn <- coeff$tn_settling_velocity %||% 10.0
      r_tn <- k_tn / (k_tn + qs)
      r_tn <- min(max(r_tn, 0), 1)
    }
  }

  new_data <- c(
    d,
    list(
      tp_retention_coeff = r_tp,
      tn_retention_coeff = r_tn,
      tp_retention_form  = r_form
    )
  )

  new_okBATHTUB(data = new_data, step = "retention", meta = x$meta)
}
