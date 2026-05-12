#' Estimate nutrient retention coefficients
#'
#' @description
#' `ok_retention()` computes the fraction of incoming total phosphorus
#' (TP) and total nitrogen (TN) that is retained within the reservoir.
#' The retention coefficient is defined as
#' \deqn{R = 1 - C_{lake}/C_{in}}
#' and is applied in [ok_inlake()] to predict in-lake nutrient
#' concentrations.
#'
#' Three retention model families are supported, selected via the
#' `coefficients` argument to [ok_load()]:
#'
#' \describe{
#'   \item{`"walker_model1"` (Walker BATHTUB Model 1, default)}{
#'     Second-order available-phosphorus sedimentation
#'     (Walker 1985, 1996). The mass balance solution is
#'     \deqn{C_{lake} = \frac{-1 + \sqrt{1 + 4 K A_1 C_{in} \tau}}{2 K A_1 \tau}}
#'     where \eqn{A_1 = 0.17 \cdot Q_s/(Q_s + 13.3)} for TP and
#'     \eqn{B_1 = 0.0045 \cdot Q_s/(Q_s + 7.2)} for TN, with
#'     \eqn{Q_s = \max(Z/T,\,4)}.}
#'   \item{`"vollenweider"` (Vollenweider 1976 / Larsen-Mercier 1976)}{
#'     First-order hydraulic-residence model:
#'     \deqn{R_{TP} = \frac{1}{1 + 1/\sqrt{\tau}}}
#'     Mathematically equivalent to Walker BATHTUB Model 5 (Northern
#'     Lakes). Single parameter (residence time), no ortho-P data
#'     required. Walker (1996) notes this form is not calibrated to
#'     Corps of Engineers reservoir data.}
#'   \item{`"settling_velocity"` (used as TN companion to vollenweider)}{
#'     \deqn{R = v_s / (v_s + q_s)}
#'     where \eqn{v_s} is an apparent settling velocity (m/yr).}
#' }
#'
#' @param x An `okBATHTUB` object produced by [ok_hydraulics()].
#' @param tp_retention_override Numeric between 0 and 1. If supplied,
#'   bypasses the equation and uses this value directly. Useful when
#'   observed retention is available from paired inflow/outflow monitoring.
#'   Default `NULL`.
#' @param tn_retention_override Numeric between 0 and 1. Same as above for
#'   TN. Default `NULL`.
#'
#' @return An `okBATHTUB` object at pipeline step `"retention"` with the
#'   following fields added to `$data`:
#'   \describe{
#'     \item{`tp_retention_coeff`}{TP retention coefficient (0-1).}
#'     \item{`tn_retention_coeff`}{TN retention coefficient (0-1), or
#'       `NULL` if TN was not supplied.}
#'     \item{`tp_retention_form`, `tn_retention_form`}{Character. Which
#'       retention equation was applied.}
#'   }
#'
#' @examples
#' result <- ok_load(
#'   inflow_m3yr   = 45e6,
#'   tp_inflow_ugl = 120,
#'   tn_inflow_ugl = 1800
#' ) |>
#'   ok_hydraulics(surface_area_ha = 890, mean_depth_m = 4.2) |>
#'   ok_retention()
#' print(result)
#'
#' # Vollenweider / Larsen-Mercier retention
#' result_v <- ok_load(
#'   inflow_m3yr   = 45e6,
#'   tp_inflow_ugl = 120,
#'   coefficients  = "vollenweider"
#' ) |>
#'   ok_hydraulics(surface_area_ha = 890, mean_depth_m = 4.2) |>
#'   ok_retention()
#'
#' # Observed retention coefficient override
#' result_obs <- ok_load(inflow_m3yr = 45e6, tp_inflow_ugl = 120) |>
#'   ok_hydraulics(surface_area_ha = 890, mean_depth_m = 4.2) |>
#'   ok_retention(tp_retention_override = 0.42)
#'
#' @seealso [ok_hydraulics()], [ok_inlake()]
#' @export
ok_retention <- function(x,
                         tp_retention_override = NULL,
                         tn_retention_override = NULL) {

  assert_okBATHTUB(x, required_step = "retention")

  d     <- x$data
  coeff <- x$meta$coeff

  tau <- d$hydraulic_residence_time_yr
  qs  <- d$areal_water_load_myr
  z   <- d$mean_depth_m

  # --- TP retention ---
  if (!is.null(tp_retention_override)) {
    .assert_nonneg(tp_retention_override, "tp_retention_override")
    if (tp_retention_override > 1)
      stop("'tp_retention_override' must be between 0 and 1.", call. = FALSE)
    r_tp        <- tp_retention_override
    tp_form_out <- "observed_override"
    tp_lake_w1  <- d$tp_inflow_ugl * (1 - r_tp)

  } else {
    tp_form <- coeff$tp_retention_form %||% "walker_model1"
    tp_form_out <- tp_form

    switch(tp_form,

      # Walker BATHTUB Model 1: second-order, available-P
      walker_model1 = {
        a1_num       <- coeff$tp_a1_num       %||% 0.17
        a1_denom_add <- coeff$tp_a1_denom_add %||% 13.3
        qs_min       <- coeff$tp_qs_min       %||% 4.0
        K            <- coeff$tp_calib_factor %||% 1.0
        qs_eff       <- max(z / tau, qs_min)
        a1           <- a1_num * qs_eff / (qs_eff + a1_denom_add)
        Pi           <- d$tp_inflow_ugl
        # Quadratic mass balance solution:
        #   K*A1*T * P^2 + P - Pi = 0  ->  P = (-1 + sqrt(1 + 4*K*A1*Pi*T)) / (2*K*A1*T)
        # Note on units: A1 carries implicit units of m^3/(mg*yr). The numeric
        # value 0.17 is correct ONLY when TP is in mg/m^3 (= ug/L) and tau is
        # in years. Do not substitute other unit systems without rescaling.
        # Defensive guard: K, a1, tau, and Pi are all guaranteed > 0 by upstream
        # validation in ok_load() and ok_hydraulics(), so the else branch always
        # executes in practice. The K*a1*tau<=0 case is here only as a numerical
        # safety net for pathological custom coefficient lists.
        if (K * a1 * tau <= 0 || Pi <= 0) {
          tp_lake_w1 <- 0
          r_tp       <- if (Pi > 0) 1 else 0
        } else {
          discr      <- 1 + 4 * K * a1 * Pi * tau
          tp_lake_w1 <- (-1 + sqrt(discr)) / (2 * K * a1 * tau)
          r_tp       <- 1 - tp_lake_w1 / Pi
        }
      },

      # Vollenweider (1976) / Larsen-Mercier (1976) -- first-order
      vollenweider = {
        if (tau <= 0) {
          r_tp <- 0
          tp_lake_w1 <- d$tp_inflow_ugl
        } else {
          r_tp <- 1 / (1 + 1 / sqrt(tau))
          tp_lake_w1 <- d$tp_inflow_ugl * (1 - r_tp)
        }
      },

      stop(sprintf(
        "Unknown tp_retention_form '%s'. Expected one of: 'walker_model1', 'vollenweider'.",
        tp_form
      ), call. = FALSE)
    )

    # Safety clamp for floating-point edge cases
    r_tp <- min(max(r_tp, 0), 1)
  }

  # --- TN retention ---
  r_tn        <- NULL
  tn_form_out <- NULL

  if (!is.null(d$tn_inflow_ugl)) {
    if (!is.null(tn_retention_override)) {
      .assert_nonneg(tn_retention_override, "tn_retention_override")
      if (tn_retention_override > 1)
        stop("'tn_retention_override' must be between 0 and 1.", call. = FALSE)
      r_tn        <- tn_retention_override
      tn_form_out <- "observed_override"

    } else {
      tn_form <- coeff$tn_retention_form %||% "walker_model1"
      tn_form_out <- tn_form

      switch(tn_form,

        walker_model1 = {
          b1_num       <- coeff$tn_b1_num       %||% 0.0045
          b1_denom_add <- coeff$tn_b1_denom_add %||% 7.2
          qs_min       <- coeff$tn_qs_min       %||% 4.0
          K            <- coeff$tn_calib_factor %||% 1.0
          qs_eff       <- max(z / tau, qs_min)
          b1           <- b1_num * qs_eff / (qs_eff + b1_denom_add)
          Ni           <- d$tn_inflow_ugl
          if (K * b1 * tau <= 0 || Ni <= 0) {
            r_tn <- if (Ni > 0) 1 else 0
          } else {
            discr     <- 1 + 4 * K * b1 * Ni * tau
            tn_lake   <- (-1 + sqrt(discr)) / (2 * K * b1 * tau)
            r_tn      <- 1 - tn_lake / Ni
          }
        },

        settling_velocity = {
          ks   <- coeff$tn_settling_velocity %||% 10.0
          r_tn <- ks / (ks + qs)
        },

        stop(sprintf(
          "Unknown tn_retention_form '%s'. Expected one of: 'walker_model1', 'settling_velocity'.",
          tn_form
        ), call. = FALSE)
      )

      r_tn <- min(max(r_tn, 0), 1)
    }
  }

  new_data <- c(
    d,
    list(
      tp_retention_coeff = r_tp,
      tn_retention_coeff = r_tn,
      tp_retention_form  = tp_form_out,
      tn_retention_form  = tn_form_out
    )
  )

  new_okBATHTUB(data = new_data, step = "retention", meta = x$meta)
}
