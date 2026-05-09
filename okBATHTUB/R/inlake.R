#' Predict in-lake water quality concentrations
#'
#' @description
#' \code{ok_inlake()} applies nutrient retention coefficients from
#' \code{ok_retention()} to predict in-lake total phosphorus (TP) and total
#' nitrogen (TN) concentrations, then uses empirical regression equations to
#' predict chlorophyll-a and Secchi depth from in-lake TP.
#'
#' @section In-lake TP and TN:
#' In-lake concentration is derived from the mass balance:
#' \deqn{C_{lake} = C_{in} \times (1 - R)}
#' where \eqn{C_{in}} is the inflow concentration (ug/L) and \eqn{R} is
#' the retention coefficient from \code{ok_retention()}.
#'
#' @section Chlorophyll-a from TP:
#' Log-log linear regression (Walker 1996 defaults):
#' \deqn{\log_{10}(\text{Chl-a}) = a + b \times \log_{10}(\text{TP}_{lake})}
#' Default coefficients: \eqn{a = -1.136}, \eqn{b = 1.449}.
#'
#' @section Secchi depth from chlorophyll-a:
#' Log-log linear regression (Walker 1996 defaults):
#' \deqn{\log_{10}(\text{Secchi}) = a + b \times \log_{10}(\text{Chl-a})}
#' Default coefficients: \eqn{a = 0.616}, \eqn{b = -0.473}.
#'
#' Note that Secchi depth in high-turbidity Oklahoma reservoirs is often
#' controlled more by inorganic suspended sediment than by algae. The
#' Oklahoma coefficient set will apply a TSS correction when those
#' coefficients are calibrated.
#'
#' @param x An \code{okBATHTUB} object produced by \code{ok_retention()}.
#' @param predict_chla Logical. Whether to predict chlorophyll-a from in-lake
#'   TP. Default \code{TRUE}.
#' @param predict_secchi Logical. Whether to predict Secchi depth from
#'   chlorophyll-a. Requires \code{predict_chla = TRUE}. Default \code{TRUE}.
#'
#' @return An \code{okBATHTUB} object at pipeline step \code{"inlake"},
#'   with the following fields added to \code{$data}:
#'   \describe{
#'     \item{\code{tp_inlake_ugl}}{Predicted in-lake TP (ug/L).}
#'     \item{\code{tn_inlake_ugl}}{Predicted in-lake TN (ug/L), or \code{NULL}
#'       if TN was not supplied.}
#'     \item{\code{chla_ugl}}{Predicted chlorophyll-a (ug/L), or \code{NULL}
#'       if \code{predict_chla = FALSE}.}
#'     \item{\code{secchi_m}}{Predicted Secchi depth (m), or \code{NULL} if
#'       \code{predict_secchi = FALSE} or \code{predict_chla = FALSE}.}
#'   }
#'
#' @examples
#' result <- ok_load(
#'   inflow_m3yr   = 45e6,
#'   tp_inflow_ugl = 120,
#'   tn_inflow_ugl = 1800
#' ) |>
#' ok_hydraulics(surface_area_ha = 890, mean_depth_m = 4.2) |>
#' ok_retention() |>
#' ok_inlake()
#' print(result)
#'
#' @seealso \code{\link{ok_retention}}, \code{\link{ok_tsi}}
#' @export
ok_inlake <- function(x,
                      predict_chla   = TRUE,
                      predict_secchi = TRUE) {

  assert_okBATHTUB(x, required_step = "inlake")

  d     <- x$data
  coeff <- x$meta$coeff

  # --- In-lake TP ---
  # Mass balance: C_lake = C_in * (1 - R_tp)
  tp_inlake <- d$tp_inflow_ugl * (1 - d$tp_retention_coeff)

  # Guard against negative values from floating point edge cases
  tp_inlake <- max(tp_inlake, 0)

  # --- In-lake TN ---
  tn_inlake <- if (!is.null(d$tn_inflow_ugl) && !is.null(d$tn_retention_coeff)) {
    max(d$tn_inflow_ugl * (1 - d$tn_retention_coeff), 0)
  } else NULL

  # --- Chlorophyll-a prediction ---
  # log10(chla) = a + b * log10(tp_inlake)
  # Coefficients: Oklahoma ecoregion-specific > Oklahoma statewide > Walker
  chla        <- NULL
  chla_source <- NULL
  if (predict_chla) {
    if (tp_inlake <= 0) {
      warning(
        "In-lake TP is zero or negative; chlorophyll-a prediction skipped.",
        call. = FALSE
      )
    } else {
      a           <- coeff$chla_intercept %||% -1.136
      b           <- coeff$chla_slope     %||%  1.449
      chla        <- 10^(a + b * log10(tp_inlake))
      chla_source <- coeff$chla_source    %||% "walker_1996"
    }
  }

  # --- Secchi depth prediction ---
  # log10(secchi) = a + b * log10(chla)
  secchi        <- NULL
  secchi_source <- NULL
  if (predict_secchi && !is.null(chla)) {
    if (chla <= 0) {
      warning(
        "Chlorophyll-a is zero or negative; Secchi depth prediction skipped.",
        call. = FALSE
      )
    } else {
      a             <- coeff$secchi_intercept %||%  0.616
      b             <- coeff$secchi_slope     %||% -0.473
      secchi        <- 10^(a + b * log10(chla))
      secchi_source <- coeff$secchi_source    %||% "walker_1996"
    }
  }

  new_data <- c(
    d,
    list(
      tp_inlake_ugl    = tp_inlake,
      tn_inlake_ugl    = tn_inlake,
      chla_ugl         = chla,
      chla_coeff_source = chla_source,
      secchi_m         = secchi,
      secchi_coeff_source = secchi_source,
      ecoregion_applied = coeff$ecoregion_applied %||% x$meta$coefficients
    )
  )

  new_okBATHTUB(data = new_data, step = "inlake", meta = x$meta)
}
