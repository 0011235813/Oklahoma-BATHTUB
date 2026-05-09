#' Compute Carlson Trophic State Indices
#'
#' @description
#' \code{ok_tsi()} computes Carlson (1977) Trophic State Index (TSI) values
#' from in-lake water quality predictions produced by \code{ok_inlake()} and
#' assigns an overall trophic state classification.
#'
#' @section TSI equations (Carlson 1977):
#' \deqn{TSI(TP)     = 14.42 \times \ln(TP)     + 4.15}
#' \deqn{TSI(Chl\text{-}a) = 9.81  \times \ln(Chl\text{-}a) + 30.6}
#' \deqn{TSI(Secchi) = 60.0  - 14.41 \times \ln(Secchi)}
#'
#' where TP is in ug/L, chlorophyll-a is in ug/L, and Secchi depth is in
#' metres.
#'
#' @section Trophic state classification:
#' TSI below 40: Oligotrophic (clear, low nutrients).
#' TSI 40 to 50: Mesotrophic (moderate nutrients and clarity).
#' TSI 50 to 70: Eutrophic (high nutrients, algal blooms possible).
#' TSI above 70: Hypereutrophic (severe enrichment, high HAB risk).
#'
#' Classification is based on the mean TSI across all available parameters.
#'
#' @param x An \code{okBATHTUB} object produced by \code{ok_inlake()}.
#' @param observed_tp_ugl Numeric. If supplied, computes TSI(TP) from this
#'   observed value instead of the model-predicted in-lake TP. Useful for
#'   comparing predicted vs. observed trophic state. Default \code{NULL}.
#' @param observed_chla_ugl Numeric. Observed chlorophyll-a (ug/L) to use
#'   instead of predicted. Default \code{NULL}.
#' @param observed_secchi_m Numeric. Observed Secchi depth (m) to use instead
#'   of predicted. Default \code{NULL}.
#'
#' @return An \code{okBATHTUB} object at pipeline step \code{"tsi"},
#'   with the following fields added to \code{$data}:
#'   \describe{
#'     \item{\code{tsi_tp}}{Carlson TSI computed from TP.}
#'     \item{\code{tsi_chla}}{Carlson TSI computed from chlorophyll-a, or
#'       \code{NULL} if chlorophyll-a was not predicted.}
#'     \item{\code{tsi_secchi}}{Carlson TSI computed from Secchi depth, or
#'       \code{NULL} if Secchi depth was not predicted.}
#'     \item{\code{tsi_mean}}{Mean TSI across all available parameters.}
#'     \item{\code{trophic_state}}{Character. Trophic state classification
#'       based on \code{tsi_mean}.}
#'   }
#'
#' @references
#' Carlson, R.E. (1977). A trophic state index for lakes.
#' \emph{Limnology and Oceanography}, 22(2), 361--369.
#'
#' @examples
#' result <- ok_load(
#'   inflow_m3yr   = 45e6,
#'   tp_inflow_ugl = 120,
#'   tn_inflow_ugl = 1800
#' ) |>
#' ok_hydraulics(surface_area_ha = 890, mean_depth_m = 4.2) |>
#' ok_retention() |>
#' ok_inlake()   |>
#' ok_tsi()
#'
#' summary(result)
#'
#' # With observed values for comparison
#' result_obs <- ok_load(
#'   inflow_m3yr   = 45e6,
#'   tp_inflow_ugl = 120
#' ) |>
#' ok_hydraulics(surface_area_ha = 890, mean_depth_m = 4.2) |>
#' ok_retention() |>
#' ok_inlake()   |>
#' ok_tsi(observed_tp_ugl = 85, observed_chla_ugl = 22, observed_secchi_m = 0.8)
#'
#' @seealso \code{\link{ok_inlake}}, \code{\link{ok_tsi_observed}}
#' @export
ok_tsi <- function(x,
                   observed_tp_ugl    = NULL,
                   observed_chla_ugl  = NULL,
                   observed_secchi_m  = NULL) {

  assert_okBATHTUB(x, required_step = "tsi")

  d <- x$data

  # Select TP source: observed override takes priority
  tp_for_tsi <- if (!is.null(observed_tp_ugl)) {
    .assert_positive(observed_tp_ugl, "observed_tp_ugl")
    observed_tp_ugl
  } else {
    d$tp_inlake_ugl
  }

  chla_for_tsi <- if (!is.null(observed_chla_ugl)) {
    .assert_positive(observed_chla_ugl, "observed_chla_ugl")
    observed_chla_ugl
  } else {
    d$chla_ugl
  }

  secchi_for_tsi <- if (!is.null(observed_secchi_m)) {
    .assert_positive(observed_secchi_m, "observed_secchi_m")
    observed_secchi_m
  } else {
    d$secchi_m
  }

  # --- TSI(TP) ---
  tsi_tp <- if (!is.null(tp_for_tsi) && tp_for_tsi > 0) {
    14.42 * log(tp_for_tsi) + 4.15
  } else {
    warning("TP value unavailable or non-positive; TSI(TP) not computed.",
            call. = FALSE)
    NULL
  }

  # --- TSI(Chl-a) ---
  tsi_chla <- if (!is.null(chla_for_tsi) && chla_for_tsi > 0) {
    9.81 * log(chla_for_tsi) + 30.6
  } else NULL

  # --- TSI(Secchi) ---
  tsi_secchi <- if (!is.null(secchi_for_tsi) && secchi_for_tsi > 0) {
    60.0 - 14.41 * log(secchi_for_tsi)
  } else NULL

  # --- Mean TSI across available parameters ---
  tsi_vals <- c(tsi_tp, tsi_chla, tsi_secchi)
  tsi_mean <- if (length(tsi_vals) > 0) mean(tsi_vals, na.rm = TRUE) else NULL

  # --- Trophic state classification (Carlson 1977 breakpoints) ---
  trophic_state <- if (!is.null(tsi_mean)) {
    .classify_trophic_state(tsi_mean)
  } else NULL

  new_data <- c(
    d,
    list(
      tsi_tp        = tsi_tp,
      tsi_chla      = tsi_chla,
      tsi_secchi    = tsi_secchi,
      tsi_mean      = tsi_mean,
      trophic_state = trophic_state
    )
  )

  new_okBATHTUB(data = new_data, step = "tsi", meta = x$meta)
}


#' Classify trophic state from mean TSI
#' @keywords internal
.classify_trophic_state <- function(tsi) {
  if (tsi < 40)       return("Oligotrophic")
  if (tsi < 50)       return("Mesotrophic")
  if (tsi < 70)       return("Eutrophic")
  return("Hypereutrophic")
}


#' Compute Carlson TSI from observed values only
#'
#' @description
#' A standalone helper for computing Carlson TSI directly from observed water
#' quality measurements, without running the full BATHTUB prediction pipeline.
#' Useful for computing observed trophic state from AWQMS or LMP grab sample
#' data for comparison against modeled predictions.
#'
#' @param tp_ugl Numeric. Observed in-lake total phosphorus (ug/L). Optional.
#' @param chla_ugl Numeric. Observed chlorophyll-a (ug/L). Optional.
#' @param secchi_m Numeric. Observed Secchi depth (m). Optional.
#'
#' @return A named list with elements \code{tsi_tp}, \code{tsi_chla},
#'   \code{tsi_secchi}, \code{tsi_mean}, and \code{trophic_state}.
#'
#' @examples
#' ok_tsi_observed(tp_ugl = 85, chla_ugl = 22, secchi_m = 0.8)
#'
#' @references
#' Carlson, R.E. (1977). A trophic state index for lakes.
#' \emph{Limnology and Oceanography}, 22(2), 361--369.
#'
#' @export
ok_tsi_observed <- function(tp_ugl    = NULL,
                            chla_ugl  = NULL,
                            secchi_m  = NULL) {

  if (is.null(tp_ugl) && is.null(chla_ugl) && is.null(secchi_m))
    stop("At least one of tp_ugl, chla_ugl, or secchi_m must be supplied.",
         call. = FALSE)

  tsi_tp <- if (!is.null(tp_ugl)) {
    .assert_positive(tp_ugl, "tp_ugl")
    14.42 * log(tp_ugl) + 4.15
  } else NULL

  tsi_chla <- if (!is.null(chla_ugl)) {
    .assert_positive(chla_ugl, "chla_ugl")
    9.81 * log(chla_ugl) + 30.6
  } else NULL

  tsi_secchi <- if (!is.null(secchi_m)) {
    .assert_positive(secchi_m, "secchi_m")
    60.0 - 14.41 * log(secchi_m)
  } else NULL

  tsi_vals <- c(tsi_tp, tsi_chla, tsi_secchi)
  tsi_mean <- mean(tsi_vals, na.rm = TRUE)

  list(
    tsi_tp        = tsi_tp,
    tsi_chla      = tsi_chla,
    tsi_secchi    = tsi_secchi,
    tsi_mean      = tsi_mean,
    trophic_state = .classify_trophic_state(tsi_mean)
  )
}
