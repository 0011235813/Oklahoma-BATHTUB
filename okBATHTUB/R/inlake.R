#' Predict in-lake water quality concentrations
#'
#' @description
#' `ok_inlake()` applies nutrient retention coefficients from
#' [ok_retention()] to predict in-lake total phosphorus (TP) and total
#' nitrogen (TN) concentrations via the mass balance
#' \deqn{C_{lake} = C_{in} \times (1 - R)}
#' then uses empirical log-log regression to predict chlorophyll-a from
#' in-lake TP and Secchi depth from chlorophyll-a.
#'
#' Note on retention identity: when `coefficients = "walker"` (Walker
#' BATHTUB Model 1), the retention coefficient stored by [ok_retention()]
#' is back-calculated from Walker's quadratic mass balance solution so
#' that `C_lake = C_in * (1 - R)` exactly reproduces the Model 1 result.
#'
#' @section Chlorophyll-a from TP:
#' Log-log linear regression:
#' \deqn{\log_{10}(\text{Chl-a}) = a + b \times \log_{10}(\text{TP}_{lake})}
#' Default coefficients are Walker's nationally-derived values
#' (\eqn{a = -1.136}, \eqn{b = 1.449}); Oklahoma ecoregion-specific
#' values are applied when `coefficients = "oklahoma"`.
#'
#' @section Secchi depth from chlorophyll-a:
#' \deqn{\log_{10}(\text{Secchi}) = a + b \times \log_{10}(\text{Chl-a})}
#' Default Walker national: \eqn{a = 0.616}, \eqn{b = -0.473}.
#'
#' In high-turbidity Oklahoma reservoirs, Secchi depth is often
#' controlled more by inorganic suspended sediment than by algal biomass.
#' This is partly captured by the Oklahoma ecoregion-specific Secchi
#' regressions, but for reservoirs with very high non-algal turbidity
#' (e.g. central and western Oklahoma), Secchi predictions should be
#' interpreted with caution.
#'
#' @param x An `okBATHTUB` object produced by [ok_retention()].
#' @param predict_chla Logical. Whether to predict chlorophyll-a from
#'   in-lake TP. Default `TRUE`.
#' @param predict_secchi Logical. Whether to predict Secchi depth from
#'   chlorophyll-a. Requires `predict_chla = TRUE`. Default `TRUE`.
#'
#' @return An `okBATHTUB` object at pipeline step `"inlake"`.
#'
#' @examples
#' result <- ok_load(
#'   inflow_m3yr   = 45e6,
#'   tp_inflow_ugl = 120,
#'   tn_inflow_ugl = 1800
#' ) |>
#'   ok_hydraulics(surface_area_ha = 890, mean_depth_m = 4.2) |>
#'   ok_retention() |>
#'   ok_inlake()
#' print(result)
#'
#' @seealso [ok_retention()], [ok_tsi()]
#' @export
ok_inlake <- function(x,
                      predict_chla   = TRUE,
                      predict_secchi = TRUE) {

  assert_okBATHTUB(x, required_step = "inlake")

  d     <- x$data
  coeff <- x$meta$coeff

  # --- In-lake TP via mass balance ---
  tp_inlake <- d$tp_inflow_ugl * (1 - d$tp_retention_coeff)
  tp_inlake <- max(tp_inlake, 0)

  # --- In-lake TN ---
  tn_inlake <- if (!is.null(d$tn_inflow_ugl) && !is.null(d$tn_retention_coeff)) {
    max(d$tn_inflow_ugl * (1 - d$tn_retention_coeff), 0)
  } else NULL

  # --- Chlorophyll-a ---
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
      chla_source <- coeff$chla_source    %||% "walker_1985_national"
    }
  }

  # --- Secchi depth ---
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
      secchi_source <- coeff$secchi_source    %||% "walker_1985_national"
    }
  }

  new_data <- c(
    d,
    list(
      tp_inlake_ugl       = tp_inlake,
      tn_inlake_ugl       = tn_inlake,
      chla_ugl            = chla,
      chla_coeff_source   = chla_source,
      secchi_m            = secchi,
      secchi_coeff_source = secchi_source,
      ecoregion_applied   = coeff$ecoregion_applied %||% NA_character_
    )
  )

  new_okBATHTUB(data = new_data, step = "inlake", meta = x$meta)
}
