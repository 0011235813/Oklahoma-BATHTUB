#' Compute reservoir hydraulic characteristics
#'
#' @description
#' `ok_hydraulics()` takes the inflow volume from [ok_load()] and the
#' reservoir's morphometric parameters to compute two quantities that
#' drive nutrient retention in all subsequent steps:
#'
#' \itemize{
#'   \item **Hydraulic residence time** (`tau`, yr): reservoir volume
#'     divided by annual outflow, where volume is approximated as
#'     `mean_depth * surface_area`.
#'   \item **Areal water load** (`q_s`, m/yr): inflow volume divided by
#'     surface area. The primary driver of settling-velocity based
#'     retention.
#' }
#'
#' @section Volume approximation:
#' Reservoir volume is computed as `mean_depth * surface_area`, treating
#' the reservoir as a right rectangular prism with flat bottom. This is a
#' simplification: real reservoirs have varying bathymetry, and the
#' relationship `V = Z * A` is exact only when `Z` is the
#' volume-weighted mean depth, which is what bathymetric surveys
#' typically report. If you only have maximum depth or a depth-area
#' regression estimate, expect roughly a factor-of-1.5 uncertainty in
#' `tau` and proportional downstream uncertainty in predicted in-lake
#' concentrations.
#'
#' @param x An `okBATHTUB` object produced by [ok_load()].
#' @param surface_area_ha Numeric. Reservoir surface area at normal pool
#'   (ha). Must be positive.
#' @param mean_depth_m Numeric. Mean reservoir depth at normal pool (m).
#'   Must be positive. For Oklahoma reservoirs this is typically 2-10 m.
#' @param outflow_m3yr Numeric. Annual outflow volume (m^3/yr). If
#'   `NULL` (default), outflow is assumed equal to inflow (steady-state
#'   water balance). Supply an explicit value when significant
#'   evaporation, diversion, or storage change alters the water balance.
#'
#' @return An `okBATHTUB` object at pipeline step `"hydraulics"`.
#'
#' @examples
#' result <- ok_load(
#'   inflow_m3yr   = 45e6,
#'   tp_inflow_ugl = 120,
#'   tn_inflow_ugl = 1800
#' ) |>
#'   ok_hydraulics(surface_area_ha = 890, mean_depth_m = 4.2)
#' print(result)
#'
#' @seealso [ok_load()], [ok_retention()]
#' @export
ok_hydraulics <- function(x,
                          surface_area_ha,
                          mean_depth_m,
                          outflow_m3yr = NULL) {

  assert_okBATHTUB(x, required_step = "hydraulics")
  .assert_positive(surface_area_ha, "surface_area_ha")
  .assert_positive(mean_depth_m,    "mean_depth_m")

  inflow <- x$data$inflow_m3yr

  surface_area_m2 <- surface_area_ha * 1e4
  volume_m3       <- mean_depth_m * surface_area_m2

  if (is.null(outflow_m3yr)) {
    outflow_m3yr <- inflow
  } else {
    .assert_positive(outflow_m3yr, "outflow_m3yr")
  }

  tau <- volume_m3 / outflow_m3yr        # yr
  qs  <- inflow / surface_area_m2        # m/yr

  new_data <- c(
    x$data,
    list(
      surface_area_ha             = surface_area_ha,
      surface_area_m2             = surface_area_m2,
      mean_depth_m                = mean_depth_m,
      volume_m3                   = volume_m3,
      outflow_m3yr                = outflow_m3yr,
      hydraulic_residence_time_yr = tau,
      areal_water_load_myr        = qs
    )
  )

  new_okBATHTUB(data = new_data, step = "hydraulics", meta = x$meta)
}
