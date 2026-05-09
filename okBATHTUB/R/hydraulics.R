#' Compute reservoir hydraulic characteristics
#'
#' @description
#' \code{ok_hydraulics()} takes the inflow volume from \code{ok_load()} and
#' the reservoir's morphometric parameters to compute two quantities that
#' drive nutrient retention in all subsequent steps:
#'
#' \itemize{
#'   \item \strong{Hydraulic residence time} (\eqn{\tau}, yr): the average
#'     time water spends in the reservoir before leaving via the outlet.
#'     Calculated as reservoir volume divided by annual outflow, where volume
#'     is approximated from surface area and mean depth.
#'   \item \strong{Areal water load} (\eqn{q_s}, m/yr): inflow volume divided
#'     by surface area. This is the primary driver of the settling-velocity
#'     based nutrient retention equations. Also called the hydraulic overflow
#'     rate.
#' }
#'
#' @param x An \code{okBATHTUB} object produced by \code{ok_load()}.
#' @param surface_area_ha Numeric. Reservoir surface area at normal pool (ha).
#'   Must be positive.
#' @param mean_depth_m Numeric. Mean reservoir depth at normal pool (m).
#'   Must be positive. Mean depth = volume / surface area; for Oklahoma
#'   reservoirs this is typically 2--10 m.
#' @param outflow_m3yr Numeric. Annual outflow volume (m\eqn{^3}/yr). If
#'   \code{NULL} (default), outflow is assumed equal to inflow (steady-state
#'   water balance). Supply an explicit value when significant evaporation or
#'   diversion alters the water balance.
#'
#' @return An \code{okBATHTUB} object at pipeline step \code{"hydraulics"},
#'   with the following fields added to \code{$data}:
#'   \describe{
#'     \item{\code{surface_area_ha}}{Reservoir surface area (ha).}
#'     \item{\code{surface_area_m2}}{Reservoir surface area (m\eqn{^2}).}
#'     \item{\code{mean_depth_m}}{Mean depth (m).}
#'     \item{\code{volume_m3}}{Estimated reservoir volume (m\eqn{^3}).}
#'     \item{\code{outflow_m3yr}}{Annual outflow volume (m\eqn{^3}/yr).}
#'     \item{\code{hydraulic_residence_time_yr}}{Hydraulic residence time (yr).}
#'     \item{\code{areal_water_load_myr}}{Areal water load, \eqn{q_s} (m/yr).}
#'   }
#'
#' @examples
#' result <- ok_load(
#'   inflow_m3yr   = 45e6,
#'   tp_inflow_ugl = 120,
#'   tn_inflow_ugl = 1800
#' ) |>
#' ok_hydraulics(
#'   surface_area_ha = 890,
#'   mean_depth_m    = 4.2
#' )
#' print(result)
#'
#' @seealso \code{\link{ok_load}}, \code{\link{ok_retention}}
#' @export
ok_hydraulics <- function(x,
                          surface_area_ha,
                          mean_depth_m,
                          outflow_m3yr = NULL) {

  assert_okBATHTUB(x, required_step = "hydraulics")
  .assert_positive(surface_area_ha, "surface_area_ha")
  .assert_positive(mean_depth_m,    "mean_depth_m")

  inflow <- x$data$inflow_m3yr

  # Unit conversion
  surface_area_m2 <- surface_area_ha * 1e4   # ha -> m2

  # Reservoir volume approximation: V = mean_depth * surface_area
  volume_m3 <- mean_depth_m * surface_area_m2

  # Outflow defaults to inflow under steady-state water balance assumption
  if (is.null(outflow_m3yr)) {
    outflow_m3yr <- inflow
  } else {
    .assert_positive(outflow_m3yr, "outflow_m3yr")
  }

  # Hydraulic residence time (yr): volume / outflow
  tau <- volume_m3 / outflow_m3yr

  # Areal water load (m/yr): inflow volume / surface area
  # This is the hydraulic overflow rate - the primary retention driver
  qs <- inflow / surface_area_m2

  # Merge new fields into existing data
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
