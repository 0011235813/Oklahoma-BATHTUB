#' Oklahoma Lake Monitoring Program reservoir morphometry dataset
#'
#' @description
#' A data frame containing morphometric and geographic characteristics for
#' reservoirs monitored under the OWRB Lake Monitoring Program. This dataset
#' allows users to retrieve surface area, mean depth, and other parameters
#' needed for okBATHTUB modelling without manual lookup.
#'
#' @format A data frame with one row per reservoir and the following columns:
#' \describe{
#'   \item{lake_name}{Character. Lake name as used in AWQMS and the
#'     \code{lake_ecoregion_lookup} table.}
#'   \item{alt_name}{Character. Common alternate name, if applicable.}
#'   \item{county}{Character. Primary Oklahoma county.}
#'   \item{managing_agency}{Character. Primary managing agency.}
#'   \item{primary_use}{Character. Primary designated use.}
#'   \item{surface_area_ha}{Numeric. Normal pool surface area (hectares).}
#'   \item{mean_depth_m}{Numeric. Mean depth at normal pool (metres).}
#'   \item{max_depth_m}{Numeric. Maximum depth at normal pool (metres).}
#'   \item{volume_m3}{Numeric. Total storage at normal pool (m\eqn{^3}).}
#'   \item{watershed_area_km2}{Numeric. Contributing watershed area (km\eqn{^2}).}
#'   \item{eco_l3_code}{Character. EPA Level III ecoregion code.}
#'   \item{eco_l3_name}{Character. EPA Level III ecoregion name.}
#'   \item{latitude}{Numeric. Approximate dam latitude (WGS84 decimal degrees).}
#'   \item{longitude}{Numeric. Approximate dam longitude (WGS84 decimal degrees).}
#'   \item{year_completed}{Integer. Year dam was completed.}
#'   \item{data_quality}{Character. Data quality code:
#'     \code{"A"} = from USACE design memoranda, OWRB bathymetric survey,
#'     or National Inventory of Dams;
#'     \code{"B"} = mean depth estimated from Oklahoma regional regression;
#'     \code{"C"} = derived from volume/area.}
#'   \item{notes}{Character. Data source or caveat.}
#' }
#'
#' @section Data quality:
#' Mean depth is the most critical morphometric parameter for BATHTUB modelling
#' (it drives hydraulic residence time). For reservoirs coded \code{"B"},
#' mean depth was estimated using an Oklahoma regional log-log regression
#' fitted to reservoirs with known bathymetry:
#' \deqn{\log(\text{mean depth}) = 0.28 \times \log(\text{area ha}) - 0.34}
#' Users with access to USACE or OWRB bathymetric data for specific reservoirs
#' are encouraged to supply those values directly to \code{ok_hydraulics()}
#' rather than relying on the estimated values here.
#'
#' @source
#' USACE Tulsa District design memoranda; OWRB bathymetric survey program;
#' OWRB Oklahoma Water Atlas; National Inventory of Dams (NID);
#' OWRB Beneficial Use Monitoring Program reports.
#'
#' @examples
#' # View all reservoirs
#' head(ok_reservoirs)
#'
#' # Filter to a specific ecoregion
#' ok_reservoirs[ok_reservoirs$eco_l3_name == "Cross Timbers", ]
#'
#' @seealso \code{\link{ok_reservoir}}
"ok_reservoirs"


#' Look up Oklahoma reservoir morphometry for okBATHTUB modelling
#'
#' @description
#' \code{ok_reservoir()} retrieves morphometric parameters for one or more
#' Oklahoma Lake Monitoring Program reservoirs from the bundled
#' \code{\link{ok_reservoirs}} dataset. Returns a data frame that can be
#' used directly with \code{ok_hydraulics()} in the okBATHTUB pipeline.
#'
#' @param lake_name Character. One or more lake names to look up. Partial
#'   matching is supported - \code{"Arcadia"} will match
#'   \code{"Arcadia Lake"}. Case-insensitive.
#' @param exact Logical. If \code{TRUE}, requires exact name match
#'   (case-insensitive). Default \code{FALSE} (partial matching).
#' @param ecoregion Character. Filter results to a specific EPA Level III
#'   ecoregion name. Default \code{NULL} (no filter).
#' @param data_quality Character vector. Filter to specific data quality
#'   codes. Default \code{c("A", "B", "C")} (all).
#'
#' @return A data frame with one row per matched reservoir containing all
#'   columns from \code{\link{ok_reservoirs}}.
#'
#' @examples
#' \dontrun{
#' # Look up a single lake
#' ok_reservoir("Arcadia Lake")
#'
#' # Partial match
#' ok_reservoir("Arcadia")
#'
#' # Use in pipeline
#' res <- ok_reservoir("Arcadia Lake")
#' ok_load(inflow_m3yr = 45e6, tp_inflow_ugl = 120) |>
#'   ok_hydraulics(
#'     surface_area_ha = res$surface_area_ha,
#'     mean_depth_m    = res$mean_depth_m
#'   )
#'
#' # Get all Cross Timbers lakes with quality A data
#' ok_reservoir(ecoregion = "Cross Timbers", data_quality = "A")
#'
#' # List all available lakes
#' ok_reservoir()$lake_name
#'
#' @seealso \code{\link{ok_reservoirs}}, \code{\link{ok_hydraulics}}
#' @export
#' }
ok_reservoir <- function(lake_name    = NULL,
                          exact        = FALSE,
                          ecoregion    = NULL,
                          data_quality = c("A", "B", "C")) {

  df <- ok_reservoirs

  # Filter by ecoregion
  if (!is.null(ecoregion)) {
    df <- df[tolower(df$eco_l3_name) == tolower(ecoregion), ]
    if (nrow(df) == 0) {
      warning(
        sprintf("No reservoirs found for ecoregion '%s'.\n", ecoregion),
        "Available ecoregions: ",
        paste(sort(unique(ok_reservoirs$eco_l3_name)), collapse = ", "),
        call. = FALSE
      )
      return(df)
    }
  }

  # Filter by data quality
  df <- df[df$data_quality %in% data_quality, ]

  # Filter by lake name
  if (!is.null(lake_name)) {
    if (exact) {
      matched <- tolower(df$lake_name) %in% tolower(lake_name) |
                 tolower(df$alt_name)  %in% tolower(lake_name)
    } else {
      # Partial match: any supplied name appears in lake_name or alt_name
      pattern <- paste(lake_name, collapse = "|")
      matched <- grepl(pattern, df$lake_name, ignore.case = TRUE) |
                 grepl(pattern, df$alt_name,  ignore.case = TRUE)
    }

    df <- df[matched, ]

    if (nrow(df) == 0) {
      warning(
        sprintf(
          "No reservoirs matched '%s' in the ok_reservoirs dataset.\n",
          paste(lake_name, collapse = "', '")
        ),
        "Use ok_reservoir() with no arguments to see all available lakes.",
        call. = FALSE
      )
    } else if (nrow(df) > 1 && !is.null(lake_name) && length(lake_name) == 1) {
      message(sprintf(
        "ok_reservoir(): %d lakes matched '%s'. Returning all matches.",
        nrow(df), lake_name
      ))
    }
  }

  df
}


#' Summarise oklahoma reservoir dataset coverage
#'
#' @description
#' Prints a summary of the \code{ok_reservoirs} dataset by ecoregion,
#' showing the number of lakes, surface area range, and data quality
#' breakdown. Useful for understanding dataset coverage before modelling.
#'
#' @return A data frame (invisibly) summarising coverage by ecoregion.
#' @export
ok_reservoir_summary <- function() {

  summ <- ok_reservoirs |>
    dplyr::group_by(eco_l3_name) |>
    dplyr::summarise(
      n_lakes        = dplyr::n(),
      n_quality_a    = sum(data_quality == "A"),
      n_quality_b    = sum(data_quality == "B"),
      area_min_ha    = round(min(surface_area_ha), 0),
      area_max_ha    = round(max(surface_area_ha), 0),
      depth_min_m    = round(min(mean_depth_m), 1),
      depth_max_m    = round(max(mean_depth_m), 1),
      .groups        = "drop"
    ) |>
    dplyr::arrange(dplyr::desc(n_lakes))

  cat("========================================\n")
  cat("  okBATHTUB - ok_reservoirs Coverage\n")
  cat("========================================\n\n")
  cat(sprintf("  Total lakes: %d\n", nrow(ok_reservoirs)))
  cat(sprintf("  Quality A (measured): %d\n", sum(ok_reservoirs$data_quality == "A")))
  cat(sprintf("  Quality B (estimated depth): %d\n\n", sum(ok_reservoirs$data_quality == "B")))

  print(summ, n = Inf)
  cat("\n")

  invisible(summ)
}
