#' Oklahoma lakes -> EPA Level III ecoregion lookup
#'
#' @description
#' A data frame mapping 214 Oklahoma (and several border) lakes to their
#' EPA Level III ecoregion, with monitoring coverage statistics from a
#' 2000-2024 snapshot of publicly available state lake monitoring
#' records. Useful for quickly resolving an ecoregion for use with
#' `coefficients = "oklahoma"`, or for understanding which lakes have
#' sufficient monitoring history to support empirical analysis.
#'
#' @format A data frame with one row per lake and the following columns:
#' \describe{
#'   \item{lake_name}{Character. Lake name as it appeared in source
#'     monitoring records.}
#'   \item{eco_l3_code}{Character. EPA Level III ecoregion numeric code
#'     (as string), e.g. `"28"` for Cross Timbers. `NA` for lakes
#'     outside Oklahoma's ecoregion boundaries (border / out-of-state
#'     lakes that appeared in monitoring records).}
#'   \item{eco_l3_name}{Character. EPA Level III ecoregion name.
#'     `NA` for unmapped lakes (see above).}
#'   \item{n_sites_total}{Integer. Total number of monitoring stations
#'     on this lake in the source dataset.}
#'   \item{n_sites_tier1}{Integer. Number of stations that met the
#'     primary-station criteria used in the calibration workflow
#'     (1+ years of usable data). Always `<= n_sites_total`.}
#'   \item{latitude}{Numeric. Approximate lake centroid latitude
#'     (WGS84 decimal degrees), computed as the mean of monitoring
#'     station coordinates.}
#'   \item{longitude}{Numeric. Approximate lake centroid longitude.}
#'   \item{max_yrs_tp}{Integer. Maximum number of calendar years (in
#'     the 2000-2024 window) with any reported total phosphorus data at
#'     any station on the lake.}
#'   \item{max_yrs_chla}{Integer. Same, for chlorophyll-a.}
#'   \item{max_yrs_secchi}{Integer. Same, for Secchi depth.}
#'   \item{max_yrs_tn}{Integer. Same, for total nitrogen.}
#' }
#'
#' @section Coverage statistics caveats:
#' The `n_sites_*` and `max_yrs_*` columns reflect a 2000-2024 snapshot
#' taken when the calibration was performed. They are **not** updated
#' automatically with new monitoring data. Treat them as a useful
#' starting point for assessing data availability, not as a current
#' inventory. For up-to-date monitoring coverage, query the source
#' monitoring system directly.
#'
#' @section Ecoregion assignment:
#' Ecoregion assignment uses EPA Level III boundaries (Griffith et al.
#' 2004) applied to lake centroid coordinates. A handful of lakes
#' (5 in the current dataset) have `eco_l3_name = NA` either because
#' they sit outside Oklahoma's ecoregion shapefile coverage (border
#' lakes in TX/KS that appeared in monitoring records) or because the
#' centroid fell in an ambiguous boundary zone. For these lakes,
#' modelling with `coefficients = "oklahoma"` will fall back to the
#' statewide pooled regressions.
#'
#' @source
#' Compiled from publicly available Oklahoma lake monitoring records
#' (2000-2024). Ecoregion polygons from Griffith, G.E. et al. (2004),
#' *Ecoregions of Oklahoma*, U.S. Geological Survey, Reston, Virginia.
#' See `data-raw/ok_ecoregion_assignment.R` in the package source for
#' the assignment script.
#'
#' @examples
#' # All lakes in a specific ecoregion
#' head(ok_lake_ecoregions[ok_lake_ecoregions$eco_l3_name == "Cross Timbers", ])
#'
#' # Lakes with the longest monitoring history
#' top_monitored <- ok_lake_ecoregions[
#'   order(-ok_lake_ecoregions$max_yrs_tp), ][1:10, ]
#' top_monitored[, c("lake_name", "eco_l3_name", "max_yrs_tp")]
#'
#' @seealso [ok_lake_ecoregion()], [`ok_reservoirs`]
"ok_lake_ecoregions"


#' Look up an Oklahoma lake's EPA Level III ecoregion
#'
#' @description
#' Convenience wrapper around [`ok_lake_ecoregions`] for retrieving the
#' ecoregion name for one or more lakes. Returns either the ecoregion
#' name (single match) or a data frame of all matches.
#'
#' @param lake_name Character. One or more lake names to look up.
#'   Partial matching is supported (case-insensitive) unless `exact`
#'   is `TRUE`.
#' @param exact Logical. If `TRUE`, requires an exact name match
#'   (case-insensitive). Default `FALSE`.
#' @param simplify Logical. If `TRUE` and exactly one lake matches,
#'   return the ecoregion name as a length-1 character vector. If
#'   `FALSE` or multiple matches, return a data frame with all matched
#'   rows from [`ok_lake_ecoregions`]. Default `TRUE`.
#'
#' @return Character (single match, `simplify = TRUE`) or data frame
#'   (otherwise). Returns `NA_character_` or an empty data frame if
#'   no match is found.
#'
#' @examples
#' # Single lake, get back the ecoregion name
#' ok_lake_ecoregion("Arcadia Lake", exact = TRUE)
#'
#' # Partial match - returns a data frame of all matches
#' ok_lake_ecoregion("Lake")
#'
#' # Use the result in a pipeline call
#' eco <- ok_lake_ecoregion("Tenkiller", exact = FALSE,
#'                          simplify = TRUE)
#' if (!is.na(eco)) {
#'   ok_load(inflow_m3yr   = 1e9,
#'           tp_inflow_ugl = 60,
#'           coefficients  = "oklahoma",
#'           ecoregion     = eco)
#' }
#'
#' @seealso [`ok_lake_ecoregions`], [ok_reservoir()]
#' @export
ok_lake_ecoregion <- function(lake_name,
                              exact    = FALSE,
                              simplify = TRUE) {

  if (!is.character(lake_name) || length(lake_name) < 1L)
    stop("'lake_name' must be a non-empty character vector.", call. = FALSE)

  df <- get("ok_lake_ecoregions", envir = asNamespace("okBATHTUB"))

  if (exact) {
    matched <- tolower(df$lake_name) %in% tolower(lake_name)
  } else {
    escape_regex <- function(s) gsub("([][\\.|()^$*+?{}\\\\])", "\\\\\\1", s)
    pattern <- paste(vapply(lake_name, escape_regex, character(1)),
                     collapse = "|")
    matched <- grepl(pattern, df$lake_name, ignore.case = TRUE)
  }

  result <- df[matched, , drop = FALSE]

  if (nrow(result) == 0L) {
    warning(
      sprintf("No lakes matched %s in ok_lake_ecoregions.",
              paste0("'", paste(lake_name, collapse = "', '"), "'")),
      call. = FALSE
    )
    return(if (simplify) NA_character_ else result)
  }

  if (simplify && nrow(result) == 1L) {
    return(result$eco_l3_name)
  }

  if (nrow(result) > 1L && length(lake_name) == 1L && !exact) {
    message(sprintf(
      "ok_lake_ecoregion(): %d lakes matched '%s'. Returning all matches.",
      nrow(result), lake_name
    ))
  }

  result
}
