#' Oklahoma reservoir morphometry dataset
#'
#' @description
#' A data frame containing morphometric and geographic characteristics for
#' major reservoirs in Oklahoma, compiled from publicly available sources
#' to enable rapid setup of okBATHTUB pipelines without manual morphometric
#' lookup.
#'
#' @format A data frame with one row per reservoir and the following
#'   columns:
#' \describe{
#'   \item{lake_name}{Character. Canonical lake name.}
#'   \item{alt_name}{Character. Common alternate name, if any.}
#'   \item{county}{Character. Primary Oklahoma county.}
#'   \item{managing_agency}{Character. Primary managing agency.}
#'   \item{primary_use}{Character. Primary designated use.}
#'   \item{surface_area_ha}{Numeric. Normal pool surface area (hectares).}
#'   \item{mean_depth_m}{Numeric. Mean depth at normal pool (metres).}
#'   \item{max_depth_m}{Numeric. Maximum depth at normal pool (metres).}
#'   \item{volume_m3}{Numeric. Total storage at normal pool (m^3).}
#'   \item{watershed_area_km2}{Numeric. Contributing watershed area (km^2).}
#'   \item{eco_l3_code}{Character. EPA Level III ecoregion code.}
#'   \item{eco_l3_name}{Character. EPA Level III ecoregion name.}
#'   \item{latitude}{Numeric. Approximate dam latitude (WGS84 decimal degrees).}
#'   \item{longitude}{Numeric. Approximate dam longitude (WGS84 decimal degrees).}
#'   \item{year_completed}{Integer. Year dam was completed.}
#'   \item{data_quality}{Character. Data quality code:
#'     `"A"` = direct from authoritative source (USACE design memoranda,
#'     published bathymetric surveys, or National Inventory of Dams);
#'     `"B"` = mean depth estimated from Oklahoma regional regression.}
#'   \item{notes}{Character. Data source or caveat.}
#' }
#'
#' @section Data quality:
#' Mean depth is the most critical morphometric parameter for okBATHTUB
#' modelling (it drives hydraulic residence time). For reservoirs coded
#' `"B"`, mean depth was estimated using an Oklahoma regional log-log
#' regression fitted to reservoirs with known bathymetry:
#' \deqn{\log_{10}(\text{mean depth}) = 0.28 \times \log_{10}(\text{area in ha}) - 0.34}
#' This regression has substantial residual scatter; the resulting depth
#' carries roughly a factor-of-1.5 prediction interval. Users with access
#' to authoritative bathymetric data for specific reservoirs are
#' encouraged to supply those values directly to [ok_hydraulics()] rather
#' than relying on the estimated values here.
#'
#' @source
#' Compiled from publicly available sources including U.S. Army Corps of
#' Engineers (USACE) Tulsa District design memoranda, the National
#' Inventory of Dams (NID), U.S. Bureau of Reclamation (BOR) design data,
#' and published Oklahoma Water Resources Board reports. This dataset is
#' provided as a convenience starting point and should be verified against
#' the most current authoritative source for any decision-relevant
#' application.
#'
#' @examples
#' # View all reservoirs
#' head(ok_reservoirs)
#'
#' # Filter to a specific ecoregion
#' ok_reservoirs[ok_reservoirs$eco_l3_name == "Cross Timbers", ]
#'
#' @seealso [ok_reservoir()]
"ok_reservoirs"


#' Look up Oklahoma reservoir morphometry
#'
#' @description
#' `ok_reservoir()` retrieves morphometric parameters for one or more
#' Oklahoma reservoirs from the bundled [`ok_reservoirs`] dataset.
#' Returns a data frame that can be used directly with [ok_hydraulics()].
#'
#' @param lake_name Character. One or more lake names to look up. Partial
#'   matching is supported - `"Arcadia"` will match `"Arcadia Lake"`.
#'   Case-insensitive. If `NULL`, returns all reservoirs (subject to
#'   other filters).
#' @param exact Logical. If `TRUE`, requires exact name match
#'   (case-insensitive). Default `FALSE` (partial matching).
#' @param ecoregion Character. Filter results to a specific EPA Level III
#'   ecoregion name. Default `NULL` (no filter).
#' @param data_quality Character vector. Filter to specific data quality
#'   codes. Default `c("A", "B")` (all).
#'
#' @return A data frame with one row per matched reservoir containing all
#'   columns from [`ok_reservoirs`].
#'
#' @examples
#' # Look up a single lake (partial match)
#' ok_reservoir("Arcadia")
#'
#' # Exact match
#' ok_reservoir("Arcadia Lake", exact = TRUE)
#'
#' # Use in pipeline
#' res <- ok_reservoir("Arcadia Lake")
#' if (nrow(res) > 0) {
#'   ok_load(inflow_m3yr = 45e6, tp_inflow_ugl = 120) |>
#'     ok_hydraulics(
#'       surface_area_ha = res$surface_area_ha[1],
#'       mean_depth_m    = res$mean_depth_m[1]
#'     )
#' }
#'
#' # All Cross Timbers lakes with quality A data
#' ok_reservoir(ecoregion = "Cross Timbers", data_quality = "A")
#'
#' @seealso [`ok_reservoirs`], [ok_hydraulics()]
#' @export
ok_reservoir <- function(lake_name    = NULL,
                         exact        = FALSE,
                         ecoregion    = NULL,
                         data_quality = c("A", "B")) {

  df <- get("ok_reservoirs", envir = asNamespace("okBATHTUB"))

  if (!is.null(ecoregion)) {
    if (!is.character(ecoregion) || length(ecoregion) != 1L)
      stop("'ecoregion' must be a single character string.", call. = FALSE)
    df <- df[tolower(df$eco_l3_name) == tolower(ecoregion), , drop = FALSE]
    if (nrow(df) == 0L) {
      warning(
        sprintf("No reservoirs found for ecoregion '%s'.\n", ecoregion),
        "Available ecoregions: ",
        paste(sort(unique(
          get("ok_reservoirs", envir = asNamespace("okBATHTUB"))$eco_l3_name
        )), collapse = ", "),
        call. = FALSE
      )
      return(df)
    }
  }

  df <- df[df$data_quality %in% data_quality, , drop = FALSE]

  if (!is.null(lake_name)) {
    if (!is.character(lake_name))
      stop("'lake_name' must be a character vector.", call. = FALSE)

    if (exact) {
      matched <- tolower(df$lake_name) %in% tolower(lake_name) |
                 tolower(df$alt_name)  %in% tolower(lake_name)
    } else {
      # Escape regex special chars in user input to make grepl literal
      escape_regex <- function(s) gsub("([][\\.|()^$*+?{}\\\\])", "\\\\\\1", s)
      pattern <- paste(vapply(lake_name, escape_regex, character(1)),
                       collapse = "|")
      matched <- grepl(pattern, df$lake_name, ignore.case = TRUE) |
                 (!is.na(df$alt_name) &
                  grepl(pattern, df$alt_name, ignore.case = TRUE))
    }

    df <- df[matched, , drop = FALSE]

    if (nrow(df) == 0L) {
      warning(
        sprintf(
          "No reservoirs matched %s in the ok_reservoirs dataset.\n",
          paste0("'", paste(lake_name, collapse = "', '"), "'")
        ),
        "Use ok_reservoir() with no arguments to see all available lakes.",
        call. = FALSE
      )
    } else if (nrow(df) > 1L && length(lake_name) == 1L && !exact) {
      message(sprintf(
        "ok_reservoir(): %d lakes matched '%s'. Returning all matches.",
        nrow(df), lake_name
      ))
    }
  }

  df
}


#' Summarise the bundled reservoir dataset coverage
#'
#' @description
#' Prints a summary of the [`ok_reservoirs`] dataset by ecoregion, showing
#' the number of lakes, surface area range, and data quality breakdown.
#' Useful for understanding dataset coverage before modelling.
#'
#' @return A data frame (invisibly) summarising coverage by ecoregion.
#' @export
ok_reservoir_summary <- function() {

  df <- get("ok_reservoirs", envir = asNamespace("okBATHTUB"))

  if (nrow(df) == 0L) {
    cat("ok_reservoirs is empty.\n")
    return(invisible(data.frame()))
  }

  eco_groups <- split(df, df$eco_l3_name)

  summ <- do.call(rbind, lapply(names(eco_groups), function(nm) {
    g <- eco_groups[[nm]]
    data.frame(
      eco_l3_name = nm,
      n_lakes     = nrow(g),
      n_quality_a = sum(g$data_quality == "A"),
      n_quality_b = sum(g$data_quality == "B"),
      area_min_ha = round(min(g$surface_area_ha, na.rm = TRUE), 0),
      area_max_ha = round(max(g$surface_area_ha, na.rm = TRUE), 0),
      depth_min_m = round(min(g$mean_depth_m,    na.rm = TRUE), 1),
      depth_max_m = round(max(g$mean_depth_m,    na.rm = TRUE), 1),
      stringsAsFactors = FALSE
    )
  }))
  rownames(summ) <- NULL
  summ <- summ[order(-summ$n_lakes), , drop = FALSE]
  rownames(summ) <- NULL

  cat("========================================\n")
  cat("  okBATHTUB - ok_reservoirs Coverage\n")
  cat("========================================\n\n")
  cat(sprintf("  Total lakes: %d\n", nrow(df)))
  cat(sprintf("  Quality A (authoritative source): %d\n",
              sum(df$data_quality == "A")))
  cat(sprintf("  Quality B (depth estimated):      %d\n\n",
              sum(df$data_quality == "B")))

  print(summ, row.names = FALSE)
  cat("\n")

  invisible(summ)
}
