#' Chain multiple reservoir segments sequentially
#'
#' @description
#' \code{ok_segment()} links two reservoir segments in series, passing the
#' outflow water quality of an upstream segment as the inflow to the next
#' downstream segment. This reflects the longitudinal zonation common in
#' Oklahoma reservoirs - riverine, transitional, and lacustrine segments each
#' behave differently and should be modelled separately when data support it.
#'
#' The function takes a completed upstream segment result (through at least
#' \code{ok_inlake()}) and returns a new \code{okBATHTUB} object at the
#' \code{"load"} step, pre-populated with the upstream outflow concentrations
#' as the downstream inflow inputs. The downstream segment can then be run
#' through the full pipeline normally.
#'
#' @section Mass balance at the segment boundary:
#' The outflow TP concentration from the upstream segment becomes the inflow
#' TP concentration for the downstream segment:
#' \deqn{C_{in,down} = C_{lake,up} = C_{in,up} \times (1 - R_{up})}
#'
#' Inflow volume is passed through unchanged under the steady-state
#' assumption. If the downstream segment has a different surface area or
#' morphometry, supply those via \code{ok_hydraulics()} after this call.
#'
#' @param upstream An \code{okBATHTUB} object that has been run through at
#'   least \code{ok_inlake()}. The in-lake TP, TN, and inflow volume from
#'   this result become the downstream segment's inflow.
#' @param segment_label Character. Label for the downstream segment.
#'   Default \code{"downstream"}.
#' @param coefficients Coefficient set for the downstream segment. Defaults
#'   to the same coefficient set used in the upstream segment. Can be
#'   overridden to apply different ecoregion coefficients to different
#'   segments.
#' @param ecoregion Character. EPA Level III ecoregion for the downstream
#'   segment. If \code{NULL}, inherits from the upstream segment.
#'
#' @return An \code{okBATHTUB} object at pipeline step \code{"load"},
#'   ready to pipe into \code{ok_hydraulics()} for the downstream segment.
#'
#' @examples
#' # Two-segment reservoir: riverine -> lacustrine
#' riverine <- ok_load(
#'   inflow_m3yr   = 45e6,
#'   tp_inflow_ugl = 150,
#'   tn_inflow_ugl = 2200,
#'   segment_label = "riverine"
#' ) |>
#' ok_hydraulics(surface_area_ha = 280, mean_depth_m = 3.1) |>
#' ok_retention() |>
#' ok_inlake()
#'
#' lacustrine <- ok_segment(riverine, segment_label = "lacustrine") |>
#'   ok_hydraulics(surface_area_ha = 610, mean_depth_m = 5.8) |>
#'   ok_retention() |>
#'   ok_inlake() |>
#'   ok_tsi()
#'
#' summary(lacustrine)
#'
#' @seealso \code{\link{ok_load}}, \code{\link{ok_segment_chain}}
#' @export
ok_segment <- function(upstream,
                        segment_label = "downstream",
                        coefficients  = NULL,
                        ecoregion     = NULL) {

  assert_okBATHTUB(upstream)

  # Must be at least at inlake step to have predicted concentrations
  if (!upstream$step %in% c("inlake", "tsi")) {
    stop(
      "ok_segment() requires the upstream result to have passed through ",
      "ok_inlake() first. Current step: '", upstream$step, "'.",
      call. = FALSE
    )
  }

  d <- upstream$data

  # Check that in-lake TP exists
  if (is.null(d$tp_inlake_ugl) || is.na(d$tp_inlake_ugl)) {
    stop(
      "Upstream segment has no predicted in-lake TP (tp_inlake_ugl). ",
      "Ensure ok_inlake() ran successfully for the upstream segment.",
      call. = FALSE
    )
  }

  # Inherit coefficient set from upstream if not overridden
  upstream_coeff_str <- upstream$meta$coefficients %||% "walker"
  downstream_coeff   <- coefficients %||% upstream_coeff_str
  downstream_eco     <- ecoregion    %||% upstream$meta$ecoregion

  # Build the downstream ok_load() call using upstream in-lake concentrations
  # as inflow concentrations, and upstream inflow volume as inflow volume
  ok_load(
    inflow_m3yr    = d$inflow_m3yr,
    tp_inflow_ugl  = d$tp_inlake_ugl,
    tn_inflow_ugl  = d$tn_inlake_ugl,
    tss_inflow_mgl = NULL,           # TSS not tracked through segments
    segment_label  = segment_label,
    coefficients   = downstream_coeff,
    ecoregion      = downstream_eco
  )
}


#' Chain multiple reservoir segments from a list
#'
#' @description
#' A convenience wrapper around \code{ok_segment()} for reservoirs with more
#' than two segments. Accepts a list of segment morphometry specifications and
#' runs them sequentially, passing each segment's outflow into the next.
#'
#' @param inflow_m3yr Numeric. Total annual inflow (m\eqn{^3}/yr).
#' @param tp_inflow_ugl Numeric. Inflow TP (ug/L).
#' @param tn_inflow_ugl Numeric. Inflow TN (ug/L). Optional.
#' @param segments A list of named lists, one per segment, each containing:
#'   \describe{
#'     \item{\code{label}}{Character. Segment name.}
#'     \item{\code{surface_area_ha}}{Numeric. Surface area (ha).}
#'     \item{\code{mean_depth_m}}{Numeric. Mean depth (m).}
#'     \item{\code{outflow_m3yr}}{Numeric. Outflow volume (m\eqn{^3}/yr).
#'       Optional; defaults to inflow.}
#'   }
#' @param coefficients Coefficient set applied to all segments.
#'   Default \code{"walker"}.
#' @param ecoregion EPA Level III ecoregion. Applied to all segments.
#'
#' @return A named list of \code{okBATHTUB} objects at step \code{"tsi"},
#'   one per segment, in downstream order. Names match the \code{label}
#'   field of each segment specification.
#'
#' @examples
#' segments <- list(
#'   list(label = "riverine",     surface_area_ha = 280, mean_depth_m = 3.1),
#'   list(label = "transitional", surface_area_ha = 410, mean_depth_m = 4.5),
#'   list(label = "lacustrine",   surface_area_ha = 610, mean_depth_m = 5.8)
#' )
#'
#' results <- ok_segment_chain(
#'   inflow_m3yr   = 45e6,
#'   tp_inflow_ugl = 150,
#'   tn_inflow_ugl = 2200,
#'   segments      = segments
#' )
#'
#' # View trophic state of each segment
#' lapply(results, function(r) r$data$trophic_state)
#'
#' @seealso \code{\link{ok_segment}}
#' @export
ok_segment_chain <- function(inflow_m3yr,
                              tp_inflow_ugl,
                              tn_inflow_ugl = NULL,
                              segments,
                              coefficients  = "walker",
                              ecoregion     = NULL) {

  if (!is.list(segments) || length(segments) == 0)
    stop("'segments' must be a non-empty list of segment specifications.",
         call. = FALSE)

  # Validate each segment spec
  for (i in seq_along(segments)) {
    seg <- segments[[i]]
    if (is.null(seg$label))
      stop(sprintf("segments[[%d]] is missing 'label'.", i), call. = FALSE)
    if (is.null(seg$surface_area_ha))
      stop(sprintf("segments[[%d]] ('%s') is missing 'surface_area_ha'.",
                   i, seg$label), call. = FALSE)
    if (is.null(seg$mean_depth_m))
      stop(sprintf("segments[[%d]] ('%s') is missing 'mean_depth_m'.",
                   i, seg$label), call. = FALSE)
  }

  results     <- list()
  current_load <- ok_load(
    inflow_m3yr   = inflow_m3yr,
    tp_inflow_ugl = tp_inflow_ugl,
    tn_inflow_ugl = tn_inflow_ugl,
    segment_label = segments[[1]]$label,
    coefficients  = coefficients,
    ecoregion     = ecoregion
  )

  for (i in seq_along(segments)) {
    seg <- segments[[i]]

    # If not first segment, start from ok_segment() output
    if (i > 1) {
      current_load <- ok_segment(
        upstream      = results[[i - 1]],
        segment_label = seg$label,
        coefficients  = coefficients,
        ecoregion     = ecoregion
      )
    }

    result_i <- current_load |>
      ok_hydraulics(
        surface_area_ha = seg$surface_area_ha,
        mean_depth_m    = seg$mean_depth_m,
        outflow_m3yr    = seg$outflow_m3yr   # NULL = use inflow
      ) |>
      ok_retention() |>
      ok_inlake()    |>
      ok_tsi()

    results[[seg$label]] <- result_i
  }

  results
}


#' Summarise a multi-segment chain as a data frame
#'
#' @description
#' Converts the list output of \code{ok_segment_chain()} into a tidy data
#' frame with one row per segment, suitable for plotting or reporting.
#'
#' @param chain_result Named list of \code{okBATHTUB} objects returned by
#'   \code{ok_segment_chain()}.
#'
#' @return A data frame with one row per segment containing key water quality
#'   predictions and TSI values.
#'
#' @export
ok_segment_summary <- function(chain_result) {

  if (!is.list(chain_result))
    stop("'chain_result' must be a list returned by ok_segment_chain().",
         call. = FALSE)

  rows <- lapply(names(chain_result), function(nm) {
    r <- chain_result[[nm]]
    d <- r$data
    data.frame(
      segment          = nm,
      tp_inflow_ugl    = d$tp_inflow_ugl    %||% NA_real_,
      tp_inlake_ugl    = d$tp_inlake_ugl    %||% NA_real_,
      tp_retention     = round(d$tp_retention_coeff %||% NA_real_, 3),
      tn_inlake_ugl    = d$tn_inlake_ugl    %||% NA_real_,
      chla_ugl         = d$chla_ugl         %||% NA_real_,
      secchi_m         = d$secchi_m         %||% NA_real_,
      tsi_tp           = d$tsi_tp           %||% NA_real_,
      tsi_chla         = d$tsi_chla         %||% NA_real_,
      tsi_secchi       = d$tsi_secchi       %||% NA_real_,
      tsi_mean         = d$tsi_mean         %||% NA_real_,
      trophic_state    = d$trophic_state    %||% NA_character_,
      hrt_yr           = round(d$hydraulic_residence_time_yr %||% NA_real_, 3),
      surface_area_ha  = d$surface_area_ha  %||% NA_real_,
      mean_depth_m     = d$mean_depth_m     %||% NA_real_,
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, rows)
}
