#' Run load reduction scenarios and compare predicted water quality responses
#'
#' @description
#' \code{ok_scenario()} takes a baseline \code{ok_load()} result and runs
#' the full pipeline under one or more alternative loading scenarios, returning
#' a tidy comparison table of predicted water quality across all scenarios.
#' This is the primary tool for nutrient management planning - answering
#' "how much does TP need to be reduced to move this lake from eutrophic to
#' mesotrophic?"
#'
#' @section How scenarios work:
#' Each scenario modifies one or more inflow parameters relative to the
#' baseline. Reductions are expressed as fractions (0?1), where
#' \code{tp_reduction = 0.30} means a 30\% reduction in inflow TP load.
#' Scenarios can also specify absolute concentrations directly via
#' \code{tp_inflow_ugl}, which overrides the reduction fraction.
#'
#' The morphometric parameters (surface area, mean depth) from the baseline
#' \code{ok_hydraulics()} call are applied to every scenario.
#'
#' @param baseline An \code{okBATHTUB} object that has been run through at
#'   least \code{ok_hydraulics()}. All morphometry is taken from this object.
#' @param scenarios A list of named lists, one per scenario. Each list must
#'   have a \code{label} (character, required). Optional fields:
#'   \code{tp_reduction} (numeric 0-1, fractional TP reduction),
#'   \code{tn_reduction} (numeric 0-1, fractional TN reduction),
#'   \code{tp_inflow_ugl} (numeric, absolute inflow TP in ug/L, overrides
#'   tp_reduction), \code{tn_inflow_ugl} (numeric, absolute inflow TN in ug/L),
#'   \code{flow_change} (numeric, fractional change in inflow volume, e.g.
#'   \code{-0.20} = 20\% flow reduction).
#' @param include_baseline Logical. Whether to include the baseline run as
#'   the first row in the output. Default \code{TRUE}.
#' @param target_tsi Numeric. Optional TSI target. If supplied, a
#'   \code{meets_target} column is added. Default \code{NULL}.
#' @param target_class Character. One of \code{"oligotrophic"},
#'   \code{"mesotrophic"}, or \code{"eutrophic"}. Sets \code{target_tsi}
#'   to the upper bound of that class automatically.
#'
#' @return A data frame with one row per scenario (plus baseline if requested).
#'   Columns include: \code{scenario} (label), \code{tp_inflow_ugl} (inflow TP
#'   in ug/L), \code{tp_reduction_pct} (reduction from baseline in percent),
#'   \code{tp_inlake_ugl} (predicted in-lake TP), \code{chla_ugl} (predicted
#'   chlorophyll-a), \code{secchi_m} (predicted Secchi depth), \code{tsi_tp},
#'   \code{tsi_chla}, \code{tsi_mean} (Carlson TSI values), \code{trophic_state}
#'   (classification), and optionally \code{meets_target} (logical, present when
#'   \code{target_tsi} is supplied).
#'
#' @examples
#' # Baseline: Arcadia Lake with estimated inflow loading
#' baseline <- ok_load(
#'   inflow_m3yr   = 45e6,
#'   tp_inflow_ugl = 120,
#'   tn_inflow_ugl = 1800,
#'   segment_label = "lacustrine",
#'   coefficients  = "oklahoma",
#'   ecoregion     = "Cross Timbers"
#' ) |>
#' ok_hydraulics(surface_area_ha = 890, mean_depth_m = 4.2)
#'
#' # Scenario analysis: what TP reduction gets us to mesotrophic?
#' scenarios <- list(
#'   list(label = "10% TP reduction",  tp_reduction = 0.10),
#'   list(label = "20% TP reduction",  tp_reduction = 0.20),
#'   list(label = "30% TP reduction",  tp_reduction = 0.30),
#'   list(label = "40% TP reduction",  tp_reduction = 0.40),
#'   list(label = "50% TP reduction",  tp_reduction = 0.50)
#' )
#'
#' results <- ok_scenario(
#'   baseline      = baseline,
#'   scenarios     = scenarios,
#'   target_class  = "mesotrophic"
#' )
#' print(results)
#'
#' @seealso \code{\link{ok_load}}, \code{\link{ok_scenario_sweep}}
#' @export
ok_scenario <- function(baseline,
                         scenarios,
                         include_baseline = TRUE,
                         target_tsi       = NULL,
                         target_class     = NULL) {

  assert_okBATHTUB(baseline)

  # baseline must be at least at hydraulics step
  if (!baseline$step %in% c("hydraulics", "retention", "inlake", "tsi")) {
    stop(
      "ok_scenario() requires baseline to have passed through ok_hydraulics(). ",
      "Current step: '", baseline$step, "'.",
      call. = FALSE
    )
  }

  # Resolve target_tsi from target_class
  if (!is.null(target_class) && is.null(target_tsi)) {
    target_tsi <- switch(
      tolower(target_class),
      "oligotrophic"   = 40,
      "mesotrophic"    = 50,
      "eutrophic"      = 70,
      stop(sprintf(
        "'target_class' must be one of 'oligotrophic', 'mesotrophic', or 'eutrophic'. Got '%s'.",
        target_class
      ), call. = FALSE)
    )
  }

  # Extract baseline parameters
  d_base  <- baseline$data
  meta    <- baseline$meta

  base_tp_inflow  <- d_base$tp_inflow_ugl
  base_tn_inflow  <- d_base$tn_inflow_ugl
  base_inflow_vol <- d_base$inflow_m3yr
  base_area_ha    <- d_base$surface_area_ha
  base_depth_m    <- d_base$mean_depth_m
  base_outflow    <- d_base$outflow_m3yr

  # Helper: run one complete pipeline pass with modified inputs
  .run_scenario <- function(tp_ugl, tn_ugl, inflow_vol, label) {
    ok_load(
      inflow_m3yr    = inflow_vol,
      tp_inflow_ugl  = tp_ugl,
      tn_inflow_ugl  = tn_ugl,
      segment_label  = meta$segment_label %||% "main",
      coefficients   = meta$coefficients  %||% "walker",
      ecoregion      = meta$ecoregion
    ) |>
    ok_hydraulics(
      surface_area_ha = base_area_ha,
      mean_depth_m    = base_depth_m,
      outflow_m3yr    = base_outflow
    ) |>
    ok_retention() |>
    ok_inlake()    |>
    ok_tsi()
  }

  # Helper: extract row from a result
  .extract_row <- function(r, label, base_tp) {
    d <- r$data
    tp_red_pct <- if (!is.null(base_tp) && base_tp > 0) {
      round(100 * (1 - d$tp_inflow_ugl / base_tp), 1)
    } else NA_real_

    data.frame(
      scenario         = label,
      tp_inflow_ugl    = round(d$tp_inflow_ugl,  1),
      tp_reduction_pct = tp_red_pct,
      tn_inflow_ugl    = round(d$tn_inflow_ugl  %||% NA_real_, 1),
      tp_inlake_ugl    = round(d$tp_inlake_ugl  %||% NA_real_, 1),
      tp_retention_pct = round(100 * (d$tp_retention_coeff %||% NA_real_), 1),
      tn_inlake_ugl    = round(d$tn_inlake_ugl  %||% NA_real_, 1),
      chla_ugl         = round(d$chla_ugl        %||% NA_real_, 2),
      secchi_m         = round(d$secchi_m        %||% NA_real_, 2),
      tsi_tp           = round(d$tsi_tp          %||% NA_real_, 1),
      tsi_chla         = round(d$tsi_chla        %||% NA_real_, 1),
      tsi_secchi       = round(d$tsi_secchi      %||% NA_real_, 1),
      tsi_mean         = round(d$tsi_mean        %||% NA_real_, 1),
      trophic_state    = d$trophic_state         %||% NA_character_,
      hrt_yr           = round(d$hydraulic_residence_time_yr %||% NA_real_, 3),
      coeff_source     = d$chla_coeff_source     %||% NA_character_,
      stringsAsFactors = FALSE
    )
  }

  rows <- list()

  # Baseline row
  if (include_baseline) {
    base_result <- .run_scenario(
      tp_ugl     = base_tp_inflow,
      tn_ugl     = base_tn_inflow,
      inflow_vol = base_inflow_vol,
      label      = "Baseline"
    )
    rows[["Baseline"]] <- .extract_row(base_result, "Baseline", base_tp_inflow)
  }

  # Scenario rows
  for (sc in scenarios) {

    if (is.null(sc$label))
      stop("Each scenario must have a 'label' field.", call. = FALSE)

    # Resolve TP inflow for this scenario
    sc_tp <- if (!is.null(sc$tp_inflow_ugl)) {
      sc$tp_inflow_ugl
    } else if (!is.null(sc$tp_reduction)) {
      if (sc$tp_reduction < 0 || sc$tp_reduction > 1)
        stop(sprintf("tp_reduction in scenario '%s' must be between 0 and 1.",
                     sc$label), call. = FALSE)
      base_tp_inflow * (1 - sc$tp_reduction)
    } else {
      base_tp_inflow
    }

    # Resolve TN inflow
    sc_tn <- if (!is.null(sc$tn_inflow_ugl)) {
      sc$tn_inflow_ugl
    } else if (!is.null(sc$tn_reduction) && !is.null(base_tn_inflow)) {
      base_tn_inflow * (1 - sc$tn_reduction)
    } else {
      base_tn_inflow
    }

    # Resolve inflow volume
    sc_vol <- if (!is.null(sc$flow_change)) {
      base_inflow_vol * (1 + sc$flow_change)
    } else {
      base_inflow_vol
    }

    sc_result <- .run_scenario(
      tp_ugl     = sc_tp,
      tn_ugl     = sc_tn,
      inflow_vol = sc_vol,
      label      = sc$label
    )

    rows[[sc$label]] <- .extract_row(sc_result, sc$label, base_tp_inflow)
  }

  result_df <- do.call(rbind, rows)
  rownames(result_df) <- NULL

  # Add target column if requested
  if (!is.null(target_tsi)) {
    result_df$target_tsi   <- target_tsi
    result_df$meets_target <- !is.na(result_df$tsi_mean) &
                               result_df$tsi_mean <= target_tsi
  }

  result_df
}


#' Sweep TP reduction scenarios automatically
#'
#' @description
#' A convenience wrapper around \code{ok_scenario()} that automatically
#' generates a sequence of TP reduction scenarios from 0 to max_reduction_pct percent in steps of step_pct percent. Useful for
#' generating load-response curves and finding the minimum reduction needed
#' to achieve a trophic state target.
#'
#' @param baseline An \code{okBATHTUB} object through \code{ok_hydraulics()}.
#' @param max_reduction_pct Numeric. Maximum TP reduction to evaluate (percent).
#'   Default 70.
#' @param step_pct Numeric. Step size between scenarios (percent). Default 5.
#' @param target_tsi Numeric. Optional TSI target passed to
#'   \code{ok_scenario()}. Default \code{NULL}.
#' @param target_class Character. Optional trophic class target
#'   (\code{"oligotrophic"}, \code{"mesotrophic"}, or \code{"eutrophic"})
#'   passed to \code{ok_scenario()}. Default \code{NULL}.
#'
#' @return A data frame as returned by \code{ok_scenario()}, with one row
#'   per reduction step plus the baseline.
#'
#' @examples
#' baseline <- ok_load(
#'   inflow_m3yr   = 45e6,
#'   tp_inflow_ugl = 120,
#'   ecoregion     = "Cross Timbers",
#'   coefficients  = "oklahoma"
#' ) |>
#' ok_hydraulics(surface_area_ha = 890, mean_depth_m = 4.2)
#'
#' sweep <- ok_scenario_sweep(baseline, target_class = "mesotrophic")
#' print(sweep[, c("scenario", "tp_inflow_ugl", "tsi_mean",
#'                 "trophic_state", "meets_target")])
#'
#' @seealso \code{\link{ok_scenario}}
#' @export
ok_scenario_sweep <- function(baseline,
                               max_reduction_pct = 70,
                               step_pct          = 5,
                               target_tsi        = NULL,
                               target_class      = NULL) {

  reductions <- seq(step_pct, max_reduction_pct, by = step_pct) / 100

  scenarios <- lapply(reductions, function(r) {
    list(
      label        = sprintf("%.0f%% TP reduction", r * 100),
      tp_reduction = r
    )
  })

  ok_scenario(
    baseline         = baseline,
    scenarios        = scenarios,
    include_baseline = TRUE,
    target_tsi       = target_tsi,
    target_class     = target_class
  )
}
