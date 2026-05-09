# tests/testthat/test-segment-scenario.R
# Tests for ok_segment(), ok_segment_chain(), ok_scenario(), ok_scenario_sweep()

# Helper: minimal two-segment setup
.riverine <- function(tp = 150, tn = 2200, inflow = 45e6) {
  ok_load(inflow_m3yr   = inflow,
          tp_inflow_ugl = tp,
          tn_inflow_ugl = tn,
          segment_label = "riverine") |>
  ok_hydraulics(surface_area_ha = 280, mean_depth_m = 3.1) |>
  ok_retention() |>
  ok_inlake()
}

.baseline_hyd <- function(tp = 120, inflow = 45e6) {
  ok_load(inflow_m3yr = inflow, tp_inflow_ugl = tp) |>
  ok_hydraulics(surface_area_ha = 890, mean_depth_m = 4.2)
}

# =============================================================================
# ok_segment() tests
# =============================================================================

test_that("ok_segment() returns an okBATHTUB object at step 'load'", {
  down <- ok_segment(.riverine(), segment_label = "lacustrine")
  expect_s3_class(down, "okBATHTUB")
  expect_equal(down$step, "load")
})

test_that("ok_segment() passes upstream tp_inlake as downstream tp_inflow", {
  up   <- .riverine()
  down <- ok_segment(up, segment_label = "lacustrine")
  expect_equal(down$data$tp_inflow_ugl, up$data$tp_inlake_ugl)
})

test_that("ok_segment() passes upstream tn_inlake as downstream tn_inflow", {
  up   <- .riverine()
  down <- ok_segment(up, segment_label = "lacustrine")
  expect_equal(down$data$tn_inflow_ugl, up$data$tn_inlake_ugl)
})

test_that("ok_segment() passes upstream inflow volume unchanged", {
  up   <- .riverine(inflow = 45e6)
  down <- ok_segment(up)
  expect_equal(down$data$inflow_m3yr, 45e6)
})

test_that("ok_segment() sets downstream segment label correctly", {
  down <- ok_segment(.riverine(), segment_label = "lacustrine")
  expect_equal(down$meta$segment_label, "lacustrine")
})

test_that("ok_segment() inherits coefficient set from upstream", {
  up   <- ok_load(inflow_m3yr = 45e6, tp_inflow_ugl = 150,
                   coefficients = "walker") |>
          ok_hydraulics(surface_area_ha = 280, mean_depth_m = 3.1) |>
          ok_retention() |>
          ok_inlake()
  down <- ok_segment(up)
  expect_equal(down$meta$coefficients, "walker")
})

test_that("ok_segment() overrides coefficient set when supplied", {
  up   <- .riverine()
  down <- ok_segment(up, coefficients = "walker")
  expect_equal(down$meta$coefficients, "walker")
})

test_that("ok_segment() requires upstream to be at inlake or tsi step", {
  # Passing a result only at retention step should error
  ret <- ok_load(inflow_m3yr = 45e6, tp_inflow_ugl = 150) |>
         ok_hydraulics(surface_area_ha = 280, mean_depth_m = 3.1) |>
         ok_retention()
  expect_error(ok_segment(ret), "ok_inlake")
})

test_that("ok_segment() downstream in-lake TP is less than or equal to upstream", {
  up   <- .riverine()
  down <- ok_segment(up) |>
    ok_hydraulics(surface_area_ha = 610, mean_depth_m = 5.8) |>
    ok_retention() |>
    ok_inlake()
  # Downstream in-lake TP should be <= upstream in-lake TP (retention occurs)
  expect_lte(down$data$tp_inlake_ugl, up$data$tp_inlake_ugl)
})

# --- ok_segment_chain() ---

test_that("ok_segment_chain() returns a named list of results", {
  segs <- list(
    list(label = "riverine",   surface_area_ha = 280, mean_depth_m = 3.1),
    list(label = "lacustrine", surface_area_ha = 610, mean_depth_m = 5.8)
  )
  result <- ok_segment_chain(
    inflow_m3yr   = 45e6,
    tp_inflow_ugl = 150,
    segments      = segs
  )
  expect_type(result, "list")
  expect_named(result, c("riverine", "lacustrine"))
})

test_that("ok_segment_chain() each result is at tsi step", {
  segs <- list(
    list(label = "riverine",   surface_area_ha = 280, mean_depth_m = 3.1),
    list(label = "lacustrine", surface_area_ha = 610, mean_depth_m = 5.8)
  )
  result <- ok_segment_chain(45e6, 150, segments = segs)
  expect_equal(result$riverine$step,   "tsi")
  expect_equal(result$lacustrine$step, "tsi")
})

test_that("ok_segment_chain() TP decreases from segment to segment", {
  segs <- list(
    list(label = "riverine",     surface_area_ha = 200, mean_depth_m = 3.0),
    list(label = "transitional", surface_area_ha = 350, mean_depth_m = 4.5),
    list(label = "lacustrine",   surface_area_ha = 550, mean_depth_m = 6.0)
  )
  result <- ok_segment_chain(45e6, 150, segments = segs)
  tp_r <- result$riverine$data$tp_inlake_ugl
  tp_t <- result$transitional$data$tp_inlake_ugl
  tp_l <- result$lacustrine$data$tp_inlake_ugl
  expect_gt(tp_r, tp_t)
  expect_gt(tp_t, tp_l)
})

test_that("ok_segment_chain() requires non-empty segments list", {
  expect_error(ok_segment_chain(45e6, 150, segments = list()), "non-empty")
})

# --- ok_segment_summary() ---

test_that("ok_segment_summary() returns a data frame with one row per segment", {
  segs <- list(
    list(label = "riverine",   surface_area_ha = 280, mean_depth_m = 3.1),
    list(label = "lacustrine", surface_area_ha = 610, mean_depth_m = 5.8)
  )
  chain  <- ok_segment_chain(45e6, 150, segments = segs)
  summ   <- ok_segment_summary(chain)
  expect_s3_class(summ, "data.frame")
  expect_equal(nrow(summ), 2)
  expect_equal(summ$segment, c("riverine", "lacustrine"))
})

# =============================================================================
# ok_scenario() tests
# =============================================================================

test_that("ok_scenario() returns a data frame", {
  scenarios <- list(list(label = "10% reduction", tp_reduction = 0.10))
  result    <- ok_scenario(.baseline_hyd(), scenarios = scenarios)
  expect_s3_class(result, "data.frame")
})

test_that("ok_scenario() includes baseline row by default", {
  scenarios <- list(list(label = "20% reduction", tp_reduction = 0.20))
  result    <- ok_scenario(.baseline_hyd(), scenarios = scenarios)
  expect_true("Baseline" %in% result$scenario)
  expect_equal(nrow(result), 2)
})

test_that("ok_scenario() can suppress baseline row", {
  scenarios <- list(list(label = "20% reduction", tp_reduction = 0.20))
  result    <- ok_scenario(.baseline_hyd(), scenarios = scenarios,
                            include_baseline = FALSE)
  expect_false("Baseline" %in% result$scenario)
  expect_equal(nrow(result), 1)
})

test_that("ok_scenario() tp_reduction correctly reduces inflow TP", {
  baseline  <- .baseline_hyd(tp = 120)
  scenarios <- list(list(label = "30% reduction", tp_reduction = 0.30))
  result    <- ok_scenario(baseline, scenarios)
  # 30% reduction: 120 * 0.70 = 84
  scenario_row <- result[result$scenario == "30% reduction", ]
  expect_equal(scenario_row$tp_inflow_ugl, 84, tolerance = 0.1)
})

test_that("ok_scenario() tp_reduction_pct is computed correctly", {
  baseline  <- .baseline_hyd(tp = 120)
  scenarios <- list(list(label = "25% reduction", tp_reduction = 0.25))
  result    <- ok_scenario(baseline, scenarios)
  sc_row    <- result[result$scenario == "25% reduction", ]
  expect_equal(sc_row$tp_reduction_pct, 25, tolerance = 0.01)
})

test_that("ok_scenario() absolute tp_inflow_ugl overrides reduction", {
  baseline  <- .baseline_hyd(tp = 120)
  scenarios <- list(list(label = "fixed 60", tp_inflow_ugl = 60))
  result    <- ok_scenario(baseline, scenarios)
  sc_row    <- result[result$scenario == "fixed 60", ]
  expect_equal(sc_row$tp_inflow_ugl, 60)
})

test_that("ok_scenario() in-lake TP decreases as tp_reduction increases", {
  baseline <- .baseline_hyd(tp = 120)
  scenarios <- list(
    list(label = "10%", tp_reduction = 0.10),
    list(label = "30%", tp_reduction = 0.30),
    list(label = "50%", tp_reduction = 0.50)
  )
  result <- ok_scenario(baseline, scenarios, include_baseline = FALSE)
  expect_gt(result$tp_inlake_ugl[1], result$tp_inlake_ugl[2])
  expect_gt(result$tp_inlake_ugl[2], result$tp_inlake_ugl[3])
})

test_that("ok_scenario() meets_target column added when target_tsi supplied", {
  baseline  <- .baseline_hyd(tp = 120)
  scenarios <- list(list(label = "50% reduction", tp_reduction = 0.50))
  result    <- ok_scenario(baseline, scenarios, target_tsi = 50)
  expect_true("meets_target" %in% names(result))
  expect_type(result$meets_target, "logical")
})

test_that("ok_scenario() target_class sets correct TSI threshold", {
  baseline  <- .baseline_hyd(tp = 5)  # very low TP -> should be mesotrophic
  scenarios <- list(list(label = "no change", tp_reduction = 0))
  result_m  <- ok_scenario(baseline, scenarios, target_class = "mesotrophic",
                             include_baseline = FALSE)
  expect_equal(result_m$target_tsi[1], 50)
})

test_that("ok_scenario() rejects invalid target_class", {
  expect_error(
    ok_scenario(.baseline_hyd(),
                scenarios    = list(list(label = "x", tp_reduction = 0.1)),
                target_class = "hypereutrophic"),
    "target_class"
  )
})

test_that("ok_scenario() rejects tp_reduction outside [0,1]", {
  expect_error(
    ok_scenario(.baseline_hyd(),
                scenarios = list(list(label = "bad", tp_reduction = 1.5))),
    "between 0 and 1"
  )
})

test_that("ok_scenario() requires baseline at hydraulics step or later", {
  bad_baseline <- ok_load(inflow_m3yr = 45e6, tp_inflow_ugl = 120)
  expect_error(
    ok_scenario(bad_baseline,
                scenarios = list(list(label = "x", tp_reduction = 0.1))),
    "ok_hydraulics"
  )
})

# --- ok_scenario_sweep() ---

test_that("ok_scenario_sweep() returns correct number of rows", {
  # 0% to 50% in steps of 10% = 5 scenarios + 1 baseline = 6 rows
  result <- ok_scenario_sweep(.baseline_hyd(),
                               max_reduction_pct = 50,
                               step_pct          = 10)
  expect_equal(nrow(result), 6)
})

test_that("ok_scenario_sweep() reductions are monotonically increasing", {
  result <- ok_scenario_sweep(.baseline_hyd(),
                               max_reduction_pct = 40,
                               step_pct          = 10)
  sc_only <- result[result$scenario != "Baseline", ]
  expect_true(all(diff(sc_only$tp_reduction_pct) > 0))
})

test_that("ok_scenario_sweep() passes target_class through to ok_scenario()", {
  result <- ok_scenario_sweep(.baseline_hyd(tp = 5),
                               max_reduction_pct = 20,
                               step_pct          = 10,
                               target_class      = "oligotrophic")
  expect_true("meets_target" %in% names(result))
  expect_equal(result$target_tsi[1], 40)
})
