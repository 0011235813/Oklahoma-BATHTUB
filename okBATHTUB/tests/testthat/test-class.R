# tests/testthat/test-class.R
# Tests for okBATHTUB S3 class, print(), summary(), pipeline order enforcement

.full <- function(tp = 120, tn = 1800) {
  ok_load(inflow_m3yr   = 45e6,
          tp_inflow_ugl = tp,
          tn_inflow_ugl = tn) |>
  ok_hydraulics(surface_area_ha = 890, mean_depth_m = 4.2) |>
  ok_retention() |>
  ok_inlake()    |>
  ok_tsi()
}

# =============================================================================
# S3 class
# =============================================================================

test_that("All pipeline steps return class 'okBATHTUB'", {
  r_load <- ok_load(inflow_m3yr = 45e6, tp_inflow_ugl = 120)
  r_hyd  <- r_load |> ok_hydraulics(surface_area_ha = 890, mean_depth_m = 4.2)
  r_ret  <- r_hyd  |> ok_retention()
  r_ink  <- r_ret  |> ok_inlake()
  r_tsi  <- r_ink  |> ok_tsi()

  for (r in list(r_load, r_hyd, r_ret, r_ink, r_tsi)) {
    expect_s3_class(r, "okBATHTUB")
  }
})

test_that("Pipeline step field is set correctly at each stage", {
  steps <- c("load", "hydraulics", "retention", "inlake", "tsi")
  r <- ok_load(inflow_m3yr = 45e6, tp_inflow_ugl = 120)
  expect_equal(r$step, "load")
  r <- r |> ok_hydraulics(surface_area_ha = 890, mean_depth_m = 4.2)
  expect_equal(r$step, "hydraulics")
  r <- r |> ok_retention()
  expect_equal(r$step, "retention")
  r <- r |> ok_inlake()
  expect_equal(r$step, "inlake")
  r <- r |> ok_tsi()
  expect_equal(r$step, "tsi")
})

test_that("Each pipeline step preserves data from all prior steps", {
  r <- .full()
  # Fields from every stage should all be present at tsi step
  expect_true("inflow_m3yr"                 %in% names(r$data))  # load
  expect_true("hydraulic_residence_time_yr" %in% names(r$data))  # hydraulics
  expect_true("tp_retention_coeff"          %in% names(r$data))  # retention
  expect_true("tp_inlake_ugl"               %in% names(r$data))  # inlake
  expect_true("tsi_tp"                      %in% names(r$data))  # tsi
})

# =============================================================================
# Pipeline order enforcement
# =============================================================================

test_that("ok_hydraulics() rejects non-okBATHTUB input", {
  expect_error(ok_hydraulics(42, surface_area_ha = 890, mean_depth_m = 4.2),
               "okBATHTUB")
})

test_that("ok_retention() rejects input that hasn't been through ok_hydraulics()", {
  r_load <- ok_load(inflow_m3yr = 45e6, tp_inflow_ugl = 120)
  expect_error(ok_retention(r_load), "ok_hydraulics")
})

test_that("ok_inlake() rejects input that hasn't been through ok_retention()", {
  r_hyd <- ok_load(inflow_m3yr = 45e6, tp_inflow_ugl = 120) |>
            ok_hydraulics(surface_area_ha = 890, mean_depth_m = 4.2)
  expect_error(ok_inlake(r_hyd), "ok_retention")
})

test_that("ok_tsi() rejects input that hasn't been through ok_inlake()", {
  r_ret <- ok_load(inflow_m3yr = 45e6, tp_inflow_ugl = 120) |>
            ok_hydraulics(surface_area_ha = 890, mean_depth_m = 4.2) |>
            ok_retention()
  expect_error(ok_tsi(r_ret), "ok_inlake")
})

# =============================================================================
# print() and summary() methods
# =============================================================================

test_that("print.okBATHTUB() runs without error at every pipeline step", {
  r <- ok_load(inflow_m3yr = 45e6, tp_inflow_ugl = 120)
  expect_output(print(r), "okBATHTUB")

  r <- r |> ok_hydraulics(surface_area_ha = 890, mean_depth_m = 4.2)
  expect_output(print(r), "okBATHTUB")

  r <- r |> ok_retention()
  expect_output(print(r), "okBATHTUB")

  r <- r |> ok_inlake()
  expect_output(print(r), "okBATHTUB")

  r <- r |> ok_tsi()
  expect_output(print(r), "okBATHTUB")
})

test_that("summary.okBATHTUB() runs without error after full pipeline", {
  expect_output(summary(.full()), "okBATHTUB")
})

test_that("summary.okBATHTUB() shows trophic state", {
  expect_output(summary(.full()), "Trophic state")
})

test_that("summary.okBATHTUB() shows TSI values", {
  expect_output(summary(.full()), "TSI")
})

test_that("summary.okBATHTUB() shows in-lake TP", {
  expect_output(summary(.full()), "TP")
})

# =============================================================================
# Pipe operator compatibility
# =============================================================================

test_that("Full pipeline works with base pipe operator |>", {
  result <- ok_load(inflow_m3yr = 45e6, tp_inflow_ugl = 120) |>
    ok_hydraulics(surface_area_ha = 890, mean_depth_m = 4.2) |>
    ok_retention() |>
    ok_inlake()    |>
    ok_tsi()
  expect_s3_class(result, "okBATHTUB")
  expect_equal(result$step, "tsi")
  expect_false(is.null(result$data$trophic_state))
})

# =============================================================================
# Null coalescing operator
# =============================================================================

test_that("%||% returns left when non-null", {
  expect_equal("a" %||% "b", "a")
  expect_equal(42  %||% 99,  42)
})

test_that("%||% returns right when left is null", {
  expect_equal(NULL %||% "b", "b")
  expect_equal(NULL %||% 99,  99)
})
