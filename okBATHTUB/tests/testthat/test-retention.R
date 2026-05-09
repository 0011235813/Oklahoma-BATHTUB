# tests/testthat/test-retention.R
# Tests for ok_retention() — mathematical correctness of retention equations

# Helper: load + hydraulics
.base_hyd <- function(inflow = 45e6, tp = 120, tn = 1800,
                       area = 890, depth = 4.2, ...) {
  ok_load(inflow_m3yr = inflow, tp_inflow_ugl = tp,
          tn_inflow_ugl = tn, ...) |>
  ok_hydraulics(surface_area_ha = area, mean_depth_m = depth)
}

test_that("ok_retention() returns an okBATHTUB object at step 'retention'", {
  result <- .base_hyd() |> ok_retention()
  expect_s3_class(result, "okBATHTUB")
  expect_equal(result$step, "retention")
})

test_that("ok_retention() Larsen-Mercier TP retention is correct", {
  # R_tp = 1 / (1 + 1/sqrt(tau))
  # tau = (4.2 * 890 * 1e4) / 45e6
  result <- .base_hyd() |> ok_retention()
  tau <- result$data$hydraulic_residence_time_yr
  expected_r <- 1 / (1 + 1 / sqrt(tau))
  expect_equal(result$data$tp_retention_coeff, expected_r, tolerance = 1e-10)
})

test_that("ok_retention() TP retention is between 0 and 1", {
  result <- .base_hyd() |> ok_retention()
  expect_gte(result$data$tp_retention_coeff, 0)
  expect_lte(result$data$tp_retention_coeff, 1)
})

test_that("ok_retention() TN retention uses settling velocity form correctly", {
  # R_tn = ks / (ks + qs)  where ks = 10.0, qs = areal water load
  result <- .base_hyd() |> ok_retention()
  qs     <- result$data$areal_water_load_myr
  ks     <- 10.0
  expected_r_tn <- ks / (ks + qs)
  expect_equal(result$data$tn_retention_coeff, expected_r_tn, tolerance = 1e-10)
})

test_that("ok_retention() TN retention is NULL when TN not supplied", {
  result <- ok_load(inflow_m3yr = 45e6, tp_inflow_ugl = 120) |>
    ok_hydraulics(surface_area_ha = 890, mean_depth_m = 4.2) |>
    ok_retention()
  expect_null(result$data$tn_retention_coeff)
})

test_that("ok_retention() override replaces calculated TP retention", {
  result <- .base_hyd() |> ok_retention(tp_retention_override = 0.42)
  expect_equal(result$data$tp_retention_coeff, 0.42)
})

test_that("ok_retention() override replaces calculated TN retention", {
  result <- .base_hyd() |> ok_retention(tn_retention_override = 0.30)
  expect_equal(result$data$tn_retention_coeff, 0.30)
})

test_that("ok_retention() rejects TP override outside [0,1]", {
  expect_error(.base_hyd() |> ok_retention(tp_retention_override = 1.5),
               "0 and 1")
  expect_error(.base_hyd() |> ok_retention(tp_retention_override = -0.1),
               "0 and 1")
})

test_that("ok_retention() higher inflow increases retention coefficient", {
  # Higher inflow -> lower residence time -> lower retention
  low_flow  <- .base_hyd(inflow = 20e6) |> ok_retention()
  high_flow <- .base_hyd(inflow = 80e6) |> ok_retention()
  expect_gt(
    low_flow$data$tp_retention_coeff,
    high_flow$data$tp_retention_coeff
  )
})

test_that("ok_retention() deeper reservoir increases retention coefficient", {
  # Deeper -> larger volume -> longer residence time -> higher retention
  shallow <- .base_hyd(depth = 2.0) |> ok_retention()
  deep    <- .base_hyd(depth = 8.0) |> ok_retention()
  expect_gt(
    deep$data$tp_retention_coeff,
    shallow$data$tp_retention_coeff
  )
})
