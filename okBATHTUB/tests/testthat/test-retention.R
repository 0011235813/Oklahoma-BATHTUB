# tests/testthat/test-retention.R
# Tests for ok_retention() under all supported model families

# Helper: build a load+hydraulics object with chosen coefficients
.base_hyd <- function(inflow = 45e6, tp = 120, tn = 1800,
                      area = 890, depth = 4.2, coefficients = "walker",
                      ecoregion = NULL, ...) {
  ok_load(inflow_m3yr   = inflow,
          tp_inflow_ugl = tp,
          tn_inflow_ugl = tn,
          coefficients  = coefficients,
          ecoregion     = ecoregion, ...) |>
  ok_hydraulics(surface_area_ha = area, mean_depth_m = depth)
}

test_that("ok_retention() returns an okBATHTUB object at step 'retention'", {
  result <- .base_hyd() |> ok_retention()
  expect_s3_class(result, "okBATHTUB")
  expect_equal(result$step, "retention")
})

# --- Walker BATHTUB Model 1 (default) ---

test_that("Walker Model 1 TP mass balance solves the quadratic correctly", {
  # P = (-1 + sqrt(1 + 4*K*A1*Pi*T)) / (2*K*A1*T)
  Pi  <- 120
  T   <- (4.2 * 890 * 1e4) / 45e6  # 0.831 yr
  Z   <- 4.2
  Qs  <- max(Z/T, 4.0)
  A1  <- 0.17 * Qs / (Qs + 13.3)
  K   <- 1.0
  expected_P <- (-1 + sqrt(1 + 4*K*A1*Pi*T)) / (2*K*A1*T)
  expected_R <- 1 - expected_P / Pi

  result <- .base_hyd() |> ok_retention()
  expect_equal(result$data$tp_retention_coeff, expected_R, tolerance = 1e-8)
  expect_equal(result$data$tp_retention_form, "walker_model1")
})

test_that("Walker Model 1 TN mass balance solves the quadratic correctly", {
  Ni  <- 1800
  T   <- (4.2 * 890 * 1e4) / 45e6
  Z   <- 4.2
  Qs  <- max(Z/T, 4.0)
  B1  <- 0.0045 * Qs / (Qs + 7.2)
  K   <- 1.0
  expected_N <- (-1 + sqrt(1 + 4*K*B1*Ni*T)) / (2*K*B1*T)
  expected_R <- 1 - expected_N / Ni

  result <- .base_hyd() |> ok_retention()
  expect_equal(result$data$tn_retention_coeff, expected_R, tolerance = 1e-8)
})

test_that("Walker Model 1 in-lake TP is reasonable for Cross Timbers-like reservoir", {
  result <- .base_hyd() |> ok_retention() |> ok_inlake()
  # Walker Model 1 predicts ~44 ug/L for this case (verified against
  # canonical BATHTUB quadratic solution)
  expect_gt(result$data$tp_inlake_ugl, 35)
  expect_lt(result$data$tp_inlake_ugl, 55)
})

# --- Vollenweider / Larsen-Mercier ---

test_that("Vollenweider TP retention matches R = 1/(1+1/sqrt(tau))", {
  result <- .base_hyd(coefficients = "vollenweider") |> ok_retention()
  tau <- result$data$hydraulic_residence_time_yr
  expected_r <- 1 / (1 + 1 / sqrt(tau))
  expect_equal(result$data$tp_retention_coeff, expected_r, tolerance = 1e-10)
  expect_equal(result$data$tp_retention_form, "vollenweider")
})

test_that("Vollenweider TP in-lake matches C_in / (1 + sqrt(tau))", {
  result <- .base_hyd(coefficients = "vollenweider") |>
            ok_retention() |>
            ok_inlake()
  tau <- result$data$hydraulic_residence_time_yr
  expected_tp <- 120 / (1 + sqrt(tau))
  expect_equal(result$data$tp_inlake_ugl, expected_tp, tolerance = 1e-10)
})

test_that("Vollenweider TN uses settling velocity form by default", {
  result <- .base_hyd(coefficients = "vollenweider") |> ok_retention()
  qs <- result$data$areal_water_load_myr
  expected_r_tn <- 10.0 / (10.0 + qs)
  expect_equal(result$data$tn_retention_coeff, expected_r_tn, tolerance = 1e-10)
})

# --- Common to all models ---

test_that("ok_retention() TP retention is always between 0 and 1", {
  for (coef in c("walker", "vollenweider")) {
    r <- .base_hyd(coefficients = coef) |> ok_retention()
    expect_gte(r$data$tp_retention_coeff, 0,
               label = paste("TP retention >= 0 for", coef))
    expect_lte(r$data$tp_retention_coeff, 1,
               label = paste("TP retention <= 1 for", coef))
  }
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
  expect_equal(result$data$tp_retention_form, "observed_override")
})

test_that("ok_retention() override replaces calculated TN retention", {
  result <- .base_hyd() |> ok_retention(tn_retention_override = 0.30)
  expect_equal(result$data$tn_retention_coeff, 0.30)
})

test_that("ok_retention() rejects TP override outside [0,1]", {
  expect_error(.base_hyd() |> ok_retention(tp_retention_override = 1.5),
               "between 0 and 1")
  expect_error(.base_hyd() |> ok_retention(tp_retention_override = -0.1),
               "non-negative")
})

test_that("Walker Model 1: higher inflow decreases retention", {
  # Higher inflow -> shorter tau -> less retention
  low_flow  <- .base_hyd(inflow = 20e6) |> ok_retention()
  high_flow <- .base_hyd(inflow = 80e6) |> ok_retention()
  expect_gt(
    low_flow$data$tp_retention_coeff,
    high_flow$data$tp_retention_coeff
  )
})

test_that("Walker Model 1: deeper reservoir increases retention", {
  shallow <- .base_hyd(depth = 2.0) |> ok_retention()
  deep    <- .base_hyd(depth = 8.0) |> ok_retention()
  expect_gt(
    deep$data$tp_retention_coeff,
    shallow$data$tp_retention_coeff
  )
})

test_that("Walker Model 1 predicts noticeably more retention than Vollenweider", {
  # For typical Cross Timbers reservoir, Walker Model 1's second-order
  # form predicts MORE retention than the Vollenweider first-order form
  # (this is a key qualitative difference and a regression test against
  # the v0.1.0 bug where Vollenweider was incorrectly called the default)
  r_w <- .base_hyd(coefficients = "walker") |> ok_retention()
  r_v <- .base_hyd(coefficients = "vollenweider") |> ok_retention()
  expect_gt(r_w$data$tp_retention_coeff, r_v$data$tp_retention_coeff)
})
