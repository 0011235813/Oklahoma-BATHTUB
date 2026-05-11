# tests/testthat/test-inlake-tsi.R
# Tests for ok_inlake() and ok_tsi()

.base_ret <- function(tp = 120, tn = 1800, inflow = 45e6,
                      area = 890, depth = 4.2,
                      coefficients = "walker", ecoregion = NULL, ...) {
  ok_load(inflow_m3yr   = inflow,
          tp_inflow_ugl = tp,
          tn_inflow_ugl = tn,
          coefficients  = coefficients,
          ecoregion     = ecoregion, ...) |>
  ok_hydraulics(surface_area_ha = area, mean_depth_m = depth) |>
  ok_retention()
}

.full_pipeline <- function(...) {
  .base_ret(...) |> ok_inlake() |> ok_tsi()
}

# =============================================================================
# ok_inlake() tests
# =============================================================================

test_that("ok_inlake() returns an okBATHTUB object at step 'inlake'", {
  result <- .base_ret() |> ok_inlake()
  expect_s3_class(result, "okBATHTUB")
  expect_equal(result$step, "inlake")
})

test_that("ok_inlake() TP mass balance C_lake = C_in * (1 - R) holds for all models", {
  for (coef in c("walker", "vollenweider")) {
    ret    <- .base_ret(coefficients = coef)
    result <- ret |> ok_inlake()
    expect_equal(
      result$data$tp_inlake_ugl,
      ret$data$tp_inflow_ugl * (1 - ret$data$tp_retention_coeff),
      tolerance = 1e-10,
      label = paste("mass balance for", coef)
    )
  }
})

test_that("ok_inlake() TN mass balance holds", {
  ret    <- .base_ret()
  result <- ret |> ok_inlake()
  expect_equal(result$data$tn_inlake_ugl,
               ret$data$tn_inflow_ugl * (1 - ret$data$tn_retention_coeff),
               tolerance = 1e-10)
})

test_that("ok_inlake() TN is NULL when not supplied", {
  result <- ok_load(inflow_m3yr = 45e6, tp_inflow_ugl = 120) |>
    ok_hydraulics(surface_area_ha = 890, mean_depth_m = 4.2) |>
    ok_retention() |>
    ok_inlake()
  expect_null(result$data$tn_inlake_ugl)
})

test_that("ok_inlake() Walker national Chl-a prediction is correct", {
  result   <- .base_ret(coefficients = "walker") |> ok_inlake()
  tp_lake  <- result$data$tp_inlake_ugl
  expected <- 10^(-1.136 + 1.449 * log10(tp_lake))
  expect_equal(result$data$chla_ugl, expected, tolerance = 1e-8)
})

test_that("ok_inlake() Walker national Secchi prediction is correct", {
  result   <- .base_ret(coefficients = "walker") |> ok_inlake()
  chla     <- result$data$chla_ugl
  expected <- 10^(0.616 + (-0.473) * log10(chla))
  expect_equal(result$data$secchi_m, expected, tolerance = 1e-8)
})

test_that("ok_inlake() Oklahoma Cross Timbers Chl-a is correct", {
  result  <- .base_ret(coefficients = "oklahoma",
                        ecoregion    = "Cross Timbers") |> ok_inlake()
  tp_lake <- result$data$tp_inlake_ugl
  expected <- 10^(0.2823 + 0.6171 * log10(tp_lake))
  expect_equal(result$data$chla_ugl, expected, tolerance = 1e-6)
  expect_equal(result$data$chla_coeff_source, "oklahoma_ecoregion_crosstimbers")
})

test_that("ok_inlake() Oklahoma statewide Chl-a applied for unknown ecoregion", {
  expect_message(
    result <- .base_ret(coefficients = "oklahoma",
                        ecoregion    = "Nonexistent Ecoregion") |> ok_inlake(),
    "statewide"
  )
  expect_equal(result$data$chla_coeff_source, "oklahoma_statewide")
})

test_that("ok_inlake() predict_chla = FALSE suppresses Chl-a and Secchi", {
  result <- .base_ret() |> ok_inlake(predict_chla = FALSE)
  expect_null(result$data$chla_ugl)
  expect_null(result$data$secchi_m)
})

test_that("ok_inlake() predict_secchi = FALSE suppresses only Secchi", {
  result <- .base_ret() |> ok_inlake(predict_secchi = FALSE)
  expect_false(is.null(result$data$chla_ugl))
  expect_null(result$data$secchi_m)
})

test_that("ok_inlake() in-lake TP is non-negative", {
  result <- .base_ret() |> ok_inlake()
  expect_gte(result$data$tp_inlake_ugl, 0)
})

# =============================================================================
# ok_tsi() tests
# =============================================================================

test_that("ok_tsi() returns an okBATHTUB object at step 'tsi'", {
  result <- .full_pipeline()
  expect_s3_class(result, "okBATHTUB")
  expect_equal(result$step, "tsi")
})

test_that("ok_tsi() TSI(TP) equation is correct", {
  result  <- .full_pipeline()
  tp_lake <- result$data$tp_inlake_ugl
  expected <- 14.42 * log(tp_lake) + 4.15
  expect_equal(result$data$tsi_tp, expected, tolerance = 1e-8)
})

test_that("ok_tsi() TSI(Chl-a) equation is correct", {
  result  <- .full_pipeline()
  chla    <- result$data$chla_ugl
  expected <- 9.81 * log(chla) + 30.6
  expect_equal(result$data$tsi_chla, expected, tolerance = 1e-8)
})

test_that("ok_tsi() TSI(Secchi) equation is correct", {
  result  <- .full_pipeline()
  secchi  <- result$data$secchi_m
  expected <- 60.0 - 14.41 * log(secchi)
  expect_equal(result$data$tsi_secchi, expected, tolerance = 1e-8)
})

test_that("ok_tsi() mean TSI is mean of available components", {
  result <- .full_pipeline()
  expected_mean <- mean(c(result$data$tsi_tp,
                          result$data$tsi_chla,
                          result$data$tsi_secchi))
  expect_equal(result$data$tsi_mean, expected_mean, tolerance = 1e-10)
  expect_equal(result$data$tsi_n, 3L)
})

test_that("ok_tsi() reports tsi_n correctly with partial components", {
  expect_message(
    result <- .base_ret() |>
              ok_inlake(predict_secchi = FALSE) |>
              ok_tsi(),
    "TSI mean computed from 2 of 3"
  )
  expect_equal(result$data$tsi_n, 2L)
  expect_null(result$data$tsi_secchi)
})

test_that("ok_tsi() trophic state boundaries are correct", {
  expect_equal(.classify_trophic_state(30),   "Oligotrophic")
  expect_equal(.classify_trophic_state(39.9), "Oligotrophic")
  expect_equal(.classify_trophic_state(40),   "Mesotrophic")
  expect_equal(.classify_trophic_state(49.9), "Mesotrophic")
  expect_equal(.classify_trophic_state(50),   "Eutrophic")
  expect_equal(.classify_trophic_state(69.9), "Eutrophic")
  expect_equal(.classify_trophic_state(70),   "Hypereutrophic")
  expect_equal(.classify_trophic_state(90),   "Hypereutrophic")
})

test_that("ok_tsi() observed override replaces predicted values", {
  result <- .base_ret() |>
    ok_inlake() |>
    ok_tsi(observed_tp_ugl   = 85,
           observed_chla_ugl = 22,
           observed_secchi_m = 0.8)
  expect_equal(result$data$tsi_tp,   14.42 * log(85)  + 4.15, tolerance = 1e-8)
  expect_equal(result$data$tsi_chla, 9.81  * log(22)  + 30.6, tolerance = 1e-8)
})

# --- ok_tsi_observed() standalone ---

test_that("ok_tsi_observed() produces correct TSI from known values", {
  obs <- ok_tsi_observed(tp_ugl = 50, chla_ugl = 15, secchi_m = 1.0)
  expect_equal(obs$tsi_tp,     14.42 * log(50) + 4.15,   tolerance = 1e-8)
  expect_equal(obs$tsi_chla,   9.81  * log(15) + 30.6,   tolerance = 1e-8)
  expect_equal(obs$tsi_secchi, 60.0  - 14.41 * log(1.0), tolerance = 1e-8)
})

test_that("ok_tsi_observed() requires at least one parameter", {
  expect_error(ok_tsi_observed(), "At least one")
})

test_that("ok_tsi_observed() works with only TP supplied", {
  obs <- ok_tsi_observed(tp_ugl = 50)
  expect_false(is.null(obs$tsi_tp))
  expect_null(obs$tsi_chla)
  expect_null(obs$tsi_secchi)
  expect_equal(obs$tsi_n, 1L)
})

# =============================================================================
# Reference-output regression tests
# =============================================================================

test_that("Walker Model 1 reference case: Arcadia-like reservoir", {
  # Inputs: Pi=120 ug/L, T=0.831 yr, Z=4.2 m
  # Expected (from canonical Walker BATHTUB Model 1 quadratic):
  #   Qs = max(4.2/0.831, 4) = 5.054
  #   A1 = 0.17 * 5.054 / (5.054 + 13.3) = 0.04681
  #   P = (-1 + sqrt(1 + 4*1*0.04681*120*0.831)) / (2*1*0.04681*0.831)
  #     = 44.16 ug/L
  result <- .base_ret(coefficients = "walker") |> ok_inlake()
  expect_equal(result$data$tp_inlake_ugl, 44.16, tolerance = 0.2)
})

test_that("Vollenweider reference case: Arcadia-like reservoir", {
  # Expected: C = 120 / (1 + sqrt(0.831)) = 62.77 ug/L
  result <- .base_ret(coefficients = "vollenweider") |> ok_inlake()
  expect_equal(result$data$tp_inlake_ugl, 62.77, tolerance = 0.1)
})
