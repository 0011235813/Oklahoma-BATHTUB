# tests/testthat/test-inlake-tsi.R
# Tests for ok_inlake() and ok_tsi()

# Helper: run through retention
.base_ret <- function(tp = 120, tn = 1800, inflow = 45e6,
                       area = 890, depth = 4.2, ...) {
  ok_load(inflow_m3yr   = inflow,
          tp_inflow_ugl = tp,
          tn_inflow_ugl = tn, ...) |>
  ok_hydraulics(surface_area_ha = area, mean_depth_m = depth) |>
  ok_retention()
}

# Helper: full pipeline
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

test_that("ok_inlake() in-lake TP mass balance is correct", {
  ret    <- .base_ret()
  result <- ret |> ok_inlake()
  r_tp   <- ret$data$tp_retention_coeff
  tp_in  <- ret$data$tp_inflow_ugl
  # C_lake = C_in * (1 - R)
  expect_equal(result$data$tp_inlake_ugl,
               tp_in * (1 - r_tp),
               tolerance = 1e-10)
})

test_that("ok_inlake() in-lake TN mass balance is correct", {
  ret    <- .base_ret()
  result <- ret |> ok_inlake()
  r_tn   <- ret$data$tn_retention_coeff
  tn_in  <- ret$data$tn_inflow_ugl
  expect_equal(result$data$tn_inlake_ugl,
               tn_in * (1 - r_tn),
               tolerance = 1e-10)
})

test_that("ok_inlake() TN is NULL when not supplied", {
  result <- ok_load(inflow_m3yr = 45e6, tp_inflow_ugl = 120) |>
    ok_hydraulics(surface_area_ha = 890, mean_depth_m = 4.2) |>
    ok_retention() |>
    ok_inlake()
  expect_null(result$data$tn_inlake_ugl)
})

test_that("ok_inlake() Walker Chl-a prediction is mathematically correct", {
  # log10(chla) = -1.136 + 1.449 * log10(tp_inlake)
  result   <- .base_ret() |> ok_inlake()
  tp_lake  <- result$data$tp_inlake_ugl
  expected <- 10^(-1.136 + 1.449 * log10(tp_lake))
  expect_equal(result$data$chla_ugl, expected, tolerance = 1e-8)
})

test_that("ok_inlake() Walker Secchi prediction is mathematically correct", {
  # log10(secchi) = 0.616 + (-0.473) * log10(chla)
  result   <- .base_ret() |> ok_inlake()
  chla     <- result$data$chla_ugl
  expected <- 10^(0.616 + (-0.473) * log10(chla))
  expect_equal(result$data$secchi_m, expected, tolerance = 1e-8)
})

test_that("ok_inlake() Oklahoma Cross Timbers Chl-a is correct", {
  # log10(chla) = 0.2823 + 0.6171 * log10(tp_inlake)
  result  <- .base_ret(coefficients = "oklahoma",
                        ecoregion    = "Cross Timbers") |> ok_inlake()
  tp_lake <- result$data$tp_inlake_ugl
  expected <- 10^(0.2823 + 0.6171 * log10(tp_lake))
  expect_equal(result$data$chla_ugl, expected, tolerance = 1e-6)
  expect_equal(result$data$chla_coeff_source, "oklahoma_ecoregion")
})

test_that("ok_inlake() Oklahoma statewide Chl-a applied for unknown ecoregion", {
  result <- .base_ret(coefficients = "oklahoma",
                       ecoregion    = "Nonexistent Ecoregion") |> ok_inlake()
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
# ok_tsi() tests — Carlson (1977) equations
# =============================================================================

test_that("ok_tsi() returns an okBATHTUB object at step 'tsi'", {
  result <- .full_pipeline()
  expect_s3_class(result, "okBATHTUB")
  expect_equal(result$step, "tsi")
})

test_that("ok_tsi() TSI(TP) equation is correct", {
  # TSI(TP) = 14.42 * ln(TP) + 4.15
  result  <- .full_pipeline()
  tp_lake <- result$data$tp_inlake_ugl
  expected_tsi_tp <- 14.42 * log(tp_lake) + 4.15
  expect_equal(result$data$tsi_tp, expected_tsi_tp, tolerance = 1e-8)
})

test_that("ok_tsi() TSI(Chl-a) equation is correct", {
  # TSI(Chl-a) = 9.81 * ln(Chl-a) + 30.6
  result  <- .full_pipeline()
  chla    <- result$data$chla_ugl
  expected <- 9.81 * log(chla) + 30.6
  expect_equal(result$data$tsi_chla, expected, tolerance = 1e-8)
})

test_that("ok_tsi() TSI(Secchi) equation is correct", {
  # TSI(Secchi) = 60.0 - 14.41 * ln(Secchi)
  result  <- .full_pipeline()
  secchi  <- result$data$secchi_m
  expected <- 60.0 - 14.41 * log(secchi)
  expect_equal(result$data$tsi_secchi, expected, tolerance = 1e-8)
})

test_that("ok_tsi() mean TSI is average of available components", {
  result <- .full_pipeline()
  expected_mean <- mean(c(result$data$tsi_tp,
                           result$data$tsi_chla,
                           result$data$tsi_secchi))
  expect_equal(result$data$tsi_mean, expected_mean, tolerance = 1e-10)
})

test_that("ok_tsi() trophic state classification boundaries are correct", {
  # TSI < 40 = Oligotrophic
  expect_equal(.classify_trophic_state(30),  "Oligotrophic")
  expect_equal(.classify_trophic_state(39.9), "Oligotrophic")
  # 40-50 = Mesotrophic
  expect_equal(.classify_trophic_state(40),  "Mesotrophic")
  expect_equal(.classify_trophic_state(49.9), "Mesotrophic")
  # 50-70 = Eutrophic
  expect_equal(.classify_trophic_state(50),  "Eutrophic")
  expect_equal(.classify_trophic_state(69.9), "Eutrophic")
  # >= 70 = Hypereutrophic
  expect_equal(.classify_trophic_state(70),  "Hypereutrophic")
  expect_equal(.classify_trophic_state(90),  "Hypereutrophic")
})

test_that("ok_tsi() observed override replaces predicted values", {
  result <- .base_ret() |>
    ok_inlake() |>
    ok_tsi(observed_tp_ugl   = 85,
           observed_chla_ugl = 22,
           observed_secchi_m = 0.8)
  # TSI(TP) from observed TP=85
  expect_equal(result$data$tsi_tp, 14.42 * log(85) + 4.15, tolerance = 1e-8)
  # TSI(Chl-a) from observed Chl-a=22
  expect_equal(result$data$tsi_chla, 9.81 * log(22) + 30.6, tolerance = 1e-8)
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
})
