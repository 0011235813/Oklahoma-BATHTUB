# tests/testthat/test-load.R
# Tests for ok_load() and ok_load_multi()

test_that("ok_load() returns an okBATHTUB object at step 'load'", {
  result <- ok_load(inflow_m3yr = 45e6, tp_inflow_ugl = 120)
  expect_s3_class(result, "okBATHTUB")
  expect_equal(result$step, "load")
})

test_that("ok_load() stores inflow and TP correctly", {
  result <- ok_load(inflow_m3yr = 45e6, tp_inflow_ugl = 120)
  expect_equal(result$data$inflow_m3yr,   45e6)
  expect_equal(result$data$tp_inflow_ugl, 120)
})

test_that("ok_load() computes TP load correctly", {
  # TP load (kg/yr) = tp_ugl * inflow_m3yr / 1e6
  # 120 ug/L * 45e6 m3/yr / 1e6 = 5400 kg/yr
  result <- ok_load(inflow_m3yr = 45e6, tp_inflow_ugl = 120)
  expect_equal(result$data$tp_load_kgyr, 5400)
})

test_that("ok_load() computes TN load correctly when supplied", {
  # 1800 ug/L * 45e6 m3/yr / 1e6 = 81000 kg/yr
  result <- ok_load(inflow_m3yr   = 45e6,
                    tp_inflow_ugl = 120,
                    tn_inflow_ugl = 1800)
  expect_equal(result$data$tn_load_kgyr, 81000)
})

test_that("ok_load() computes TSS load correctly when supplied", {
  # TSS: mg/L = g/m3, so 35 g/m3 * 45e6 m3/yr / 1e3 = 1575000 kg/yr
  result <- ok_load(inflow_m3yr    = 45e6,
                    tp_inflow_ugl  = 120,
                    tss_inflow_mgl = 35)
  expect_equal(result$data$tss_load_kgyr, 1575000)
})

test_that("ok_load() stores NULL TN when not supplied", {
  result <- ok_load(inflow_m3yr = 45e6, tp_inflow_ugl = 120)
  expect_null(result$data$tn_inflow_ugl)
  expect_null(result$data$tn_load_kgyr)
})

test_that("ok_load() stores segment label and coefficient set in meta", {
  result <- ok_load(inflow_m3yr   = 45e6,
                    tp_inflow_ugl = 120,
                    segment_label = "lacustrine",
                    coefficients  = "walker")
  expect_equal(result$meta$segment_label, "lacustrine")
  expect_equal(result$meta$coefficients,  "walker")
})

test_that("ok_load() stores ecoregion in meta when supplied", {
  result <- ok_load(inflow_m3yr   = 45e6,
                    tp_inflow_ugl = 120,
                    coefficients  = "oklahoma",
                    ecoregion     = "Cross Timbers")
  expect_equal(result$meta$ecoregion, "Cross Timbers")
})

# --- Input validation ---

test_that("ok_load() rejects non-positive inflow", {
  expect_error(ok_load(inflow_m3yr = 0,   tp_inflow_ugl = 120), "'inflow_m3yr'")
  expect_error(ok_load(inflow_m3yr = -1e6, tp_inflow_ugl = 120), "'inflow_m3yr'")
})

test_that("ok_load() rejects negative TP", {
  expect_error(ok_load(inflow_m3yr = 45e6, tp_inflow_ugl = -5), "'tp_inflow_ugl'")
})

test_that("ok_load() rejects non-numeric TP", {
  expect_error(ok_load(inflow_m3yr = 45e6, tp_inflow_ugl = "high"), "'tp_inflow_ugl'")
})

test_that("ok_load() accepts TP of zero", {
  expect_no_error(ok_load(inflow_m3yr = 45e6, tp_inflow_ugl = 0))
})

test_that("ok_load() rejects invalid coefficient set", {
  expect_error(
    ok_load(inflow_m3yr = 45e6, tp_inflow_ugl = 120, coefficients = "unknown"),
    "coefficients"
  )
})

test_that("ok_load() accepts custom coefficient list", {
  custom <- list(tp_retention_form = "larsen_mercier",
                  tn_settling_velocity = 10,
                  chla_intercept = -1.0, chla_slope = 1.3,
                  secchi_intercept = 0.5, secchi_slope = -0.4)
  expect_no_error(
    ok_load(inflow_m3yr = 45e6, tp_inflow_ugl = 120, coefficients = custom)
  )
})

# --- ok_load_multi() ---

test_that("ok_load_multi() computes flow-weighted mean TP correctly", {
  tribs <- data.frame(
    inflow_m3yr   = c(30e6, 15e6),
    tp_inflow_ugl = c(100,  160)
  )
  # FWM = (100*30e6 + 160*15e6) / 45e6 = 120 ug/L
  result <- ok_load_multi(tribs)
  expect_equal(result$data$tp_inflow_ugl, 120)
  expect_equal(result$data$inflow_m3yr,   45e6)
})

test_that("ok_load_multi() computes flow-weighted mean TN correctly", {
  tribs <- data.frame(
    inflow_m3yr   = c(30e6, 15e6),
    tp_inflow_ugl = c(100, 160),
    tn_inflow_ugl = c(1500, 2100)
  )
  # FWM TN = (1500*30e6 + 2100*15e6) / 45e6 = 1700 ug/L
  result <- ok_load_multi(tribs)
  expect_equal(result$data$tn_inflow_ugl, 1700)
})

test_that("ok_load_multi() requires a data frame", {
  expect_error(ok_load_multi(list(a = 1)), "data frame")
})

test_that("ok_load_multi() requires inflow_m3yr and tp_inflow_ugl columns", {
  bad <- data.frame(flow = c(1e6), tp = c(100))
  expect_error(ok_load_multi(bad), "missing required columns")
})
