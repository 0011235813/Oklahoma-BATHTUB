# tests/testthat/test-hydraulics.R
# Tests for ok_hydraulics()

# Helper: minimal valid load object
.base_load <- function(...) {
  ok_load(inflow_m3yr = 45e6, tp_inflow_ugl = 120, ...)
}

test_that("ok_hydraulics() returns an okBATHTUB object at step 'hydraulics'", {
  result <- .base_load() |> ok_hydraulics(surface_area_ha = 890, mean_depth_m = 4.2)
  expect_s3_class(result, "okBATHTUB")
  expect_equal(result$step, "hydraulics")
})

test_that("ok_hydraulics() converts surface area ha to m2 correctly", {
  result <- .base_load() |> ok_hydraulics(surface_area_ha = 890, mean_depth_m = 4.2)
  expect_equal(result$data$surface_area_m2, 890 * 1e4)
})

test_that("ok_hydraulics() computes volume correctly", {
  # V = mean_depth * surface_area_m2 = 4.2 * 8900000 = 37380000 m3
  result <- .base_load() |> ok_hydraulics(surface_area_ha = 890, mean_depth_m = 4.2)
  expect_equal(result$data$volume_m3, 4.2 * 890 * 1e4)
})

test_that("ok_hydraulics() computes hydraulic residence time correctly", {
  # tau = volume / outflow = 37380000 / 45000000 = 0.8307 yr
  result <- .base_load() |> ok_hydraulics(surface_area_ha = 890, mean_depth_m = 4.2)
  expected_tau <- (4.2 * 890 * 1e4) / 45e6
  expect_equal(result$data$hydraulic_residence_time_yr, expected_tau)
})

test_that("ok_hydraulics() computes areal water load correctly", {
  # qs = inflow / surface_area_m2 = 45e6 / 8900000 = 5.056 m/yr
  result <- .base_load() |> ok_hydraulics(surface_area_ha = 890, mean_depth_m = 4.2)
  expected_qs <- 45e6 / (890 * 1e4)
  expect_equal(result$data$areal_water_load_myr, expected_qs)
})

test_that("ok_hydraulics() uses inflow as outflow when outflow_m3yr is NULL", {
  result <- .base_load() |> ok_hydraulics(surface_area_ha = 890, mean_depth_m = 4.2)
  expect_equal(result$data$outflow_m3yr, 45e6)
})

test_that("ok_hydraulics() uses supplied outflow_m3yr when given", {
  result <- .base_load() |>
    ok_hydraulics(surface_area_ha = 890, mean_depth_m = 4.2,
                  outflow_m3yr = 40e6)
  expect_equal(result$data$outflow_m3yr, 40e6)
  # tau = volume / outflow = 37380000 / 40000000
  expected_tau <- (4.2 * 890 * 1e4) / 40e6
  expect_equal(result$data$hydraulic_residence_time_yr, expected_tau)
})

test_that("ok_hydraulics() rejects non-positive surface area", {
  expect_error(
    .base_load() |> ok_hydraulics(surface_area_ha = 0, mean_depth_m = 4.2),
    "'surface_area_ha'"
  )
  expect_error(
    .base_load() |> ok_hydraulics(surface_area_ha = -100, mean_depth_m = 4.2),
    "'surface_area_ha'"
  )
})

test_that("ok_hydraulics() rejects non-positive mean depth", {
  expect_error(
    .base_load() |> ok_hydraulics(surface_area_ha = 890, mean_depth_m = 0),
    "'mean_depth_m'"
  )
})

test_that("ok_hydraulics() requires an okBATHTUB input", {
  expect_error(
    ok_hydraulics(list(data = list()), surface_area_ha = 890, mean_depth_m = 4.2),
    "okBATHTUB"
  )
})

test_that("ok_hydraulics() requires ok_load() to have run first", {
  # Passing a raw list disguised is caught by pipeline order check
  fake <- structure(
    list(data = list(inflow_m3yr = 45e6), step = "tsi", meta = list()),
    class = "okBATHTUB"
  )
  expect_error(
    ok_hydraulics(fake, surface_area_ha = 890, mean_depth_m = 4.2),
    "ok_load"
  )
})
