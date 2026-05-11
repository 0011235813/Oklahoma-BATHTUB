# tests/testthat/test-calibration-metadata.R
# Regression tests pinning calibration metadata to the values in
# data-raw/ok_calibration_report.xlsx (Final_Coefficients sheet).
#
# If you intentionally recalibrate, update both .oklahoma_coefficients()
# in R/okBATHTUB-package.R AND these tests. The XLSX is the source of
# truth; this test ensures the function and the XLSX cannot silently
# diverge again (the issue that led to the v0.1.1 -> v0.1.2 correction).

test_that("Cross Timbers Chl-a metadata matches calibration report", {
  c <- okBATHTUB:::.oklahoma_coefficients("Cross Timbers")
  expect_equal(c$chla_intercept, 0.2823, tolerance = 1e-4)
  expect_equal(c$chla_slope,     0.6171, tolerance = 1e-4)
  expect_equal(c$chla_r_squared, 0.391,  tolerance = 1e-3)
  expect_equal(c$chla_n_obs,     169L)
  expect_equal(c$chla_n_lakes,   36L)
  expect_equal(c$chla_source,    "oklahoma_ecoregion_crosstimbers")
})

test_that("Cross Timbers Secchi metadata matches calibration report", {
  c <- okBATHTUB:::.oklahoma_coefficients("Cross Timbers")
  expect_equal(c$secchi_intercept,  0.4334, tolerance = 1e-4)
  expect_equal(c$secchi_slope,     -0.5235, tolerance = 1e-4)
  expect_equal(c$secchi_r_squared,  0.359,  tolerance = 1e-3)
  expect_equal(c$secchi_n_obs,      169L)
  expect_equal(c$secchi_n_lakes,    36L)
  expect_equal(c$secchi_source,     "oklahoma_ecoregion_crosstimbers")
})

test_that("Cross Timbers Chl-a and Secchi share the same sample size", {
  # The joint TP+Chl-a+Secchi filter in ok_calibration.R guarantees this
  c <- okBATHTUB:::.oklahoma_coefficients("Cross Timbers")
  expect_equal(c$chla_n_obs,   c$secchi_n_obs)
  expect_equal(c$chla_n_lakes, c$secchi_n_lakes)
})

test_that("Central OK/TX Plains metadata matches calibration report", {
  c <- okBATHTUB:::.oklahoma_coefficients("Central Oklahoma/Texas Plains")
  expect_equal(c$chla_intercept,    0.0485, tolerance = 1e-4)
  expect_equal(c$chla_slope,        0.7462, tolerance = 1e-4)
  expect_equal(c$chla_n_obs,        24L)
  expect_equal(c$chla_n_lakes,      10L)
  expect_equal(c$secchi_intercept,  0.6489, tolerance = 1e-4)
  expect_equal(c$secchi_slope,     -0.5743, tolerance = 1e-4)
  expect_equal(c$secchi_n_obs,      24L)
  expect_equal(c$secchi_n_lakes,    10L)
})

test_that("Ozark Highlands Chl-a uses ecoregion fit; Secchi falls back", {
  c <- okBATHTUB:::.oklahoma_coefficients("Ozark Highlands")
  # Chl-a: ecoregion-specific
  expect_equal(c$chla_intercept, -0.1684, tolerance = 1e-4)
  expect_equal(c$chla_slope,      0.8021, tolerance = 1e-4)
  expect_equal(c$chla_source,     "oklahoma_ecoregion_ozark")
  expect_equal(c$chla_n_obs,      20L)
  expect_equal(c$chla_n_lakes,    14L)

  # Secchi: statewide pooled (Ozark Highlands ecoregion R^2=0.228 was
  # below 0.25 acceptance threshold)
  expect_equal(c$secchi_intercept,  0.4730, tolerance = 1e-4)
  expect_equal(c$secchi_slope,     -0.5330, tolerance = 1e-4)
  expect_equal(c$secchi_source,     "oklahoma_statewide")
  expect_equal(c$secchi_n_obs,      250L)
  expect_equal(c$secchi_n_lakes,    82L)
})

test_that("Statewide pooled fallback metadata matches calibration report", {
  c <- okBATHTUB:::.oklahoma_coefficients(NULL)
  expect_equal(c$chla_intercept,    0.1505, tolerance = 1e-4)
  expect_equal(c$chla_slope,        0.6715, tolerance = 1e-4)
  expect_equal(c$chla_r_squared,    0.442,  tolerance = 1e-3)
  expect_equal(c$chla_n_obs,        250L)
  expect_equal(c$chla_n_lakes,      82L)
  expect_equal(c$chla_source,       "oklahoma_statewide")

  expect_equal(c$secchi_intercept,  0.4730, tolerance = 1e-4)
  expect_equal(c$secchi_slope,     -0.5330, tolerance = 1e-4)
  expect_equal(c$secchi_r_squared,  0.364,  tolerance = 1e-3)
  expect_equal(c$secchi_n_obs,      250L)
  expect_equal(c$secchi_n_lakes,    82L)
})

test_that("Arkansas Valley falls back to statewide pooled (turbidity)", {
  c <- okBATHTUB:::.oklahoma_coefficients("Arkansas Valley")
  # Per CALIBRATION_README.md: Arkansas Valley Chl-a R^2 = 0.120 and
  # Secchi R^2 = 0.018 both rejected, fall back to statewide pooled
  expect_equal(c$chla_source,   "oklahoma_statewide")
  expect_equal(c$secchi_source, "oklahoma_statewide")
})

test_that("Unknown ecoregion falls back to statewide pooled with a message", {
  expect_message(
    c <- okBATHTUB:::.oklahoma_coefficients("Fake Ecoregion 99"),
    "statewide"
  )
  expect_equal(c$chla_source, "oklahoma_statewide")
})

test_that("Per-ecoregion sample sizes do not exceed the pooled total", {
  pooled <- okBATHTUB:::.oklahoma_coefficients(NULL)
  for (eco in c("Cross Timbers", "Central Oklahoma/Texas Plains",
                "Ozark Highlands")) {
    c <- okBATHTUB:::.oklahoma_coefficients(eco)
    expect_lte(c$chla_n_obs,   pooled$chla_n_obs,
               label = paste(eco, "Chl-a n <= pooled"))
    expect_lte(c$chla_n_lakes, pooled$chla_n_lakes,
               label = paste(eco, "Chl-a lakes <= pooled"))
    expect_lte(c$secchi_n_obs,   pooled$secchi_n_obs,
               label = paste(eco, "Secchi n <= pooled"))
    expect_lte(c$secchi_n_lakes, pooled$secchi_n_lakes,
               label = paste(eco, "Secchi lakes <= pooled"))
  }
})
