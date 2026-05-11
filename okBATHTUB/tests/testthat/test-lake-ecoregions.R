# tests/testthat/test-lake-ecoregions.R
# Tests for the bundled ok_lake_ecoregions dataset and ok_lake_ecoregion()
# lookup function.

test_that("ok_lake_ecoregions exists and is non-empty", {
  expect_true(exists("ok_lake_ecoregions"))
  expect_s3_class(ok_lake_ecoregions, "data.frame")
  expect_gt(nrow(ok_lake_ecoregions), 100L)
})

test_that("ok_lake_ecoregions has all expected columns", {
  expected_cols <- c(
    "lake_name", "eco_l3_code", "eco_l3_name",
    "n_sites_total", "n_sites_tier1",
    "latitude", "longitude",
    "max_yrs_tp", "max_yrs_chla", "max_yrs_secchi", "max_yrs_tn"
  )
  expect_true(all(expected_cols %in% names(ok_lake_ecoregions)))
})

test_that("ok_lake_ecoregions data integrity", {
  # Lake names are unique and non-missing
  expect_false(any(duplicated(ok_lake_ecoregions$lake_name)))
  expect_false(any(is.na(ok_lake_ecoregions$lake_name)))

  # n_sites_tier1 cannot exceed n_sites_total
  expect_true(all(
    ok_lake_ecoregions$n_sites_tier1 <= ok_lake_ecoregions$n_sites_total,
    na.rm = TRUE
  ))

  # max_yrs values are within the 2000-2024 calibration window (0-25)
  for (col in c("max_yrs_tp", "max_yrs_chla", "max_yrs_secchi", "max_yrs_tn")) {
    expect_true(all(ok_lake_ecoregions[[col]] >= 0L &
                    ok_lake_ecoregions[[col]] <= 25L,
                    na.rm = TRUE),
                label = paste(col, "in [0,25]"))
  }

  # Coordinates plausible for Oklahoma + nearby border lakes
  expect_true(all(ok_lake_ecoregions$latitude >= 33 &
                  ok_lake_ecoregions$latitude <= 38,
                  na.rm = TRUE))
  expect_true(all(ok_lake_ecoregions$longitude >= -104 &
                  ok_lake_ecoregions$longitude <= -94,
                  na.rm = TRUE))
})

test_that("ok_lake_ecoregions ecoregion values are valid EPA L3 names", {
  valid_ecoregions <- c(
    "Central Great Plains", "Flint Hills",
    "Central Oklahoma/Texas Plains", "Cross Timbers",
    "Ouachita Mountains", "Ozark Highlands",
    "South Central Plains", "Arkansas Valley"
  )
  observed <- na.omit(unique(ok_lake_ecoregions$eco_l3_name))
  expect_true(all(observed %in% valid_ecoregions))
})

test_that("ok_lake_ecoregion() exact match returns single ecoregion name", {
  eco <- ok_lake_ecoregion("Arcadia Lake", exact = TRUE)
  expect_type(eco, "character")
  expect_length(eco, 1L)
  expect_equal(eco, "Cross Timbers")
})

test_that("ok_lake_ecoregion() exact match is case-insensitive", {
  eco <- ok_lake_ecoregion("arcadia lake", exact = TRUE)
  expect_equal(eco, "Cross Timbers")
})

test_that("ok_lake_ecoregion() partial match works", {
  result <- ok_lake_ecoregion("Tenkiller", exact = FALSE, simplify = FALSE)
  expect_s3_class(result, "data.frame")
  expect_gte(nrow(result), 1L)
  expect_true(any(grepl("Tenkiller", result$lake_name, ignore.case = TRUE)))
})

test_that("ok_lake_ecoregion() returns NA on no match (simplify = TRUE)", {
  expect_warning(
    eco <- ok_lake_ecoregion("Lake of Atlantis", exact = TRUE),
    "No lakes matched"
  )
  expect_true(is.na(eco))
})

test_that("ok_lake_ecoregion() returns empty data frame on no match (simplify = FALSE)", {
  expect_warning(
    result <- ok_lake_ecoregion("Lake of Atlantis",
                                exact = TRUE, simplify = FALSE),
    "No lakes matched"
  )
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0L)
})

test_that("ok_lake_ecoregion() rejects non-character input", {
  expect_error(ok_lake_ecoregion(42),    "character vector")
  expect_error(ok_lake_ecoregion(NULL),  "character vector")
})

test_that("ok_lake_ecoregion() result is usable in ok_load() pipeline", {
  eco <- ok_lake_ecoregion("Arcadia Lake", exact = TRUE)
  result <- ok_load(
    inflow_m3yr   = 45e6,
    tp_inflow_ugl = 120,
    coefficients  = "oklahoma",
    ecoregion     = eco
  )
  expect_equal(result$meta$ecoregion, "Cross Timbers")
})

test_that("Unmapped (NA-ecoregion) lakes exist and represent border lakes", {
  # The original lookup table includes a small number of border / out-of-state
  # lakes where the EPA OK ecoregion shapefile returned NA. They should
  # still have valid coordinates and lake names.
  unmapped <- ok_lake_ecoregions[is.na(ok_lake_ecoregions$eco_l3_name), ]
  if (nrow(unmapped) > 0L) {
    expect_true(all(!is.na(unmapped$lake_name)))
    expect_true(all(!is.na(unmapped$latitude)))
    expect_true(all(!is.na(unmapped$longitude)))
  }
})
