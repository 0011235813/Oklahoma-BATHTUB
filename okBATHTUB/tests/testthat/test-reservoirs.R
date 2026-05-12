# tests/testthat/test-reservoirs.R
# Tests for the bundled ok_reservoirs dataset and ok_reservoir() lookup.
# Includes a regression test (#1 in the v0.1.0 forensic review) ensuring
# the dataset is never silently empty.

test_that("ok_reservoirs dataset exists and is non-empty (BLOCKER regression)", {
  expect_true(exists("ok_reservoirs"))
  expect_s3_class(ok_reservoirs, "data.frame")
  expect_gt(nrow(ok_reservoirs), 20L)
})

test_that("ok_reservoirs dataset has all expected columns", {
  expected_cols <- c(
    "lake_name", "alt_name", "county", "managing_agency", "primary_use",
    "surface_area_ha", "mean_depth_m", "max_depth_m", "volume_m3",
    "watershed_area_km2", "eco_l3_code", "eco_l3_name",
    "latitude", "longitude", "year_completed", "data_quality", "notes"
  )
  expect_true(all(expected_cols %in% names(ok_reservoirs)))
})

test_that("ok_reservoirs data integrity", {
  expect_true(all(ok_reservoirs$surface_area_ha > 0, na.rm = TRUE))
  expect_true(all(ok_reservoirs$mean_depth_m   > 0, na.rm = TRUE))
  expect_true(all(ok_reservoirs$data_quality %in% c("A", "B")))
  expect_false(any(duplicated(ok_reservoirs$lake_name)))
  expect_true(all(ok_reservoirs$latitude  >= 33 & ok_reservoirs$latitude  <= 37,
                  na.rm = TRUE))  # Oklahoma latitude range
  expect_true(all(ok_reservoirs$longitude >= -103 & ok_reservoirs$longitude <= -94,
                  na.rm = TRUE))  # Oklahoma longitude range
})

test_that("ok_reservoir() finds Arcadia Lake by partial match", {
  res <- ok_reservoir("Arcadia")
  expect_gt(nrow(res), 0L)
  expect_true(any(grepl("Arcadia", res$lake_name, ignore.case = TRUE)))
})

test_that("ok_reservoir() exact match is case-insensitive", {
  res <- ok_reservoir("arcadia lake", exact = TRUE)
  expect_equal(nrow(res), 1L)
  expect_equal(res$lake_name, "Arcadia Lake")
})

test_that("ok_reservoir() filters by ecoregion", {
  res <- ok_reservoir(ecoregion = "Cross Timbers")
  expect_gt(nrow(res), 0L)
  expect_true(all(res$eco_l3_name == "Cross Timbers"))
})

test_that("ok_reservoir() filters by data quality", {
  res <- ok_reservoir(data_quality = "A")
  expect_gt(nrow(res), 0L)
  expect_true(all(res$data_quality == "A"))
})

test_that("ok_reservoir() warns on unmatched lake name", {
  expect_warning(
    res <- ok_reservoir("Lake Loch Ness"),
    "No reservoirs matched"
  )
  expect_equal(nrow(res), 0L)
})

test_that("ok_reservoir() result is usable in ok_hydraulics pipeline", {
  res <- ok_reservoir("Arcadia", exact = FALSE)
  expect_gt(nrow(res), 0L)
  pipe_result <- ok_load(inflow_m3yr = 45e6, tp_inflow_ugl = 120) |>
    ok_hydraulics(
      surface_area_ha = res$surface_area_ha[1],
      mean_depth_m    = res$mean_depth_m[1]
    )
  expect_s3_class(pipe_result, "okBATHTUB")
  expect_equal(pipe_result$step, "hydraulics")
})

test_that("ok_reservoir_summary() runs without error", {
  expect_output(ok_reservoir_summary(), "ok_reservoirs Coverage")
})

# ---------------------------------------------------------------------------
# Cross-dataset regression test (Finding #8 in the v0.1.2 forensic review)
# ---------------------------------------------------------------------------
# In v0.1.2, ok_reservoirs and ok_lake_ecoregions disagreed on ecoregion
# assignment for 18 of 26 lakes that appeared in both bundled datasets.
# v0.1.3 regenerated eco_l3_name in ok_reservoirs.csv from the authoritative
# lake_ecoregion_lookup.csv so the two datasets now agree. This test guards
# against future regressions.
test_that("ok_reservoirs and ok_lake_ecoregions agree on ecoregion for shared lakes", {
  # Build a join on exact lake_name (case-insensitive)
  res <- ok_reservoirs[, c("lake_name", "eco_l3_name")]
  lkp <- ok_lake_ecoregions[, c("lake_name", "eco_l3_name")]
  res$key <- tolower(res$lake_name)
  lkp$key <- tolower(lkp$lake_name)
  shared <- merge(res, lkp, by = "key", suffixes = c("_res", "_lkp"))

  # Drop rows where lookup ecoregion is NA (border/out-of-state lakes)
  shared <- shared[!is.na(shared$eco_l3_name_lkp), ]

  # If any rows remain, ecoregions must agree
  if (nrow(shared) > 0L) {
    mismatches <- shared[shared$eco_l3_name_res != shared$eco_l3_name_lkp, ]
    expect_equal(
      nrow(mismatches), 0L,
      info = paste(
        "Cross-dataset ecoregion mismatch in", nrow(mismatches), "lakes:",
        paste(mismatches$lake_name_res, collapse = ", ")
      )
    )
  }
})

test_that("All ok_reservoirs eco_l3_name values are valid EPA L3 names", {
  # Same valid-name set as test-lake-ecoregions, plus Southwestern Tablelands
  # which contains Optima Lake (panhandle) but no calibration lakes.
  valid_ecoregions <- c(
    "Central Great Plains", "Flint Hills",
    "Central Oklahoma/Texas Plains", "Cross Timbers",
    "Ouachita Mountains", "Ozark Highlands",
    "South Central Plains", "Arkansas Valley",
    "Southwestern Tablelands"
  )
  observed <- na.omit(unique(ok_reservoirs$eco_l3_name))
  expect_true(all(observed %in% valid_ecoregions),
              info = paste("Unexpected eco_l3_name values:",
                           paste(setdiff(observed, valid_ecoregions),
                                 collapse = ", ")))
})
