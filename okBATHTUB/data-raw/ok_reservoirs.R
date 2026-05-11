# data-raw/ok_reservoirs.R
#
# Rebuild the bundled ok_reservoirs dataset.
#
# Source:
#   data-raw/ok_reservoirs.csv -- curated CSV compiled from publicly
#   available sources (USACE Tulsa District design memoranda; National
#   Inventory of Dams; U.S. Bureau of Reclamation design data; published
#   Oklahoma Water Resources Board reports and Beneficial Use Monitoring
#   Program reports).
#
# Run from package root:
#   source("data-raw/ok_reservoirs.R")
#
# This regenerates data/ok_reservoirs.rda. After running, also run
#   devtools::document()
#   devtools::install()
#
# Data quality codes:
#   "A" = direct from authoritative source
#   "B" = mean depth estimated from Oklahoma regional regression
#         log10(mean_depth) = 0.28 * log10(area_ha) - 0.34

ok_reservoirs <- read.csv(
  "data-raw/ok_reservoirs.csv",
  stringsAsFactors = FALSE,
  strip.white      = TRUE,
  na.strings       = c("", "NA", "N/A")
)

# Coerce types explicitly
ok_reservoirs$surface_area_ha     <- as.numeric(ok_reservoirs$surface_area_ha)
ok_reservoirs$mean_depth_m        <- as.numeric(ok_reservoirs$mean_depth_m)
ok_reservoirs$max_depth_m         <- as.numeric(ok_reservoirs$max_depth_m)
ok_reservoirs$volume_m3           <- as.numeric(ok_reservoirs$volume_m3)
ok_reservoirs$watershed_area_km2  <- as.numeric(ok_reservoirs$watershed_area_km2)
ok_reservoirs$latitude            <- as.numeric(ok_reservoirs$latitude)
ok_reservoirs$longitude           <- as.numeric(ok_reservoirs$longitude)
ok_reservoirs$year_completed      <- as.integer(ok_reservoirs$year_completed)

# Sanity checks
stopifnot(
  nrow(ok_reservoirs) > 0L,
  all(ok_reservoirs$surface_area_ha > 0, na.rm = TRUE),
  all(ok_reservoirs$mean_depth_m > 0, na.rm = TRUE),
  all(ok_reservoirs$data_quality %in% c("A", "B")),
  !any(duplicated(ok_reservoirs$lake_name))
)

cat(sprintf(
  "ok_reservoirs: %d lakes, %d quality A, %d quality B\n",
  nrow(ok_reservoirs),
  sum(ok_reservoirs$data_quality == "A"),
  sum(ok_reservoirs$data_quality == "B")
))

usethis::use_data(ok_reservoirs, overwrite = TRUE, compress = "bzip2")
