# data-raw/ok_lake_ecoregions_build.R
#
# Build the bundled ok_lake_ecoregions dataset from
# data-raw/lake_ecoregion_lookup.csv.
#
# Run from the package root:
#   source("data-raw/ok_lake_ecoregions_build.R")
#
# This regenerates data/ok_lake_ecoregions.rda. After running:
#   devtools::document()
#   devtools::install()

ok_lake_ecoregions <- read.csv(
  "data-raw/lake_ecoregion_lookup.csv",
  stringsAsFactors = FALSE,
  strip.white      = TRUE,
  na.strings       = c("", "NA", "N/A")
)

# Type coercions
int_cols <- c("n_sites_total", "n_sites_tier1",
              "max_yrs_tp", "max_yrs_chla",
              "max_yrs_secchi", "max_yrs_tn")
for (col in int_cols) {
  ok_lake_ecoregions[[col]] <- as.integer(ok_lake_ecoregions[[col]])
}
ok_lake_ecoregions$latitude  <- as.numeric(ok_lake_ecoregions$latitude)
ok_lake_ecoregions$longitude <- as.numeric(ok_lake_ecoregions$longitude)

# Sanity checks
stopifnot(
  nrow(ok_lake_ecoregions) > 0L,
  !any(duplicated(ok_lake_ecoregions$lake_name)),
  !any(is.na(ok_lake_ecoregions$lake_name)),
  all(ok_lake_ecoregions$n_sites_total >= ok_lake_ecoregions$n_sites_tier1,
      na.rm = TRUE)
)

cat(sprintf(
  "ok_lake_ecoregions: %d lakes (%d with assigned ecoregion, %d unmapped)\n",
  nrow(ok_lake_ecoregions),
  sum(!is.na(ok_lake_ecoregions$eco_l3_name)),
  sum(is.na(ok_lake_ecoregions$eco_l3_name))
))

usethis::use_data(ok_lake_ecoregions, overwrite = TRUE, compress = "bzip2")
