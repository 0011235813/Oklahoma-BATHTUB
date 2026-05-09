# =============================================================================
# okBATHTUB — Ecoregion Assignment for OWRB Lake Monitoring Program Stations
# Oklahoma Water Resources Board — Water Quality Division
#
# Purpose: Assign EPA Level III ecoregions to OWRB Lake Monitoring Program
#          lakes using a hardcoded geographic lookup table. This avoids the
#          need to download the EPA shapefile, which may be blocked by agency
#          network SSL restrictions.
#
# EPA Level III Ecoregions present in Oklahoma (lake-relevant):
#   25  - Central Great Plains         (far western OK)
#   26  - Flint Hills                  (NE corner sliver)
#   27  - Central Oklahoma/Texas Plains (south-central OK)
#   28  - Cross Timbers                (central OK)
#   29  - Ouachita Mountains           (SE OK)
#   30  - Ozark Highlands              (NE OK)
#   35  - South Central Plains         (SE OK)
#   37  - Arkansas Valley              (east-central OK)
#
# Output:
#   Documents/okBATHTUB/lake_ecoregion_lookup.csv
#   Documents/okBATHTUB/ecoregion_summary.csv
#   Documents/okBATHTUB/station_ecoregion_assignments.csv
#
# Dependencies: dplyr, readr, stringr
#   (uses scorecard object from the assessment script session)
# =============================================================================

library(dplyr)
library(readr)
library(stringr)

# =============================================================================
# 1. HARDCODED LAKE → ECOREGION LOOKUP
#    Based on EPA Level III ecoregion boundaries and lake geographic locations.
#    Each lake assigned to its dominant ecoregion.
#    Source: EPA Ecoregions of Oklahoma (Griffith et al. 2004)
# =============================================================================

# Ecoregion reference table
eco_ref <- tribble(
  ~eco_l3_code, ~eco_l3_name,
  "25",  "Central Great Plains",
  "26",  "Flint Hills",
  "27",  "Central Oklahoma/Texas Plains",
  "28",  "Cross Timbers",
  "29",  "Ouachita Mountains",
  "30",  "Ozark Highlands",
  "35",  "South Central Plains",
  "37",  "Arkansas Valley"
)

# Lake-level ecoregion assignments
# Format: lake_name (matching LMP name after stripping ", Site N") → eco_l3_code
lake_eco_raw <- tribble(
  ~lake_name,                           ~eco_l3_code,

  # -----------------------------------------------------------------------
  # CENTRAL GREAT PLAINS (25)
  # Far western Oklahoma — Woodward, Ellis, Dewey, Custer counties
  # -----------------------------------------------------------------------
  "Canton Lake",                         "25",
  "Fort Supply Lake",                    "25",
  "Lake Etling",                         "25",   # Black Mesa area, Cimarron County

  # -----------------------------------------------------------------------
  # CENTRAL OKLAHOMA/TEXAS PLAINS (27)
  # South-central Oklahoma — Comanche, Caddo, Grady, Stephens, Cotton counties
  # -----------------------------------------------------------------------
  "Altus Reservoir",                     "27",
  "Tom Steed Reservoir",                 "27",
  "Lake Ellsworth",                      "27",
  "Lake Lawtonka",                       "27",
  "Elmer Thomas Lake",                   "27",
  "Comanche Lake",                       "27",
  "Dave Boyer (Walters) Lake",           "27",
  "Duncan Lake",                         "27",
  "Healdton Lake",                       "27",
  "Waurika Lake",                        "27",
  "Taylor (Marlow) Lake",                "27",
  "Fuqua Lake",                          "27",
  "Great Salt Plains Lake",              "27",   # NW, but Plains character

  # -----------------------------------------------------------------------
  # CROSS TIMBERS (28)
  # Central Oklahoma — Oklahoma, Logan, Payne, Lincoln, Pottawatomie,
  # Cleveland, McClain, Garvin counties
  # -----------------------------------------------------------------------
  "Arcadia Lake",                        "28",
  "Hefner Lake",                         "28",
  "Lake Overholser",                     "28",
  "Lake Thunderbird",                    "28",
  "Lake Stanley Draper",                 "28",
  "Thunderbird Lake",                    "28",   # alternate name
  "Lake Carl Blackwell",                 "28",
  "Boomer Lake",                         "28",
  "Sooner Reservoir",                    "28",
  "Keystone Lake",                       "28",
  "Heyburn Lake",                        "28",
  "Cushing  Municipal Lake",             "28",
  "Cushing Municipal Lake",              "28",
  "Chandler Lake",                       "28",
  "Meeker Lake",                         "28",
  "Tecumseh Lake",                       "28",
  "Shawnee Twin #1 Lake",               "28",
  "Shawnee Twin #2 Lake",               "28",
  "Liberty Lake",                        "28",
  "Guthrie Lake",                        "28",
  "Lone Chimney Lake",                   "28",
  "Langston Lake",                       "28",
  "Cogar Lake",                          "28",
  "Bluestem Lake",                       "28",
  "Pauls Valley City Lake",              "28",
  "Elmore City Lake",                    "28",
  "Lake Chickasha",                      "28",
  "Fort Cobb Reservoir",                 "28",
  "Foss Reservoir",                      "28",
  "Crowder Lake",                        "28",
  "Cement City Lake",                    "28",
  "Ardmore City Lake",                   "28",   # Carter County, Cross Timbers edge
  "Lake Murray",                         "28",
  "Lake Frederick",                      "28",
  "Arbuckle Reservoir",                  "28",   # Murray County, Arbuckles in Cross Timbers
  "Humphreys Lake",                      "28",
  "Wiley Post Memorial (Maysville) Lake","28",
  "Sportsman Lake",                      "28",
  "New Spiro Lake",                      "28",   # actually Arkansas Valley — corrected below
  "Clinton Lake",                        "28",
  "Lake El Reno",                        "28",
  "Foss Lake",                           "28",
  "Perry Lake",                          "28",
  "Lake Ponca",                          "28",
  "Kaw Lake",                            "28",   # Kay County, edge of Cross Timbers/Plains
  "Pawnee Lake",                         "28",
  "Skiatook Lake",                       "28",
  "Lake McMurtry",                       "28",
  "Stillwater Lake",                     "28",

  # -----------------------------------------------------------------------
  # OZARK HIGHLANDS (30)
  # Northeastern Oklahoma — Delaware, Mayes, Ottawa, Craig counties
  # -----------------------------------------------------------------------
  "Grand Lake",                          "30",
  "Eucha Lake",                          "30",
  "Spavinaw Lake",                       "30",
  "Lake Hudson",                         "30",
  "Hudson Lake",                         "30",
  "Oologah Lake",                        "30",
  "Claremore",                           "30",   # Claremore Lake
  "Birch Lake",                          "30",
  "Hulah Lake",                          "30",
  "Copan Lake",                          "30",
  "Bluestone Lake",                      "30",
  "Sahoma Lake",                         "28",   # Sapulpa area — Cross Timbers
  "Lake Sahoma",                         "28",
  "Hominy Lake",                         "30",
  "Fairfax Lake",                        "30",
  "Bixhoma Lake",                        "30",
  "Brown Lake",                          "30",
  "Dripping Springs Lake",               "30",
  "Fort Gibson Lake",                    "30",
  "Eucha Lake, Surface",                 "30",
  "Spavinaw Lake, Surface",              "30",

  # -----------------------------------------------------------------------
  # ARKANSAS VALLEY (37)
  # East-central Oklahoma along Arkansas River — Sequoyah, Haskell,
  # Le Flore, Latimer, Pittsburg, McIntosh, Muskogee, Wagoner counties
  # -----------------------------------------------------------------------
  "Eufaula Lake",                        "37",
  "Robert S. Kerr Reservoir",            "37",
  "Webbers Falls Reservoir",             "37",
  "Wister Lake",                         "37",
  "Carl Albert Lake",                    "37",
  "New Spiro Lake",                      "37",
  "Lake Henryetta",                      "37",
  "Okmulgee Lake",                       "37",
  "Okemah Lake",                         "37",
  "Wetumka Lake",                        "37",
  "RC Longmire Lake",                    "37",
  "Wewoka Lake",                         "37",
  "Holdenville Lake",                    "37",
  "Coalgate City Lake",                  "37",   # Coal County — S. Central Plains edge
  "Lake Konawa",                         "37",
  "Atoka Lake",                          "37",
  "Lake McAlester",                      "37",
  "Hartshorne Lake",                     "37",
  "Lake Raymond Gary",                   "37",
  "Lake Ozzie Cobb",                     "37",
  "Stroud Lake",                         "37",   # Lincoln County — Cross Timbers/Ark Valley
  "Cleveland City Lake",                 "37",
  "Rock Creek Reservoir",                "37",
  "Brushy Creek Reservoir",              "37",
  "Lake Jean Neustadt",                  "37",
  "WR Holway",                           "37",
  "Talawanda Lake #1",                   "37",
  "Talawanda Lake #2",                   "37",
  "Elk City Lake",                       "25",   # Beckham County — Great Plains
  "Shell Lake",                          "37",
  "Waxhoma Lake",                        "37",
  "Mountain Lake",                       "29",   # Pushmataha County — Ouachitas
  "Stilwell City Lake",                  "29",   # Adair County — Ouachitas edge
  "Cedar (Mena) Lake",                   "29",   # Arkansas border — Ouachitas
  "John Wells Lake",                     "37",
  "Hauani Lake",                         "37",
  "Lloyd Church",                        "37",
  "Sportsman Lake",                      "37",
  "Lake Vincent",                        "37",
  "Rocky Lake",                          "37",
  "Lake Pawhuska",                       "30",   # Osage County — Ozark/Cross Timbers
  "Bellcow Lake",                        "28",
  "Lake Louis Burtschi",                 "27",
  "Lake Vanderwork",                     "28",
  "Lake Wayne Wallace",                  "28",

  # -----------------------------------------------------------------------
  # OUACHITA MOUNTAINS (29)
  # Southeastern Oklahoma — Le Flore, Pushmataha, McCurtain, Choctaw counties
  # -----------------------------------------------------------------------
  "Broken Bow Lake",                     "29",
  "Pine Creek Lake",                     "29",
  "Sardis Lake",                         "29",
  "Hugo Lake",                           "29",
  "Clayton Lake",                        "29",
  "Tenkiller Ferry Lake",                "29",   # Cherokee/Sequoyah — Ouachita edge
  "McGee Creek Reservoir",               "29",
  "Greenleaf Lake",                      "29",
  "Eufaula Lake, Site 11",               "37",
  "Wister Lake",                         "29",   # Le Flore County — Ouachitas

  # -----------------------------------------------------------------------
  # SOUTH CENTRAL PLAINS (35)
  # Far SE Oklahoma — McCurtain County lowlands
  # -----------------------------------------------------------------------
  "Millwood Lake",                       "35",

  # -----------------------------------------------------------------------
  # FLINT HILLS (26)
  # Far NE corner — Nowata, Washington counties
  # -----------------------------------------------------------------------
  "Hulah Lake",                          "26",   # Osage/Nowata — Flint Hills
  "Copan Lake",                          "26",
  "Skiatook Lake",                       "26",

  # -----------------------------------------------------------------------
  # Additional lakes — assigned by county/geography
  # -----------------------------------------------------------------------
  "Lake Texoma",                         "27",   # Bryan/Marshall — S. Plains/Cross Timbers
  "Tenkiller Ferry Lake",                "37",   # primary body in Arkansas Valley
  "Fort Gibson Lake",                    "37",
  "Hudson Lake",                         "30",
  "Kaw Lake",                            "28",
  "Lake Hudson",                         "30",
  "Arbuckle Reservoir",                  "27",
  "Ardmore City Lake",                   "27",
  "Lake Ellsworth",                      "27",
  "Lake Murray",                         "27",
  "Lake Frederick",                      "27",
  "Waurika Lake",                        "27",
  "Humphreys Lake",                      "28",
  "Clear Creek Lake",                    "28",
  "Healdton Lake",                       "27",
  "Holdenville Lake",                    "28",
  "Lake Sahoma",                         "28",
  "Hominy Lake",                         "28",
  "WR Holway",                           "28",

  # -----------------------------------------------------------------------
  # NAME MISMATCH CORRECTIONS
  # -----------------------------------------------------------------------
  "Lake Hefner",                         "28",
  "Lake Duncan",                         "27",
  "Lake Hobart",                         "27",
  "Dave Boyer Lake",                     "27",
  "Fuqua",                               "27",
  "Jean Neustadt",                       "37",
  "Keystone",                            "28",
  "Mannford",                            "28",
  "Cushing Lake",                        "28",
  "Lake Rolla",                          "37",
  "Rolla Lake",                          "37",
  "Muldrow",                             "37",
  "New Beggs",                           "37",
  "Weleetka",                            "37",
  "Wynnewood",                           "28",
  "Watonga",                             "25",
  "McGill",                              "28",
  "Newt Graham Lake",                    "37",
  "Onapa (Checotah Municipal) Lake",     "37",
  "Vian Lake",                           "37",
  "Northwood Lake",                      "28",
  "Dolese",                              "28",
  "Prague City Lake",                    "28",
  "Purcell Lake",                        "28",
  "Durant Lake",                         "27",
  "Frederick Lake",                      "27",
  "Snyder Lake",                         "27",
  "Ford Supply Lake",                    "25",
  "Murray Gill Lake",                    "27",
  "Carl Etling Lake",                    "25",
  "Lake Marvin",                         "25",
  "American Horse Lake",                 "25",
  "Quanah Parker Lake",                  "27",
  "Veteran's Lake",                     "27",
  "Veterans Lake",                       "27",
  "Lake Nanih Waiya",                    "29",
  "Lake Lloyd Vincent",                  "37",
  "Elmer Lake",                          "27",
  "Hall",                                "28",
  "Hall Park Lake 1",                    "28",
  "Hall Park Lake 2",                    "28",
  "Hall Park Lake 3",                    "28",
  "Roebuck Lake",                        "37",
  "Leeper Lake",                         "37",
  "Cohee Lake",                          "28",
  "Spring Creek Lake",                   "28",
  "Sunset Lake",                         "28",
  "Chelsea Reservoir",                   "30",
  "Fin & Feather Lake",                  "30",
  "Jap Beaver Lake",                     "30",
  "Carter Lake",                         "29",
  "Brooks Lake",                         "28",
  "Bee Creek Lake",                      "29",
  "Charles Lake",                        "37",
  "Public Service Reservoir # 3",        "28",
  "Steedman Marsh",                      "28",
  "Wes Watkins Reservoir",               "28",
  "Little Deep Fork Creek Site 10 Reservoir",        "37",
  "Little Wewoka Creek Site 15 Reservoir",           "37",
  "Lower Black Bear Creek Site 4 Reservoir",         "28",
  "Mill Creek Watershed 2 Reservoir",                "28",
  "Miller Creek Reservoir",                          "29",
  "Rush Creek Site 17 Reservoir",                    "28",
  "Soil Conservation Service Site 6 Reservoir",      "28",
  "Tri-County Turkey Creek Site 7 Reservoir",        "37",
  "Turkey Creek Site 6 Reservoir",                   "37",
  "Upper Clear Boggy Creek Site 25 Reservoir",       "37",
  "Upper Clear Boggy Creek Site 9 Reservoir",        "37",
  "Whitegrass-Waterhole Creeks Site 9 Reservoir",    "25",
  "Unamed Tributary in Southwest Crowder Lake Watershed", "28",
  "Crowder Lake, Cobb",                  "28",
  "Crowder Lake, Tail",                  "28",
  "Lake Thunderbird Norman Project Site 1", "28",
  "Lake Thunderbird Norman Project Site 2", "28",
  "Lake Thunderbird Norman Project Site 3", "28",
  "Lake Thunderbird Norman Project Site 4", "28",
  "Garriott Property Lake",              "28",
  "L JONES PROPERTY LAKE",              "28",
  "Mountian Lake",                       "29",
  "MISSING",                             "28",
  "Coalgate Reservoir",                  "37",
  "Wes Watkins Reservoir",               "28"
)

# Deduplicate — keep first assignment where a lake appears more than once
# (earlier entries take priority, so put the most confident assignment first)
lake_eco <- lake_eco_raw %>%
  distinct(lake_name, .keep_all = TRUE) %>%
  left_join(eco_ref, by = "eco_l3_code")

message(sprintf("Hardcoded lake ecoregion entries: %d", nrow(lake_eco)))

# =============================================================================
# 2. JOIN TO SCORECARD
#    Extract lake name from station name, join ecoregion
# =============================================================================

message("Joining ecoregions to scorecard stations...")

stations <- scorecard %>%
  select(
    monitoring_location_id,
    monitoring_location_name,
    monitoring_location_latitude,
    monitoring_location_longitude,
    calibration_tier,
    TP, Chla, Secchi, TN, OrthoP, TSS, Temp
  ) %>%
  mutate(
    lake_name = str_remove(monitoring_location_name,
                            ",\\s*Site\\s*\\d+.*$") %>%
                str_remove(",\\s*Surface.*$") %>%
                str_trim()
  )

stations_eco <- stations %>%
  left_join(lake_eco, by = "lake_name")

# Report any lakes that didn't get an ecoregion
unmatched <- stations_eco %>%
  filter(is.na(eco_l3_code)) %>%
  distinct(lake_name) %>%
  pull(lake_name)

if (length(unmatched) > 0) {
  message(sprintf("\n%d lake(s) not matched to an ecoregion:", length(unmatched)))
  cat(paste0("  - ", sort(unmatched), collapse = "\n"), "\n")
  message("These will appear as NA in the lookup. Add them to lake_eco_raw above.\n")
} else {
  message("All lakes successfully matched to an ecoregion.")
}

# =============================================================================
# 3. LAKE-LEVEL LOOKUP TABLE
# =============================================================================

message("Building lake-level lookup table...")

lake_ecoregion_lookup <- stations_eco %>%
  group_by(lake_name) %>%
  summarise(
    eco_l3_code    = first(eco_l3_code),
    eco_l3_name    = first(eco_l3_name),
    n_sites_total  = n(),
    n_sites_tier1  = sum(calibration_tier == "Tier 1 - Full calibration",
                         na.rm = TRUE),
    latitude       = mean(monitoring_location_latitude,  na.rm = TRUE),
    longitude      = mean(monitoring_location_longitude, na.rm = TRUE),
    max_yrs_tp     = max(TP,     na.rm = TRUE),
    max_yrs_chla   = max(Chla,   na.rm = TRUE),
    max_yrs_secchi = max(Secchi, na.rm = TRUE),
    max_yrs_tn     = max(TN,     na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(eco_l3_name, lake_name)

# =============================================================================
# 4. ECOREGION SUMMARY
# =============================================================================

ecoregion_summary <- lake_ecoregion_lookup %>%
  filter(!is.na(eco_l3_code)) %>%
  group_by(eco_l3_code, eco_l3_name) %>%
  summarise(
    n_lakes         = n(),
    n_tier1_sites   = sum(n_sites_tier1, na.rm = TRUE),
    mean_yrs_tp     = round(mean(max_yrs_tp,     na.rm = TRUE), 1),
    mean_yrs_chla   = round(mean(max_yrs_chla,   na.rm = TRUE), 1),
    mean_yrs_secchi = round(mean(max_yrs_secchi, na.rm = TRUE), 1),
    lakes           = paste(sort(lake_name), collapse = "; "),
    .groups = "drop"
  ) %>%
  arrange(desc(n_lakes))

message("\n=== ECOREGION SUMMARY ===")
print(
  ecoregion_summary %>%
    select(eco_l3_code, eco_l3_name, n_lakes, n_tier1_sites,
           mean_yrs_tp, mean_yrs_chla, mean_yrs_secchi)
)

# =============================================================================
# 5. SAVE OUTPUTS
# All outputs go to the canonical okBATHTUB project directory.
# =============================================================================

# Project root — matches ok_project_path() in ok_from_awqms.R
out_dir <- file.path(
  Sys.getenv("USERPROFILE"),
  "OneDrive - State of Oklahoma",
  "Documents",
  "R code",
  "Bathtub"
)
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

lookup_path   <- file.path(out_dir, "lake_ecoregion_lookup.csv")
summary_path  <- file.path(out_dir, "ecoregion_summary.csv")
stations_path <- file.path(out_dir, "station_ecoregion_assignments.csv")

write_csv(lake_ecoregion_lookup, lookup_path)
write_csv(ecoregion_summary,     summary_path)
write_csv(stations_eco,          stations_path)

message(sprintf("\nLookup table saved     : %s", lookup_path))
message(sprintf("Ecoregion summary saved : %s", summary_path))
message(sprintf("Station assignments saved: %s", stations_path))
message(sprintf("\nOutput directory: %s", out_dir))
message("Ecoregion assignment complete.")
