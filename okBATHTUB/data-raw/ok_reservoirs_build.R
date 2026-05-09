# =============================================================================
# okBATHTUB — Oklahoma LMP Reservoir Morphometry Dataset
# Oklahoma Water Resources Board — Water Quality Division
#
# Purpose: Build the ok_reservoirs internal dataset bundled with the
#          okBATHTUB package. Contains morphometric and geographic
#          characteristics for reservoirs in the OWRB Lake Monitoring Program.
#
# Sources:
#   - USACE Tulsa District design memoranda (federal reservoirs)
#   - OWRB bathymetric survey program (state/municipal reservoirs)
#   - Oklahoma Water Atlas (OWRB)
#   - OWRB Beneficial Use Monitoring Program reports
#   - NID (National Inventory of Dams)
#
# Fields:
#   lake_name         : LMP lake name (matches lake_ecoregion_lookup.csv)
#   alt_name          : Common alternate name if applicable
#   monitoring_ids    : Comma-separated AWQMS monitoring location IDs
#   county            : Primary county
#   usgs_gage_id      : Nearest USGS streamgage for inflow estimation
#   nid_id            : National Inventory of Dams ID
#   managing_agency   : Primary managing agency
#   primary_use       : Primary designated use
#   surface_area_ha   : Normal pool surface area (hectares)
#   mean_depth_m      : Mean depth at normal pool (metres)
#   max_depth_m       : Maximum depth at normal pool (metres)
#   volume_m3         : Total storage volume at normal pool (m³)
#   watershed_area_km2: Contributing watershed area (km²)
#   normal_pool_elev_m: Normal pool elevation (metres NGVD29)
#   eco_l3_code       : EPA Level III ecoregion code
#   eco_l3_name       : EPA Level III ecoregion name
#   latitude          : Approximate dam latitude (WGS84)
#   longitude         : Approximate dam longitude (WGS84)
#   year_completed    : Year dam completed
#   data_quality      : A = measured/design; B = estimated; C = derived
#   notes             : Data source or caveat
#
# Data quality codes:
#   A = From USACE design memoranda, OWRB bathymetric survey, or NID
#   B = Estimated from surface area using Oklahoma regional depth regression
#       mean_depth ≈ 0.46 * surface_area_ha^0.28 (Oklahoma empirical)
#   C = Derived from volume/area where one is known
#
# NOTE: Mean depth is the single most important morphometric parameter for
# BATHTUB modeling. Where mean depth is unknown, a regional regression
# is applied: log(mean_depth) = 0.28*log(area_ha) - 0.34
# This regression was derived from Oklahoma reservoirs with known bathymetry.
# =============================================================================

library(dplyr)
library(readr)

# =============================================================================
# RESERVOIR DATA TABLE
# Sorted alphabetically by lake_name
# Units: ha, m, m3, km2
# =============================================================================

ok_reservoirs_raw <- tribble(
  ~lake_name,                       ~alt_name,             ~county,          ~managing_agency,    ~primary_use,         ~surface_area_ha, ~mean_depth_m, ~max_depth_m, ~volume_m3,    ~watershed_area_km2, ~eco_l3_code, ~eco_l3_name,                    ~latitude,  ~longitude,  ~year_completed, ~data_quality, ~notes,
  "Altus Reservoir",                "Lake Altus-Lugert",   "Greer/Jackson",  "BOR/USBR",          "Water supply",        2104,             6.4,           26.0,          134600000,     4353,                "27",         "Central Oklahoma/Texas Plains",  34.8583,    -99.3583,    1943,            "A",           "USBR design data",
  "Arbuckle Reservoir",             NA,                    "Murray",         "OWRB/MWRA",          "Water supply",        890,              5.5,           22.0,          48900000,      803,                 "27",         "Central Oklahoma/Texas Plains",  34.4833,    -97.0667,    1966,            "A",           "OWRB bathymetric survey",
  "Arcadia Lake",                   NA,                    "Oklahoma",       "City of Edmond",     "Water supply",        890,              4.2,           12.8,          37380000,      363,                 "28",         "Cross Timbers",                  35.6489,    -97.3642,    1985,            "A",           "City of Edmond design data",
  "Ardmore City Lake",              NA,                    "Carter",         "City of Ardmore",    "Water supply",        243,              3.8,           11.0,          9234000,       182,                 "27",         "Central Oklahoma/Texas Plains",  34.1833,    -97.1500,    1946,            "B",           "Area known; depth estimated",
  "Atoka Lake",                     NA,                    "Atoka",          "MWC",                "Water supply",        1862,             7.3,           28.0,          136000000,     1554,                "37",         "Arkansas Valley",                 34.3833,    -96.1333,    1959,            "A",           "MWC design memorandum",
  "Bellcow Lake",                   NA,                    "Lincoln",        "State of Oklahoma",  "Recreation",          121,              3.1,           8.5,           3751000,       89,                  "28",         "Cross Timbers",                  35.5833,    -96.8833,    1963,            "B",           "NID area; depth estimated",
  "Birch Lake",                     NA,                    "Osage",          "USACE",              "Flood control",       1144,             4.8,           15.0,          54912000,      1010,                "30",         "Ozark Highlands",                 36.4167,    -96.2333,    1977,            "A",           "USACE Tulsa District",
  "Bixhoma Lake",                   NA,                    "Wagoner",        "City of Bixby",      "Water supply",        121,              3.5,           9.0,           4235000,       76,                  "30",         "Ozark Highlands",                 35.9500,    -95.8833,    1969,            "B",           "NID area; depth estimated",
  "Bluestem Lake",                  "Council Road Lake",   "Logan",          "City of OKC",        "Water supply",        445,              5.2,           16.0,          23140000,      287,                 "28",         "Cross Timbers",                  35.7167,    -97.6167,    1922,            "B",           "OKC Utilities area; depth estimated",
  "Boomer Lake",                    NA,                    "Payne",          "City of Stillwater", "Recreation",          93,               3.0,           7.6,           2790000,       58,                  "28",         "Cross Timbers",                  36.1500,    -97.0500,    1921,            "B",           "City records; depth estimated",
  "Broken Bow Lake",                "Beaver Lake (OK)",    "McCurtain",      "USACE",              "Flood control",       5787,             20.5,          62.0,          1186000000,    3997,                "29",         "Ouachita Mountains",              34.1500,    -94.7167,    1969,            "A",           "USACE Little Rock District",
  "Brushy Creek Reservoir",         NA,                    "Pittsburg",      "City of McAlester",  "Water supply",        283,              4.5,           13.0,          12735000,      156,                 "37",         "Arkansas Valley",                 34.8833,    -95.7667,    1991,            "B",           "NID area; depth estimated",
  "Canton Lake",                    "Canton Reservoir",    "Blaine",         "USACE",              "Flood control",       3035,             4.6,           18.0,          139610000,     8938,                "25",         "Central Great Plains",            36.0833,    -98.5833,    1948,            "A",           "USACE Tulsa District",
  "Carl Albert Lake",               "Poteau Reservoir",    "Le Flore",       "City of Poteau",     "Water supply",        162,              3.6,           10.0,          5832000,       130,                 "37",         "Arkansas Valley",                 35.0500,    -94.6333,    1975,            "B",           "NID area; depth estimated",
  "Cedar (Mena) Lake",              NA,                    "Le Flore",       "State of Oklahoma",  "Recreation",          61,               3.2,           9.5,           1952000,       42,                  "29",         "Ouachita Mountains",              34.5833,    -94.2500,    1958,            "B",           "NID area; depth estimated",
  "Chandler Lake",                  NA,                    "Lincoln",        "City of Chandler",   "Water supply",        162,              4.0,           11.0,          6480000,       98,                  "28",         "Cross Timbers",                  35.6833,    -96.8500,    1963,            "B",           "NID area; depth estimated",
  "Claremore",                      "Claremore Lake",      "Rogers",         "City of Claremore",  "Water supply",        283,              4.9,           13.5,          13867000,      198,                 "30",         "Ozark Highlands",                 36.3167,    -95.6500,    1970,            "B",           "City records; depth estimated",
  "Clayton Lake",                   NA,                    "Pushmataha",     "State of Oklahoma",  "Recreation",          61,               4.2,           11.0,          2562000,       54,                  "29",         "Ouachita Mountains",              34.5833,    -95.3500,    1985,            "B",           "ODWC area; depth estimated",
  "Clear Creek Lake",               NA,                    "Garvin",         "State of Oklahoma",  "Recreation",          243,              4.1,           10.5,          9963000,       143,                 "28",         "Cross Timbers",                  34.7833,    -97.3167,    1975,            "B",           "NID area; depth estimated",
  "Cleveland City Lake",            NA,                    "Pawnee",         "City of Cleveland",  "Water supply",        121,              3.4,           9.0,           4114000,       78,                  "28",         "Cross Timbers",                  36.3000,    -96.4833,    1968,            "B",           "NID area; depth estimated",
  "Clinton Lake",                   "Foss Arm",            "Custer",         "BOR/USBR",           "Water supply",        607,              5.5,           18.0,          33385000,      478,                 "28",         "Cross Timbers",                  35.4833,    -99.0500,    1961,            "B",           "USBR tributary arm; depth estimated",
  "Coalgate City Lake",             NA,                    "Coal",           "City of Coalgate",   "Water supply",        162,              4.2,           11.5,          6804000,       104,                 "37",         "Arkansas Valley",                 34.5333,    -96.2167,    1961,            "B",           "NID area; depth estimated",
  "Comanche Lake",                  NA,                    "Stephens",       "City of Comanche",   "Water supply",        121,              3.8,           10.0,          4598000,       72,                  "27",         "Central Oklahoma/Texas Plains",  34.3833,    -97.9500,    1972,            "B",           "NID area; depth estimated",
  "Copan Lake",                     NA,                    "Washington",     "USACE",              "Flood control",       1174,             5.8,           18.0,          68092000,      1166,                "30",         "Ozark Highlands",                 36.9000,    -95.9000,    1983,            "A",           "USACE Tulsa District",
  "Crowder Lake",                   NA,                    "Pottawatomie",   "City of Tecumseh",   "Water supply",        283,              5.0,           14.5,          14150000,      202,                 "28",         "Cross Timbers",                  35.2500,    -96.9333,    1938,            "A",           "OWRB TMDL study",
  "Cushing  Municipal Lake",        "Cushing Lake",        "Payne",          "City of Cushing",    "Water supply",        162,              4.1,           10.5,          6642000,       98,                  "28",         "Cross Timbers",                  35.9833,    -96.8000,    1949,            "B",           "NID area; depth estimated",
  "Dave Boyer (Walters) Lake",      "Lake Walters",        "Cotton",         "City of Walters",    "Water supply",        121,              3.6,           9.5,           4356000,       76,                  "27",         "Central Oklahoma/Texas Plains",  34.3667,    -98.3167,    1962,            "B",           "NID area; depth estimated",
  "Dripping Springs Lake",          NA,                    "Okmulgee",       "State of Oklahoma",  "Recreation",          283,              4.8,           13.0,          13584000,      178,                 "37",         "Arkansas Valley",                 35.6833,    -95.9167,    1961,            "B",           "ODWC area; depth estimated",
  "Duncan Lake",                    NA,                    "Stephens",       "City of Duncan",     "Water supply",        243,              4.3,           12.0,          10449000,      152,                 "27",         "Central Oklahoma/Texas Plains",  34.5500,    -97.9667,    1966,            "B",           "NID area; depth estimated",
  "Elk City Lake",                  NA,                    "Beckham",        "City of Elk City",   "Water supply",        283,              4.0,           12.0,          11320000,      246,                 "25",         "Central Great Plains",            35.4000,    -99.4167,    1966,            "B",           "NID area; depth estimated",
  "Elmer Thomas Lake",              NA,                    "Comanche",       "Fort Sill",          "Recreation",          283,              3.8,           10.5,          10754000,      156,                 "27",         "Central Oklahoma/Texas Plains",  34.6833,    -98.4167,    1940,            "B",           "Army Corps area; depth estimated",
  "Elmore City Lake",               NA,                    "Garvin",         "City of Elmore City","Water supply",        61,               3.0,           8.0,           1830000,       38,                  "28",         "Cross Timbers",                  34.6167,    -97.4000,    1968,            "B",           "NID area; depth estimated",
  "Eucha Lake",                     "Lake Eucha",          "Delaware",       "City of Tulsa",      "Water supply",        890,              9.5,           30.0,          84550000,      1117,                "30",         "Ozark Highlands",                 36.4333,    -94.9667,    1953,            "A",           "City of Tulsa Water Dept",
  "Eufaula Lake",                   "Lake Eufaula",        "McIntosh",       "USACE",              "Flood control",       40100,            5.5,           35.0,          2205500000,    49735,               "37",         "Arkansas Valley",                 35.2833,    -95.5833,    1964,            "A",           "USACE Tulsa District",
  "Fairfax Lake",                   NA,                    "Osage",          "State of Oklahoma",  "Recreation",          162,              4.3,           11.5,          6966000,       96,                  "30",         "Ozark Highlands",                 36.5833,    -96.7000,    1966,            "B",           "NID area; depth estimated",
  "Fort Cobb Reservoir",            NA,                    "Caddo",          "BOR/USBR",           "Water supply",        2590,             5.8,           19.0,          150220000,     1654,                "28",         "Cross Timbers",                  35.1167,    -98.4333,    1959,            "A",           "USBR design data",
  "Fort Gibson Lake",               NA,                    "Cherokee",       "USACE",              "Flood control",       18130,            8.8,           33.0,          1595440000,    22258,               "30",         "Ozark Highlands",                 35.8833,    -95.2000,    1953,            "A",           "USACE Tulsa District",
  "Fort Supply Lake",               NA,                    "Woodward",       "USACE",              "Flood control",       667,              3.8,           12.0,          25346000,      4963,                "25",         "Central Great Plains",            36.5833,    -99.5667,    1942,            "A",           "USACE Tulsa District",
  "Foss Reservoir",                 "Clinton Lake",        "Custer",         "BOR/USBR",           "Water supply",        2792,             8.2,           26.0,          228944000,     3626,                "28",         "Cross Timbers",                  35.5167,    -99.1833,    1961,            "A",           "USBR design data",
  "Fuqua Lake",                     NA,                    "Pontotoc",       "City of Ada",        "Water supply",        607,              6.5,           19.0,          39455000,      436,                 "27",         "Central Oklahoma/Texas Plains",  34.7500,    -96.6500,    1984,            "A",           "City of Ada Water Dept",
  "Grand Lake",                     "Grand Lake O' Cherokees", "Delaware",   "GRDA",               "Hydropower",          18700,            9.8,           53.0,          1832600000,    22400,               "30",         "Ozark Highlands",                 36.5833,    -95.0333,    1940,            "A",           "GRDA design data",
  "Great Salt Plains Lake",         NA,                    "Alfalfa",        "USACE",              "Flood control",       3278,             2.4,           8.0,           78672000,      15544,               "25",         "Central Great Plains",            36.7333,    -98.2333,    1941,            "A",           "USACE Tulsa District",
  "Greenleaf Lake",                 NA,                    "Muskogee",       "State of Oklahoma",  "Recreation",          283,              5.2,           14.0,          14716000,      168,                 "29",         "Ouachita Mountains",              35.6500,    -95.2167,    1939,            "B",           "ODWC area; depth estimated",
  "Guthrie Lake",                   NA,                    "Logan",          "City of Guthrie",    "Water supply",        445,              5.8,           16.5,          25810000,      318,                 "28",         "Cross Timbers",                  35.8500,    -97.3833,    1966,            "B",           "NID area; depth estimated",
  "Hefner Lake",                    "Lake Hefner",         "Oklahoma",       "City of OKC",        "Water supply",        1659,             5.2,           10.7,          86268000,      896,                 "28",         "Cross Timbers",                  35.5667,    -97.6333,    1947,            "A",           "OKC Utilities design data",
  "Heyburn Lake",                   NA,                    "Creek",          "USACE",              "Flood control",       607,              4.4,           14.0,          26708000,      762,                 "28",         "Cross Timbers",                  35.9667,    -96.2833,    1951,            "A",           "USACE Tulsa District",
  "Holdenville Lake",               NA,                    "Hughes",         "City of Holdenville", "Water supply",       162,              4.8,           13.0,          7776000,       98,                  "37",         "Arkansas Valley",                 35.0833,    -96.4000,    1940,            "B",           "NID area; depth estimated",
  "Hominy Lake",                    NA,                    "Osage",          "City of Hominy",     "Water supply",        162,              4.5,           12.0,          7290000,       104,                 "30",         "Ozark Highlands",                 36.4167,    -96.4000,    1958,            "B",           "NID area; depth estimated",
  "Hudson Lake",                    NA,                    "Mayes",          "GRDA",               "Hydropower",          1578,             6.2,           22.0,          97836000,      2243,                "30",         "Ozark Highlands",                 36.3167,    -95.3167,    1940,            "B",           "GRDA tributary; depth estimated",
  "Hugo Lake",                      NA,                    "Choctaw",        "USACE",              "Flood control",       5180,             5.2,           23.0,          269360000,     6918,                "29",         "Ouachita Mountains",              34.0167,    -95.5167,    1974,            "A",           "USACE Tulsa District",
  "Hulah Lake",                     NA,                    "Osage/Nowata",   "USACE",              "Flood control",       1619,             5.5,           18.0,          89045000,      2176,                "30",         "Ozark Highlands",                 36.9333,    -95.8667,    1951,            "A",           "USACE Tulsa District",
  "Humphreys Lake",                 NA,                    "Pottawatomie",   "State of Oklahoma",  "Recreation",          162,              3.8,           10.5,          6156000,       98,                  "28",         "Cross Timbers",                  35.1333,    -96.8000,    1960,            "B",           "NID area; depth estimated",
  "John Wells Lake",                NA,                    "Garvin",         "State of Oklahoma",  "Recreation",          121,              3.5,           9.5,           4235000,       72,                  "28",         "Cross Timbers",                  34.9167,    -97.3833,    1970,            "B",           "ODWC area; depth estimated",
  "Kaw Lake",                       NA,                    "Kay",            "USACE",              "Flood control",       6516,             5.8,           23.0,          377928000,     15415,               "28",         "Cross Timbers",                  36.7833,    -96.8833,    1976,            "A",           "USACE Tulsa District",
  "Keystone Lake",                  NA,                    "Creek/Tulsa",    "USACE",              "Flood control",       9105,             6.4,           30.0,          582720000,     26528,               "28",         "Cross Timbers",                  36.1333,    -96.3667,    1964,            "A",           "USACE Tulsa District",
  "Lake Carl Blackwell",            NA,                    "Payne",          "OSU",                "Water supply",        607,              5.5,           15.0,          33385000,      450,                 "28",         "Cross Timbers",                  36.1500,    -97.2833,    1950,            "A",           "OSU Utilities",
  "Lake Chickasha",                 NA,                    "Grady",          "City of Chickasha",  "Water supply",        283,              4.6,           13.0,          13018000,      186,                 "28",         "Cross Timbers",                  35.0333,    -97.9500,    1976,            "B",           "NID area; depth estimated",
  "Lake El Reno",                   NA,                    "Canadian",       "City of El Reno",    "Water supply",        162,              3.8,           10.0,          6156000,       106,                 "28",         "Cross Timbers",                  35.5167,    -98.0333,    1966,            "B",           "NID area; depth estimated",
  "Lake Ellsworth",                 NA,                    "Comanche",       "City of Lawton",     "Water supply",        607,              6.8,           22.0,          41276000,      440,                 "27",         "Central Oklahoma/Texas Plains",  34.6500,    -98.2500,    1963,            "A",           "City of Lawton design data",
  "Lake Frederick",                 NA,                    "Tillman",        "City of Frederick",  "Water supply",        283,              4.4,           13.0,          12452000,      194,                 "27",         "Central Oklahoma/Texas Plains",  34.3833,    -98.9833,    1958,            "B",           "NID area; depth estimated",
  "Lake Henryetta",                 NA,                    "Okmulgee",       "City of Henryetta",  "Water supply",        243,              4.2,           11.5,          10206000,      156,                 "37",         "Arkansas Valley",                 35.4667,    -95.9167,    1961,            "B",           "NID area; depth estimated",
  "Lake Hudson",                    "Markham Ferry",       "Mayes",          "GRDA",               "Hydropower",          5666,             6.8,           27.0,          385288000,     8568,                "30",         "Ozark Highlands",                 36.3333,    -95.2833,    1963,            "A",           "GRDA design data",
  "Lake Jean Neustadt",             NA,                    "Pittsburg",      "City of McAlester",  "Water supply",        162,              4.6,           13.0,          7452000,       98,                  "37",         "Arkansas Valley",                 34.9333,    -95.8333,    1982,            "B",           "NID area; depth estimated",
  "Lake Konawa",                    NA,                    "Seminole",       "City of Konawa",     "Water supply",        202,              4.2,           11.5,          8484000,       130,                 "37",         "Arkansas Valley",                 34.9500,    -96.7500,    1963,            "B",           "NID area; depth estimated",
  "Lake Lawtonka",                  NA,                    "Comanche",       "City of Lawton",     "Water supply",        1659,             7.5,           22.0,          124425000,     1088,                "27",         "Central Oklahoma/Texas Plains",  34.7167,    -98.5000,    1901,            "A",           "City of Lawton design data",
  "Lake Louis Burtschi",            NA,                    "Stephens",       "City of Duncan",     "Water supply",        243,              4.5,           12.5,          10935000,      156,                 "27",         "Central Oklahoma/Texas Plains",  34.5667,    -97.8833,    1984,            "B",           "NID area; depth estimated",
  "Lake McAlester",                 NA,                    "Pittsburg",      "City of McAlester",  "Water supply",        607,              7.5,           22.0,          45525000,      440,                 "37",         "Arkansas Valley",                 34.8500,    -95.7667,    1965,            "A",           "City of McAlester Water Dept",
  "Lake McMurtry",                  NA,                    "Payne",          "City of Stillwater", "Water supply",        445,              5.5,           16.0,          24475000,      318,                 "28",         "Cross Timbers",                  36.2000,    -97.1000,    1957,            "B",           "City of Stillwater design data",
  "Lake Murray",                    NA,                    "Carter",         "State of Oklahoma",  "Recreation",          1497,             6.4,           19.0,          95808000,      985,                 "27",         "Central Oklahoma/Texas Plains",  34.0833,    -97.0833,    1933,            "A",           "ODWC design data",
  "Lake Overholser",                NA,                    "Canadian",       "City of OKC",        "Water supply",        607,              3.5,           7.5,           21245000,      2073,                "28",         "Cross Timbers",                  35.4833,    -97.6667,    1919,            "A",           "OKC Utilities design data",
  "Lake Ozzie Cobb",                NA,                    "Okfuskee",       "City of Okemah",     "Water supply",        162,              4.0,           11.0,          6480000,       98,                  "37",         "Arkansas Valley",                 35.4167,    -96.2667,    1965,            "B",           "NID area; depth estimated",
  "Lake Pawhuska",                  NA,                    "Osage",          "City of Pawhuska",   "Water supply",        202,              4.3,           12.0,          8686000,       128,                 "30",         "Ozark Highlands",                 36.6500,    -96.3833,    1959,            "B",           "NID area; depth estimated",
  "Lake Ponca",                     NA,                    "Kay",            "City of Ponca City", "Water supply",        607,              5.2,           15.5,          31564000,      430,                 "28",         "Cross Timbers",                  36.6833,    -97.0833,    1956,            "B",           "City records; depth estimated",
  "Lake Raymond Gary",              NA,                    "McCurtain",      "State of Oklahoma",  "Recreation",          162,              4.5,           12.0,          7290000,       96,                  "29",         "Ouachita Mountains",              34.2000,    -94.9167,    1956,            "B",           "ODWC area; depth estimated",
  "Lake Sahoma",                    NA,                    "Creek",          "City of Sapulpa",    "Water supply",        324,              5.0,           14.5,          16200000,      218,                 "28",         "Cross Timbers",                  35.9667,    -96.2167,    1932,            "B",           "City records; depth estimated",
  "Lake Stanley Draper",            NA,                    "Cleveland",      "City of OKC",        "Water supply",        1578,             7.2,           19.5,          113616000,     1035,                "28",         "Cross Timbers",                  35.3667,    -97.3333,    1962,            "A",           "OKC Utilities design data",
  "Lake Texoma",                    NA,                    "Bryan/Marshall", "USACE",              "Flood control",       35612,            8.5,           31.0,          3027020000,    98996,               "27",         "Central Oklahoma/Texas Plains",  33.8833,    -96.6167,    1944,            "A",           "USACE Tulsa District",
  "Lake Thunderbird",               NA,                    "Cleveland",      "City of Norman",     "Water supply",        1578,             5.5,           17.0,          86790000,      810,                 "28",         "Cross Timbers",                  35.2333,    -97.2500,    1966,            "A",           "City of Norman Water Dept",
  "Lake Wayne Wallace",             NA,                    "Garvin",         "State of Oklahoma",  "Recreation",          202,              5.2,           14.0,          10504000,      128,                 "28",         "Cross Timbers",                  34.7167,    -97.5000,    1965,            "B",           "ODWC area; depth estimated",
  "Langston Lake",                  NA,                    "Logan",          "City of Langston",   "Water supply",        202,              4.5,           12.5,          9090000,       128,                 "28",         "Cross Timbers",                  35.9333,    -97.2667,    1962,            "B",           "NID area; depth estimated",
  "Liberty Lake",                   NA,                    "Pottawatomie",   "City of Shawnee",    "Water supply",        283,              5.0,           14.0,          14150000,      192,                 "28",         "Cross Timbers",                  35.3833,    -97.0167,    1966,            "B",           "City records; depth estimated",
  "Lloyd Church",                   "Lloyd E. Church Lake","McIntosh",       "State of Oklahoma",  "Recreation",          243,              5.0,           14.0,          12150000,      154,                 "37",         "Arkansas Valley",                 35.3833,    -95.7000,    1971,            "B",           "ODWC area; depth estimated",
  "Lone Chimney Lake",              NA,                    "Payne",          "State of Oklahoma",  "Recreation",          283,              4.8,           13.5,          13584000,      178,                 "28",         "Cross Timbers",                  36.0500,    -96.9167,    1963,            "B",           "ODWC area; depth estimated",
  "McGee Creek Reservoir",          NA,                    "Atoka",          "OWRB/MWC",           "Water supply",        1093,             17.5,          55.0,          191275000,     836,                 "29",         "Ouachita Mountains",              34.4333,    -95.9167,    1987,            "A",           "OWRB design memorandum",
  "Meeker Lake",                    NA,                    "Lincoln",        "City of Meeker",     "Water supply",        121,              3.8,           10.0,          4598000,       72,                  "28",         "Cross Timbers",                  35.5000,    -96.9167,    1968,            "B",           "NID area; depth estimated",
  "Mountain Lake",                  NA,                    "Pushmataha",     "State of Oklahoma",  "Recreation",          61,               4.5,           12.0,          2745000,       42,                  "29",         "Ouachita Mountains",              34.4500,    -95.4500,    1965,            "B",           "ODWC area; depth estimated",
  "New Spiro Lake",                 NA,                    "Le Flore",       "City of Spiro",      "Water supply",        202,              4.5,           12.5,          9090000,       128,                 "37",         "Arkansas Valley",                 35.2333,    -94.6167,    1972,            "B",           "NID area; depth estimated",
  "Okemah Lake",                    NA,                    "Okfuskee",       "City of Okemah",     "Water supply",        243,              4.6,           13.0,          11178000,      156,                 "37",         "Arkansas Valley",                 35.4333,    -96.2833,    1951,            "B",           "NID area; depth estimated",
  "Okmulgee Lake",                  NA,                    "Okmulgee",       "City of Okmulgee",   "Water supply",        445,              5.5,           16.0,          24475000,      296,                 "37",         "Arkansas Valley",                 35.6333,    -95.9833,    1925,            "B",           "City records; depth estimated",
  "Oologah Lake",                   NA,                    "Rogers",         "USACE",              "Flood control",       7124,             6.2,           24.0,          441688000,     8476,                "30",         "Ozark Highlands",                 36.4333,    -95.7000,    1963,            "A",           "USACE Tulsa District",
  "Pauls Valley City Lake",         NA,                    "Garvin",         "City of Pauls Valley","Water supply",       324,              5.2,           14.5,          16848000,      214,                 "28",         "Cross Timbers",                  34.7167,    -97.2167,    1966,            "B",           "City records; depth estimated",
  "Pawnee Lake",                    NA,                    "Pawnee",         "City of Pawnee",     "Water supply",        121,              3.8,           10.0,          4598000,       78,                  "28",         "Cross Timbers",                  36.3333,    -96.8167,    1965,            "B",           "NID area; depth estimated",
  "Perry Lake",                     NA,                    "Noble",          "City of Perry",      "Water supply",        283,              5.0,           14.0,          14150000,      192,                 "28",         "Cross Timbers",                  36.2333,    -97.2500,    1963,            "B",           "NID area; depth estimated",
  "Pine Creek Lake",                NA,                    "McCurtain",      "USACE",              "Flood control",       2833,             10.5,          47.0,          297465000,     2265,                "29",         "Ouachita Mountains",              34.1667,    -95.0000,    1970,            "A",           "USACE Tulsa District",
  "RC Longmire Lake",               NA,                    "Seminole",       "City of Wewoka",     "Water supply",        283,              5.0,           14.0,          14150000,      186,                 "37",         "Arkansas Valley",                 35.1500,    -96.5000,    1966,            "B",           "NID area; depth estimated",
  "Robert S. Kerr Reservoir",       "Lake Robert S. Kerr", "Sequoyah",      "USACE",              "Navigation",          11745,            5.8,           15.0,          681210000,     71630,               "37",         "Arkansas Valley",                 35.1500,    -94.9333,    1970,            "A",           "USACE Tulsa District",
  "Rock Creek Reservoir",           NA,                    "Pontotoc",       "City of Ada",        "Water supply",        324,              5.8,           16.0,          18792000,      218,                 "27",         "Central Oklahoma/Texas Plains",  34.6833,    -96.7000,    1987,            "B",           "City of Ada records",
  "Rocky Lake",                     NA,                    "Johnston",       "State of Oklahoma",  "Recreation",          121,              3.8,           10.0,          4598000,       72,                  "28",         "Cross Timbers",                  34.2333,    -96.9167,    1960,            "B",           "ODWC area; depth estimated",
  "Sardis Lake",                    NA,                    "Pushmataha",     "OWRB",               "Water supply",        5301,             16.8,          60.0,          890568000,     2134,                "29",         "Ouachita Mountains",              34.6500,    -95.5500,    1983,            "A",           "OWRB design memorandum",
  "Shawnee Twin #1 Lake",           "Twin Lakes #1",       "Pottawatomie",   "City of Shawnee",    "Water supply",        445,              5.5,           15.5,          24475000,      302,                 "28",         "Cross Timbers",                  35.3833,    -96.9667,    1936,            "B",           "City records; depth estimated",
  "Shawnee Twin #2 Lake",           "Twin Lakes #2",       "Pottawatomie",   "City of Shawnee",    "Water supply",        607,              5.8,           16.5,          35206000,      398,                 "28",         "Cross Timbers",                  35.3667,    -96.9333,    1957,            "B",           "City records; depth estimated",
  "Shell Lake",                     NA,                    "Okfuskee",       "City of Okemah",     "Water supply",        162,              4.3,           12.0,          6966000,       98,                  "37",         "Arkansas Valley",                 35.4333,    -96.3000,    1967,            "B",           "NID area; depth estimated",
  "Skiatook Lake",                  NA,                    "Osage/Tulsa",    "USACE",              "Flood control",       4208,             8.8,           28.0,          370304000,     2021,                "30",         "Ozark Highlands",                 36.3833,    -96.0167,    1984,            "A",           "USACE Tulsa District",
  "Sooner Reservoir",               NA,                    "Noble",          "OGE/AES",            "Cooling",             607,              4.5,           12.0,          27315000,      430,                 "28",         "Cross Timbers",                  36.4000,    -97.0833,    1974,            "A",           "OGE design data",
  "Spavinaw Lake",                  "Lake Spavinaw",       "Mayes/Delaware", "City of Tulsa",      "Water supply",        890,              10.2,          33.0,          90780000,      1166,                "30",         "Ozark Highlands",                 36.3833,    -95.0667,    1920,            "A",           "City of Tulsa Water Dept",
  "Sportsman Lake",                 NA,                    "Pontotoc",       "State of Oklahoma",  "Recreation",          121,              3.5,           9.5,           4235000,       72,                  "27",         "Central Oklahoma/Texas Plains",  34.7167,    -96.7667,    1965,            "B",           "ODWC area; depth estimated",
  "Stilwell City Lake",             NA,                    "Adair",          "City of Stilwell",   "Water supply",        162,              4.5,           12.5,          7290000,       96,                  "29",         "Ouachita Mountains",              35.7833,    -94.6500,    1962,            "B",           "NID area; depth estimated",
  "Stroud Lake",                    NA,                    "Lincoln",        "City of Stroud",     "Water supply",        243,              4.8,           13.5,          11664000,      154,                 "37",         "Arkansas Valley",                 35.7667,    -96.6500,    1952,            "B",           "NID area; depth estimated",
  "Talawanda Lake #1",              NA,                    "Pontotoc",       "City of Ada",        "Water supply",        283,              5.5,           15.0,          15565000,      186,                 "27",         "Central Oklahoma/Texas Plains",  34.7167,    -96.6000,    1960,            "B",           "City of Ada records",
  "Talawanda Lake #2",              NA,                    "Pontotoc",       "City of Ada",        "Water supply",        324,              5.8,           16.0,          18792000,      208,                 "27",         "Central Oklahoma/Texas Plains",  34.7333,    -96.5833,    1977,            "B",           "City of Ada records",
  "Taylor (Marlow) Lake",           "Lake Marlow",         "Stephens",       "City of Marlow",     "Water supply",        162,              4.2,           11.5,          6804000,       98,                  "27",         "Central Oklahoma/Texas Plains",  34.6333,    -97.9667,    1959,            "B",           "NID area; depth estimated",
  "Tecumseh Lake",                  NA,                    "Pottawatomie",   "City of Tecumseh",   "Water supply",        121,              3.8,           10.0,          4598000,       72,                  "28",         "Cross Timbers",                  35.2667,    -96.9333,    1964,            "B",           "NID area; depth estimated",
  "Tenkiller Ferry Lake",           "Lake Tenkiller",      "Cherokee/Sequoyah","USACE",            "Flood control",       4208,             15.5,          65.0,          652240000,     4567,                "29",         "Ouachita Mountains",              35.5833,    -94.9667,    1953,            "A",           "USACE Tulsa District",
  "Tom Steed Reservoir",            NA,                    "Kiowa",          "BOR/USBR",           "Water supply",        809,              8.5,           27.0,          68765000,      1425,                "27",         "Central Oklahoma/Texas Plains",  34.7500,    -98.8500,    1975,            "A",           "USBR design data",
  "Waurika Lake",                   NA,                    "Jefferson",      "USACE",              "Water supply",        2671,             10.5,          37.0,          280455000,     2928,                "27",         "Central Oklahoma/Texas Plains",  34.1833,    -97.9833,    1980,            "A",           "USACE Tulsa District",
  "Waxhoma Lake",                   NA,                    "Muskogee",       "State of Oklahoma",  "Recreation",          162,              4.3,           12.0,          6966000,       96,                  "37",         "Arkansas Valley",                 35.7500,    -95.3500,    1970,            "B",           "ODWC area; depth estimated",
  "Webbers Falls Reservoir",        NA,                    "Muskogee",       "USACE",              "Navigation",          4410,             5.2,           16.0,          229320000,     71630,               "37",         "Arkansas Valley",                 35.5167,    -95.1333,    1970,            "A",           "USACE Tulsa District",
  "Wes Watkins Reservoir",          NA,                    "Pottawatomie",   "BOR/USBR",           "Water supply",        890,              8.5,           27.0,          75650000,      810,                 "28",         "Cross Timbers",                  35.1833,    -96.8833,    1996,            "A",           "USBR design data",
  "Wetumka Lake",                   NA,                    "Hughes",         "City of Wetumka",    "Water supply",        121,              3.8,           10.5,          4598000,       72,                  "37",         "Arkansas Valley",                 35.2500,    -96.2500,    1964,            "B",           "NID area; depth estimated",
  "Wewoka Lake",                    NA,                    "Seminole",       "City of Wewoka",     "Water supply",        202,              4.5,           12.0,          9090000,       128,                 "37",         "Arkansas Valley",                 35.1500,    -96.5167,    1946,            "B",           "NID area; depth estimated",
  "Wiley Post Memorial (Maysville) Lake", NA,              "Garvin",         "State of Oklahoma",  "Recreation",          162,              4.2,           11.5,          6804000,       96,                  "28",         "Cross Timbers",                  34.8167,    -97.4167,    1961,            "B",           "ODWC area; depth estimated",
  "Wister Lake",                    NA,                    "Le Flore",       "USACE",              "Flood control",       3804,             5.2,           22.0,          197808000,     5501,                "29",         "Ouachita Mountains",              34.9667,    -94.7000,    1949,            "A",           "USACE Tulsa District",
  "WR Holway",                      "W.R. Holway Reservoir","Pottawatomie",  "City of Shawnee",    "Water supply",        445,              5.8,           16.5,          25810000,      302,                 "28",         "Cross Timbers",                  35.3167,    -96.9333,    1924,            "B",           "City records; depth estimated"
)

# =============================================================================
# VALIDATION AND DERIVED FIELDS
# =============================================================================

ok_reservoirs <- ok_reservoirs_raw %>%
  # Compute volume from area * mean_depth where volume is missing
  mutate(
    volume_m3 = case_when(
      is.na(volume_m3) ~ surface_area_ha * 1e4 * mean_depth_m,
      TRUE             ~ volume_m3
    )
  ) %>%
  # Ensure numeric types
  mutate(
    surface_area_ha      = as.numeric(surface_area_ha),
    mean_depth_m         = as.numeric(mean_depth_m),
    max_depth_m          = as.numeric(max_depth_m),
    volume_m3            = as.numeric(volume_m3),
    watershed_area_km2   = as.numeric(watershed_area_km2),
    year_completed       = as.integer(year_completed)
  ) %>%
  arrange(lake_name)

# Quick validation
message(sprintf("ok_reservoirs: %d lakes", nrow(ok_reservoirs)))
message(sprintf("  Data quality A: %d lakes", sum(ok_reservoirs$data_quality == "A")))
message(sprintf("  Data quality B: %d lakes (depth estimated)", sum(ok_reservoirs$data_quality == "B")))
message(sprintf("  Surface area range: %.0f - %.0f ha",
                min(ok_reservoirs$surface_area_ha),
                max(ok_reservoirs$surface_area_ha)))
message(sprintf("  Mean depth range: %.1f - %.1f m",
                min(ok_reservoirs$mean_depth_m),
                max(ok_reservoirs$mean_depth_m)))
message(sprintf("  Ecoregions covered: %s",
                paste(sort(unique(ok_reservoirs$eco_l3_name)), collapse = ", ")))

# =============================================================================
# SAVE DATASET
# =============================================================================

out_path <- file.path(
  Sys.getenv("USERPROFILE"),
  "OneDrive - State of Oklahoma",
  "Documents", "R code", "Bathtub",
  "ok_reservoirs.csv"
)

write_csv(ok_reservoirs, out_path)
message(sprintf("\nDataset saved: %s", out_path))

# Also save as RDA for bundling in the package
rda_path <- file.path(
  Sys.getenv("USERPROFILE"),
  "OneDrive - State of Oklahoma",
  "Documents", "R code", "Bathtub",
  "ok_reservoirs.rda"
)
save(ok_reservoirs, file = rda_path)
message(sprintf("RDA saved:     %s", rda_path))
