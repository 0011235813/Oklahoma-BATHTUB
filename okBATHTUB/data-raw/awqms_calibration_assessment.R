# =============================================================================
# AWQMS Calibration Assessment Query for okBATHTUB
# Oklahoma Water Resources Board — Water Quality Division
#
# Purpose: Pull a structured summary of available lake water quality data
#          from AWQMS to assess calibration readiness for the okBATHTUB
#          reservoir eutrophication model.
#
# Output:  An Excel workbook with multiple assessment tabs saved to Documents.
#
# Instructions:
#   1. Make sure you are on the VPN or agency network
#   2. Credentials are pulled from keyring service "awqms_credentials"
#      If not yet set, run once:
#        keyring::key_set(service = "awqms_credentials", username = "oklahomawrb")
#   3. Source this script or run sections interactively
#   4. Send the output Excel file to Claude
#
# Confirmed AWQMS column names (ext.results_standard_vw):
#   - relative_depth          (not activity_relative_depth)
#   - result_measure_unit     (not result_unit)
#   - data_quality_level_name (not data_quality_level)
#   - result_detection_condition (not detection_condition)
#
# Confirmed AWQMS characteristic name mappings:
#   - TP         = "Phosphorus"
#   - Secchi     = "Depth, Secchi disk depth"
#   - Lake type  = "Lake"
# =============================================================================

library(DBI)
library(odbc)
library(keyring)
library(dplyr)
library(lubridate)
library(openxlsx)

# =============================================================================
# 1. AWQMS CONNECTION CONFIGURATION
# =============================================================================

AWQMS <- list(
  service        = "awqms_credentials",
  server         = "owrb.gselements.com",
  port           = 1433,
  database       = NULL,        # NULL = use driver default database
  default_schema = "ext",       # views live in the ext schema
  driver         = "SQL Server",
  default_user   = "oklahomawrb"
)

.awqms_connect <- function(cfg) {
  uid <- cfg$default_user

  pwd <- tryCatch(
    keyring::key_get(service = cfg$service, username = cfg$default_user),
    error = function(e) Sys.getenv("AWQMS_PWD", unset = NA)
  )

  if (is.na(pwd) || pwd == "") {
    stop(
      "AWQMS password not found in keyring.\n",
      "Run once to store it:\n",
      "  keyring::key_set(service = 'awqms_credentials', username = 'oklahomawrb')",
      call. = FALSE
    )
  }

  server_str <- paste0(cfg$server, ",", cfg$port)

  args <- list(
    drv                    = odbc::odbc(),
    Driver                 = cfg$driver,
    Server                 = server_str,
    UID                    = uid,
    PWD                    = pwd,
    trustServerCertificate = "yes"
  )

  if (!is.null(cfg$database)) args$Database <- cfg$database

  do.call(DBI::dbConnect, args)
}

message("Connecting to AWQMS...")
con <- .awqms_connect(AWQMS)
message("Connected successfully.")

# Schema-qualified view name
view_name <- paste0(AWQMS$default_schema, ".results_standard_vw")

# =============================================================================
# 2. MAIN DATA PULL
# Lake and River/Stream stations, last 25 years, BATHTUB target parameters.
# QC activity types excluded. Column names confirmed against actual view schema.
# =============================================================================

message("Pulling main dataset — this may take a minute...")

cutoff_date <- format(Sys.Date() - years(25), "%Y-%m-%d")

sql_main <- paste0("
  SELECT
    r.monitoring_location_id,
    r.monitoring_location_name,
    r.monitoring_location_type,
    r.monitoring_location_state,
    r.monitoring_location_latitude,
    r.monitoring_location_longitude,
    r.activity_type,
    r.relative_depth,
    CAST(r.activity_start_date AS DATE)      AS sample_date,
    YEAR(r.activity_start_date)              AS sample_year,
    MONTH(r.activity_start_date)             AS sample_month,
    r.characteristic_name,
    r.sample_fraction,
    r.result_measure                         AS result_value,
    r.result_measure_unit                    AS result_unit,
    r.result_status,
    r.data_quality_level_name                AS data_quality_level,
    r.result_detection_condition             AS detection_condition,
    r.result_depth_height,
    r.result_depth_height_unit
  FROM ", view_name, " r
  WHERE
    r.monitoring_location_type IN (
      'Lake',
      'River/Stream',
      'Wetland Palustrine Pond'
    )
    AND r.activity_start_date >= ?
    AND r.characteristic_name IN (
      'Phosphorus',
      'Orthophosphate',
      'Total Nitrogen, mixed forms',
      'Nitrogen',
      'Nitrate',
      'Nitrite',
      'Nitrate + Nitrite',
      'Kjeldahl nitrogen',
      'Total Kjeldahl nitrogen',
      'Ammonia',
      'Ammonia and ammonium',
      'Inorganic nitrogen (nitrate and nitrite)',
      'Chlorophyll a',
      'Chlorophyll a, corrected for pheophytin',
      'Chlorophyll a (probe relative fluorescence)',
      'Chlorophyll a (probe)',
      'Pheophytin a',
      'Depth, Secchi disk depth',
      'Turbidity',
      'Total suspended solids',
      'Suspended Sediment Concentration (SSC)',
      'Temperature, water',
      'Dissolved oxygen (DO)',
      'Dissolved oxygen saturation',
      'Specific conductance',
      'pH'
    )
    AND r.result_status NOT IN ('Rejected', 'Invalidated')
    AND r.activity_type NOT LIKE 'Quality Control%'
")

df_raw <- dbGetQuery(con, sql_main, params = list(cutoff_date))
message(sprintf("Raw records pulled: %s", format(nrow(df_raw), big.mark = ",")))

# =============================================================================
# 3. SPLIT LAKE VS STREAM
# =============================================================================

lake_data   <- df_raw %>% filter(monitoring_location_type == "Lake")
stream_data <- df_raw %>% filter(monitoring_location_type == "River/Stream")

message(sprintf("Lake records:   %s", format(nrow(lake_data),   big.mark = ",")))
message(sprintf("Stream records: %s", format(nrow(stream_data), big.mark = ",")))

# =============================================================================
# 4. SUMMARY TABLES
# =============================================================================

# ---- 4a. Parameter inventory by location type --------------------------------

param_inventory <- df_raw %>%
  group_by(monitoring_location_type, characteristic_name, sample_fraction) %>%
  summarise(
    n_records    = n(),
    n_locations  = n_distinct(monitoring_location_id),
    n_years      = n_distinct(sample_year),
    year_min     = min(sample_year, na.rm = TRUE),
    year_max     = max(sample_year, na.rm = TRUE),
    pct_detected = round(
      100 * mean(
        is.na(detection_condition) | detection_condition == "",
        na.rm = TRUE
      ), 1
    ),
    .groups = "drop"
  ) %>%
  arrange(monitoring_location_type, characteristic_name, sample_fraction)

# ---- 4b. Lake parameter coverage per station --------------------------------
# Years of data per lake station per BATHTUB parameter group

lake_coverage <- lake_data %>%
  mutate(
    param_group = case_when(
      characteristic_name == "Phosphorus"
        ~ "TP",
      characteristic_name == "Orthophosphate"
        ~ "OrthoP",
      characteristic_name %in% c(
        "Total Nitrogen, mixed forms", "Nitrogen",
        "Total Kjeldahl nitrogen",    "Kjeldahl nitrogen"
      ) ~ "TN",
      characteristic_name %in% c(
        "Chlorophyll a",
        "Chlorophyll a, corrected for pheophytin",
        "Chlorophyll a (probe relative fluorescence)",
        "Chlorophyll a (probe)"
      ) ~ "Chla",
      characteristic_name == "Depth, Secchi disk depth"
        ~ "Secchi",
      characteristic_name %in% c(
        "Total suspended solids",
        "Suspended Sediment Concentration (SSC)"
      ) ~ "TSS",
      characteristic_name == "Turbidity"
        ~ "Turbidity",
      characteristic_name == "Temperature, water"
        ~ "Temp",
      characteristic_name == "Dissolved oxygen (DO)"
        ~ "DO",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(param_group)) %>%
  group_by(
    monitoring_location_id, monitoring_location_name,
    monitoring_location_latitude, monitoring_location_longitude,
    param_group
  ) %>%
  summarise(
    n_records = n(),
    yr_min    = min(sample_year, na.rm = TRUE),
    yr_max    = max(sample_year, na.rm = TRUE),
    n_years   = n_distinct(sample_year),
    .groups   = "drop"
  ) %>%
  tidyr::pivot_wider(
    id_cols     = c(
      monitoring_location_id, monitoring_location_name,
      monitoring_location_latitude, monitoring_location_longitude
    ),
    names_from  = param_group,
    values_from = n_years,
    values_fill = 0
  ) %>%
  arrange(monitoring_location_name)

# ---- 4c. Growing season sample density --------------------------------------
# Samples per lake per growing season year (May-October)
# for the three core BATHTUB calibration parameters

growing_season <- lake_data %>%
  filter(
    sample_month >= 5,
    sample_month <= 10,
    characteristic_name %in% c(
      "Phosphorus",
      "Chlorophyll a",
      "Chlorophyll a, corrected for pheophytin",
      "Chlorophyll a (probe relative fluorescence)",
      "Chlorophyll a (probe)",
      "Depth, Secchi disk depth"
    )
  ) %>%
  mutate(
    param_group = case_when(
      characteristic_name == "Phosphorus"
        ~ "TP",
      characteristic_name %in% c(
        "Chlorophyll a",
        "Chlorophyll a, corrected for pheophytin",
        "Chlorophyll a (probe relative fluorescence)",
        "Chlorophyll a (probe)"
      ) ~ "Chla",
      characteristic_name == "Depth, Secchi disk depth"
        ~ "Secchi",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(param_group)) %>%
  group_by(
    monitoring_location_id, monitoring_location_name,
    sample_year, param_group
  ) %>%
  summarise(n_samples = n(), .groups = "drop") %>%
  tidyr::pivot_wider(
    names_from  = param_group,
    values_from = n_samples,
    values_fill = 0
  ) %>%
  arrange(monitoring_location_name, sample_year)

# ---- 4d. Activity type inventory --------------------------------------------

activity_types <- df_raw %>%
  group_by(monitoring_location_type, activity_type, relative_depth) %>%
  summarise(
    n_records   = n(),
    n_locations = n_distinct(monitoring_location_id),
    n_params    = n_distinct(characteristic_name),
    year_min    = min(sample_year, na.rm = TRUE),
    year_max    = max(sample_year, na.rm = TRUE),
    .groups     = "drop"
  ) %>%
  arrange(monitoring_location_type, activity_type)

# ---- 4e. Stream/tributary stations ------------------------------------------
# Potential inflow monitoring sites for BATHTUB load estimation

stream_summary <- stream_data %>%
  group_by(
    monitoring_location_id, monitoring_location_name,
    monitoring_location_latitude, monitoring_location_longitude
  ) %>%
  summarise(
    n_records      = n(),
    n_params       = n_distinct(characteristic_name),
    params         = paste(sort(unique(characteristic_name)), collapse = "; "),
    year_min       = min(sample_year, na.rm = TRUE),
    year_max       = max(sample_year, na.rm = TRUE),
    n_years        = n_distinct(sample_year),
    has_tp         = any(characteristic_name == "Phosphorus"),
    has_tn         = any(characteristic_name %in% c(
                       "Total Nitrogen, mixed forms", "Nitrogen")),
    has_flow_proxy = any(characteristic_name == "Specific conductance"),
    .groups        = "drop"
  ) %>%
  arrange(monitoring_location_name)

# ---- 4f. Chlorophyll version check ------------------------------------------
# Phaeophytin-corrected vs uncorrected vs probe — which exist in lake data?

chla_check <- lake_data %>%
  filter(grepl("chlorophyll|pheophytin", characteristic_name,
               ignore.case = TRUE)) %>%
  group_by(characteristic_name, sample_fraction) %>%
  summarise(
    n_records   = n(),
    n_locations = n_distinct(monitoring_location_id),
    n_years     = n_distinct(sample_year),
    year_min    = min(sample_year, na.rm = TRUE),
    year_max    = max(sample_year, na.rm = TRUE),
    .groups     = "drop"
  ) %>%
  arrange(desc(n_records))

# ---- 4g. Calibration scorecard per lake -------------------------------------
# Tiers based on years of data for each BATHTUB parameter

scorecard <- lake_coverage %>%
  mutate(
    has_tp       = if ("TP"       %in% names(.)) TP       >= 3 else FALSE,
    has_orthop   = if ("OrthoP"   %in% names(.)) OrthoP   >= 1 else FALSE,
    has_tn       = if ("TN"       %in% names(.)) TN       >= 3 else FALSE,
    has_chla     = if ("Chla"     %in% names(.)) Chla     >= 3 else FALSE,
    has_secchi   = if ("Secchi"   %in% names(.)) Secchi   >= 3 else FALSE,
    has_tss      = if ("TSS"      %in% names(.)) TSS      >= 1 else FALSE,
    has_profiles = if ("Temp"     %in% names(.)) Temp     >= 3 else FALSE
  ) %>%
  rowwise() %>%
  mutate(
    core_params_met = sum(c(has_tp, has_chla, has_secchi),        na.rm = TRUE),
    full_params_met = sum(c(has_tp, has_orthop, has_tn, has_chla,
                             has_secchi, has_tss, has_profiles),   na.rm = TRUE),
    calibration_tier = case_when(
      core_params_met == 3 & full_params_met >= 5 ~ "Tier 1 - Full calibration",
      core_params_met == 3 & full_params_met >= 3 ~ "Tier 2 - Core calibration",
      core_params_met >= 2                         ~ "Tier 3 - Partial calibration",
      TRUE                                         ~ "Insufficient data"
    )
  ) %>%
  ungroup() %>%
  select(
    monitoring_location_id, monitoring_location_name,
    monitoring_location_latitude, monitoring_location_longitude,
    calibration_tier, core_params_met, full_params_met,
    everything()
  ) %>%
  arrange(calibration_tier, monitoring_location_name)

# Quick console summary
message("\n=== CALIBRATION TIER SUMMARY ===")
print(table(scorecard$calibration_tier))

# =============================================================================
# 5. WRITE OUTPUT WORKBOOK
# =============================================================================

message("\nWriting output workbook...")

out_path <- file.path(
  Sys.getenv("USERPROFILE"),
  "OneDrive - State of Oklahoma",
  "Documents",
  "R code",
  "Bathtub",
  paste0("okBATHTUB_calibration_assessment_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
)

wb <- createWorkbook()

# Helper: add sheet with auto-width columns
add_sheet <- function(wb, sheet_name, data, tab_color = NULL) {
  addWorksheet(wb, sheet_name, tabColour = tab_color)
  if (nrow(data) > 0) {
    writeDataTable(wb, sheet_name, data, tableStyle = "TableStyleMedium2")
    setColWidths(wb, sheet_name, cols = seq_len(ncol(data)), widths = "auto")
  } else {
    writeData(wb, sheet_name, data.frame(Note = "No data returned for this tab."))
  }
}

add_sheet(wb, "1_Scorecard",       scorecard,       tab_color = "#2E75B6")
add_sheet(wb, "2_Lake_Coverage",   lake_coverage,   tab_color = "#70AD47")
add_sheet(wb, "3_Growing_Season",  growing_season,  tab_color = "#70AD47")
add_sheet(wb, "4_Param_Inventory", param_inventory, tab_color = "#ED7D31")
add_sheet(wb, "5_Activity_Types",  activity_types,  tab_color = "#ED7D31")
add_sheet(wb, "6_Stream_Stations", stream_summary,  tab_color = "#FFC000")
add_sheet(wb, "7_Chla_Check",      chla_check,      tab_color = "#A9D18E")

# README tab
addWorksheet(wb, "README", tabColour = "#BFBFBF")
readme_text <- data.frame(
  Section = c(
    "1_Scorecard",
    "2_Lake_Coverage",
    "3_Growing_Season",
    "4_Param_Inventory",
    "5_Activity_Types",
    "6_Stream_Stations",
    "7_Chla_Check",
    "---",
    "Tier 1 - Full calibration",
    "Tier 2 - Core calibration",
    "Tier 3 - Partial calibration",
    "Insufficient data"
  ),
  Description = c(
    "Per-lake calibration readiness tier. Start here.",
    "Years of data per lake station per BATHTUB parameter (pivoted wide).",
    "Growing-season (May-Oct) sample counts per lake per year for TP, Chl-a, Secchi.",
    "Full parameter inventory by location type, characteristic name, and sample fraction.",
    "Activity types and relative depths — shows sampling depth and type breakdown.",
    "Stream/tributary stations with TP/TN coverage — potential inflow monitoring sites.",
    "Chlorophyll characteristic name breakdown — corrected vs uncorrected vs probe.",
    "--- CALIBRATION TIER DEFINITIONS ---",
    "TP + Chla + Secchi >= 3 years AND 5+ of 7 full parameters met.",
    "TP + Chla + Secchi >= 3 years AND 3+ of 7 full parameters met.",
    "Only 2 of 3 core parameters (TP, Chla, Secchi) have >= 3 years of data.",
    "Fewer than 2 core parameters with >= 3 years of data."
  ),
  stringsAsFactors = FALSE
)
writeDataTable(wb, "README", readme_text, tableStyle = "TableStyleLight1")
setColWidths(wb, "README", cols = 1:2, widths = c(30, 80))

saveWorkbook(wb, out_path, overwrite = TRUE)
message(sprintf("Done! Output saved to:\n  %s", out_path))

# =============================================================================
# 6. DISCONNECT
# =============================================================================

dbDisconnect(con)
message("Disconnected from AWQMS.")
