# =============================================================================
# okBATHTUB - ok_from_awqms.R
# Oklahoma Water Resources Board - Water Quality Division
#
# Purpose: Pull, aggregate, and format OWRB AWQMS lake water quality data
#          into a structure ready for the okBATHTUB calibration pipeline.
#
# Main exported function: ok_from_awqms()
#
# Key design decisions:
#   - Growing season = May through October (months 5-10)
#   - Surface samples only (relative_depth = 'Surface' or result_depth <= 1m)
#   - QC activity types excluded
#   - Non-detects set to half the detection limit (standard substitution)
#   - Chlorophyll priority: corrected for pheophytin > uncorrected > probe
#   - TP = "Phosphorus" (confirmed AWQMS characteristic name)
#   - Secchi = "Depth, Secchi disk depth"
#   - Aggregation: mean of growing-season surface samples per location per year
#   - Ecoregion joined from lake_ecoregion_lookup.csv
# =============================================================================


#' Pull and format AWQMS lake data for okBATHTUB calibration
#'
#' @description
#' \code{ok_from_awqms()} connects to the OWRB AWQMS database, retrieves
#' lake water quality grab sample data for the specified lakes and date range,
#' aggregates to growing-season annual means, and returns a data frame
#' formatted for use with the okBATHTUB calibration pipeline.
#'
#' The function pulls total phosphorus (TP), total nitrogen (TN),
#' orthophosphate, chlorophyll-a (pheophytin-corrected preferred),
#' Secchi depth, total suspended solids (TSS), and turbidity from
#' \code{ext.results_standard_vw}.
#'
#' @param lake_names Character vector. One or more lake names as they appear
#'   in AWQMS \code{monitoring_location_name} (e.g. \code{"Arcadia Lake"}).
#'   Partial matching is used - \code{"Arcadia"} will match all Arcadia Lake
#'   sites. Pass \code{NULL} to pull all Lake Monitoring Program lakes.
#' @param year_start Integer. First year to include (default: 2000).
#' @param year_end Integer. Last year to include (default: current year).
#' @param grow_months Integer vector. Growing season months to include.
#'   Default \code{5:10} (May through October).
#' @param aggregation One of \code{"annual"} (default) or \code{"seasonal"}.
#'   \code{"annual"} returns one row per lake-site per year.
#'   \code{"seasonal"} returns one row per lake-site per year with
#'   early (May-Jul) and late (Aug-Oct) season columns.
#' @param ecoregion_lookup_path Character. Path to
#'   \code{lake_ecoregion_lookup.csv} produced by the ecoregion assignment
#'   script. If \code{NULL}, ecoregion columns are omitted.
#' @param min_samples Integer. Minimum number of surface grab samples required
#'   per parameter per lake-year for that year to be included. Default 3.
#'   Walker (1996) recommends a minimum of 3 samples per growing season.
#' @param cfg Named list. AWQMS connection configuration. Defaults to the
#'   standard OWRB configuration. Override only if connecting to a different
#'   server or schema.
#' @param verbose Logical. Print progress messages. Default \code{TRUE}.
#'
#' @return A data frame (tibble) with one row per monitoring location per year,
#'   containing the following columns:
#'   \describe{
#'     \item{\code{monitoring_location_id}}{AWQMS station ID.}
#'     \item{\code{monitoring_location_name}}{Full station name.}
#'     \item{\code{lake_name}}{Lake name (station name with site suffix removed).}
#'     \item{\code{eco_l3_code}}{EPA Level III ecoregion code.}
#'     \item{\code{eco_l3_name}}{EPA Level III ecoregion name.}
#'     \item{\code{latitude}}{Station latitude (WGS84).}
#'     \item{\code{longitude}}{Station longitude (WGS84).}
#'     \item{\code{sample_year}}{Calendar year.}
#'     \item{\code{n_sample_dates}}{Number of unique sample dates contributing
#'       to the annual means.}
#'     \item{\code{tp_ugl}}{Growing-season mean total phosphorus (ug/L).}
#'     \item{\code{tp_n}}{Number of TP samples.}
#'     \item{\code{tn_ugl}}{Growing-season mean total nitrogen (ug/L).}
#'     \item{\code{tn_n}}{Number of TN samples.}
#'     \item{\code{orthop_ugl}}{Growing-season mean orthophosphate (ug/L).}
#'     \item{\code{orthop_n}}{Number of orthophosphate samples.}
#'     \item{\code{chla_ugl}}{Growing-season mean chlorophyll-a (ug/L),
#'       pheophytin-corrected preferred.}
#'     \item{\code{chla_corrected}}{Logical. \code{TRUE} if pheophytin-corrected
#'       chlorophyll was used; \code{FALSE} if uncorrected or probe.}
#'     \item{\code{chla_n}}{Number of chlorophyll-a samples.}
#'     \item{\code{secchi_m}}{Growing-season mean Secchi depth (m).}
#'     \item{\code{secchi_n}}{Number of Secchi depth measurements.}
#'     \item{\code{tss_mgl}}{Growing-season mean total suspended solids (mg/L).}
#'     \item{\code{tss_n}}{Number of TSS samples.}
#'     \item{\code{turbidity_ntu}}{Growing-season mean turbidity (NTU).}
#'     \item{\code{turbidity_n}}{Number of turbidity measurements.}
#'   }
#'
#' @examples
#' \dontrun{
#' # Pull data for Arcadia Lake, 2010-2024
#' arcadia <- ok_from_awqms(
#'   lake_names = "Arcadia Lake",
#'   year_start = 2010,
#'   year_end   = 2024,
#'   ecoregion_lookup_path = "C:/Users/you/Documents/okBATHTUB/lake_ecoregion_lookup.csv"
#' )
#'
#' # Pull all LMP lakes, last 25 years
#' all_lakes <- ok_from_awqms(
#'   year_start = 2000,
#'   ecoregion_lookup_path = "C:/Users/you/Documents/okBATHTUB/lake_ecoregion_lookup.csv"
#' )
#'
#' # Feed directly into okBATHTUB pipeline
#' arcadia %>%
#'   filter(sample_year == 2020) %>%
#'   ok_load(
#'     inflow_m3yr   = 45e6,
#'     tp_inflow_ugl = tp_ugl
#'   )
#' }
#'
#' @seealso \code{\link{ok_load}}, \code{\link{ok_hydraulics}}
#' @export
ok_from_awqms <- function(lake_names            = NULL,
                           year_start            = 2000,
                           year_end              = as.integer(format(Sys.Date(), "%Y")),
                           grow_months           = 5:10,
                           aggregation           = c("annual", "seasonal"),
                           ecoregion_lookup_path = NULL,
                           min_samples           = 3L,
                           cfg                   = .awqms_default_config(),
                           verbose               = TRUE) {

  aggregation <- match.arg(aggregation)

  # ---- Packages (suggest only - not hard imports) ---------------------------
  .check_pkg <- function(pkg) {
    if (!requireNamespace(pkg, quietly = TRUE))
      stop(sprintf("Package '%s' is required. Install with: install.packages('%s')",
                   pkg, pkg), call. = FALSE)
  }
  .check_pkg("DBI")
  .check_pkg("odbc")
  .check_pkg("dplyr")
  .check_pkg("tidyr")
  .check_pkg("stringr")
  .check_pkg("readr")

  if (verbose) message("ok_from_awqms: connecting to AWQMS...")

  # ---- Connection -----------------------------------------------------------
  con <- .awqms_connect(cfg)
  on.exit(try(DBI::dbDisconnect(con), silent = TRUE), add = TRUE)

  if (verbose) message("ok_from_awqms: connected.")

  view_name <- paste0(cfg$default_schema, ".results_standard_vw")

  # ---- Build lake name filter -----------------------------------------------
  # If lake_names supplied, build a LIKE filter; otherwise pull all lakes
  if (!is.null(lake_names)) {
    like_clauses <- paste0(
      "r.monitoring_location_name LIKE '%", lake_names, "%'",
      collapse = "\n      OR "
    )
    lake_filter <- paste0("AND (\n      ", like_clauses, "\n    )")
  } else {
    lake_filter <- ""  # no filter - pull all lake stations
  }

  # ---- Build month filter ---------------------------------------------------
  month_list <- paste(grow_months, collapse = ", ")

  # ---- Main SQL query -------------------------------------------------------
  sql <- paste0("
    SELECT
      r.monitoring_location_id,
      r.monitoring_location_name,
      r.monitoring_location_latitude,
      r.monitoring_location_longitude,
      r.activity_type,
      r.relative_depth,
      CAST(r.activity_start_date AS DATE)    AS sample_date,
      YEAR(r.activity_start_date)            AS sample_year,
      MONTH(r.activity_start_date)           AS sample_month,
      r.characteristic_name,
      r.sample_fraction,
      r.result_measure                       AS result_value,
      r.result_measure_unit                  AS result_unit,
      r.result_detection_condition           AS detection_condition,
      r.detection_limit_measure1             AS detection_limit,
      r.detection_limit_unit1                AS detection_limit_unit,
      r.result_depth_height,
      r.result_depth_height_unit,
      r.result_status,
      r.data_quality_level_name              AS data_quality_level
    FROM ", view_name, " r
    WHERE
      r.monitoring_location_type = 'Lake'
      AND YEAR(r.activity_start_date) BETWEEN ", year_start, " AND ", year_end, "
      AND MONTH(r.activity_start_date) IN (", month_list, ")
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
        'Chlorophyll a',
        'Chlorophyll a, corrected for pheophytin',
        'Chlorophyll a (probe relative fluorescence)',
        'Chlorophyll a (probe)',
        'Depth, Secchi disk depth',
        'Total suspended solids',
        'Suspended Sediment Concentration (SSC)',
        'Turbidity'
      )
      AND r.result_status NOT IN ('Rejected', 'Invalidated')
      AND r.activity_type NOT LIKE 'Quality Control%'
      ", lake_filter, "
  ")

  if (verbose) message("ok_from_awqms: pulling data from AWQMS...")
  df_raw <- DBI::dbGetQuery(con, sql)
  if (verbose) message(sprintf("ok_from_awqms: %s raw records retrieved.",
                                format(nrow(df_raw), big.mark = ",")))

  if (nrow(df_raw) == 0) {
    warning("ok_from_awqms: no records returned. Check lake_names and date range.",
            call. = FALSE)
    return(data.frame())
  }

  # ---- Surface sample filter ------------------------------------------------
  # Keep samples flagged as Surface relative depth OR result depth <= 1.0 m
  df_surface <- df_raw %>%
    dplyr::filter(
      relative_depth == "Surface" |
        (!is.na(result_depth_height) & result_depth_height <= 1.0) |
        # Secchi has no depth flag - always keep it
        characteristic_name == "Depth, Secchi disk depth"
    )

  if (verbose) message(sprintf(
    "ok_from_awqms: %s surface records after depth filter.",
    format(nrow(df_surface), big.mark = ",")
  ))

  # ---- Non-detect handling --------------------------------------------------
  # Substitute half the detection limit for non-detect results.
  # suppressWarnings: non-numeric result_value entries are intentionally
  # coerced to NA and removed in the downstream filter step.
  df_nd <- df_surface %>%
    dplyr::mutate(
      result_value_num = suppressWarnings(as.numeric(result_value)),
      detection_limit  = suppressWarnings(as.numeric(detection_limit)),
      result_value = dplyr::case_when(
        !is.na(detection_condition) &
          detection_condition != "" &
          !is.na(detection_limit) &
          detection_limit > 0
          ~ detection_limit / 2,
        TRUE ~ result_value_num
      ),
      nondetect = (!is.na(detection_condition) & detection_condition != "")
    ) %>%
    dplyr::select(-result_value_num)

  # ---- Parameter grouping ---------------------------------------------------
  df_grouped <- df_nd %>%
    dplyr::mutate(
      param_group = dplyr::case_when(
        characteristic_name == "Phosphorus"
          ~ "tp",
        characteristic_name == "Orthophosphate"
          ~ "orthop",
        characteristic_name %in% c(
          "Total Nitrogen, mixed forms", "Nitrogen",
          "Total Kjeldahl nitrogen",    "Kjeldahl nitrogen",
          "Nitrate", "Nitrite", "Nitrate + Nitrite",
          "Ammonia", "Ammonia and ammonium"
        ) ~ "tn_component",
        characteristic_name == "Chlorophyll a, corrected for pheophytin"
          ~ "chla_corrected",
        characteristic_name %in% c(
          "Chlorophyll a",
          "Chlorophyll a (probe relative fluorescence)",
          "Chlorophyll a (probe)"
        ) ~ "chla_uncorrected",
        characteristic_name == "Depth, Secchi disk depth"
          ~ "secchi",
        characteristic_name %in% c(
          "Total suspended solids",
          "Suspended Sediment Concentration (SSC)"
        ) ~ "tss",
        characteristic_name == "Turbidity"
          ~ "turbidity",
        TRUE ~ NA_character_
      )
    ) %>%
    dplyr::filter(!is.na(param_group))

  # ---- Unit standardization -------------------------------------------------
  # TP, TN, OrthoP, Chl-a -> ug/L
  # Secchi -> m
  # TSS, Turbidity -> mg/L and NTU respectively (usually already correct)
  df_units <- df_grouped %>%
    dplyr::mutate(
      result_value = dplyr::case_when(
        # mg/L -> ug/L for nutrients and chlorophyll
        param_group %in% c("tp", "orthop", "tn_component",
                            "chla_corrected", "chla_uncorrected") &
          tolower(result_unit) == "mg/l"
          ~ result_value * 1000,
        # cm -> m for Secchi
        param_group == "secchi" &
          tolower(result_unit) %in% c("cm", "centimeters")
          ~ result_value / 100,
        # ft -> m for Secchi
        param_group == "secchi" &
          tolower(result_unit) %in% c("ft", "feet")
          ~ result_value * 0.3048,
        TRUE ~ result_value
      )
    ) %>%
    dplyr::filter(!is.na(result_value), result_value >= 0)

  # ---- TN reconstruction ----------------------------------------------------
  # AWQMS stores TN components separately. Use Total Nitrogen, mixed forms
  # when available; otherwise reconstruct as TKN + (NO3+NO2)
  tn_direct <- df_units %>%
    dplyr::filter(characteristic_name %in% c(
      "Total Nitrogen, mixed forms", "Nitrogen"
    )) %>%
    dplyr::mutate(param_group = "tn")

  tn_components <- df_units %>%
    dplyr::filter(param_group == "tn_component",
                  !characteristic_name %in% c(
                    "Total Nitrogen, mixed forms", "Nitrogen"
                  )) %>%
    dplyr::group_by(
      monitoring_location_id, monitoring_location_name,
      sample_date, sample_year, sample_month
    ) %>%
    dplyr::summarise(
      result_value = sum(result_value, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      param_group              = "tn",
      characteristic_name      = "TN_reconstructed",
      result_unit              = "ug/l",
      nondetect                = FALSE,
      relative_depth           = NA_character_,
      monitoring_location_latitude  = NA_real_,
      monitoring_location_longitude = NA_real_
    )

  # Bind direct TN records; reconstructed used only where direct is absent
  # Deduplicate: for any station-year where direct TN exists, drop reconstructed
  station_years_with_direct_tn <- tn_direct %>%
    dplyr::distinct(monitoring_location_id, sample_year)

  tn_components_filtered <- tn_components %>%
    dplyr::anti_join(
      station_years_with_direct_tn,
      by = c("monitoring_location_id", "sample_year")
    )

  df_tn_combined <- dplyr::bind_rows(tn_direct, tn_components_filtered)

  # ---- Combine all parameter groups -----------------------------------------
  df_all <- dplyr::bind_rows(
    df_units %>% dplyr::filter(param_group %in% c(
      "tp", "orthop", "chla_corrected", "chla_uncorrected",
      "secchi", "tss", "turbidity"
    )),
    df_tn_combined
  )

  # ---- Annual growing-season aggregation ------------------------------------
  # For chlorophyll, prefer corrected over uncorrected for each station-year
  # Aggregate to mean per station per year per param_group

  # Step 1: flag which chla version to use per station-year
  chla_priority <- df_all %>%
    dplyr::filter(param_group %in% c("chla_corrected", "chla_uncorrected")) %>%
    dplyr::group_by(monitoring_location_id, sample_year) %>%
    dplyr::summarise(
      use_corrected = any(param_group == "chla_corrected"),
      .groups = "drop"
    )

  chla_data <- df_all %>%
    dplyr::filter(param_group %in% c("chla_corrected", "chla_uncorrected")) %>%
    dplyr::left_join(chla_priority,
                     by = c("monitoring_location_id", "sample_year")) %>%
    dplyr::filter(
      (use_corrected  & param_group == "chla_corrected") |
      (!use_corrected & param_group == "chla_uncorrected")
    ) %>%
    dplyr::mutate(chla_corrected_flag = use_corrected,
                  param_group = "chla")

  # Step 2: combine and aggregate
  df_final_input <- dplyr::bind_rows(
    df_all %>% dplyr::filter(!param_group %in% c(
      "chla_corrected", "chla_uncorrected"
    )),
    chla_data
  )

  # Step 3: aggregate to annual means
  # Carry forward lat/lon from non-TN records to fill NAs introduced
  # by TN reconstruction (which sets lat/lon to NA_real_)
  coords_lookup <- df_final_input %>%
    dplyr::filter(!is.na(monitoring_location_latitude)) %>%
    dplyr::distinct(monitoring_location_id,
                    monitoring_location_latitude,
                    monitoring_location_longitude)

  df_final_input <- df_final_input %>%
    dplyr::select(-monitoring_location_latitude,
                  -monitoring_location_longitude) %>%
    dplyr::left_join(coords_lookup, by = "monitoring_location_id")

  annual_means <- df_final_input %>%
    dplyr::group_by(
      monitoring_location_id,
      monitoring_location_name,
      monitoring_location_latitude,
      monitoring_location_longitude,
      sample_year,
      param_group
    ) %>%
    dplyr::summarise(
      mean_value      = mean(result_value, na.rm = TRUE),
      n               = dplyr::n(),
      n_dates         = dplyr::n_distinct(sample_date),
      chla_corrected  = dplyr::first(chla_corrected_flag),
      .groups         = "drop"
    ) %>%
    dplyr::filter(n >= min_samples | param_group == "secchi")

  # Step 4: pivot wide
  result_wide <- annual_means %>%
    dplyr::select(
      monitoring_location_id, monitoring_location_name,
      monitoring_location_latitude, monitoring_location_longitude,
      sample_year, param_group, mean_value, n, chla_corrected
    ) %>%
    tidyr::pivot_wider(
      id_cols     = c(monitoring_location_id, monitoring_location_name,
                      monitoring_location_latitude, monitoring_location_longitude,
                      sample_year),
      names_from  = param_group,
      values_from = c(mean_value, n, chla_corrected),
      names_glue  = "{param_group}_{.value}"
    ) %>%
    dplyr::rename_with(
      ~ stringr::str_replace(., "_mean_value$", ""),
      dplyr::ends_with("_mean_value")
    )

  # Step 5: clean up column names to match okBATHTUB conventions
  result_clean <- result_wide %>%
    dplyr::rename(
      latitude  = monitoring_location_latitude,
      longitude = monitoring_location_longitude
    ) %>%
    dplyr::rename_with(~ dplyr::case_when(
      . == "tp"               ~ "tp_ugl",
      . == "tp_n"             ~ "tp_n",
      . == "tn"               ~ "tn_ugl",
      . == "tn_n"             ~ "tn_n",
      . == "orthop"           ~ "orthop_ugl",
      . == "orthop_n"         ~ "orthop_n",
      . == "chla"             ~ "chla_ugl",
      . == "chla_n"           ~ "chla_n",
      . == "chla_chla_corrected" ~ "chla_corrected",
      . == "secchi"           ~ "secchi_m",
      . == "secchi_n"         ~ "secchi_n",
      . == "tss"              ~ "tss_mgl",
      . == "tss_n"            ~ "tss_n",
      . == "turbidity"        ~ "turbidity_ntu",
      . == "turbidity_n"      ~ "turbidity_n",
      TRUE                    ~ .
    ))

  # ---- Sample date count per station-year -----------------------------------
  n_dates <- df_final_input %>%
    dplyr::group_by(monitoring_location_id, sample_year) %>%
    dplyr::summarise(n_sample_dates = dplyr::n_distinct(sample_date),
                     .groups = "drop")

  result_clean <- result_clean %>%
    dplyr::left_join(n_dates,
                     by = c("monitoring_location_id", "sample_year"))

  # ---- Lake name extraction -------------------------------------------------
  result_clean <- result_clean %>%
    dplyr::mutate(
      lake_name = stringr::str_remove(monitoring_location_name,
                                       ",\\s*Site\\s*\\d+.*$") %>%
                  stringr::str_remove(",\\s*Surface.*$") %>%
                  stringr::str_trim()
    )

  # ---- Ecoregion join -------------------------------------------------------
  if (!is.null(ecoregion_lookup_path)) {
    if (!file.exists(ecoregion_lookup_path)) {
      warning(
        sprintf(
          "ok_from_awqms: ecoregion lookup file not found at:\n  %s\nEcoregion columns will be omitted.",
          ecoregion_lookup_path
        ),
        call. = FALSE
      )
    } else {
      eco_lookup <- readr::read_csv(
        ecoregion_lookup_path,
        col_types = readr::cols(
          lake_name   = readr::col_character(),
          eco_l3_code = readr::col_character(),
          eco_l3_name = readr::col_character()
        ),
        show_col_types = FALSE
      ) %>%
        dplyr::select(lake_name, eco_l3_code, eco_l3_name) %>%
        dplyr::distinct(lake_name, .keep_all = TRUE)

      result_clean <- result_clean %>%
        dplyr::left_join(eco_lookup, by = "lake_name")

      n_unmatched <- sum(is.na(result_clean$eco_l3_code))
      if (n_unmatched > 0 && verbose) {
        unmatched_lakes <- result_clean %>%
          dplyr::filter(is.na(eco_l3_code)) %>%
          dplyr::distinct(lake_name) %>%
          dplyr::pull(lake_name)
        message(sprintf(
          "ok_from_awqms: %d station-years have no ecoregion match. Lakes: %s",
          n_unmatched,
          paste(unmatched_lakes, collapse = ", ")
        ))
      }
    }
  }

  # ---- Final column ordering ------------------------------------------------
  core_cols <- c(
    "monitoring_location_id", "monitoring_location_name",
    "lake_name", "latitude", "longitude", "sample_year", "n_sample_dates"
  )

  eco_cols  <- if ("eco_l3_code" %in% names(result_clean))
    c("eco_l3_code", "eco_l3_name") else character(0)

  param_cols <- c(
    "tp_ugl",       "tp_n",
    "tn_ugl",       "tn_n",
    "orthop_ugl",   "orthop_n",
    "chla_ugl",     "chla_corrected", "chla_n",
    "secchi_m",     "secchi_n",
    "tss_mgl",      "tss_n",
    "turbidity_ntu","turbidity_n"
  )

  present_cols <- c(core_cols, eco_cols, param_cols)
  present_cols <- present_cols[present_cols %in% names(result_clean)]

  result_out <- result_clean %>%
    dplyr::select(dplyr::all_of(present_cols)) %>%
    dplyr::arrange(lake_name, monitoring_location_id, sample_year)

  if (verbose) {
    message(sprintf(
      "ok_from_awqms: complete. %d station-years returned across %d lakes.",
      nrow(result_out),
      dplyr::n_distinct(result_out$lake_name)
    ))
  }

  result_out
}


# =============================================================================
# AWQMS CONNECTION HELPERS
# Matches the established OWRB connection pattern
# =============================================================================

#' Default AWQMS connection configuration
#' @keywords internal
.awqms_default_config <- function() {
  list(
    service        = "awqms_credentials",
    server         = "owrb.gselements.com",
    port           = 1433,
    database       = NULL,
    default_schema = "ext",
    driver         = "SQL Server",
    default_user   = "oklahomawrb"
  )
}


#' Connect to AWQMS using keyring credentials
#' @keywords internal
.awqms_connect <- function(cfg) {
  uid <- cfg$default_user

  pwd <- tryCatch(
    keyring::key_get(service  = cfg$service,
                     username = cfg$default_user),
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

# =============================================================================
# PROJECT PATH HELPERS
# =============================================================================

#' okBATHTUB project root path
#'
#' Returns the canonical project directory for okBATHTUB scripts and outputs.
#' All scripts, data files, and outputs should be read from and saved to here.
#'
#' @return Character. Full path to the okBATHTUB project directory.
#' @examples
#' ok_project_path()
#' @export
ok_project_path <- function() {
  file.path(
    Sys.getenv("USERPROFILE"),
    "OneDrive - State of Oklahoma",
    "Documents",
    "R code",
    "Bathtub"
  )
}

#' Build a path within the okBATHTUB project directory
#'
#' Convenience wrapper around \code{file.path(ok_project_path(), ...)}.
#' Use this everywhere a file path is needed so that the project root is
#' defined in one place and never hardcoded.
#'
#' @param ... Path components passed to \code{file.path()}.
#' @return Character. Full path within the okBATHTUB project directory.
#' @examples
#' ok_path("lake_ecoregion_lookup.csv")
#' ok_path("outputs", "arcadia_calibration.csv")
#' @export
ok_path <- function(...) {
  file.path(ok_project_path(), ...)
}
