# ==============================================================================
# fems_download.R -- GraphQL replacements for download_nfdrs_data() and
#                    download_weather_data()
# Branch: fems-graphql-api
#
# DESIGN CONTRACT
# ---------------
# These functions return data frames with EXACTLY the column names app.R's
# plotting, summary, and download code already expects. The migration is
# confined to this file: nothing downstream of all_data_cache() has to change.
#
#   keys      station_id (chr), date (Date), hour (int), record_type ("O"/"F")
#   nfdrs     energyReleaseComponent, burningIndex, ignitionComponent,
#             spreadComponent, kbdi, oneHR_TL_FuelMoisture,
#             tenHR_TL_FuelMoisture, hundredHR_TL_FuelMoisture,
#             thousandHR_TL_FuelMoisture, woodyLFI_fuelMoisture,
#             herbaceousLFI_fuelMoisture, gsi
#   weather   temperature, relativeHumidity, precipitation, windSpeed,
#             windDirection, gustSpeed, gustDirection, solarRadiation,
#             vpd, hdw, dewpoint
#
# WHAT CHANGED VS THE CSV VERSION
# -------------------------------
#   * No year chunking. The GraphQL endpoint accepts a 21-year range in one
#     request (probe A), so fetch_in_year_chunks() and harmonize_chunk_types()
#     are not needed here -- JSON is typed, so readr's per-request type drift
#     does not exist either.
#   * Paging replaces chunking, with an explicit sortBy/sortOrder. The two
#     queries returned opposite default sort orders, and paging an unpinned
#     sort can drop or duplicate rows.
#   * date/hour come from display_hour_lst (station local, rounded to the
#     hour). The CSV path derived hour from a UTC-parsed timestamp, so the
#     "1300LST" statistic was not necessarily 1 PM local.
#   * vpd comes from the API (vapor_press_def, in pascals) rather than being
#     computed. dewpoint and hdw are still computed locally -- the API has
#     no equivalent.
#   * One request covers multiple stations (probe D), so only stations missing
#     from the cache need fetching.
# ==============================================================================

source("R/fems_api.R")

# ------------------------------------------------------------------
# GraphQL documents
# ------------------------------------------------------------------
.Q_NFDRS <- '
  query NfdrsObsDownload($fuelModels: String!, $stationIds: String,
                         $startDateRange: Date, $endDateRange: Date,
                         $hasHistoricData: TriState, $dateTimeFormat: DateTimeFormat,
                         $sortBy: NfdrObsSortBy, $sortOrder: SortOrder,
                         $page: Int, $perPage: Int) {
    nfdrsObs(
      fuelModels: $fuelModels
      stationIds: $stationIds
      startDateRange: $startDateRange
      endDateRange: $endDateRange
      hasHistoricData: $hasHistoricData
      dateTimeFormat: $dateTimeFormat
      sortBy: $sortBy
      sortOrder: $sortOrder
      page: $page
      per_page: $perPage
    ) {
      _metadata { page per_page total_count page_count }
      data {
        station_id
        display_hour_lst
        nfdr_type
        fuel_model
        kbdi
        one_hr_tl_fuel_moisture
        ten_hr_tl_fuel_moisture
        hun_hr_tl_fuel_moisture
        thou_hr_tl_fuel_moisture
        ignition_component
        spread_component
        energy_release_component
        burning_index
        herbaceous_lfi_fuel_moisture
        woody_lfi_fuel_moisture
        gsi
      }
    }
  }'

.Q_WEATHER <- '
  query WeatherObsDownload($startDateTimeRange: DateTime!, $endDateTimeRange: DateTime!,
                           $stationIds: String, $hasHistoricData: TriState,
                           $sortBy: WxObsSortBy, $sortOrder: SortOrder,
                           $page: Int, $perPage: Int) {
    weatherObs(
      startDateTimeRange: $startDateTimeRange
      endDateTimeRange: $endDateTimeRange
      stationIds: $stationIds
      hasHistoricData: $hasHistoricData
      sortBy: $sortBy
      sortOrder: $sortOrder
      page: $page
      per_page: $perPage
    ) {
      _metadata { page per_page total_count page_count }
      data {
        station_id
        display_hour_lst
        observation_type
        temperature
        relative_humidity
        hourly_precip
        wind_speed
        wind_direction
        peak_gust_speed
        peak_gust_dir
        sol_rad
        vapor_press_def
      }
    }
  }'

# Default page size. The guide's own examples use per_page up to 300000, and
# benchmarked at full-POR scale: 25000 -> 62.5s, 100000 -> 30.6s, 300000 ->
# 29.3s. 100000 is the knee, and keeps 2 pages rather than 1 so a failure
# retries half the pull rather than all of it.
FEMS_PER_PAGE <- 100000

# ------------------------------------------------------------------
# Internal helpers
# ------------------------------------------------------------------

# "2026-08-14T13:00:00.000-07:00" -> date 2026-08-14, hour 13
# Deliberately string surgery rather than date parsing: the value is already
# station-local wall-clock time, so parsing it into a POSIXct would only
# create an opportunity to shift it into some other zone.
.lst_date <- function(x) as.Date(substr(as.character(x), 1, 10))
.lst_hour <- function(x) as.integer(substr(as.character(x), 12, 13))

.as_num <- function(x) suppressWarnings(as.numeric(x))

.station_ids_arg <- function(station_ids) {
  paste(unique(as.character(station_ids)), collapse = ",")
}

# ------------------------------------------------------------------
# WHY THE STATION LOOP (do not "optimize" this back into one request)
#
# FEMS rejects a local-station-time query spanning more than one station:
#
#     "Querying in local station time is only allowed for one station"
#
# which is reasonable -- stations sit in different time zones, so one
# response could not carry a single consistent local clock. Local time is
# exactly what makes date/hour correct here, so stations are requested one
# at a time and the results bound together.
#
# `progress`, when supplied, is called as progress(i, n, station_id) before
# each station so a Shiny caller can report which one is downloading.
# Stations that return nothing are recorded in attr(out, "failed_stations").
# ------------------------------------------------------------------
.fetch_by_station <- function(ids, fetch_one, label, progress = NULL, verbose = TRUE) {
  parts  <- list()
  failed <- character(0)

  for (i in seq_along(ids)) {
    if (is.function(progress)) progress(i, length(ids), ids[i])
    if (isTRUE(verbose)) message("  ", label, " station ", ids[i],
                                 " (", i, " of ", length(ids), ")")

    one <- tryCatch(fetch_one(ids[i]), error = function(e) {
      warning(label, " download failed for station ", ids[i], ": ",
              conditionMessage(e), call. = FALSE)
      NULL
    })

    if (!is.null(one) && nrow(one) > 0) {
      parts[[length(parts) + 1]] <- one
    } else {
      failed <- c(failed, ids[i])
    }
  }

  if (length(parts) == 0) return(NULL)

  out <- dplyr::bind_rows(parts)
  attr(out, "failed_stations") <- failed
  out
}

# ------------------------------------------------------------------
# NFDRS
# ------------------------------------------------------------------
fems_download_nfdrs <- function(station_ids,
                                start_date,
                                end_date,
                                fuel_model = "Y",
                                per_page   = FEMS_PER_PAGE,
                                progress   = NULL,
                                verbose    = TRUE) {

  ids_vec <- unique(as.character(station_ids))

  if (length(ids_vec) > 1) {
    return(.fetch_by_station(
      ids_vec,
      function(id) fems_download_nfdrs(id, start_date, end_date,
                                       fuel_model = fuel_model,
                                       per_page = per_page, verbose = verbose),
      label = "NFDRS", progress = progress, verbose = verbose))
  }

  ids <- .station_ids_arg(ids_vec)

  raw <- fems_gql_paged(
    .Q_NFDRS,
    variables = list(
      fuelModels      = fuel_model,
      stationIds      = ids,
      startDateRange  = format(as.Date(start_date)),
      endDateRange    = format(as.Date(end_date)),
      hasHistoricData = "ALL",
      dateTimeFormat  = "LocalStationTime",
      sortBy          = "observation_time",
      sortOrder       = "asc"
    ),
    root     = "nfdrsObs",
    per_page = per_page,
    verbose  = verbose
  )

  if (nrow(raw) == 0) {
    warning("No NFDRS data returned for station(s) ", ids, call. = FALSE)
    return(NULL)
  }

  out <- raw %>%
    transmute(
      station_id  = as.character(station_id),
      date        = .lst_date(display_hour_lst),
      hour        = .lst_hour(display_hour_lst),
      record_type = toupper(as.character(nfdr_type)),
      fuel_model  = as.character(fuel_model),

      kbdi                       = .as_num(kbdi),
      oneHR_TL_FuelMoisture      = .as_num(one_hr_tl_fuel_moisture),
      tenHR_TL_FuelMoisture      = .as_num(ten_hr_tl_fuel_moisture),
      hundredHR_TL_FuelMoisture  = .as_num(hun_hr_tl_fuel_moisture),
      thousandHR_TL_FuelMoisture = .as_num(thou_hr_tl_fuel_moisture),
      ignitionComponent          = .as_num(ignition_component),
      spreadComponent            = .as_num(spread_component),
      energyReleaseComponent     = .as_num(energy_release_component),
      burningIndex               = .as_num(burning_index),
      herbaceousLFI_fuelMoisture = .as_num(herbaceous_lfi_fuel_moisture),
      woodyLFI_fuelMoisture      = .as_num(woody_lfi_fuel_moisture),
      gsi                        = .as_num(gsi)
    ) %>%
    filter(!is.na(date), !is.na(hour)) %>%
    # One row per station/date/hour/type. Guards the downstream left_join
    # against row multiplication if the API ever returns an overlap.
    distinct(station_id, date, hour, record_type, .keep_all = TRUE)

  out
}

# ------------------------------------------------------------------
# Weather
#
# Note the date arguments are DateTime! (with time and Z), not Date.
# ------------------------------------------------------------------
fems_download_weather <- function(station_ids,
                                  start_date,
                                  end_date,
                                  per_page = FEMS_PER_PAGE,
                                  progress = NULL,
                                  verbose  = TRUE) {

  ids_vec <- unique(as.character(station_ids))

  # weatherObs takes no dateTimeFormat argument, so it may well accept several
  # stations at once. It is looped anyway: one code path, uniform progress
  # reporting, and no reliance on an untested asymmetry between the two feeds.
  if (length(ids_vec) > 1) {
    return(.fetch_by_station(
      ids_vec,
      function(id) fems_download_weather(id, start_date, end_date,
                                         per_page = per_page, verbose = verbose),
      label = "Weather", progress = progress, verbose = verbose))
  }

  ids <- .station_ids_arg(ids_vec)

  raw <- fems_gql_paged(
    .Q_WEATHER,
    variables = list(
      startDateTimeRange = paste0(format(as.Date(start_date)), "T00:00:00Z"),
      endDateTimeRange   = paste0(format(as.Date(end_date)),   "T23:59:59Z"),
      stationIds         = ids,
      hasHistoricData    = "ALL",
      sortBy             = "observation_time",
      sortOrder          = "asc"
    ),
    root     = "weatherObs",
    per_page = per_page,
    verbose  = verbose
  )

  if (nrow(raw) == 0) {
    warning("No weather data returned for station(s) ", ids, call. = FALSE)
    return(NULL)
  }

  out <- raw %>%
    transmute(
      station_id  = as.character(station_id),
      date        = .lst_date(display_hour_lst),
      hour        = .lst_hour(display_hour_lst),
      record_type = toupper(as.character(observation_type)),

      temperature      = .as_num(temperature),
      relativeHumidity = .as_num(relative_humidity),
      precipitation    = .as_num(hourly_precip),
      windSpeed        = .as_num(wind_speed),
      windDirection    = .as_num(wind_direction),
      gustSpeed        = .as_num(peak_gust_speed),
      # Confirmed 0-359 over a 30-day sample, i.e. plain degrees -- no scaling.
      gustDirection    = .as_num(peak_gust_dir),
      solarRadiation   = .as_num(sol_rad),
      # API reports pascals; the app has always worked in kPa.
      vpd              = .as_num(vapor_press_def) / 1000
    ) %>%
    filter(!is.na(date), !is.na(hour)) %>%
    distinct(station_id, date, hour, record_type, .keep_all = TRUE)

  # Derived variables the API does not supply.
  out <- out %>%
    mutate(
      # Hot-Dry-Windy: wind in m/s times VPD in hPa
      hdw = (windSpeed * 0.44704) * (vpd * 10),

      # Dewpoint, Magnus-Tetens. pmax() keeps log() finite if RH hits 0.
      .temp_c  = (temperature - 32) * 5 / 9,
      .gamma   = log(pmax(relativeHumidity, 0.1) / 100) +
                 (17.27 * .temp_c) / (237.3 + .temp_c),
      dewpoint = ((237.3 * .gamma) / (17.27 - .gamma)) * 9 / 5 + 32
    ) %>%
    select(-.temp_c, -.gamma)

  out
}

# ------------------------------------------------------------------
# Combined fetch: both feeds, joined, for one or more stations.
#
# Returns the same shape app.R's all_data_cache() holds today.
#
# `progress` is an optional function(fraction, message) so the Shiny caller
# can drive a progress bar; it is ignored when NULL.
# ------------------------------------------------------------------
fems_fetch_station_data <- function(station_ids,
                                    start_date = "2005-01-01",
                                    end_date   = Sys.Date() + 7,
                                    fuel_model = "Y",
                                    per_page   = FEMS_PER_PAGE,
                                    progress   = NULL,
                                    verbose    = TRUE) {

  tick <- function(f, msg) {
    if (is.function(progress)) progress(f, msg)
    if (isTRUE(verbose)) message(msg)
  }

  tick(0.05, "Requesting NFDRS observations...")
  nfdrs <- fems_download_nfdrs(station_ids, start_date, end_date,
                               fuel_model = fuel_model,
                               per_page = per_page, verbose = verbose)

  tick(0.55, "Requesting weather observations...")
  wx <- fems_download_weather(station_ids, start_date, end_date,
                              per_page = per_page, verbose = verbose)

  if (is.null(nfdrs) || is.null(wx)) {
    tick(1, "One or both feeds returned no data.")
    return(NULL)
  }

  tick(0.95, "Joining...")
  all_data <- left_join(nfdrs, wx,
                        by = c("station_id", "date", "hour", "record_type"))

  tick(1, paste0("Done: ", nrow(all_data), " rows."))
  all_data
}
