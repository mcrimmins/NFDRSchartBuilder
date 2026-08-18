# ==============================================================================
# test_fems_api.R -- Connectivity / auth / schema tests for the FEMS GraphQL API
# Branch: fems-graphql-api
#
# Run this from the project directory:
#     source("test_fems_api.R")
#
# Prereqs in .Renviron (then restart R -- Session > Restart R):
#     FEMS_USER=crimmins@arizona.edu
#     FEMS_KEY=<your api key>
#
# What it does, in order. Each stage is independent and reports PASS/FAIL,
# so a later failure still leaves you with everything the earlier stages found.
#
#   0  Environment check          -- are the credentials loaded at all?
#   1  Auth probe                 -- smallest possible authenticated request
#   2  Schema introspection       -- writes fems_schema.json, prints root fields
#                                    and their argument names (this is how we
#                                    confirm real field names instead of
#                                    trusting the PDF transcription)
#   3  StationMetaData            -- one station
#   4  NfdrsObs                   -- minimal fields, then full field set
#   5  WeatherObs                 -- minimal fields, then full field set
#   6  Range-limit probe          -- OFF by default; flips on to find out
#                                    whether GraphQL has the 1-year cap that
#                                    forced year-chunking on the CSV endpoints
#
# Nothing here writes to app.R. Results land in the list `TEST` for poking at
# in the console, and a transcript is written to test_fems_api_log.txt.
# ==============================================================================

source("R/fems_api.R")

# ---- configuration -----------------------------------------------------------
STATION_ID      <- "21202"          # the app's default station
FUEL_MODEL      <- "Y"
DAYS_BACK       <- 3                # small window - this is a smoke test
RUN_LIMIT_PROBE <- FALSE            # set TRUE to test a 1-year+ request
LOG_FILE <- "tests/logs/test_fems_api_log.txt"

end_date   <- Sys.Date()
start_date <- end_date - DAYS_BACK

# ---- test harness ------------------------------------------------------------
TEST    <- list()
.results <- character(0)

.hr <- function(ch = "-") cat(strrep(ch, 78), "\n", sep = "")

run_stage <- function(id, title, expr) {
  .hr()
  cat("STAGE ", id, ": ", title, "\n", sep = "")
  .hr()
  t0  <- Sys.time()
  out <- tryCatch(
    {
      val <- force(expr)
      el  <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 2)
      cat("\n  [PASS] ", id, " (", el, "s)\n\n", sep = "")
      .results[[id]] <<- "PASS"
      val
    },
    error = function(e) {
      el <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 2)
      cat("\n  [FAIL] ", id, " (", el, "s)\n", sep = "")
      cat("  ", conditionMessage(e), "\n\n", sep = "")
      .results[[id]] <<- "FAIL"
      NULL
    }
  )
  TEST[[id]] <<- out
  invisible(out)
}

# Transcript. Note: message()/warning() go to stderr and stay in the console
# rather than the log file; cat() output is captured in both.
sink(LOG_FILE, split = TRUE)

cat("FEMS GraphQL API test run --", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("Endpoint:", FEMS_ENDPOINT, "\n")
cat("Station :", STATION_ID, " Fuel model:", FUEL_MODEL, "\n")
cat("Window  :", format(start_date), "to", format(end_date), "\n\n")


# ==============================================================================
# STAGE 0 -- environment
# ==============================================================================
run_stage("0-env", "Credentials present in the R session", {
  user <- Sys.getenv("FEMS_USER", unset = "")
  key  <- Sys.getenv("FEMS_KEY",  unset = "")

  cat("  FEMS_USER : ", if (nzchar(user)) user else "<NOT SET>", "\n", sep = "")
  cat("  FEMS_KEY  : ", fems_mask_key(key),
      if (nzchar(key)) paste0("  (", nchar(key), " chars)") else "", "\n", sep = "")

  if (!nzchar(user)) stop("FEMS_USER not set. Add it to .Renviron and restart R.")
  if (!nzchar(key))  stop("FEMS_KEY not set (or R was not restarted after editing .Renviron).")

  # A key pasted with surrounding quotes or whitespace is a classic silent 401.
  if (grepl('^["\']|["\']$', key)) {
    warning("FEMS_KEY appears to be wrapped in quotes. .Renviron values should ",
            "be unquoted: FEMS_KEY=abc123")
  }
  if (key != trimws(key)) {
    warning("FEMS_KEY has leading/trailing whitespace.")
  }

  list(user = user, key_chars = nchar(key))
})


# ==============================================================================
# STAGE 1 -- auth probe
# The smallest legal GraphQL document. If this returns, the URL is right, the
# account has the API role, and Basic auth with the key works. Any failure here
# makes every later stage meaningless.
# ==============================================================================
run_stage("1-auth", "Authenticate against the endpoint", {
  d <- fems_gql("query Ping { __typename }", verbose = TRUE)
  cat("  Response: __typename = ", d$`__typename`, "\n", sep = "")
  cat("  Credentials accepted.\n")
  d
})


# ==============================================================================
# STAGE 2 -- schema introspection
# Saves the full schema so we can read the real field and argument names rather
# than working from the PDF. If introspection is disabled server-side this
# stage fails harmlessly and stages 3-5 still tell us what we need.
# ==============================================================================
run_stage("2-schema", "Introspect the schema", {
  q <- '
    query Introspect {
      __schema {
        queryType { name }
        types {
          kind
          name
          fields(includeDeprecated: false) {
            name
            args { name type { kind name ofType { kind name ofType { kind name } } } }
            type { kind name ofType { kind name ofType { kind name } } }
          }
          inputFields { name type { kind name ofType { kind name } } }
          enumValues(includeDeprecated: false) { name }
        }
      }
    }'

  d <- fems_gql(q)
  jsonlite::write_json(d, "tests/logs/fems_schema.json", auto_unbox = TRUE, pretty = TRUE)
  cat("  Wrote fems_schema.json\n\n")

  types      <- d$`__schema`$types
  root_name  <- d$`__schema`$queryType$name
  root       <- Filter(function(t) identical(t$name, root_name), types)[[1]]

  cat("  Query root '", root_name, "' exposes ", length(root$fields),
      " field(s):\n\n", sep = "")
  for (f in root$fields) {
    args <- vapply(f$args, function(a) a$name, character(1))
    cat("    ", f$name, "(", paste(args, collapse = ", "), ")\n", sep = "")
  }

  # Enum values are the other thing worth having in front of us -- these are
  # the acceptable strings for TriState, SortOrder, fuel models, etc.
  cat("\n  Enums:\n")
  for (t in types) {
    if (identical(t$kind, "ENUM") && !grepl("^__", t$name)) {
      vals <- vapply(t$enumValues, function(v) v$name, character(1))
      cat("    ", t$name, ": ", paste(vals, collapse = ", "), "\n", sep = "")
    }
  }

  # Helper for the console: fems_fields("NfdrsObs") to list a type's fields.
  fems_fields <<- function(type_name) {
    t <- Filter(function(x) identical(x$name, type_name), types)
    if (length(t) == 0) {
      cat("No type named '", type_name, "'. Candidates:\n", sep = "")
      nm <- vapply(types, function(x) if (is.null(x$name)) "" else x$name, character(1))
      print(grep(type_name, nm, ignore.case = TRUE, value = TRUE))
      return(invisible(NULL))
    }
    vapply(t[[1]]$fields, function(f) f$name, character(1))
  }
  cat("\n  Tip: run fems_fields(\"TypeName\") in the console to list a type's fields.\n")

  d
})


# ==============================================================================
# STAGE 3 -- StationMetaData
# This query could eventually replace the static station_metadata CSV.
# ==============================================================================
run_stage("3-stationmeta", "StationMetaData for one station", {
  q <- '
    query StationMetaData($stationIds: String, $hasHistoricData: TriState,
                          $page: Int, $perPage: Int) {
      stationMetaData(
        stationIds: $stationIds
        hasHistoricData: $hasHistoricData
        page: $page
        per_page: $perPage
      ) {
        _metadata { page per_page total_count page_count }
        data {
          station_id
          station_name
          latitude
          longitude
          elevation
          state
          time_zone
          time_zone_offset
          period_record_start
          period_record_stop
          station_status
        }
      }
    }'

  d <- fems_gql(q, list(stationIds = STATION_ID, hasHistoricData = "ALL",
                        page = 0, perPage = 10))

  meta <- d$stationMetaData$`_metadata`
  cat("  total_count: ", meta$total_count, "   page_count: ", meta$page_count, "\n", sep = "")

  df <- fems_as_tibble(d$stationMetaData$data)
  cat("  Returned ", nrow(df), " row(s), ", ncol(df), " column(s)\n\n", sep = "")
  print(as.data.frame(df))
  df
})


# ==============================================================================
# STAGE 4 -- NfdrsObs  (replacement for download-nfdr)
# Two passes: a minimal field set proves the query shape and arguments are
# right, then the full set proves every field name we need actually exists.
# ==============================================================================
run_stage("4a-nfdrs-min", "NfdrsObs, minimal fields", {
  q <- '
    query NfdrsObs($fuelModels: String!, $stationIds: String,
                   $startDateRange: Date, $endDateRange: Date,
                   $hasHistoricData: TriState, $dateTimeFormat: DateTimeFormat,
                   $page: Int, $perPage: Int) {
      nfdrsObs(
        fuelModels: $fuelModels
        stationIds: $stationIds
        startDateRange: $startDateRange
        endDateRange: $endDateRange
        hasHistoricData: $hasHistoricData
        dateTimeFormat: $dateTimeFormat
        page: $page
        per_page: $perPage
      ) {
        _metadata { page per_page total_count page_count }
        data { station_id observation_time observation_time_lst nfdr_type }
      }
    }'

  d <- fems_gql(q, list(
    fuelModels      = FUEL_MODEL,
    stationIds      = STATION_ID,
    startDateRange  = format(start_date),
    endDateRange    = format(end_date),
    hasHistoricData = "ALL",
    dateTimeFormat  = "LocalStationTime",
    page            = 0,
    perPage         = 5000
  ), verbose = TRUE)

  meta <- d$nfdrsObs$`_metadata`
  cat("  total_count: ", meta$total_count, "   page_count: ", meta$page_count,
      "   per_page: ", meta$per_page, "\n", sep = "")

  df <- fems_as_tibble(d$nfdrsObs$data)
  cat("  Returned ", nrow(df), " row(s)\n\n", sep = "")
  print(utils::head(as.data.frame(df), 5))

  # nfdr_type is what the app uses to split observed from forecast records.
  if ("nfdr_type" %in% names(df)) {
    cat("\n  nfdr_type values present: ",
        paste(sort(unique(as.character(df$nfdr_type))), collapse = ", "), "\n", sep = "")
  }
  df
})

run_stage("4b-nfdrs-full", "NfdrsObs, full field set the app needs", {
  q <- '
    query NfdrsObsFull($fuelModels: String!, $stationIds: String,
                       $startDateRange: Date, $endDateRange: Date,
                       $hasHistoricData: TriState, $dateTimeFormat: DateTimeFormat,
                       $page: Int, $perPage: Int) {
      nfdrsObs(
        fuelModels: $fuelModels
        stationIds: $stationIds
        startDateRange: $startDateRange
        endDateRange: $endDateRange
        hasHistoricData: $hasHistoricData
        dateTimeFormat: $dateTimeFormat
        page: $page
        per_page: $perPage
      ) {
        _metadata { total_count page_count }
        data {
          station_id
          station_name
          observation_time
          observation_time_lst
          display_hour
          display_hour_lst
          nfdr_type
          fuel_model
          fuel_model_version
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
          quality_code
        }
      }
    }'

  d <- fems_gql(q, list(
    fuelModels      = FUEL_MODEL,
    stationIds      = STATION_ID,
    startDateRange  = format(start_date),
    endDateRange    = format(end_date),
    hasHistoricData = "ALL",
    dateTimeFormat  = "LocalStationTime",
    page            = 0,
    perPage         = 5000
  ))

  df <- fems_as_tibble(d$nfdrsObs$data)
  cat("  Returned ", nrow(df), " row(s), ", ncol(df), " column(s)\n", sep = "")
  cat("  Columns: ", paste(names(df), collapse = ", "), "\n\n", sep = "")
  print(utils::head(as.data.frame(df), 3))

  # Which of these are all-NA for this station/window? Tells us what the app
  # can actually plot vs. what is nominally in the schema.
  allna <- names(df)[vapply(df, function(x) all(is.na(x)), logical(1))]
  if (length(allna)) cat("\n  All-NA in this window: ", paste(allna, collapse = ", "), "\n", sep = "")
  df
})


# ==============================================================================
# STAGE 5 -- WeatherObs  (replacement for download-weather)
# Note the different date arguments: DateTime (with time + Z), not Date.
# Also note vapor_press_def comes from the API -- the app currently computes
# vpd locally, so we get a cross-check for free.
# ==============================================================================
run_stage("5a-weather-min", "WeatherObs, minimal fields", {
  q <- '
    query WeatherObs($startDateTimeRange: DateTime!, $endDateTimeRange: DateTime!,
                     $stationIds: String, $hasHistoricData: TriState,
                     $page: Int, $perPage: Int) {
      weatherObs(
        startDateTimeRange: $startDateTimeRange
        endDateTimeRange: $endDateTimeRange
        stationIds: $stationIds
        hasHistoricData: $hasHistoricData
        page: $page
        per_page: $perPage
      ) {
        _metadata { page per_page total_count page_count }
        data { station_id observation_time observation_time_lst observation_type }
      }
    }'

  d <- fems_gql(q, list(
    startDateTimeRange = paste0(format(start_date), "T00:00:00Z"),
    endDateTimeRange   = paste0(format(end_date),   "T23:59:59Z"),
    stationIds         = STATION_ID,
    hasHistoricData    = "ALL",
    page               = 0,
    perPage            = 5000
  ), verbose = TRUE)

  meta <- d$weatherObs$`_metadata`
  cat("  total_count: ", meta$total_count, "   page_count: ", meta$page_count, "\n", sep = "")

  df <- fems_as_tibble(d$weatherObs$data)
  cat("  Returned ", nrow(df), " row(s)\n\n", sep = "")
  print(utils::head(as.data.frame(df), 5))

  if ("observation_type" %in% names(df)) {
    cat("\n  observation_type values present: ",
        paste(sort(unique(as.character(df$observation_type))), collapse = ", "), "\n", sep = "")
  }
  df
})

run_stage("5b-weather-full", "WeatherObs, full field set the app needs", {
  q <- '
    query WeatherObsFull($startDateTimeRange: DateTime!, $endDateTimeRange: DateTime!,
                         $stationIds: String, $hasHistoricData: TriState,
                         $page: Int, $perPage: Int) {
      weatherObs(
        startDateTimeRange: $startDateTimeRange
        endDateTimeRange: $endDateTimeRange
        stationIds: $stationIds
        hasHistoricData: $hasHistoricData
        page: $page
        per_page: $perPage
      ) {
        _metadata { total_count page_count }
        data {
          station_id
          station_name
          observation_time
          observation_time_lst
          display_hour
          display_hour_lst
          display_date
          observation_type
          temperature
          relative_humidity
          hourly_precip
          wind_speed
          wind_direction
          peak_gust_speed
          peak_gust_dir
          sol_rad
          snow_flag
          vapor_press_def
        }
      }
    }'

  d <- fems_gql(q, list(
    startDateTimeRange = paste0(format(start_date), "T00:00:00Z"),
    endDateTimeRange   = paste0(format(end_date),   "T23:59:59Z"),
    stationIds         = STATION_ID,
    hasHistoricData    = "ALL",
    page               = 0,
    perPage            = 5000
  ))

  df <- fems_as_tibble(d$weatherObs$data)
  cat("  Returned ", nrow(df), " row(s), ", ncol(df), " column(s)\n", sep = "")
  cat("  Columns: ", paste(names(df), collapse = ", "), "\n\n", sep = "")
  print(utils::head(as.data.frame(df), 3))

  allna <- names(df)[vapply(df, function(x) all(is.na(x)), logical(1))]
  if (length(allna)) cat("\n  All-NA in this window: ", paste(allna, collapse = ", "), "\n", sep = "")
  df
})


# ==============================================================================
# STAGE 6 -- range limit probe (opt-in)
# The CSV endpoints cap at 1 year per station, which is why app.R chunks by
# calendar year. GraphQL is paginated instead, so the cap may not apply --
# but we should find out before designing the new download layer. Set
# RUN_LIMIT_PROBE <- TRUE above and re-source to run this.
# ==============================================================================
if (isTRUE(RUN_LIMIT_PROBE)) {
  run_stage("6-limit", "Multi-year request (is there still a 1-year cap?)", {
    q <- '
      query NfdrsObsRange($fuelModels: String!, $stationIds: String,
                          $startDateRange: Date, $endDateRange: Date,
                          $hasHistoricData: TriState, $page: Int, $perPage: Int) {
        nfdrsObs(
          fuelModels: $fuelModels
          stationIds: $stationIds
          startDateRange: $startDateRange
          endDateRange: $endDateRange
          hasHistoricData: $hasHistoricData
          page: $page
          per_page: $perPage
        ) {
          _metadata { page per_page total_count page_count }
          data { station_id observation_time }
        }
      }'

    for (span_years in c(1, 2, 5)) {
      s <- end_date - (365 * span_years)
      cat("\n  --- requesting ", span_years, " year(s): ", format(s), " to ",
          format(end_date), " ---\n", sep = "")
      r <- tryCatch({
        d <- fems_gql(q, list(fuelModels = FUEL_MODEL, stationIds = STATION_ID,
                              startDateRange = format(s), endDateRange = format(end_date),
                              hasHistoricData = "ALL", page = 0, perPage = 10))
        m <- d$nfdrsObs$`_metadata`
        cat("  OK -- total_count ", m$total_count, ", page_count ", m$page_count, "\n", sep = "")
        m$total_count
      }, error = function(e) {
        cat("  REJECTED: ", conditionMessage(e), "\n", sep = "")
        NA
      })
    }
    "see output above"
  })
}


# ==============================================================================
# Summary
# ==============================================================================
.hr("=")
cat("SUMMARY\n")
.hr("=")
if (length(.results) == 0) {
  cat("  (no stages ran)\n")
} else {
  for (nm in names(.results)) {
    cat(sprintf("  %-18s %s\n", nm, .results[[nm]]))
  }
}
cat("\nResults are in the list `TEST` (e.g. TEST[['4b-nfdrs-full']]).\n")
cat("Transcript written to ", LOG_FILE, "\n", sep = "")
if (file.exists("tests/logs/fems_schema.json")) {
  cat("Schema written to fems_schema.json\n")
}
.hr("=")

sink(NULL)
