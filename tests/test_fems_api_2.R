# ==============================================================================
# test_fems_api_2.R -- Design probes for the FEMS GraphQL download layer
# Branch: fems-graphql-api
#
# Run:  source("test_fems_api_2.R")
#
# test_fems_api.R proved the key works and the queries are shaped right.
# This script answers the questions that determine HOW to rewrite the
# download functions in app.R. Each probe is independent; run the whole
# thing or comment out the slow ones.
#
#   A  Range cap        -- do multi-year requests work, or is there still a
#                          1-year limit like the CSV endpoints? This decides
#                          whether fetch_in_year_chunks() survives at all.
#   B  Sort stability   -- nfdrsObs came back ascending and weatherObs
#                          descending in the first test. Unstable or differing
#                          default sort makes pagination unsafe. Does an
#                          explicit sortBy/sortOrder fix it?
#   C  Pagination       -- page a known result set with a small per_page and
#                          check for duplicate/missing records.
#   D  Multi-station    -- is stationIds a comma-separated list?
#   E  Field units      -- peak_gust_dir looked like tens-of-degrees next to
#                          wind_direction. Check the observed range.
#   F  Full POR timing  -- how long does 2005-present actually take for one
#                          station? Determines the progress/chunking strategy.
#                          SLOW. Off by default.
#
# Transcript -> test_fems_api_2_log.txt
# ==============================================================================

source("R/fems_api.R")

STATION_A  <- "21202"   # SAGUARO,   POR starts 2005-01-01
STATION_B  <- "21207"   # RINCON
FUEL_MODEL <- "Y"
LOG_FILE <- "tests/logs/test_fems_api_2_log.txt"

RUN_A <- TRUE
RUN_B <- TRUE
RUN_C <- TRUE
RUN_D <- TRUE
RUN_E <- TRUE
RUN_F <- FALSE          # slow: full period of record

end_date <- Sys.Date()

PROBE <- list()
.hr <- function(ch = "-") cat(strrep(ch, 78), "\n", sep = "")

probe <- function(id, title, expr) {
  .hr(); cat("PROBE ", id, ": ", title, "\n", sep = ""); .hr()
  t0 <- Sys.time()
  out <- tryCatch(force(expr), error = function(e) {
    cat("\n  [ERROR] ", conditionMessage(e), "\n", sep = ""); NULL
  })
  cat("\n  (", round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 2),
      "s)\n\n", sep = "")
  PROBE[[id]] <<- out
  invisible(out)
}

# Reusable query fragments -----------------------------------------------------
Q_NFDRS_COUNT <- '
  query NfdrsCount($fuelModels: String!, $stationIds: String,
                   $startDateRange: Date, $endDateRange: Date,
                   $hasHistoricData: TriState, $page: Int, $perPage: Int) {
    nfdrsObs(fuelModels: $fuelModels, stationIds: $stationIds,
             startDateRange: $startDateRange, endDateRange: $endDateRange,
             hasHistoricData: $hasHistoricData, page: $page, per_page: $perPage) {
      _metadata { page per_page total_count page_count }
      data { station_id observation_time_lst }
    }
  }'

Q_NFDRS_SORTED <- '
  query NfdrsSorted($fuelModels: String!, $stationIds: String,
                    $startDateRange: Date, $endDateRange: Date,
                    $hasHistoricData: TriState, $sortBy: NfdrObsSortBy,
                    $sortOrder: SortOrder, $page: Int, $perPage: Int) {
    nfdrsObs(fuelModels: $fuelModels, stationIds: $stationIds,
             startDateRange: $startDateRange, endDateRange: $endDateRange,
             hasHistoricData: $hasHistoricData, sortBy: $sortBy,
             sortOrder: $sortOrder, page: $page, per_page: $perPage) {
      _metadata { page per_page total_count page_count }
      data { station_id observation_time observation_time_lst nfdr_type }
    }
  }'

Q_WX_SORTED <- '
  query WxSorted($startDateTimeRange: DateTime!, $endDateTimeRange: DateTime!,
                 $stationIds: String, $hasHistoricData: TriState,
                 $sortBy: WxObsSortBy, $sortOrder: SortOrder,
                 $page: Int, $perPage: Int) {
    weatherObs(startDateTimeRange: $startDateTimeRange,
               endDateTimeRange: $endDateTimeRange, stationIds: $stationIds,
               hasHistoricData: $hasHistoricData, sortBy: $sortBy,
               sortOrder: $sortOrder, page: $page, per_page: $perPage) {
      _metadata { page per_page total_count page_count }
      data { station_id observation_time observation_time_lst observation_type
             wind_direction peak_gust_speed peak_gust_dir }
    }
  }'

sink(LOG_FILE, split = TRUE)
cat("FEMS GraphQL design probes --", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")


# ==============================================================================
# A -- Is there still a 1-year range cap?
# The CSV endpoints capped at 1 year, which is the entire reason app.R has
# fetch_in_year_chunks() and harmonize_chunk_types(). If GraphQL accepts a
# 21-year range, both of those functions can be deleted.
# ==============================================================================
if (RUN_A) probe("A-range", "Multi-year range requests", {
  res <- data.frame(years = numeric(0), ok = logical(0),
                    total_count = numeric(0), secs = numeric(0),
                    note = character(0), stringsAsFactors = FALSE)

  for (yrs in c(1, 2, 5, 10, 21)) {
    s  <- end_date - round(365.25 * yrs)
    t0 <- Sys.time()
    r  <- tryCatch({
      d <- fems_gql(Q_NFDRS_COUNT, list(
        fuelModels = FUEL_MODEL, stationIds = STATION_A,
        startDateRange = format(s), endDateRange = format(end_date),
        hasHistoricData = "ALL", page = 0, perPage = 1))
      m <- d$nfdrsObs$`_metadata`
      list(ok = TRUE, n = m$total_count, note = paste0("page_count ", m$page_count))
    }, error = function(e) list(ok = FALSE, n = NA, note = conditionMessage(e)))

    el <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 2)
    cat(sprintf("  %2d yr (%s to %s): %s  total_count=%s  [%ss]\n",
                yrs, format(s), format(end_date),
                if (r$ok) "OK    " else "REJECT",
                format(r$n), el))
    if (!r$ok) cat("      ", substr(r$note, 1, 300), "\n", sep = "")

    res <- rbind(res, data.frame(years = yrs, ok = r$ok,
                                 total_count = if (is.null(r$n)) NA_real_ else as.numeric(r$n),
                                 secs = el, note = substr(r$note, 1, 200),
                                 stringsAsFactors = FALSE))
    Sys.sleep(0.3)
  }

  cat("\n  >> If every row is OK, fetch_in_year_chunks() and\n")
  cat("     harmonize_chunk_types() can both be retired.\n")
  res
})


# ==============================================================================
# B -- Default sort order, and whether we can pin it
# In the first test nfdrsObs returned oldest-first and weatherObs newest-first.
# Paging an unsorted or inconsistently sorted result set can silently drop or
# duplicate records, so we need an explicit, stable sort before paging.
# ==============================================================================
if (RUN_B) probe("B-sort", "Default vs explicit sort order", {
  s <- end_date - 3

  cat("  -- nfdrsObs, no sort specified --\n")
  d <- fems_gql(Q_NFDRS_COUNT, list(
    fuelModels = FUEL_MODEL, stationIds = STATION_A,
    startDateRange = format(s), endDateRange = format(end_date),
    hasHistoricData = "ALL", page = 0, perPage = 5))
  print(fems_as_tibble(d$nfdrsObs$data))

  cat("\n  -- nfdrsObs, sortBy observation_time asc --\n")
  d2 <- tryCatch(
    fems_gql(Q_NFDRS_SORTED, list(
      fuelModels = FUEL_MODEL, stationIds = STATION_A,
      startDateRange = format(s), endDateRange = format(end_date),
      hasHistoricData = "ALL", sortBy = "observation_time", sortOrder = "asc",
      page = 0, perPage = 5)),
    error = function(e) { cat("   sortBy rejected: ", conditionMessage(e), "\n"); NULL })
  if (!is.null(d2)) print(fems_as_tibble(d2$nfdrsObs$data))

  cat("\n  -- weatherObs, sortBy observation_time asc --\n")
  d3 <- tryCatch(
    fems_gql(Q_WX_SORTED, list(
      startDateTimeRange = paste0(format(s), "T00:00:00Z"),
      endDateTimeRange   = paste0(format(end_date), "T23:59:59Z"),
      stationIds = STATION_A, hasHistoricData = "ALL",
      sortBy = "observation_time", sortOrder = "asc",
      page = 0, perPage = 5)),
    error = function(e) { cat("   sortBy rejected: ", conditionMessage(e), "\n"); NULL })
  if (!is.null(d3)) print(fems_as_tibble(d3$weatherObs$data))

  cat("\n  >> Whichever sort argument is accepted becomes mandatory in the\n")
  cat("     download layer, so paging is deterministic.\n")
  list(default = d, nfdrs_sorted = d2, wx_sorted = d3)
})


# ==============================================================================
# C -- Pagination integrity
# Page a ~30 day window with a deliberately small per_page and confirm the
# union of pages equals total_count with no duplicates.
# ==============================================================================
if (RUN_C) probe("C-paging", "Page a window in small pages, check for gaps/dupes", {
  s <- end_date - 30

  # single-shot reference count
  d <- fems_gql(Q_NFDRS_COUNT, list(
    fuelModels = FUEL_MODEL, stationIds = STATION_A,
    startDateRange = format(s), endDateRange = format(end_date),
    hasHistoricData = "ALL", page = 0, perPage = 1))
  expected <- as.integer(d$nfdrsObs$`_metadata`$total_count)
  cat("  Expected total_count: ", expected, "\n\n", sep = "")

  paged <- fems_gql_paged(
    Q_NFDRS_SORTED,
    variables = list(fuelModels = FUEL_MODEL, stationIds = STATION_A,
                     startDateRange = format(s), endDateRange = format(end_date),
                     hasHistoricData = "ALL",
                     sortBy = "observation_time", sortOrder = "asc"),
    root = "nfdrsObs", per_page = 100)

  key  <- paste(paged$station_id, paged$observation_time, paged$nfdr_type)
  dupes <- sum(duplicated(key))

  cat("\n  Rows collected : ", nrow(paged), "\n", sep = "")
  cat("  Expected       : ", expected, "\n", sep = "")
  cat("  Duplicate keys : ", dupes, "\n", sep = "")
  cat("  VERDICT        : ",
      if (nrow(paged) == expected && dupes == 0) "CLEAN - paging is safe"
      else "PROBLEM - see counts above", "\n", sep = "")
  paged
})


# ==============================================================================
# D -- Multi-station in one request
# The guide's examples show stationIds as a single string holding several ids.
# If that works, the app can drop its per-station map_dfr loop.
# ==============================================================================
if (RUN_D) probe("D-multistation", "Comma-separated stationIds", {
  s <- end_date - 2
  for (ids in c(STATION_A,
                paste0(STATION_A, ",", STATION_B),
                paste0(STATION_A, ", ", STATION_B))) {
    r <- tryCatch({
      d <- fems_gql(Q_NFDRS_COUNT, list(
        fuelModels = FUEL_MODEL, stationIds = ids,
        startDateRange = format(s), endDateRange = format(end_date),
        hasHistoricData = "ALL", page = 0, perPage = 5000))
      df <- fems_as_tibble(d$nfdrsObs$data)
      paste0("OK  total_count=", d$nfdrsObs$`_metadata`$total_count,
             "  distinct stations=",
             paste(sort(unique(as.character(df$station_id))), collapse = "/"))
    }, error = function(e) paste0("ERROR ", conditionMessage(e)))
    cat("  stationIds=\"", ids, "\"\n    -> ", substr(r, 1, 200), "\n", sep = "")
    Sys.sleep(0.3)
  }
  cat("\n  >> If two distinct stations come back, one request replaces the\n")
  cat("     per-station loop in observeEvent(input$fetch_data).\n")
  "see output"
})


# ==============================================================================
# E -- peak_gust_dir units
# First test showed wind_direction in 0-360 but peak_gust_dir at 22-34, which
# smells like tens of degrees. The app plots gustDirection as degrees, so this
# needs settling. A month of data will show the range.
# ==============================================================================
if (RUN_E) probe("E-gustdir", "Range of wind_direction vs peak_gust_dir", {
  s <- end_date - 30
  d <- fems_gql(Q_WX_SORTED, list(
    startDateTimeRange = paste0(format(s), "T00:00:00Z"),
    endDateTimeRange   = paste0(format(end_date), "T23:59:59Z"),
    stationIds = STATION_A, hasHistoricData = "ALL",
    sortBy = "observation_time", sortOrder = "asc",
    page = 0, perPage = 5000))

  df <- fems_as_tibble(d$weatherObs$data)
  cat("  n = ", nrow(df), "\n\n", sep = "")
  for (v in c("wind_direction", "peak_gust_speed", "peak_gust_dir")) {
    if (v %in% names(df)) {
      x <- suppressWarnings(as.numeric(df[[v]]))
      cat(sprintf("  %-16s min=%-8s max=%-8s mean=%-8s n_distinct=%s\n",
                  v, format(min(x, na.rm = TRUE)), format(max(x, na.rm = TRUE)),
                  format(round(mean(x, na.rm = TRUE), 1)),
                  length(unique(x[!is.na(x)]))))
    }
  }
  cat("\n  >> If peak_gust_dir maxes out near 36, it is tens of degrees and\n")
  cat("     needs a x10 to be plotted on the same axis as wind_direction.\n")
  df
})


# ==============================================================================
# F -- Full period-of-record timing  (SLOW -- set RUN_F <- TRUE to enable)
# The real question for the UI: can we pull 2005-present in one paged request
# in a tolerable amount of time, and how many rows is that?
# ==============================================================================
if (RUN_F) probe("F-fullpor", "Full POR pull for one station (timing)", {
  t0 <- Sys.time()
  df <- fems_gql_paged(
    Q_NFDRS_SORTED,
    variables = list(fuelModels = FUEL_MODEL, stationIds = STATION_A,
                     startDateRange = "2005-01-01",
                     endDateRange   = format(end_date + 7),
                     hasHistoricData = "ALL",
                     sortBy = "observation_time", sortOrder = "asc"),
    root = "nfdrsObs", per_page = 10000)
  el <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1)

  cat("\n  Rows: ", nrow(df), "   Elapsed: ", el, "s\n", sep = "")
  cat("  Date span: ", format(min(df$observation_time_lst, na.rm = TRUE)), " to ",
      format(max(df$observation_time_lst, na.rm = TRUE)), "\n", sep = "")
  cat("  Record types: ",
      paste(sort(unique(as.character(df$nfdr_type))), collapse = ", "), "\n", sep = "")
  df
})


.hr("=")
cat("PROBES COMPLETE -- results in the list `PROBE`\n")
cat("Transcript: ", LOG_FILE, "\n", sep = "")
.hr("=")
sink(NULL)
