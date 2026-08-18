# ==============================================================================
# test_fems_api_4.R -- Re-benchmark the full pull after the parser rewrite
# Branch: fems-graphql-api
#
# Run:  source("test_fems_api_4.R")     # restart R first, or at least re-source
#
# Probe 2 in test_fems_api_3.R took 211.6s for one station's full period of
# record. Two suspects:
#
#   (a) client-side parsing -- the old fems_as_tibble() built one tibble per
#       record, i.e. ~190,000 of them, then bound them together
#   (b) server-side paging cost at large offsets
#
# fems_api.R now lets jsonlite simplify each page into a data frame in C, which
# removes (a) entirely. This script re-measures, and separately times the
# network leg against the parse leg so we can see which one is actually left.
#
# It also settles the 1300LST question by comparing the OLD CSV download path
# against the new one on the same day.
#
# Transcript -> test_fems_api_4_log.txt
# ==============================================================================

source("R/fems_download.R")

STATION    <- "21202"
FUEL_MODEL <- "Y"
LOG_FILE <- "tests/logs/test_fems_api_4_log.txt"

RUN_1 <- TRUE    # where does the time actually go?
RUN_2 <- TRUE    # page size at full-POR scale
RUN_3 <- TRUE    # old CSV path vs new GraphQL path -- the 1300LST question

.hr <- function(ch = "-") cat(strrep(ch, 78), "\n", sep = "")
B <- list()

sink(LOG_FILE, split = TRUE)
cat("FEMS re-benchmark --", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")


# ==============================================================================
# 1 -- Split network time from parse time
# One request for a big page, timed in two halves: how long the server takes to
# hand over the bytes, and how long R takes to turn them into a data frame.
# ==============================================================================
if (RUN_1) {
  .hr(); cat("PROBE 1: network vs parse, single 25k-row page\n"); .hr()

  q <- '
    query Timing($fuelModels: String!, $stationIds: String,
                 $startDateRange: Date, $endDateRange: Date,
                 $hasHistoricData: TriState, $sortBy: NfdrObsSortBy,
                 $sortOrder: SortOrder, $page: Int, $perPage: Int) {
      nfdrsObs(fuelModels: $fuelModels, stationIds: $stationIds,
               startDateRange: $startDateRange, endDateRange: $endDateRange,
               hasHistoricData: $hasHistoricData, sortBy: $sortBy,
               sortOrder: $sortOrder, page: $page, per_page: $perPage) {
        _metadata { total_count page_count }
        data { station_id display_hour_lst nfdr_type kbdi
               energy_release_component burning_index }
      }
    }'

  vars <- list(fuelModels = FUEL_MODEL, stationIds = STATION,
               startDateRange = "2005-01-01", endDateRange = format(Sys.Date()),
               hasHistoricData = "ALL", sortBy = "observation_time",
               sortOrder = "asc", page = 0, perPage = 25000)

  creds <- fems_credentials()
  body  <- paste0('{"query":', as.character(jsonlite::toJSON(q, auto_unbox = TRUE)),
                  ',"variables":',
                  as.character(jsonlite::toJSON(vars, auto_unbox = TRUE)), '}')

  t0  <- Sys.time()
  res <- httr::POST(FEMS_ENDPOINT,
                    config = httr::authenticate(creds$user, creds$key, type = "basic"),
                    httr::content_type_json(), httr::accept_json(),
                    httr::timeout(600), body = body, encode = "raw")
  txt <- httr::content(res, as = "text", encoding = "UTF-8")
  t1  <- Sys.time()
  p   <- jsonlite::fromJSON(txt, simplifyVector = TRUE, flatten = TRUE)
  t2  <- Sys.time()

  net   <- as.numeric(difftime(t1, t0, units = "secs"))
  parse <- as.numeric(difftime(t2, t1, units = "secs"))
  rows  <- if (is.data.frame(p$data$nfdrsObs$data)) nrow(p$data$nfdrsObs$data) else NA

  cat("  Payload      : ", format(round(nchar(txt) / 1e6, 2)), " MB\n", sep = "")
  cat("  Rows         : ", rows, "\n", sep = "")
  cat("  Network      : ", round(net, 2), "s\n", sep = "")
  cat("  Parse (C)    : ", round(parse, 2), "s\n", sep = "")
  cat("  Total        : ", round(net + parse, 2), "s\n\n", sep = "")
  cat("  >> If network dominates, the remaining cost is FEMS-side and the only\n")
  cat("     lever left is fetching less or caching. If parse still dominates,\n")
  cat("     there is more to win in R.\n\n")

  B$split <- list(net = net, parse = parse, rows = rows, mb = nchar(txt) / 1e6)
}


# ==============================================================================
# 2 -- Page size at full-POR scale
# Probe 1 in the previous script tested page size on 8,800 rows, where
# everything fit in one page. The interesting question is 190,000 rows:
# 25,000/page is 8 round trips, 100,000/page is 2.
# ==============================================================================
if (RUN_2) {
  .hr(); cat("PROBE 2: full-POR page size (NFDRS only, one station)\n"); .hr()

  res <- data.frame()
  for (pp in c(25000, 100000, 300000)) {
    t0 <- Sys.time()
    r <- tryCatch({
      df <- fems_download_nfdrs(STATION, "2005-01-01", Sys.Date() + 7,
                                fuel_model = FUEL_MODEL,
                                per_page = pp, verbose = FALSE)
      list(ok = TRUE, n = nrow(df))
    }, error = function(e) list(ok = FALSE, n = NA, msg = conditionMessage(e)))

    el <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1)
    cat(sprintf("  per_page=%-7d %s rows=%-8s %ss\n", pp,
                if (r$ok) "OK    " else "FAIL  ", format(r$n), el))
    if (!r$ok) cat("      ", substr(r$msg, 1, 250), "\n", sep = "")
    res <- rbind(res, data.frame(per_page = pp, ok = r$ok, rows = r$n, secs = el))
  }

  cat("\n  Previous run, old parser, per_page=25000, NFDRS+weather: 211.6s\n\n")
  B$page <- res
}


# ==============================================================================
# 3 -- The 1300LST question, settled
# Runs the OLD CSV download function and the NEW GraphQL one over the same
# recent window and compares the diurnal temperature curve. Local time peaks
# mid-afternoon. If the old path peaks somewhere else, its `hour` was UTC and
# the app's "1300LST" statistic has not been 1 PM local.
#
# Requires the old functions. app.R defines them at the top but also calls
# shinyApp() at the bottom, so we pull just the function definitions rather
# than sourcing the whole file.
# ==============================================================================
if (RUN_3) {
  .hr(); cat("PROBE 3: old CSV path vs new GraphQL path -- is 'hour' local?\n"); .hr()

  s <- Sys.Date() - 30
  e <- Sys.Date()

  cat("  -- NEW path --\n")
  new <- tryCatch(
    fems_download_weather(STATION, s, e, verbose = FALSE),
    error = function(err) { cat("   error: ", conditionMessage(err), "\n"); NULL })

  if (!is.null(new)) {
    o  <- new[new$record_type == "O" & !is.na(new$temperature), ]
    bh <- tapply(o$temperature, o$hour, mean, na.rm = TRUE)
    cat("   mean temp by hour:\n"); print(round(bh, 1))
    cat("   hottest hour: ", names(bh)[which.max(bh)], "\n\n", sep = "")
    B$new_hours <- bh
  }

  cat("  -- OLD path (CSV endpoint, no auth) --\n")
  old_ok <- tryCatch({
    # Pull only the old download_weather_data() definition out of app.R.
    src   <- readLines("app.R", warn = FALSE)
    start <- grep("^download_weather_data <- function", src)
    if (length(start) == 0) stop("could not find download_weather_data in app.R")
    # find the closing brace of the function at column 1
    ends  <- grep("^\\}", src)
    end   <- min(ends[ends > start[1]])
    eval(parse(text = paste(src[start[1]:end], collapse = "\n")), envir = globalenv())
    TRUE
  }, error = function(err) { cat("   could not extract: ", conditionMessage(err), "\n"); FALSE })

  if (isTRUE(old_ok)) {
    old <- tryCatch(download_weather_data(STATION, s, e),
                    error = function(err) { cat("   error: ", conditionMessage(err), "\n"); NULL })
    if (!is.null(old)) {
      oo  <- old[old$record_type == "O" & !is.na(old$temperature), ]
      bho <- tapply(oo$temperature, oo$hour, mean, na.rm = TRUE)
      cat("   mean temp by hour:\n"); print(round(bho, 1))
      cat("   hottest hour: ", names(bho)[which.max(bho)], "\n\n", sep = "")
      B$old_hours <- bho

      if (!is.null(B$new_hours)) {
        shift <- as.integer(names(bho)[which.max(bho)]) -
                 as.integer(names(B$new_hours)[which.max(B$new_hours)])
        cat("  >> Peak-hour difference (old minus new): ", shift, " hours\n", sep = "")
        cat("     0 means both paths agree and 1300LST was always correct.\n")
        cat("     +7 (or -17) means the old path was on UTC at this MST station,\n")
        cat("     and the existing '1300LST' option has really been 0600 local.\n")
      }
    }
  }
  cat("\n")
}

.hr("=")
cat("COMPLETE -- results in `B`\n")
cat("Transcript: ", LOG_FILE, "\n", sep = "")
.hr("=")
sink(NULL)
