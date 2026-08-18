# ==============================================================================
# test_fems_api_3.R -- Benchmark and validate the new download layer
# Branch: fems-graphql-api
#
# Run:  source("test_fems_api_3.R")
#
# Probe C showed ~6s per request for a 100-row page, which means per-request
# latency dominates, not row count. Full period of record is ~184,000 rows per
# station, so page size is the single biggest lever on fetch time. Probe 1
# below finds the right value; probe 2 does a real full-POR pull with it.
#
# Probe 3 then checks that what comes back actually satisfies the contract
# app.R depends on -- because a fast download that returns the wrong column
# names is worse than no download at all.
#
# Transcript -> test_fems_api_3_log.txt
# ==============================================================================

source("R/fems_download.R")

STATION    <- "21202"
FUEL_MODEL <- "Y"
LOG_FILE <- "tests/logs/test_fems_api_3_log.txt"

RUN_1 <- TRUE    # page-size sweep on a 1-year pull
RUN_2 <- TRUE    # full period of record, one station   (the slow one)
RUN_3 <- TRUE    # validate the output contract

.hr <- function(ch = "-") cat(strrep(ch, 78), "\n", sep = "")
BENCH <- list()

sink(LOG_FILE, split = TRUE)
cat("FEMS download-layer benchmark --", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")


# ==============================================================================
# 1 -- Page size sweep
# Same 1-year pull (~8,800 rows) at four page sizes. If latency is per-request,
# time should fall roughly in proportion to the number of pages, then flatten
# once one page holds everything.
# ==============================================================================
if (RUN_1) {
  .hr(); cat("PROBE 1: page size sweep, 1 year of NFDRS\n"); .hr()

  s <- Sys.Date() - 365
  res <- data.frame()

  for (pp in c(1000, 5000, 25000, 100000)) {
    t0 <- Sys.time()
    r <- tryCatch({
      df <- fems_download_nfdrs(STATION, s, Sys.Date(),
                                fuel_model = FUEL_MODEL,
                                per_page = pp, verbose = FALSE)
      list(ok = TRUE, n = nrow(df), note = "")
    }, error = function(e) list(ok = FALSE, n = NA, note = conditionMessage(e)))

    el <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 2)
    cat(sprintf("  per_page=%-7d %s  rows=%-8s %ss\n", pp,
                if (r$ok) "OK    " else "FAIL  ", format(r$n), el))
    if (!r$ok) cat("      ", substr(r$note, 1, 250), "\n", sep = "")

    res <- rbind(res, data.frame(per_page = pp, ok = r$ok, rows = r$n, secs = el))
    Sys.sleep(0.5)
  }

  cat("\n  >> Pick the smallest per_page that is at or near the fastest time.\n")
  cat("     If a large per_page errors or truncates, that is the real ceiling.\n\n")
  BENCH$page_sweep <- res
}


# ==============================================================================
# 2 -- Full period of record, one station
# This is what the app will actually do on 'Fetch Station Data'. ~184,000 NFDRS
# rows plus a comparable number of weather rows.
# ==============================================================================
if (RUN_2) {
  .hr(); cat("PROBE 2: full POR pull (2005 -> today+7), one station\n"); .hr()

  best <- if (!is.null(BENCH$page_sweep)) {
    ok <- BENCH$page_sweep[BENCH$page_sweep$ok, ]
    if (nrow(ok)) ok$per_page[which.min(ok$secs)] else FEMS_PER_PAGE
  } else FEMS_PER_PAGE
  cat("  Using per_page = ", best, "\n\n", sep = "")

  t0 <- Sys.time()
  all_data <- tryCatch(
    fems_fetch_station_data(STATION,
                            start_date = "2005-01-01",
                            end_date   = Sys.Date() + 7,
                            fuel_model = FUEL_MODEL,
                            per_page   = best,
                            verbose    = TRUE),
    error = function(e) { cat("  [ERROR] ", conditionMessage(e), "\n", sep = ""); NULL })
  el <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1)

  if (!is.null(all_data)) {
    cat("\n  Rows      : ", nrow(all_data), "\n", sep = "")
    cat("  Columns   : ", ncol(all_data), "\n", sep = "")
    cat("  Elapsed   : ", el, "s\n", sep = "")
    cat("  Date span : ", format(min(all_data$date, na.rm = TRUE)), " to ",
        format(max(all_data$date, na.rm = TRUE)), "\n", sep = "")
    cat("  Rec types : ",
        paste(names(table(all_data$record_type)), table(all_data$record_type),
              sep = "=", collapse = "  "), "\n", sep = "")

    # How complete is each year? A year well under 8760 observed hours means
    # station downtime, and the climatology quietly rests on fewer samples.
    yr <- all_data[all_data$record_type == "O", ]
    yr <- table(format(yr$date, "%Y"))
    cat("\n  Observed rows per year:\n")
    print(yr)
  }
  BENCH$full <- all_data
  BENCH$full_secs <- el
  cat("\n")
}


# ==============================================================================
# 3 -- Contract validation
# app.R's plotting code refers to columns by name. If any are missing or
# misnamed, the variable selector silently drops them and charts go blank.
# ==============================================================================
if (RUN_3 && !is.null(BENCH$full)) {
  .hr(); cat("PROBE 3: does the output satisfy what app.R expects?\n"); .hr()

  d <- BENCH$full

  required <- c(
    "station_id", "date", "hour", "record_type",
    "energyReleaseComponent", "burningIndex", "ignitionComponent",
    "spreadComponent", "kbdi", "oneHR_TL_FuelMoisture", "tenHR_TL_FuelMoisture",
    "hundredHR_TL_FuelMoisture", "thousandHR_TL_FuelMoisture",
    "woodyLFI_fuelMoisture", "herbaceousLFI_fuelMoisture", "gsi",
    "temperature", "relativeHumidity", "precipitation", "windSpeed",
    "windDirection", "gustSpeed", "gustDirection", "solarRadiation",
    "vpd", "hdw", "dewpoint"
  )

  missing <- setdiff(required, names(d))
  cat("  Required columns present: ", length(required) - length(missing),
      "/", length(required), "\n", sep = "")
  if (length(missing)) {
    cat("  MISSING: ", paste(missing, collapse = ", "), "\n", sep = "")
  } else {
    cat("  All required columns present.\n")
  }

  # app.R builds its variable list from numeric columns, so a column that
  # arrives as character would vanish from the dropdown.
  non_num <- setdiff(names(d)[!sapply(d, is.numeric)],
                     c("station_id", "date", "record_type", "fuel_model"))
  if (length(non_num)) {
    cat("  NON-NUMERIC (would drop out of the variable selector): ",
        paste(non_num, collapse = ", "), "\n", sep = "")
  }

  # Coverage: all-NA columns are the quiet failure mode.
  allna <- names(d)[sapply(d, function(x) all(is.na(x)))]
  if (length(allna)) {
    cat("  ALL-NA over the full record: ", paste(allna, collapse = ", "), "\n", sep = "")
  } else {
    cat("  No all-NA columns.\n")
  }

  # --- the 1300LST question ---------------------------------------------
  # If hour is genuinely station-local, hour 13 should be the hot, dry part of
  # the day. If the old UTC-derived hour was wrong, this is where it shows.
  cat("\n  Mean temperature by hour (should peak mid-afternoon, ~14-16):\n")
  obs <- d[d$record_type == "O" & !is.na(d$temperature), ]
  bh  <- tapply(obs$temperature, obs$hour, mean, na.rm = TRUE)
  print(round(bh, 1))
  cat("\n  Hottest hour: ", names(bh)[which.max(bh)],
      "   Coolest hour: ", names(bh)[which.min(bh)], "\n", sep = "")
  cat("  >> A mid-afternoon peak confirms hour is local time.\n")

  # --- vpd cross-check ---------------------------------------------------
  cat("\n  API vpd vs locally computed (should agree to ~0.001 kPa):\n")
  chk <- obs[!is.na(obs$relativeHumidity) & !is.na(obs$vpd), ]
  if (nrow(chk) > 0) {
    chk   <- chk[seq_len(min(nrow(chk), 5000)), ]
    tc    <- (chk$temperature - 32) * 5 / 9
    local <- (1 - chk$relativeHumidity / 100) *
             (0.6108 * exp((17.27 * tc) / (tc + 237.3)))
    cat("    max abs difference: ",
        format(max(abs(local - chk$vpd), na.rm = TRUE), digits = 3), " kPa\n", sep = "")
  }

  # --- join integrity ----------------------------------------------------
  cat("\n  Join check (NFDRS rows with no matching weather row):\n")
  cat("    rows total          : ", nrow(d), "\n", sep = "")
  cat("    missing temperature : ", sum(is.na(d$temperature)),
      "  (", round(100 * mean(is.na(d$temperature)), 1), "%)\n", sep = "")
  cat("    duplicate keys      : ",
      sum(duplicated(paste(d$station_id, d$date, d$hour, d$record_type))), "\n", sep = "")
  cat("\n")
}

.hr("=")
cat("BENCHMARK COMPLETE -- results in `BENCH`\n")
if (!is.null(BENCH$full_secs)) {
  cat("Full POR for one station took ", BENCH$full_secs, "s.\n", sep = "")
}
cat("Transcript: ", LOG_FILE, "\n", sep = "")
.hr("=")
sink(NULL)
