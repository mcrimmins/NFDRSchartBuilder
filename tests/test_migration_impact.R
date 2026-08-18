# ==============================================================================
# test_migration_impact.R -- How much do the charts actually move?
# Branch: fems-graphql-api
#
# Run from the project root:  source("tests/test_migration_impact.R")
#
# The old CSV path derived both `date` and `hour` from a timestamp parsed as
# UTC. test_1300lst.R proved it: at SAGUARO (MST, UTC-7) the old hour was the
# new hour plus 7. Two things follow, and only the first is obvious.
#
#   1. "1300LST" selected UTC 13:00 = 06:00 local -- the daily minimum, not
#      the mid-afternoon peak it advertises.
#
#   2. Less obvious: `date` was also the UTC date, so every "daily" mean, min,
#      max, precipitation total and burn-period count was computed over a UTC
#      day. At MST that window runs 17:00 local the previous day to 16:59
#      local. Afternoon and evening observations land on the following day.
#
# This script quantifies both from a single download. Because the offset is a
# fixed -7 with no DST at this station, the old UTC basis can be reconstructed
# exactly from the new local one by adding 7 hours -- no need for the retired
# CSV code, which is why this still works after the migration.
#
# Transcript -> tests/logs/test_migration_impact_log.txt
# ==============================================================================

library(dplyr)
library(lubridate)

source("R/fems_download.R")

STATION   <- "21202"          # SAGUARO, MST (UTC-7), no DST
FUEL      <- "Y"
YEARS     <- 5                # bump to 21 for the full record
TZ_OFFSET <- 7                # hours to add to local to get UTC
LOG_FILE  <- "tests/logs/test_migration_impact_log.txt"

.hr <- function(ch = "-") cat(strrep(ch, 78), "\n", sep = "")

sink(LOG_FILE, split = TRUE)
cat("Migration impact --", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("Station ", STATION, ", ", YEARS, " years, fuel model ", FUEL, "\n\n", sep = "")

start <- Sys.Date() - round(365.25 * YEARS)

cat("Downloading...\n")
d <- fems_fetch_station_data(STATION, start, Sys.Date(), fuel_model = FUEL,
                             verbose = FALSE)
stopifnot(!is.null(d))
d <- d[d$record_type == "O", ]
cat("  ", nrow(d), " observed rows, ", format(min(d$date)), " to ",
    format(max(d$date)), "\n\n", sep = "")

# Reconstruct the old UTC basis: shift the local stamp forward by the offset,
# then take the date and hour of the result. This is what the CSV path saw.
d <- d %>%
  mutate(
    local_time = as.POSIXct(paste0(date, " ", sprintf("%02d", hour), ":00:00"),
                            tz = "UTC"),          # tz label is inert here
    utc_time   = local_time + TZ_OFFSET * 3600,
    utc_date   = as.Date(utc_time),
    utc_hour   = as.integer(format(utc_time, "%H"))
  )

VARS <- c("temperature", "relativeHumidity", "vpd",
          "energyReleaseComponent", "burningIndex", "ignitionComponent",
          "oneHR_TL_FuelMoisture", "hundredHR_TL_FuelMoisture")
VARS <- intersect(VARS, names(d))


# ==============================================================================
# 1 -- The 1300LST statistic
# ==============================================================================
.hr("="); cat("1. The '1300LST' daily statistic\n"); .hr("=")
cat("What the option showed before (UTC hour 13) vs what it shows now\n")
cat("(local hour 13), averaged over ", YEARS, " years:\n\n", sep = "")

res <- data.frame()
for (v in VARS) {
  before <- mean(d[[v]][d$utc_hour == 13], na.rm = TRUE)
  after  <- mean(d[[v]][d$hour     == 13], na.rm = TRUE)
  res <- rbind(res, data.frame(
    variable = v,
    was      = round(before, 2),
    now      = round(after, 2),
    change   = round(after - before, 2),
    pct      = ifelse(before == 0, NA, round(100 * (after - before) / abs(before), 1))
  ))
}
print(res, row.names = FALSE)
cat("\n  This affects only users who selected the 1300LST option.\n\n")


# ==============================================================================
# 2 -- Daily aggregation boundaries
# The quieter half of the change: mean/min/max were computed over a UTC day.
# ==============================================================================
.hr("="); cat("2. Daily mean / min / max, UTC day vs local day\n"); .hr("=")
cat("Mean of the daily statistic across the whole period, both ways:\n\n")

res2 <- data.frame()
for (v in VARS) {
  for (fn_name in c("mean", "min", "max")) {
    fn <- match.fun(fn_name)
    old_daily <- tapply(d[[v]], d$utc_date, function(x) if (all(is.na(x))) NA else fn(x, na.rm = TRUE))
    new_daily <- tapply(d[[v]], d$date,     function(x) if (all(is.na(x))) NA else fn(x, na.rm = TRUE))
    res2 <- rbind(res2, data.frame(
      variable = v, stat = fn_name,
      was = round(mean(old_daily, na.rm = TRUE), 2),
      now = round(mean(new_daily, na.rm = TRUE), 2),
      change = round(mean(new_daily, na.rm = TRUE) - mean(old_daily, na.rm = TRUE), 3)
    ))
  }
}
print(res2, row.names = FALSE)
cat("\n  Daily min and max barely move: at MST the UTC day still contains both\n")
cat("  the dawn minimum and the afternoon peak of the same local day. Means\n")
cat("  shift slightly because the window swaps one evening for another.\n\n")


# ==============================================================================
# 3 -- Precipitation, where the day boundary actually bites
# Storms are common late afternoon and evening. Under a UTC day, anything
# after 17:00 local is credited to the NEXT day.
# ==============================================================================
if ("precipitation" %in% names(d)) {
  .hr("="); cat("3. Daily precipitation totals\n"); .hr("=")

  old_p <- tapply(d$precipitation, d$utc_date, sum, na.rm = TRUE)
  new_p <- tapply(d$precipitation, d$date,     sum, na.rm = TRUE)

  cat("  total precip, UTC-day basis   : ", round(sum(old_p, na.rm = TRUE), 2), " in\n", sep = "")
  cat("  total precip, local-day basis : ", round(sum(new_p, na.rm = TRUE), 2), " in\n", sep = "")
  cat("  (totals should match -- the same rain, attributed to different days)\n\n")

  common <- intersect(names(old_p), names(new_p))
  diffs  <- new_p[common] - old_p[common]
  wet    <- diffs[abs(diffs) > 0.001]

  cat("  days where the daily total changed : ", length(wet), " of ", length(common), "\n", sep = "")
  if (length(wet) > 0) {
    cat("  largest single-day reattribution   : ", round(max(abs(wet)), 2), " in\n", sep = "")
    cat("  mean absolute change on those days : ", round(mean(abs(wet)), 3), " in\n", sep = "")
  }

  # Evening rain is the mechanism -- show how much falls after 17:00 local.
  evening <- sum(d$precipitation[d$hour >= 17], na.rm = TRUE)
  cat("  share of all precip falling 17:00-23:00 local: ",
      round(100 * evening / sum(d$precipitation, na.rm = TRUE), 1), "%\n", sep = "")
  cat("  >> That is the fraction that was being credited to the following day.\n\n")
}


# ==============================================================================
# 4 -- Burn period
# ==============================================================================
if ("relativeHumidity" %in% names(d)) {
  .hr("="); cat("4. Burn period (hours per day with RH < 20%)\n"); .hr("=")
  old_b <- tapply(d$relativeHumidity < 20, d$utc_date, sum, na.rm = TRUE)
  new_b <- tapply(d$relativeHumidity < 20, d$date,     sum, na.rm = TRUE)
  cat("  mean hours/day, UTC-day basis   : ", round(mean(old_b, na.rm = TRUE), 2), "\n", sep = "")
  cat("  mean hours/day, local-day basis : ", round(mean(new_b, na.rm = TRUE), 2), "\n", sep = "")
  common <- intersect(names(old_b), names(new_b))
  cat("  days where the count changed    : ",
      sum(old_b[common] != new_b[common], na.rm = TRUE), " of ", length(common), "\n\n", sep = "")
}

.hr("=")
cat("Transcript: ", LOG_FILE, "\n", sep = "")
.hr("=")
sink(NULL)
