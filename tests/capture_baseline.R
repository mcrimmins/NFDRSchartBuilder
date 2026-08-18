# ==============================================================================
# tests/capture_baseline.R
#
# PURPOSE
#   Freeze the current (pre-refactor) behaviour of the daily-aggregation chain
#   so the three refactor steps can be verified by file comparison instead of
#   by eyeballing charts.
#
#   Run this ONCE, at HEAD, BEFORE any change to app.R.
#
#   It does two things:
#     1. Downloads the raw FEMS data once and freezes it to
#        tests/logs/baseline_rawdata.rds. Every later verification run reads
#        that frozen file, so the input is byte-identical across steps. If we
#        re-downloaded each time, the current year would gain days between
#        runs and every diff would be full of noise that isn't the refactor.
#     2. Runs a verbatim copy of the HEAD aggregation chain over 8 combos that
#        stress the parts the refactor touches, and writes each result to
#        tests/logs/baseline_<n>_<label>.rds.
#
#   The copy of the chain below is lifted line-for-line from the
#   download_plot_data handler in app.R (currently lines 894-926), which is
#   identical to the copies in climatology_plot and plotly_climatology_plot.
#   Carrying a copy is legitimate here and ONLY here: at capture time, HEAD is
#   the reference. After step 1 lands, tests/test_daily_series.R calls the real
#   build_daily_series() and compares against these files.
#
# HOW TO RUN
#   With NFDRSChartBuilder.Rproj open, from the project root:
#     source("tests/capture_baseline.R")
#   Takes several minutes -- it pulls a full 21-year period of record for two
#   stations. Output goes to tests/logs/ (gitignored).
#
# Helpers are dot-prefixed per CLAUDE.md so they cannot mask a shiny export
# and stay out of ls().
# ==============================================================================

library(dplyr)
library(tidyr)

source("R/fems_download.R")

# ------------------------------------------------------------------ config ---

.FETCH_START_DATE <- "2005-01-01"          # same constant app.R uses
.FUEL_MODEL       <- "Y"
.PRIMARY_STATION  <- "21202"               # app.R's default_station_id
.OUT_DIR          <- "tests/logs"
.RAW_RDS          <- file.path(.OUT_DIR, "baseline_rawdata.rds")

dir.create(.OUT_DIR, showWarnings = FALSE, recursive = TRUE)

# ----------------------------------------------------------------- helpers ---

.transcript <- character(0)

.say <- function(...) {
  line <- paste0(...)
  .transcript <<- c(.transcript, line)
  cat(line, "\n", sep = "")
  invisible(NULL)
}

.rule <- function() .say(strrep("-", 78))

# Identical body to safe_summary1() in app.R. Copied rather than sourced
# because app.R ends in shinyApp() and cannot be sourced from a script.
.safe_summary1 <- function(x, fun) {
  if (all(is.na(x))) NA else fun(x, na.rm = TRUE)
}

# A deterministic fingerprint for the transcript. The .rds holds the exact
# data for all.equal(); this is just so a human reading the log can see at a
# glance whether two runs agree.
.fingerprint <- function(df) {
  v <- df$value
  sprintf("rows=%d  na=%d  sum=%.6f  mean=%.6f  min=%.6f  max=%.6f",
          nrow(df), sum(is.na(v)),
          sum(v, na.rm = TRUE), mean(v, na.rm = TRUE),
          suppressWarnings(min(v, na.rm = TRUE)),
          suppressWarnings(max(v, na.rm = TRUE)))
}

# ------------------------------------------------------------- HEAD  chain ---
# VERBATIM from app.R download_plot_data, with input$variable -> variable,
# input$daily_stat -> daily_stat, input$month_range -> month_range.
# Do not "improve" anything in here. Its only job is to reproduce HEAD.

.chain_head <- function(all_data, variable, daily_stat, month_range) {

  stn_data <- all_data %>%
    group_by(station_id, date, record_type) %>%
    summarise(
      value = if (variable %in% c("precip_total", "precip_cum")) {
        .safe_summary1(precipitation, sum)
      } else if (variable == "burn_period") {
        .safe_summary1(relativeHumidity < 20, sum)
      } else if (daily_stat == "1300LST") {
        .safe_summary1(.data[[variable]][hour == 13], mean)
      } else {
        .safe_summary1(.data[[variable]], match.fun(daily_stat))
      },
      .groups = "drop"
    ) %>%
    mutate(year = as.integer(format(date, "%Y")),
           month_day = as.Date(format(date, "2024-%m-%d")))

  # cumulative step -- MUST stay before the month filter
  if (variable == "precip_cum") {
    stn_data <- stn_data %>%
      arrange(date) %>%
      group_by(station_id, year, record_type) %>%
      mutate(value = cumsum(coalesce(value, 0))) %>%
      ungroup()
  }

  # month filtering step
  stn_data <- stn_data %>%
    filter(lubridate::month(date) >= month_range[1] &
           lubridate::month(date) <= month_range[2])

  # station averaging
  stn_data %>%
    group_by(month_day, year, record_type) %>%
    summarise(value = mean(value, na.rm = TRUE), .groups = "drop")
}

# --------------------------------------------------------- pick 2nd station ---
# Chosen automatically as the nearest neighbour to the primary station so the
# pair is deterministic across runs and needs no hand-editing. Falls through
# the 5 nearest until one actually returns data.

.pick_partner_station <- function(meta, primary) {
  p <- meta[as.character(meta$station_id) == primary, ]
  if (nrow(p) == 0) stop("Primary station not found in station_metadata")
  d <- sqrt((meta$latitude  - p$latitude[1])^2 +
            ((meta$longitude - p$longitude[1]) *
               cos(p$latitude[1] * pi / 180))^2)
  ord <- order(d)
  cand <- as.character(meta$station_id[ord])
  cand <- setdiff(cand, primary)
  head(cand, 5)
}

# ==============================================================================
# 1. Raw data: fetch once, freeze
# ==============================================================================

.say("")
.rule()
.say("BASELINE CAPTURE  --  ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
.rule()

station_metadata <- read.csv("data/station_metadata_FEMS3_042225.csv",
                             stringsAsFactors = FALSE)

if (file.exists(.RAW_RDS)) {
  .say("Frozen raw data already exists at ", .RAW_RDS)
  .say("Delete it first if you want a fresh download.")
  .raw <- readRDS(.RAW_RDS)
  .stations_used <- .raw$stations
  all_data_multi <- .raw$all_data
  .end_date <- .raw$end_date
} else {
  .end_date <- Sys.Date() + 7
  .say("Fetching ", .FETCH_START_DATE, " to ", as.character(.end_date),
       "  (fuel model ", .FUEL_MODEL, ")")
  .say("This pulls a full period of record for two stations. Expect minutes.")

  .fetch_one <- function(id) {
    nf <- fems_download_nfdrs(id, .FETCH_START_DATE, .end_date,
                              fuel_model = .FUEL_MODEL, verbose = FALSE)
    if (is.null(nf) || nrow(nf) == 0) return(NULL)
    wx <- fems_download_weather(id, .FETCH_START_DATE, .end_date,
                                verbose = FALSE)
    if (is.null(wx) || nrow(wx) == 0) return(NULL)
    left_join(nf, wx, by = c("station_id", "date", "hour", "record_type"))
  }

  .say("  primary station ", .PRIMARY_STATION, " ...")
  d1 <- .fetch_one(.PRIMARY_STATION)
  if (is.null(d1)) stop("Primary station returned no data -- check FEMS_USER / FEMS_KEY")
  .say("    ", nrow(d1), " rows")

  partner <- NULL
  for (cand in .pick_partner_station(station_metadata, .PRIMARY_STATION)) {
    .say("  trying partner station ", cand, " ...")
    d2 <- .fetch_one(cand)
    if (!is.null(d2)) { partner <- cand; break }
    .say("    no data, next")
  }
  if (is.null(partner)) stop("No partner station returned data")
  .say("    ", nrow(d2), " rows  (partner = ", partner, ")")

  .stations_used <- c(.PRIMARY_STATION, partner)
  all_data_multi <- bind_rows(d1, d2)

  saveRDS(list(all_data = all_data_multi,
               stations  = .stations_used,
               end_date  = .end_date,
               captured  = Sys.time()),
          .RAW_RDS)
  .say("Frozen to ", .RAW_RDS)
}

all_data_single <- all_data_multi %>%
  filter(as.character(station_id) == .stations_used[1])

.say("")
.say("Stations : ", paste(.stations_used, collapse = ", "))
.say("Raw rows : ", nrow(all_data_multi), " (multi), ",
     nrow(all_data_single), " (single)")
.say("Date span: ", as.character(min(all_data_multi$date, na.rm = TRUE)),
     " to ", as.character(max(all_data_multi$date, na.rm = TRUE)))

# ==============================================================================
# 2. Combos
# ==============================================================================
# Each one exercises a branch the refactor touches. 3, 7 and 8 are the ones
# that matter most for step 2 (the crop/station-average reorder).

.combos <- list(
  list(label = "erc_max_full",        variable = "energyReleaseComponent", daily_stat = "max",     month_range = c(1, 12), scope = "single",
       why = "plain index, uncropped -- the simple control case"),
  list(label = "erc_max_crop",        variable = "energyReleaseComponent", daily_stat = "max",     month_range = c(3, 6),  scope = "single",
       why = "same, cropped -- isolates the month filter"),
  list(label = "precipcum_crop",      variable = "precip_cum",             daily_stat = "mean",    month_range = c(3, 6),  scope = "single",
       why = "CRITICAL: cumsum must start Jan 1, not at the crop boundary"),
  list(label = "preciptotal_full",    variable = "precip_total",           daily_stat = "mean",    month_range = c(1, 12), scope = "single",
       why = "sum path with NA handling"),
  list(label = "burnperiod_crop",     variable = "burn_period",            daily_stat = "mean",    month_range = c(5, 9),  scope = "single",
       why = "RH<20 count path"),
  list(label = "rh_1300lst_full",     variable = "relativeHumidity",       daily_stat = "1300LST", month_range = c(1, 12), scope = "single",
       why = "hour==13 subset path"),
  list(label = "erc_max_crop_multi",  variable = "energyReleaseComponent", daily_stat = "max",     month_range = c(3, 6),  scope = "multi",
       why = "CRITICAL: station-average x crop -- exactly what step 2 reorders"),
  list(label = "precipcum_crop_multi",variable = "precip_cum",             daily_stat = "mean",    month_range = c(3, 6),  scope = "multi",
       why = "CRITICAL: both hazards at once")
)

.say("")
.rule()
.say("CAPTURING ", length(.combos), " COMBOS")
.rule()

.manifest <- list()
.failures <- 0L

for (i in seq_along(.combos)) {
  cb <- .combos[[i]]
  dat <- if (cb$scope == "multi") all_data_multi else all_data_single

  .say("")
  .say(sprintf("[%d/%d] %s", i, length(.combos), cb$label))
  .say("      variable=", cb$variable, "  stat=", cb$daily_stat,
       "  months=", cb$month_range[1], "-", cb$month_range[2],
       "  stations=", cb$scope)
  .say("      why: ", cb$why)

  res <- tryCatch(
    .chain_head(dat, cb$variable, cb$daily_stat, cb$month_range),
    error = function(e) { .say("      ERROR: ", conditionMessage(e)); NULL })

  if (is.null(res)) { .failures <- .failures + 1L; next }
  if (nrow(res) == 0) {
    .say("      ERROR: zero rows returned")
    .failures <- .failures + 1L
    next
  }

  fn <- file.path(.OUT_DIR, sprintf("baseline_%d_%s.rds", i, cb$label))
  saveRDS(list(spec = cb, result = res), fn)

  .say("      ", .fingerprint(res))
  .say("      -> ", fn)

  .manifest[[length(.manifest) + 1L]] <- data.frame(
    n = i, label = cb$label, variable = cb$variable,
    daily_stat = cb$daily_stat,
    months = paste0(cb$month_range[1], "-", cb$month_range[2]),
    scope = cb$scope, rows = nrow(res),
    n_na = sum(is.na(res$value)),
    sum_value = round(sum(res$value, na.rm = TRUE), 6),
    file = basename(fn), stringsAsFactors = FALSE)
}

# ==============================================================================
# 3. Sanity checks on the baselines themselves
# ==============================================================================
# If HEAD is already wrong, freezing it would enshrine the bug. Two cheap
# checks that would catch that.

.say("")
.rule()
.say("SANITY CHECKS ON HEAD")
.rule()

.checks_failed <- 0L

# (a) precip_cum on a cropped range must NOT start near zero in March. If the
#     cumsum were running after the crop, the first March value would be ~0.
f3 <- file.path(.OUT_DIR, "baseline_3_precipcum_crop.rds")
if (file.exists(f3)) {
  r3 <- readRDS(f3)$result
  first_md <- min(r3$month_day, na.rm = TRUE)
  firsts <- r3 %>% filter(month_day == first_md, record_type == "O")
  mn <- mean(firsts$value, na.rm = TRUE)
  .say("(a) precip_cum first cropped day (", format(first_md, "%b-%d"),
       ") mean across years = ", round(mn, 4))
  if (is.finite(mn) && mn > 0.01) {
    .say("    PASS -- accumulation carried in from Jan 1")
  } else {
    .say("    FAIL -- looks like cumsum ran after the crop")
    .checks_failed <- .checks_failed + 1L
  }
}

# (b) cropped ERC must be a strict subset of uncropped ERC, same values.
f1 <- file.path(.OUT_DIR, "baseline_1_erc_max_full.rds")
f2 <- file.path(.OUT_DIR, "baseline_2_erc_max_crop.rds")
if (file.exists(f1) && file.exists(f2)) {
  r1 <- readRDS(f1)$result
  r2 <- readRDS(f2)$result
  j <- inner_join(r2, r1, by = c("month_day", "year", "record_type"),
                  suffix = c(".crop", ".full"))
  ok <- nrow(j) == nrow(r2) &&
        isTRUE(all.equal(j$value.crop, j$value.full, tolerance = 1e-12))
  .say("(b) cropped ERC is a subset of uncropped, values unchanged: ",
       nrow(j), " of ", nrow(r2), " rows matched")
  if (ok) {
    .say("    PASS")
  } else {
    .say("    FAIL -- the crop is altering values, not just filtering rows")
    .checks_failed <- .checks_failed + 1L
  }
}

# ==============================================================================
# 4. Manifest + verdict
# ==============================================================================

if (length(.manifest) > 0) {
  man <- bind_rows(.manifest)
  write.csv(man, file.path(.OUT_DIR, "baseline_manifest.csv"), row.names = FALSE)
  .say("")
  .rule()
  .say("MANIFEST -> tests/logs/baseline_manifest.csv")
  .rule()
  .transcript <- c(.transcript, capture.output(print(man, row.names = FALSE)))
  print(man, row.names = FALSE)
}

.say("")
.rule()
if (.failures == 0L && .checks_failed == 0L && length(.manifest) == length(.combos)) {
  .say("VERDICT: PASS -- ", length(.manifest), " baselines captured, ",
       "HEAD sanity checks clean.")
  .say("Safe to start step 1 (extract build_daily_series).")
  .say("Do NOT delete tests/logs/ until all three steps are verified.")
} else {
  .say("VERDICT: FAIL -- ", .failures, " combo failure(s), ",
       .checks_failed, " sanity check failure(s).")
  .say("Paste this transcript back before any change to app.R.")
}
.rule()

.logfile <- file.path(.OUT_DIR,
                      paste0("baseline_capture_",
                             format(Sys.time(), "%Y%m%d_%H%M%S"), ".txt"))
writeLines(.transcript, .logfile)
cat("\nTranscript written to", .logfile, "\n")
