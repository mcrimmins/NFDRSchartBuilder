# ==============================================================================
# tests/test_daily_series.R
#
# PURPOSE
#   Verify that build_daily_series() (R/daily_series.R) reproduces the frozen
#   pre-refactor baselines captured by tests/capture_baseline.R.
#
#   Run this after EVERY refactor step:
#     step 1 (extract)  -- expect BIT-IDENTICAL results. Nothing may move.
#     step 2 (reorder)  -- expect BIT-IDENTICAL results. The crop moving after
#                          the station average is supposed to be behaviour-
#                          neutral; this is the test that proves it.
#     step 3 (smoothing) -- expect the `value` column to stay bit-identical.
#                          Smoothing adds a `value_smooth` column and must not
#                          disturb the raw series. Set .EXPECT_EXTRA_COLS below.
#
#   It reads tests/logs/baseline_rawdata.rds, so the input data is byte-for-byte
#   the same as at capture time. Do not delete that file mid-refactor.
#
# HOW TO RUN
#   With NFDRSChartBuilder.Rproj open, from the project root:
#     source("tests/test_daily_series.R")
#   Fast -- no network, no downloads. Seconds, not minutes.
#
# Helpers are dot-prefixed per CLAUDE.md so they cannot mask a shiny export.
# ==============================================================================

library(dplyr)

source("R/daily_series.R")

.OUT_DIR <- "tests/logs"
.RAW_RDS <- file.path(.OUT_DIR, "baseline_rawdata.rds")

# Columns build_daily_series() is allowed to return beyond the baseline's four.
# Leave empty for steps 1 and 2; set to "value_smooth" for step 3.
.EXPECT_EXTRA_COLS <- character(0)

# ----------------------------------------------------------------- helpers ---

.transcript <- character(0)
.say <- function(...) {
  line <- paste0(...)
  .transcript <<- c(.transcript, line)
  cat(line, "\n", sep = "")
  invisible(NULL)
}
.rule <- function() .say(strrep("-", 78))

.say("")
.rule()
.say("DAILY SERIES REGRESSION  --  ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
.rule()

if (!file.exists(.RAW_RDS)) {
  stop("No frozen raw data at ", .RAW_RDS,
       " -- run tests/capture_baseline.R first (at HEAD, before any change).")
}
if (!exists("build_daily_series")) {
  stop("build_daily_series() not found -- is R/daily_series.R present?")
}

.raw       <- readRDS(.RAW_RDS)
.stations  <- .raw$stations
all_multi  <- .raw$all_data
all_single <- all_multi %>% filter(as.character(station_id) == .stations[1])

.say("Frozen input : ", .RAW_RDS)
.say("  captured   : ", format(.raw$captured, "%Y-%m-%d %H:%M:%S"))
.say("  stations   : ", paste(.stations, collapse = ", "))
.say("  raw rows   : ", nrow(all_multi), " (multi), ", nrow(all_single), " (single)")

.files <- sort(list.files(.OUT_DIR, pattern = "^baseline_[0-9]+_.*\\.rds$",
                          full.names = TRUE))
if (length(.files) == 0) stop("No baseline_*.rds files in ", .OUT_DIR)
.say("Baselines    : ", length(.files), " found")

# ============================================================== comparison ===

.n_pass <- 0L
.n_fail <- 0L
.fail_names <- character(0)

.say("")
.rule()
.say("COMPARING")
.rule()

for (f in .files) {
  b    <- readRDS(f)
  spec <- b$spec
  want <- b$result

  dat <- if (spec$scope == "multi") all_multi else all_single

  .say("")
  .say(basename(f))
  .say("  variable=", spec$variable, "  stat=", spec$daily_stat,
       "  months=", spec$month_range[1], "-", spec$month_range[2],
       "  stations=", spec$scope)

  got <- tryCatch(
    build_daily_series(dat, spec$variable, spec$daily_stat, spec$month_range),
    error = function(e) { .say("  ERROR: ", conditionMessage(e)); NULL })

  if (is.null(got)) {
    .n_fail <- .n_fail + 1L; .fail_names <- c(.fail_names, basename(f)); next
  }

  problems <- character(0)

  # --- shape -----------------------------------------------------------------
  expect_cols <- c(names(want), .EXPECT_EXTRA_COLS)
  if (!identical(names(got), expect_cols)) {
    problems <- c(problems, paste0("columns differ: expected [",
                                   paste(expect_cols, collapse = ", "),
                                   "] got [", paste(names(got), collapse = ", "), "]"))
  }
  if (nrow(got) != nrow(want)) {
    problems <- c(problems, paste0("row count: expected ", nrow(want),
                                   " got ", nrow(got)))
  }

  # --- keys, in the order returned (order itself must not drift) -------------
  if (nrow(got) == nrow(want)) {
    for (k in c("month_day", "year", "record_type")) {
      if (k %in% names(got) && !identical(got[[k]], want[[k]])) {
        n_off <- sum(as.character(got[[k]]) != as.character(want[[k]]))
        problems <- c(problems, paste0("key '", k, "' differs in ", n_off, " rows"))
      }
    }
  }

  # --- values: exact bit equality is the bar ---------------------------------
  if (nrow(got) == nrow(want) && "value" %in% names(got)) {
    if (identical(got$value, want$value)) {
      .say("  values: BIT-IDENTICAL (", nrow(got), " rows)")
    } else {
      d <- suppressWarnings(abs(got$value - want$value))
      na_mismatch <- sum(xor(is.na(got$value), is.na(want$value)))
      maxd <- suppressWarnings(max(d, na.rm = TRUE))
      nd   <- sum(d > 0, na.rm = TRUE)
      problems <- c(problems,
                    paste0("values differ in ", nd, " rows, max abs diff ",
                           format(maxd, scientific = TRUE, digits = 4),
                           ", NA-pattern mismatches ", na_mismatch))
      idx <- utils::head(which(d > 0 | xor(is.na(got$value), is.na(want$value))), 5)
      for (i in idx) {
        problems <- c(problems,
                      sprintf("    %s %s %s : baseline %s -> now %s",
                              format(want$month_day[i], "%b-%d"), want$year[i],
                              want$record_type[i],
                              format(want$value[i]), format(got$value[i])))
      }
    }
  }

  if (length(problems) == 0) {
    .say("  PASS")
    .n_pass <- .n_pass + 1L
  } else {
    for (p in problems) .say("  FAIL: ", p)
    .n_fail <- .n_fail + 1L
    .fail_names <- c(.fail_names, basename(f))
  }
}

# ================================================================= verdict ===

.say("")
.rule()
if (.n_fail == 0L) {
  .say("VERDICT: PASS -- ", .n_pass, " of ", length(.files),
       " baselines reproduced bit-identically.")
  .say("build_daily_series() is behaviourally identical to the pre-refactor chain.")
} else {
  .say("VERDICT: FAIL -- ", .n_fail, " of ", length(.files), " baselines differ:")
  for (nm in .fail_names) .say("  ", nm)
  .say("Do not commit. Paste this transcript back.")
}
.rule()

.logfile <- file.path(.OUT_DIR,
                      paste0("test_daily_series_",
                             format(Sys.time(), "%Y%m%d_%H%M%S"), ".txt"))
writeLines(.transcript, .logfile)
cat("\nTranscript written to", .logfile, "\n")
