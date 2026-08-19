# ==============================================================================
# tests/test_daily_series.R
#
# PURPOSE
#   Verify that build_daily_series() (R/daily_series.R) still reproduces the
#   frozen pre-refactor baselines captured by tests/capture_baseline.R, and
#   that turning smoothing on does not disturb the raw series.
#
#   Run this after EVERY refactor step:
#     step 1 (extract)  -- expect BIT-IDENTICAL results.
#     step 2 (reorder)  -- expect BIT-IDENTICAL results. The crop moving after
#                          the station average is supposed to be behaviour-
#                          neutral; this is the test that proves it.
#     step 3 (smoothing) -- expect `value` to STAY bit-identical. Smoothing adds
#                          a `value_smooth` column and must not move a single
#                          raw number.
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

# Columns build_daily_series() returns beyond the baseline's four.
.EXPECT_EXTRA_COLS <- "value_smooth"

# Smoothing settings used for the phase 2 smoke run.
.SM_FUN    <- "mean"
.SM_WINDOW <- 7
.SM_ALIGN  <- "center"

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
.say("Smoothing    : ", .SM_WINDOW, "-day ", .SM_ALIGN, " ", .SM_FUN,
     " (phase 2 only)")

.n_pass <- 0L
.n_fail <- 0L
.fail_names <- character(0)

.check <- function(name, problems) {
  if (length(problems) == 0) {
    .say("  PASS")
    .n_pass <<- .n_pass + 1L
  } else {
    for (p in problems) .say("  FAIL: ", p)
    .n_fail <<- .n_fail + 1L
    .fail_names <<- c(.fail_names, name)
  }
}

# Compare a returned `value` column against a baseline, exactly.
.value_problems <- function(got, want) {
  problems <- character(0)
  if (nrow(got) != nrow(want)) {
    return(paste0("row count: expected ", nrow(want), " got ", nrow(got)))
  }
  for (k in c("month_day", "year", "record_type")) {
    if (k %in% names(got) && !identical(got[[k]], want[[k]])) {
      problems <- c(problems, paste0("key '", k, "' differs in ",
                                     sum(as.character(got[[k]]) != as.character(want[[k]])),
                                     " rows -- row ORDER may have drifted"))
    }
  }
  if (!identical(got$value, want$value)) {
    d <- suppressWarnings(abs(got$value - want$value))
    problems <- c(problems,
                  paste0("values differ in ", sum(d > 0, na.rm = TRUE),
                         " rows, max abs diff ",
                         format(suppressWarnings(max(d, na.rm = TRUE)),
                                scientific = TRUE, digits = 4),
                         ", NA-pattern mismatches ",
                         sum(xor(is.na(got$value), is.na(want$value)))))
    idx <- utils::head(which(d > 0 | xor(is.na(got$value), is.na(want$value))), 5)
    for (i in idx) {
      problems <- c(problems,
                    sprintf("    %s %s %s : baseline %s -> now %s",
                            format(want$month_day[i], "%b-%d"), want$year[i],
                            want$record_type[i],
                            format(want$value[i]), format(got$value[i])))
    }
  }
  problems
}

# ==================================================== phase 1: smoothing off ===

.say("")
.rule()
.say("PHASE 1 -- smoothing OFF, must reproduce the baselines bit-identically")
.rule()

for (f in .files) {
  b <- readRDS(f); spec <- b$spec; want <- b$result
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
  expect_cols <- c(names(want), .EXPECT_EXTRA_COLS)
  if (!identical(names(got), expect_cols)) {
    problems <- c(problems, paste0("columns: expected [",
                                   paste(expect_cols, collapse = ", "),
                                   "] got [", paste(names(got), collapse = ", "), "]"))
  }
  if ("value_smooth" %in% names(got) && !all(is.na(got$value_smooth))) {
    problems <- c(problems, "value_smooth should be all-NA when smooth = FALSE")
  }
  problems <- c(problems, .value_problems(got, want))

  if (length(problems) == 0) .say("  values: BIT-IDENTICAL (", nrow(got), " rows)")
  .check(paste0(basename(f), " [off]"), problems)
}

# ===================================================== phase 2: smoothing on ===
# The raw series must not move. Two further properties are worth asserting
# because they are exactly what the step 2 reorder bought us:
#
#   (a) on a CROPPED range, the first plotted day must have a smoothed value.
#       If the smoother ran after the crop it would have no history before the
#       crop boundary and the first days would be NA.
#   (b) on a FULL-YEAR range, the first days of January must be NA, because
#       nothing precedes them -- that is the edge padding working as specified.

.say("")
.rule()
.say("PHASE 2 -- smoothing ON, raw values must not move")
.rule()

for (f in .files) {
  b <- readRDS(f); spec <- b$spec; want <- b$result
  dat <- if (spec$scope == "multi") all_multi else all_single

  .say("")
  .say(basename(f))

  got <- tryCatch(
    build_daily_series(dat, spec$variable, spec$daily_stat, spec$month_range,
                       smooth = TRUE, smooth_fun = .SM_FUN,
                       smooth_window = .SM_WINDOW, smooth_align = .SM_ALIGN),
    error = function(e) { .say("  ERROR: ", conditionMessage(e)); NULL })

  if (is.null(got)) {
    .n_fail <- .n_fail + 1L
    .fail_names <- c(.fail_names, paste0(basename(f), " [on]")); next
  }

  problems <- .value_problems(got, want)
  if (length(problems) == 0) {
    .say("  raw values: BIT-IDENTICAL with smoothing on (", nrow(got), " rows)")
  }

  n_sm <- sum(!is.na(got$value_smooth))
  .say("  smoothed values present: ", n_sm, " of ", nrow(got))
  if (n_sm == 0) problems <- c(problems, "value_smooth is entirely NA")

  if (spec$month_range[1] > 1) {
    # (a) cropped range -- the first plotted day must carry a smoothed value
    first_md <- min(got$month_day, na.rm = TRUE)
    edge <- got %>% filter(month_day == first_md, !is.na(value))
    n_missing <- sum(is.na(edge$value_smooth))
    .say("  (a) first cropped day ", format(first_md, "%b-%d"),
         ": ", nrow(edge) - n_missing, " of ", nrow(edge),
         " year/type rows have a smoothed value")
    if (n_missing > 0) {
      problems <- c(problems,
                    paste0("(a) ", n_missing, " rows at the first cropped day have no ",
                           "smoothed value -- the smoother is not seeing pre-crop history"))
    } else {
      .say("      PASS -- smoothing ran before the crop")
    }
  } else {
    # (b) full year -- the first (window-1)/2 days must be NA
    half <- (.SM_WINDOW - 1L) %/% 2L
    head_days <- as.Date("2024-01-01") + seq_len(half) - 1L
    edge <- got %>% filter(month_day %in% head_days)
    n_present <- sum(!is.na(edge$value_smooth))
    .say("  (b) Jan-01..", format(max(head_days), "%b-%d"), ": ", n_present, " of ", nrow(edge),
         " rows have a smoothed value (expected 0)")
    if (n_present > 0) {
      problems <- c(problems,
                    paste0("(b) ", n_present, " rows in the first ", half,
                           " days of January have a smoothed value -- edge padding ",
                           "is not being applied"))
    } else {
      .say("      PASS -- edges padded with NA as specified")
    }
  }

  .check(paste0(basename(f), " [on]"), problems)
}

# ================================================================= verdict ===

.n_total <- 2L * length(.files)
.say("")
.rule()
if (.n_fail == 0L) {
  .say("VERDICT: PASS -- ", .n_pass, " of ", .n_total, " checks.")
  .say("Raw values are bit-identical to the pre-refactor chain with smoothing ",
       "both off and on.")
} else {
  .say("VERDICT: FAIL -- ", .n_fail, " of ", .n_total, " checks failed:")
  for (nm in .fail_names) .say("  ", nm)
  .say("Do not commit. Paste this transcript back.")
}
.rule()

.logfile <- file.path(.OUT_DIR,
                      paste0("test_daily_series_",
                             format(Sys.time(), "%Y%m%d_%H%M%S"), ".txt"))
writeLines(.transcript, .logfile)
cat("\nTranscript written to", .logfile, "\n")
