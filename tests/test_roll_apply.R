# ==============================================================================
# tests/test_roll_apply.R
#
# Unit tests for roll_apply() in R/daily_series.R.
#
# Pure arithmetic -- no data, no network, no API. Runs in well under a second.
# Every expected value below was worked out by hand, not read off the
# implementation, so a wrong implementation fails rather than agreeing with
# itself.
#
# HOW TO RUN
#   With NFDRSChartBuilder.Rproj open, from the project root:
#     source("tests/test_roll_apply.R")
#
# Helpers are dot-prefixed per CLAUDE.md so they cannot mask a shiny export.
# ==============================================================================

source("R/daily_series.R")

.transcript <- character(0)
.say <- function(...) {
  line <- paste0(...)
  .transcript <<- c(.transcript, line)
  cat(line, "\n", sep = "")
  invisible(NULL)
}
.rule <- function() .say(strrep("-", 78))

.n_pass <- 0L
.n_fail <- 0L

.fmt <- function(v) paste0("[", paste(ifelse(is.na(v), "NA", format(v, trim = TRUE)),
                                      collapse = ", "), "]")

.expect <- function(label, got, want) {
  ok <- isTRUE(all.equal(as.numeric(got), as.numeric(want), tolerance = 1e-12)) &&
        identical(is.na(got), is.na(want))
  if (ok) {
    .say("  PASS  ", label)
    .n_pass <<- .n_pass + 1L
  } else {
    .say("  FAIL  ", label)
    .say("        want ", .fmt(want))
    .say("        got  ", .fmt(got))
    .n_fail <<- .n_fail + 1L
  }
  invisible(ok)
}

.expect_error <- function(label, expr) {
  err <- tryCatch({ force(expr); NULL }, error = function(e) conditionMessage(e))
  if (!is.null(err)) {
    .say("  PASS  ", label, "  (", err, ")")
    .n_pass <<- .n_pass + 1L
  } else {
    .say("  FAIL  ", label, "  -- no error raised")
    .n_fail <<- .n_fail + 1L
  }
}

.say("")
.rule()
.say("ROLL_APPLY UNIT TESTS  --  ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
.rule()

# --- shape and the trivial cases ---------------------------------------------
.say("")
.say("basic shape")
.expect("constant series is unchanged in the interior",
        roll_apply(rep(5, 7), 3), c(NA, 5, 5, 5, 5, 5, NA))
.expect("n = 1 is the identity",
        roll_apply(1:10, 1), as.numeric(1:10))
.expect("n = 0 is the identity",
        roll_apply(1:10, 0), as.numeric(1:10))

# --- the ramp invariant -------------------------------------------------------
# A centered mean of a linear ramp returns the ramp. If the window is off by
# one, or lopsided, this is the test that notices.
.say("")
.say("centered mean of a linear ramp returns the ramp")
.expect("n = 3", roll_apply(1:10, 3), c(NA, 2:9, NA))
.expect("n = 5", roll_apply(1:10, 5), c(NA, NA, 3:8, NA, NA))
.expect("n = 7", roll_apply(1:10, 7), c(NA, NA, NA, 4:7, NA, NA, NA))

# --- alignment ---------------------------------------------------------------
.say("")
.say("alignment")
.expect("trailing n = 5 lags the ramp by two",
        roll_apply(1:10, 5, align = "right"), c(NA, NA, NA, NA, 3:8))
.expect("trailing n = 7 reaches the last day",
        roll_apply(1:10, 7, align = "right"), c(rep(NA, 6), 4:7))

.x <- c(3, 1, 4, 1, 5, 9, 2, 6, 5, 3)
.n <- 5L
.half <- (.n - 1L) %/% 2L
.ctr <- roll_apply(.x, .n)
.trl <- roll_apply(.x, .n, align = "right")
.expect("trailing[i] equals centered[i - half]",
        .trl[(.half + 1):length(.x)], .ctr[1:(length(.x) - .half)])

# --- edge padding -------------------------------------------------------------
.say("")
.say("edge padding -- no partial windows")
.e7c <- roll_apply(1:30, 7)
.e7r <- roll_apply(1:30, 7, align = "right")
.expect("centered n = 7 pads 3 at the head", sum(is.na(head(.e7c, 3))), 3)
.expect("centered n = 7 pads 3 at the tail", sum(is.na(tail(.e7c, 3))), 3)
.expect("centered n = 7 pads nothing else",  sum(is.na(.e7c)), 6)
.expect("trailing n = 7 pads 6 at the head", sum(is.na(head(.e7r, 6))), 6)
.expect("trailing n = 7 pads nothing at the tail", sum(is.na(tail(.e7r, 1))), 0)

.e31 <- roll_apply(1:100, 31)
.expect("centered n = 31 stops 15 days short of each end",
        c(sum(is.na(head(.e31, 15))), sum(is.na(tail(.e31, 15))), sum(is.na(.e31))),
        c(15, 15, 30))

# --- NA handling inside the window --------------------------------------------
.say("")
.say("NA handling inside the window (min_frac)")
.expect("the DEFAULT requires a complete window",
        roll_apply(c(1, 2, NA, 4, 5), 3), rep(NA_real_, 5))
.expect("the default tolerates no gap even in a wide window",
        sum(!is.na(roll_apply(c(1:10, NA, 12:20), 7))), 7)
.expect("min_frac 0.75 on n = 3 needs all three",
        roll_apply(c(1, 2, NA, 4, 5), 3, min_frac = 0.75), rep(NA_real_, 5))
.expect("min_frac 0.50 on n = 3 needs two",
        roll_apply(c(1, 2, NA, 4, 5), 3, min_frac = 0.5), c(NA, 1.5, 3, 4.5, NA))
.expect("an all-NA window yields NA",
        roll_apply(rep(NA_real_, 5), 3, min_frac = 0.5), rep(NA_real_, 5))
.expect("min_frac 1 requires a complete window",
        roll_apply(c(1, 2, NA, 4, 5), 3, min_frac = 1), rep(NA_real_, 5))

# --- the other two functions ---------------------------------------------------
.say("")
.say("sum and median")
.expect("rolling sum n = 3",
        roll_apply(1:5, 3, fun = "sum"), c(NA, 6, 9, 12, NA))
.expect("rolling median n = 3 ignores the spike",
        roll_apply(c(1, 2, 100, 4, 5), 3, fun = "median"), c(NA, 2, 4, 5, NA))
.expect("rolling median n = 5 on the same series",
        roll_apply(c(1, 2, 100, 4, 5), 5, fun = "median"), c(NA, NA, 4, NA, NA))

# --- guards --------------------------------------------------------------------
.say("")
.say("guards")
.expect_error("a centered even window is refused", roll_apply(1:10, 6))
.expect_error("an unknown function is refused", roll_apply(1:10, 3, fun = "geomean"))
.expect_error("an unknown alignment is refused", roll_apply(1:10, 3, align = "left"))

# --- verdict -------------------------------------------------------------------
.say("")
.rule()
if (.n_fail == 0L) {
  .say("VERDICT: PASS -- ", .n_pass, " of ", .n_pass, " assertions.")
} else {
  .say("VERDICT: FAIL -- ", .n_fail, " of ", .n_pass + .n_fail, " assertions failed.")
  .say("Do not wire this into the UI. Paste this transcript back.")
}
.rule()

dir.create("tests/logs", showWarnings = FALSE, recursive = TRUE)
.logfile <- file.path("tests/logs",
                      paste0("test_roll_apply_",
                             format(Sys.time(), "%Y%m%d_%H%M%S"), ".txt"))
writeLines(.transcript, .logfile)
cat("\nTranscript written to", .logfile, "\n")
