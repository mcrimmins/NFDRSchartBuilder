# ==============================================================================
# tests/test_multi_series.R
#
# PURPOSE
#   Prove that build_multi_series() (R/multi_series.R) is a LOOP AND NOTHING
#   MORE. The claim the whole design rests on is that it adds no aggregation
#   logic of its own, so every per-variable slice of what it returns must be
#   BIT-IDENTICAL to a standalone build_daily_series() call for that variable,
#   with the same arguments. If that ever stops holding, multi_series.R has
#   grown logic it should not have and the Compare Variables tab can silently
#   disagree with the Static and Interactive tabs.
#
#   Bit-identity is a strictly stronger check than "the smoothing arguments
#   reach every variable" or "the column contract holds", so those come along
#   for free -- but they are asserted separately anyway, because when identity
#   fails it is much easier to read a log that says WHICH property broke.
#
#   Also checked here:
#     - the returned column contract, in order
#     - N = 1 and N = 3, so nothing assumes exactly two
#     - the same variable twice collapses to one block (scope doc section 8)
#     - an unavailable variable FAILS LOUDLY rather than returning a short frame
#
#   Reads tests/logs/baseline_rawdata.rds, so the input is byte-for-byte what
#   the smoothing work was verified against. No network.
#
# HOW TO RUN
#   With NFDRSChartBuilder.Rproj open, from the project root:
#     source("tests/test_multi_series.R")
#   Under a minute. It runs ~48 aggregations, so that every comparison is
#   against a freshly computed standalone result rather than a stored one.
#
# Helpers are dot-prefixed per CLAUDE.md so they cannot mask a shiny export.
# ==============================================================================

library(dplyr)

source("R/daily_series.R")
source("R/multi_series.R")

.OUT_DIR <- "tests/logs"
.RAW_RDS <- file.path(.OUT_DIR, "baseline_rawdata.rds")

.CONTRACT <- c("variable", "month_day", "year", "record_type",
               "value", "value_smooth")

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

# Compare one per-variable slice against its standalone result. identical() is
# the verdict; everything below it exists only to make a failure readable.
.identity_problems <- function(got, want, v) {

  if (identical(got, want)) return(character(0))

  problems <- character(0)

  if (!identical(dim(got), dim(want))) {
    return(paste0("[", v, "] dimensions differ: slice ", nrow(got), "x", ncol(got),
                  ", standalone ", nrow(want), "x", ncol(want)))
  }
  if (!identical(names(got), names(want))) {
    problems <- c(problems,
                  paste0("[", v, "] column names differ: slice [",
                         paste(names(got), collapse = ", "), "], standalone [",
                         paste(names(want), collapse = ", "), "]"))
  }

  for (cl in intersect(names(got), names(want))) {
    a <- got[[cl]]; b <- want[[cl]]
    if (identical(a, b)) next

    if (!identical(class(a), class(b))) {
      problems <- c(problems,
                    paste0("[", v, "] column '", cl, "' TYPE differs: ",
                           paste(class(a), collapse = "/"), " vs ",
                           paste(class(b), collapse = "/")))
      next
    }

    na_mismatch <- sum(xor(is.na(a), is.na(b)))
    both        <- !is.na(a) & !is.na(b)
    val_mismatch <- sum(a[both] != b[both])
    problems <- c(problems,
                  paste0("[", v, "] column '", cl, "' differs: ", val_mismatch,
                         " value mismatches, ", na_mismatch, " NA-pattern mismatches"))

    idx <- utils::head(which(xor(is.na(a), is.na(b)) |
                               (both & a != b)), 3)
    for (i in idx) {
      problems <- c(problems,
                    sprintf("    row %d: standalone %s -> slice %s",
                            i, format(b[i]), format(a[i])))
    }
  }

  # The case that would otherwise print an empty FAIL: every column compares
  # equal but the frames are still not identical, i.e. class or attribute drift
  # introduced by add_column()/bind_rows(). Worth naming precisely.
  if (length(problems) == 0) {
    .attrs <- function(x) paste(setdiff(names(attributes(x)),
                                        c("names", "row.names", "class")),
                                collapse = ",")
    problems <- paste0("[", v, "] identical() is FALSE but every column compares ",
                       "equal -- frame-level drift. class(slice)=[",
                       paste(class(got), collapse = "/"), "] class(standalone)=[",
                       paste(class(want), collapse = "/"), "] extra-attrs slice=[",
                       .attrs(got), "] standalone=[", .attrs(want), "]")
  }

  problems
}

# ------------------------------------------------------------------- setup ---

.say("")
.rule()
.say("MULTI SERIES COMPOSITION  --  ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
.rule()

if (!file.exists(.RAW_RDS)) {
  stop("No frozen raw data at ", .RAW_RDS,
       " -- tests/capture_baseline.R produces it.")
}
if (!exists("build_daily_series")) stop("build_daily_series() not found -- is R/daily_series.R present?")
if (!exists("build_multi_series")) stop("build_multi_series() not found -- is R/multi_series.R present?")

.raw       <- readRDS(.RAW_RDS)
.stations  <- .raw$stations
all_multi  <- .raw$all_data
all_single <- all_multi %>% filter(as.character(station_id) == .stations[1])

.say("Frozen input : ", .RAW_RDS)
.say("  captured   : ", format(.raw$captured, "%Y-%m-%d %H:%M:%S"))
.say("  stations   : ", paste(.stations, collapse = ", "))
.say("  raw rows   : ", nrow(all_multi), " (multi), ", nrow(all_single), " (single)")
.say("Smoothing    : ", .SM_WINDOW, "-day ", .SM_ALIGN, " ", .SM_FUN,
     " (phase 2 only)")
.say("Contract     : ", paste(.CONTRACT, collapse = ", "))

.cases <- list(
  list(label = "two vars, full year",              variables = c("energyReleaseComponent", "relativeHumidity"),
       daily_stat = "max",     month_range = c(1, 12), scope = "single"),
  list(label = "two vars, cropped, multi-station", variables = c("energyReleaseComponent", "relativeHumidity"),
       daily_stat = "max",     month_range = c(3, 6),  scope = "multi"),
  list(label = "both computed paths together",     variables = c("precip_cum", "burn_period"),
       daily_stat = "mean",    month_range = c(3, 6),  scope = "single"),
  list(label = "order reversed, 1300 LST",         variables = c("relativeHumidity", "energyReleaseComponent"),
       daily_stat = "1300LST", month_range = c(1, 12), scope = "single"),
  list(label = "N = 1",                            variables = c("precip_total"),
       daily_stat = "mean",    month_range = c(1, 12), scope = "single"),
  list(label = "N = 3",                            variables = c("energyReleaseComponent", "relativeHumidity", "precip_cum"),
       daily_stat = "mean",    month_range = c(4, 8),  scope = "single")
)

# Drop any case whose variables are not in the frozen frame, loudly, rather
# than failing halfway through with a confusing message.
.available <- function(v) multi_series_source_column(v) %in% names(all_multi)
.keep <- vapply(.cases, function(cs) all(vapply(cs$variables, .available, logical(1))),
                logical(1))
if (any(!.keep)) {
  for (cs in .cases[!.keep]) {
    .say("SKIPPED case '", cs$label, "': variable(s) not in the frozen data (",
         paste(cs$variables[!vapply(cs$variables, .available, logical(1))],
               collapse = ", "), ")")
  }
}
.cases <- .cases[.keep]
.say("Cases        : ", length(.cases), " x 2 smoothing settings")

# ===================================== phase 1+2: composition equivalence ====

for (.smooth in c(FALSE, TRUE)) {

  .say("")
  .rule()
  .say("PHASE ", if (.smooth) "2" else "1", " -- smoothing ", if (.smooth) "ON" else "OFF",
       ": every per-variable slice must be BIT-IDENTICAL")
  .rule()

  for (cs in .cases) {

    dat <- if (cs$scope == "multi") all_multi else all_single

    .say("")
    .say(cs$label)
    .say("  variables=", paste(cs$variables, collapse = " + "),
         "  stat=", cs$daily_stat,
         "  months=", cs$month_range[1], "-", cs$month_range[2],
         "  stations=", cs$scope)

    got <- tryCatch(
      build_multi_series(dat, cs$variables, cs$daily_stat, cs$month_range,
                         smooth = .smooth, smooth_fun = .SM_FUN,
                         smooth_window = .SM_WINDOW, smooth_align = .SM_ALIGN),
      error = function(e) { .say("  ERROR: ", conditionMessage(e)); NULL })

    if (is.null(got)) {
      .n_fail <- .n_fail + 1L
      .fail_names <- c(.fail_names, paste0(cs$label, if (.smooth) " [on]" else " [off]"))
      next
    }

    problems <- character(0)

    if (!identical(names(got), .CONTRACT)) {
      problems <- c(problems, paste0("column contract: expected [",
                                     paste(.CONTRACT, collapse = ", "), "] got [",
                                     paste(names(got), collapse = ", "), "]"))
    }
    if (!identical(unique(got$variable), cs$variables)) {
      problems <- c(problems, paste0("variable column: expected blocks in order [",
                                     paste(cs$variables, collapse = ", "), "] got [",
                                     paste(unique(got$variable), collapse = ", "), "]"))
    }

    total_standalone <- 0L
    for (v in cs$variables) {

      want <- build_daily_series(dat, v, cs$daily_stat, cs$month_range,
                                 smooth = .smooth, smooth_fun = .SM_FUN,
                                 smooth_window = .SM_WINDOW, smooth_align = .SM_ALIGN)
      total_standalone <- total_standalone + nrow(want)

      slice <- got %>% filter(variable == v) %>% select(-all_of("variable"))
      p <- .identity_problems(slice, want, v)

      if (length(p) == 0) {
        n_sm <- sum(!is.na(slice$value_smooth))
        .say("    ", v, ": BIT-IDENTICAL (", nrow(slice), " rows, ",
             n_sm, " smoothed)")
        # Smoothing reached this variable, not just the first one.
        if (.smooth && n_sm == 0) {
          problems <- c(problems, paste0("[", v, "] value_smooth is entirely NA ",
                                         "with smoothing ON -- arguments are not ",
                                         "reaching this variable"))
        }
        if (!.smooth && n_sm > 0) {
          problems <- c(problems, paste0("[", v, "] value_smooth has ", n_sm,
                                         " non-NA values with smoothing OFF"))
        }
      }
      problems <- c(problems, p)
    }

    if (nrow(got) != total_standalone) {
      problems <- c(problems, paste0("row count: ", nrow(got), " returned, ",
                                     total_standalone, " across standalone calls"))
    }

    .check(paste0(cs$label, if (.smooth) " [on]" else " [off]"), problems)
  }
}

# ================================================== duplicate collapse ======

.say("")
.rule()
.say("DUPLICATES -- the same variable twice must collapse to one block")
.rule()

.dup_var <- .cases[[1]]$variables[1]
.say("")
.say("build_multi_series(single, c('", .dup_var, "', '", .dup_var, "'), 'max', 1-12)")

.problems <- character(0)
.dup <- tryCatch(build_multi_series(all_single, c(.dup_var, .dup_var), "max", c(1, 12)),
                 error = function(e) { .say("  ERROR: ", conditionMessage(e)); NULL })
.one <- build_multi_series(all_single, .dup_var, "max", c(1, 12))

if (is.null(.dup)) {
  .problems <- "errored instead of collapsing"
} else {
  .say("  blocks returned: ", length(unique(.dup$variable)),
       " (", nrow(.dup), " rows); single-variable call: ",
       length(unique(.one$variable)), " (", nrow(.one), " rows)")
  if (!identical(.dup, .one)) {
    .problems <- c(.problems, "c(v, v) did not return exactly the same frame as v")
    .problems <- c(.problems, .identity_problems(.dup, .one, .dup_var))
  }
}
.check("duplicate collapse", .problems)

# ================================================== clean failure ===========

.say("")
.rule()
.say("BAD INPUT -- must fail loudly, never return a short frame")
.rule()

.expect_error <- function(label, expr, must_mention) {
  .say("")
  .say(label)
  msg <- tryCatch({ force(expr); NULL }, error = function(e) conditionMessage(e))
  problems <- character(0)
  if (is.null(msg)) {
    problems <- "returned without error -- a bad request produced data"
  } else {
    .say("  error: ", msg)
    for (m in must_mention) {
      if (!grepl(m, msg, fixed = TRUE)) {
        problems <- c(problems, paste0("message does not mention '", m, "'"))
      }
    }
  }
  .check(label, problems)
}

.expect_error("one good variable, one that does not exist",
              build_multi_series(all_single,
                                 c(.cases[[1]]$variables[1], "notAVariable"),
                                 "mean", c(1, 12)),
              c("notAVariable"))

.expect_error("every variable missing",
              build_multi_series(all_single, c("nopeOne", "nopeTwo"),
                                 "mean", c(1, 12)),
              c("nopeOne", "nopeTwo"))

.expect_error("variables is not a character vector",
              build_multi_series(all_single, 1:2, "mean", c(1, 12)),
              c("character"))

.expect_error("variables is empty",
              build_multi_series(all_single, character(0), "mean", c(1, 12)),
              c("non-empty"))

# ================================================================= verdict ===

.n_total <- .n_pass + .n_fail
.say("")
.rule()
if (.n_fail == 0L) {
  .say("VERDICT: PASS -- ", .n_pass, " of ", .n_total, " checks.")
  .say("build_multi_series() is a loop over build_daily_series() and nothing ",
       "more: every per-variable slice is bit-identical to a standalone call, ",
       "with smoothing both off and on.")
} else {
  .say("VERDICT: FAIL -- ", .n_fail, " of ", .n_total, " checks failed:")
  for (nm in .fail_names) .say("  ", nm)
  .say("Do not commit. Paste this transcript back.")
}
.rule()

.logfile <- file.path(.OUT_DIR,
                      paste0("test_multi_series_",
                             format(Sys.time(), "%Y%m%d_%H%M%S"), ".txt"))
writeLines(.transcript, .logfile)
cat("\nTranscript written to", .logfile, "\n")
