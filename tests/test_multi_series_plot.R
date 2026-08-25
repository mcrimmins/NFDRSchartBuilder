# ==============================================================================
# tests/test_multi_series_plot.R
#
# PURPOSE
#   Assert the STRUCTURE of the Compare Variables chart, by building the plotly
#   object and reading its layout back -- not by looking at a picture.
#
#   This matters more here than on the other tabs. A dual-axis chart is only
#   defensible because of three specific mitigations (docs/MULTI_VARIABLE_SCOPE.md
#   section 3), and every one of them is a single line that looks like clutter to
#   anyone tidying up later:
#
#     1. axis title AND tick labels drawn in the series colour
#     2. gridlines from the LEFT axis only
#     3. nothing forcing the two scales to share zero or share breaks
#
#   Mitigation 3 is an ABSENCE, which is exactly the kind of thing that gets
#   "fixed" by a well-meaning addition. So this test asserts that rangemode,
#   scaleanchor, matches, tickvals and dtick are all unset on both y-axes. If
#   someone adds rangemode = "tozero" to make the chart look tidier, this fails
#   and says why.
#
#   None of this can be seen by eye: two charts, one honest and one that quietly
#   aligns its scales, look equally plausible.
#
#   Reads tests/logs/baseline_rawdata.rds. No network, no Shiny session --
#   plot_multi_series() is a plain function precisely so this is possible.
#
# HOW TO RUN
#   With NFDRSChartBuilder.Rproj open, from the project root:
#     source("tests/test_multi_series_plot.R")
#   Seconds.
#
# Helpers are dot-prefixed per CLAUDE.md so they cannot mask a shiny export.
# ==============================================================================

library(dplyr)
library(plotly)

source("R/daily_series.R")
source("R/multi_series.R")
source("R/multi_series_plot.R")

.OUT_DIR <- "tests/logs"
.RAW_RDS <- file.path(.OUT_DIR, "baseline_rawdata.rds")

.VAR_A <- "energyReleaseComponent"
.VAR_B <- "relativeHumidity"
.STAT  <- "max"

# Layout keys that must NEVER appear on either y-axis. Each one makes the two
# scales look commensurate, which is the whole problem.
.FORBIDDEN <- c("rangemode", "scaleanchor", "matches", "tickvals", "dtick")

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

.or <- function(a, b) if (is.null(a)) b else a

.traces <- function(b) {
  do.call(rbind, lapply(b$x$data, function(tr) data.frame(
    name    = .or(tr$name, NA_character_),
    yaxis   = .or(tr$yaxis, "y"),
    color   = .or(tr$line$color, NA_character_),
    dash    = .or(tr$line$dash, ""),
    opacity = .or(tr$opacity, 1),
    stringsAsFactors = FALSE)))
}

.say("")
.rule()
.say("MULTI SERIES PLOT STRUCTURE  --  ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
.rule()

if (!file.exists(.RAW_RDS)) stop("No frozen raw data at ", .RAW_RDS)
if (!exists("plot_multi_series")) stop("plot_multi_series() not found -- is R/multi_series_plot.R present?")

.raw <- readRDS(.RAW_RDS)
.dat <- .raw$all_data %>% filter(as.character(station_id) == .raw$stations[1])

.ms  <- build_multi_series(.dat, c(.VAR_A, .VAR_B), .STAT, c(1, 12))
.yr  <- max(.ms$year)

.say("Frozen input : ", .RAW_RDS)
.say("  station    : ", .raw$stations[1])
.say("  variables  : ", .VAR_A, " (slot a, ", MULTI_SERIES_COLORS[["a"]], ")",
     " + ", .VAR_B, " (slot b, ", MULTI_SERIES_COLORS[["b"]], ")")
.say("  plot year  : ", .yr, "   record types: ",
     paste(sort(unique(.ms$record_type[.ms$year == .yr])), collapse = ", "))

# ================================================ two variables, plain =======

.say("")
.rule()
.say("TWO VARIABLES -- traces land on the right axes in the right colours")
.rule()

.p <- plot_multi_series(.ms, c(.VAR_A, .VAR_B), .yr, .STAT)
.b <- plotly::plotly_build(.p)
.tr <- .traces(.b)

.say("")
for (i in seq_len(nrow(.tr))) {
  .say(sprintf("  %-42s axis=%-3s colour=%-8s dash=%-5s opacity=%.2f",
               .tr$name[i], .tr$yaxis[i], .tr$color[i], .tr$dash[i], .tr$opacity[i]))
}

.problems <- character(0)
if (!inherits(.p, "plotly")) .problems <- c(.problems, "not a plotly object")

.a_rows <- .tr[grepl(.VAR_A, .tr$name, fixed = TRUE), , drop = FALSE]
.b_rows <- .tr[grepl(.VAR_B, .tr$name, fixed = TRUE), , drop = FALSE]

if (nrow(.a_rows) == 0L) .problems <- c(.problems, "no trace for slot a")
if (nrow(.b_rows) == 0L) .problems <- c(.problems, "no trace for slot b")
if (!all(.a_rows$yaxis == "y"))  .problems <- c(.problems, "a slot-a trace is not on the left axis")
if (!all(.b_rows$yaxis == "y2")) .problems <- c(.problems, "a slot-b trace is not on the right axis")
if (!all(.a_rows$color == MULTI_SERIES_COLORS[["a"]]))
  .problems <- c(.problems, "a slot-a trace is not in the slot-a colour")
if (!all(.b_rows$color == MULTI_SERIES_COLORS[["b"]]))
  .problems <- c(.problems, "a slot-b trace is not in the slot-b colour")

.fc <- .tr[grepl("forecast", .tr$name, fixed = TRUE), , drop = FALSE]
.say("")
.say("  forecast traces: ", nrow(.fc),
     if (nrow(.fc) > 0) paste0(" (dash = ", paste(unique(.fc$dash), collapse = "/"), ")") else "")
if (nrow(.fc) > 0 && !all(.fc$dash == "dash")) {
  .problems <- c(.problems, "a forecast trace is not dashed -- observed and forecast would be indistinguishable")
}
if (nrow(.fc) > 0 && !all(.fc$color %in% unname(MULTI_SERIES_COLORS))) {
  .problems <- c(.problems, "a forecast trace has its own hue -- colour must mean variable, dash must mean forecast")
}
.check("two variables", .problems)

# ==================================================== the three mitigations ==

.say("")
.rule()
.say("MITIGATIONS -- the three things that make a dual axis defensible")
.rule()

.lay <- .b$x$layout
.problems <- character(0)

.say("")
.say("  1. axis titles and ticks carry the series colour")
.say("     yaxis  title=", .or(.lay$yaxis$title$font$color, "UNSET"),
     "  ticks=", .or(.lay$yaxis$tickfont$color, "UNSET"))
.say("     yaxis2 title=", .or(.lay$yaxis2$title$font$color, "UNSET"),
     "  ticks=", .or(.lay$yaxis2$tickfont$color, "UNSET"))
if (!identical(.or(.lay$yaxis$title$font$color, ""), MULTI_SERIES_COLORS[["a"]]))
  .problems <- c(.problems, "left axis title is not in the slot-a colour")
if (!identical(.or(.lay$yaxis$tickfont$color, ""), MULTI_SERIES_COLORS[["a"]]))
  .problems <- c(.problems, "left axis ticks are not in the slot-a colour")
if (!identical(.or(.lay$yaxis2$title$font$color, ""), MULTI_SERIES_COLORS[["b"]]))
  .problems <- c(.problems, "right axis title is not in the slot-b colour")
if (!identical(.or(.lay$yaxis2$tickfont$color, ""), MULTI_SERIES_COLORS[["b"]]))
  .problems <- c(.problems, "right axis ticks are not in the slot-b colour")

.say("")
.say("  2. gridlines from one axis only")
.say("     yaxis showgrid=", .or(.lay$yaxis$showgrid, "UNSET"),
     "   yaxis2 showgrid=", .or(.lay$yaxis2$showgrid, "UNSET"))
if (!isTRUE(.lay$yaxis$showgrid))
  .problems <- c(.problems, "left axis has no gridlines -- the chart has no grid at all")
if (!identical(.lay$yaxis2$showgrid, FALSE))
  .problems <- c(.problems,
                 "right axis draws gridlines -- two grids imply the two scales are aligned, which they are not")

.say("")
.say("  3. nothing forces the scales to share zero or share breaks")
for (ax in c("yaxis", "yaxis2")) {
  present <- .FORBIDDEN[vapply(.FORBIDDEN, function(k) !is.null(.lay[[ax]][[k]]), logical(1))]
  .say("     ", ax, ": ",
       if (length(present) == 0) "none of [" else "SET -> [",
       paste(.FORBIDDEN, collapse = ", "), "]",
       if (length(present) == 0) " are set" else paste0("  <- ", paste(present, collapse = ", ")))
  if (length(present) > 0) {
    .problems <- c(.problems,
                   paste0(ax, " sets ", paste(present, collapse = ", "),
                          " -- this makes the two scales look commensurate and re-creates the ",
                          "false-correlation problem the design is built to avoid"))
  }
}

.say("")
.say("  3b. the x axis never shows month_day's synthetic year")
.say("     xaxis tickformat=", .or(.lay$xaxis$tickformat, "UNSET"),
     "   dtick=", .or(.lay$xaxis$dtick, "UNSET"))
if (!identical(.or(.lay$xaxis$tickformat, ""), "%b")) {
  .problems <- c(.problems,
                 paste0("xaxis tickformat is '", .or(.lay$xaxis$tickformat, "UNSET"),
                        "', not '%b' -- month_day carries a synthetic 2024, so the axis ",
                        "will label a 2026 chart as 2024"))
}

.say("")
.say("  4. unified hover (reading beats projecting)")
.say("     hovermode=", .or(.lay$hovermode, "UNSET"))
if (!identical(.or(.lay$hovermode, ""), "x unified"))
  .problems <- c(.problems, "hovermode is not \"x unified\" -- readers must project onto an arbitrary axis")

.check("mitigations", .problems)

# ================================================== title ===================

.say("")
.rule()
.say("TITLE -- plotly has no subtitle argument, so it is one string")
.rule()

.b_t <- plotly::plotly_build(
  plot_multi_series(.ms, c(.VAR_A, .VAR_B), .yr, .STAT,
                    title = "Alpha vs Beta", subtitle = "STATION  |  2026"))
.tt <- .or(.b_t$x$layout$title$text, "")

.say("")
.say("  title text: ", .tt)
.say("  align: x=", .or(.b_t$x$layout$title$x, "UNSET"),
     " xanchor=", .or(.b_t$x$layout$title$xanchor, "UNSET"))
.say("  top margin: ", .or(.b_t$x$layout$margin$t, "UNSET"))

.problems <- character(0)
if (!grepl("Alpha vs Beta", .tt, fixed = TRUE))
  .problems <- c(.problems, "the title text is missing")
if (!grepl("STATION  |  2026", .tt, fixed = TRUE))
  .problems <- c(.problems, "the subtitle text is missing")
if (!grepl("<sup>", .tt, fixed = TRUE))
  .problems <- c(.problems, "the subtitle is not marked up as <sup> -- it will render at title size")
if (!identical(.or(.b_t$x$layout$title$xanchor, ""), "left"))
  .problems <- c(.problems, "the title is not left-aligned like the Static tab's")
if (is.null(.b_t$x$layout$margin$t) || .b_t$x$layout$margin$t < 80)
  .problems <- c(.problems, "top margin was not grown -- the title will overlap the plotting area")

# And no title block at all when neither is supplied, so the function stays
# usable headless without a stray empty title reserving space.
.b_nt <- plotly::plotly_build(plot_multi_series(.ms, c(.VAR_A, .VAR_B), .yr, .STAT))
.say("  with no title supplied, layout$title present: ", !is.null(.b_nt$x$layout$title))
if (!is.null(.b_nt$x$layout$title))
  .problems <- c(.problems, "a title block was added when none was asked for")

.check("title", .problems)

# ================================================== smoothing ================

.say("")
.rule()
.say("SMOOTHING -- bold smoothed line over a faded raw one, per variable")
.rule()

.ms_sm <- build_multi_series(.dat, c(.VAR_A, .VAR_B), .STAT, c(1, 12),
                             smooth = TRUE, smooth_window = 7)
.b_sm  <- plotly::plotly_build(
  plot_multi_series(.ms_sm, c(.VAR_A, .VAR_B), .yr, .STAT,
                    smooth = TRUE, smooth_spec = "7-day centered mean"))
.tr_sm <- .traces(.b_sm)

.say("")
for (i in seq_len(nrow(.tr_sm))) {
  .say(sprintf("  %-42s axis=%-3s colour=%-8s dash=%-5s opacity=%.2f",
               .tr_sm$name[i], .tr_sm$yaxis[i], .tr_sm$color[i], .tr_sm$dash[i], .tr_sm$opacity[i]))
}

.problems <- character(0)
.faded <- .tr_sm[.tr_sm$opacity < 1, , drop = FALSE]
.say("")
.say("  faded raw traces: ", nrow(.faded), " (expected one per variable)")
if (nrow(.faded) != 2L) {
  .problems <- c(.problems, paste0("expected 2 faded raw traces, got ", nrow(.faded)))
}
if (nrow(.faded) > 0 && !all(.faded$color %in% unname(MULTI_SERIES_COLORS))) {
  .problems <- c(.problems,
                 "a faded raw trace is not in its series colour -- with two variables on screen, ",
                 "two grey ghosts cannot be told apart")
}
if (nrow(.tr_sm) <= nrow(.tr)) {
  .problems <- c(.problems, "smoothing added no traces")
}
.check("smoothing", .problems)

# ================================================== forecast toggle ==========

.say("")
.rule()
.say("FORECAST TOGGLE")
.rule()

.tr_nf <- .traces(plotly::plotly_build(
  plot_multi_series(.ms, c(.VAR_A, .VAR_B), .yr, .STAT, show_forecast = FALSE)))
.n_fc <- sum(grepl("forecast", .tr_nf$name, fixed = TRUE))
.say("")
.say("  show_forecast = FALSE -> forecast traces: ", .n_fc, " (expected 0)")
.check("forecast toggle",
       if (.n_fc == 0L) character(0)
       else paste0(.n_fc, " forecast traces drawn with show_forecast = FALSE"))

# ================================================== one variable ============

.say("")
.rule()
.say("ONE VARIABLE -- no second axis at all")
.rule()

.b1 <- plotly::plotly_build(plot_multi_series(.ms, .VAR_A, .yr, .STAT))
.say("")
.say("  yaxis2 in layout: ", !is.null(.b1$x$layout$yaxis2), " (expected FALSE)")
.check("one variable",
       if (is.null(.b1$x$layout$yaxis2)) character(0)
       else "a second axis was drawn for a single series")

# ================================================== duplicate ===============

.say("")
.rule()
.say("SAME VARIABLE TWICE -- collapses to one series on one axis")
.rule()

.b_dup <- plotly::plotly_build(plot_multi_series(.ms, c(.VAR_A, .VAR_A), .yr, .STAT))
.say("")
.say("  yaxis2 in layout: ", !is.null(.b_dup$x$layout$yaxis2), " (expected FALSE)")
.check("duplicate variable",
       if (is.null(.b_dup$x$layout$yaxis2)) character(0)
       else "two identical series were drawn against two independently scaled axes")

# ================================================== three variables =========

.say("")
.rule()
.say("THREE VARIABLES -- the renderer refuses, the data layer does not")
.rule()

.msg <- tryCatch({
  plot_multi_series(.ms, c(.VAR_A, .VAR_B, "vpd"), .yr, .STAT); NULL
}, error = function(e) conditionMessage(e))

.say("")
if (is.null(.msg)) {
  .check("three variables", "drew three variables instead of refusing")
} else {
  .say("  error: ", .msg)
  .check("three variables",
         if (grepl("two", .msg, fixed = TRUE)) character(0)
         else "error message does not explain the two-variable cap")
}

# ================================================================= verdict ===

.n_total <- .n_pass + .n_fail
.say("")
.rule()
if (.n_fail == 0L) {
  .say("VERDICT: PASS -- ", .n_pass, " of ", .n_total, " checks.")
  .say("Traces land on the correct axes in the correct colours, and all three ",
       "dual-axis mitigations are in place -- including the one that is an ",
       "absence rather than a setting.")
} else {
  .say("VERDICT: FAIL -- ", .n_fail, " of ", .n_total, " checks failed:")
  for (nm in .fail_names) .say("  ", nm)
  .say("Do not commit. Paste this transcript back.")
}
.rule()

.logfile <- file.path(.OUT_DIR,
                      paste0("test_multi_series_plot_",
                             format(Sys.time(), "%Y%m%d_%H%M%S"), ".txt"))
writeLines(.transcript, .logfile)
cat("\nTranscript written to", .logfile, "\n")
