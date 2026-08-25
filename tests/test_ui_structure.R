# ==============================================================================
# tests/test_ui_structure.R
#
# PURPOSE
#   Assert the SHAPE of the app's UI without launching it: that app.R parses,
#   that the main tabsetPanel carries the id the conditional sidebar keys off,
#   and that the tabs are the ones expected, in the expected order.
#
#   This exists because the alternative way to check "did the new tab appear in
#   the right place, and did anything else move" is to run the app and look --
#   which is slow, is not a record, and cannot tell you that tab four is still
#   tab four. Here it is a diffable list in a log file.
#
# HOW IT WORKS
#   app.R is PARSED, not sourced, and only the `ui <- fluidPage(...)`
#   expression is evaluated. Sourcing would also define the server function and
#   read data/station_metadata_*.csv; none of that is needed to inspect the UI,
#   and skipping it means this test runs anywhere in about a second.
#
#   The evaluation happens in an environment where the widget functions that
#   come from other packages are available if those packages are installed, and
#   replaced with harmless stubs if they are not. A stub changes nothing this
#   test looks at -- tab titles and the tabset id come from shiny itself.
#
# HOW TO RUN
#   With NFDRSChartBuilder.Rproj open, from the project root:
#     source("tests/test_ui_structure.R")
#
# WHEN THIS FAILS
#   Either a tab was added, removed or reordered -- update .EXPECT_TABS below
#   and say so in the commit -- or something upstream of the tabsetPanel broke
#   the UI expression, in which case the parse or eval check fails first and
#   names the error.
#
# Helpers are dot-prefixed per CLAUDE.md so they cannot mask a shiny export.
# ==============================================================================

library(shiny)

# Sourced for MULTI_SERIES_COLORS / MULTI_SERIES_SIDES, which the palette check
# below asserts against. Pure constants and one tag helper -- no side effects.
source("R/multi_series_plot.R")

.OUT_DIR <- "tests/logs"

# The contract. Order matters: these are the tabs, left to right.
.EXPECT_TABS <- c("Static Plot",
                  "Interactive Plot",
                  "Compare Variables",
                  "Summary Stats",
                  "About",
                  "Submit Feedback")

# The id the sidebar's conditionalPanel keys off. If this ever changes, the
# conditional controls silently stop appearing -- there is no error, the panel
# just never shows. Hence a test.
.EXPECT_TABSET_ID <- "main_tabs"

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

.say("")
.rule()
.say("UI STRUCTURE  --  ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
.rule()

# ------------------------------------------------------------- parse app.R ---

.say("")
.say("app.R parses")
.exprs <- tryCatch(parse("app.R"),
                   error = function(e) { .say("  ERROR: ", conditionMessage(e)); NULL })
if (is.null(.exprs)) {
  .check("parse", "app.R does not parse -- nothing further can be checked")
  stop("app.R does not parse; see the transcript above.")
}
.say("  ", length(.exprs), " top-level expressions")
.check("parse", character(0))

# ------------------------------------------------- locate and build the ui ---

.say("")
.say("ui assignment is present and evaluates")

.is_ui <- vapply(as.list(.exprs), function(e) {
  is.call(e) && length(e) >= 2L &&
    identical(as.character(e[[1]]), "<-") &&
    identical(as.character(e[[2]]), "ui")
}, logical(1))

.problems <- character(0)
.ui <- NULL

if (sum(.is_ui) != 1L) {
  .problems <- paste0("expected exactly one top-level `ui <- ...` assignment, found ",
                      sum(.is_ui))
} else {
  .say("  found at top-level expression ", which(.is_ui))

  # Attach what we can; stub what we cannot. Stubs affect nothing this test
  # inspects, but they let the check run on a machine that lacks a mapping or
  # plotting package.
  .env <- new.env(parent = as.environment("package:shiny"))
  .stub <- function(...) shiny::tags$div(class = "stub")
  .stubbed <- character(0)
  for (.pkg_fn in list(c("leaflet", "leafletOutput"),
                       c("plotly",  "plotlyOutput"),
                       c("DT",      "DTOutput"),
                       c("bslib",   "bs_theme"))) {
    if (!requireNamespace(.pkg_fn[1], quietly = TRUE)) {
      assign(.pkg_fn[2], .stub, envir = .env)
      .stubbed <- c(.stubbed, .pkg_fn[2])
    } else {
      assign(.pkg_fn[2],
             get(.pkg_fn[2], envir = asNamespace(.pkg_fn[1])), envir = .env)
    }
  }
  .say("  stubbed (package not installed): ",
       if (length(.stubbed)) paste(.stubbed, collapse = ", ") else "none")

  .ui <- tryCatch({ eval(.exprs[[which(.is_ui)]], envir = .env); get("ui", envir = .env) },
                  error = function(e) { .say("  ERROR: ", conditionMessage(e)); NULL })

  if (is.null(.ui)) {
    .problems <- "the ui expression did not evaluate"
  } else {
    .say("  ui object: ", paste(class(.ui), collapse = "/"))
  }
}
.check("ui builds", .problems)

if (is.null(.ui)) {
  .say("")
  .say("VERDICT: FAIL -- the ui could not be built, so tabs were not checked.")
  stop("ui did not build; see the transcript above.")
}

.html <- as.character(.ui)

# ------------------------------------------------------------ tabset id ------

.say("")
.say("main tabsetPanel carries id=\"", .EXPECT_TABSET_ID, "\"")
.has_id <- grepl(paste0('id="', .EXPECT_TABSET_ID, '"'), .html, fixed = TRUE)
.say("  present in rendered HTML: ", .has_id)
.check("tabset id",
       if (.has_id) character(0)
       else paste0("no element with id=\"", .EXPECT_TABSET_ID,
                   "\" -- the sidebar's conditionalPanel will never show"))

# ------------------------------------------------------------ tab titles ----
#
# Read the tab titles out of the TAG TREE, not out of the rendered HTML string.
#
# The first version of this test scraped data-value="..." out of
# as.character(ui) with gregexpr()/regmatches(). It passed on Linux and failed
# on Windows, returning shredded names like 'ata-value="Static Plot">'. app.R
# carries emoji in the sidebar headers and the About tab, and regmatches()
# slices by offsets that stop lining up with the string once multibyte
# characters are in it, so every match after the first emoji came back shifted
# by a character. The tab values were never wrong -- the extraction was.
#
# Walking the tree has no offsets to get wrong. Each tabPanel contributes a nav
# <a data-value="..."> and a pane <div data-value="...">, so restricting to <a>
# yields exactly one entry per tab, already in document order.

.collect_tab_values <- function(x, acc = character(0)) {
  if (inherits(x, "shiny.tag")) {
    v <- x$attribs[["data-value"]]
    if (!is.null(v) && identical(x$name, "a")) acc <- c(acc, as.character(v))
    acc <- .collect_tab_values(x$children, acc)
  } else if (is.list(x)) {
    for (el in x) acc <- .collect_tab_values(el, acc)
  }
  acc
}

# Fallback, in case a future Bootstrap version stops putting data-value on the
# nav anchor: take every data-value in tree order and de-duplicate.
.collect_any_tab_values <- function(x, acc = character(0)) {
  if (inherits(x, "shiny.tag")) {
    v <- x$attribs[["data-value"]]
    if (!is.null(v)) acc <- c(acc, as.character(v))
    acc <- .collect_any_tab_values(x$children, acc)
  } else if (is.list(x)) {
    for (el in x) acc <- .collect_any_tab_values(el, acc)
  }
  acc
}

.say("")
.say("tabs are as expected, in order")

.tabs <- .collect_tab_values(.ui)
.via  <- "nav anchors"
if (length(.tabs) == 0L) {
  .tabs <- unique(.collect_any_tab_values(.ui))
  .via  <- "all data-value attributes, de-duplicated"
}

.say("  read from the tag tree via ", .via)
.say("  found ", length(.tabs), ":")
for (i in seq_along(.tabs)) .say("    ", i, ". ", .tabs[i])

.problems <- character(0)
if (!identical(.tabs, .EXPECT_TABS)) {
  .problems <- c(.problems,
                 paste0("expected [", paste(.EXPECT_TABS, collapse = " | "), "]"),
                 paste0("     got [", paste(.tabs, collapse = " | "), "]"))
  for (m in setdiff(.EXPECT_TABS, .tabs)) .problems <- c(.problems, paste0("missing: ", m))
  for (m in setdiff(.tabs, .EXPECT_TABS)) .problems <- c(.problems, paste0("unexpected: ", m))
}
.check("tab titles", .problems)

# ------------------------------------------- sidebar variable controls -------
#
# The main "Select Variable" dropdown and the Compare Variables pair are
# mutually exclusive, gated by two conditionalPanels on input.main_tabs. This
# is worth a test for the same reason the tabset id is: a conditionalPanel
# whose condition never matches raises NO error. The control simply never
# appears, and the first symptom is a sidebar that looks empty.
#
# What this CANNOT check: the two selectInputs themselves are built server-
# side by renderUI, so a static pass over the ui object sees only the empty
# uiOutput divs. That the dropdowns are populated, defaulted and swatched
# correctly is a visual check, not this one.

.collect_attr <- function(x, attr, acc = character(0)) {
  if (inherits(x, "shiny.tag")) {
    v <- x$attribs[[attr]]
    if (!is.null(v)) acc <- c(acc, as.character(v))
    acc <- .collect_attr(x$children, attr, acc)
  } else if (is.list(x)) {
    for (el in x) acc <- .collect_attr(el, attr, acc)
  }
  acc
}

.EXPECT_CONDS <- c("input.main_tabs != 'Compare Variables'",
                   "input.main_tabs == 'Compare Variables'")
.EXPECT_OUTPUTS <- c("variable_selector", "multi_variable_selectors")

.say("")
.say("sidebar gates the variable controls on the active tab")

.conds <- .collect_attr(.ui, "data-display-if")
.conds <- .conds[grepl("main_tabs", .conds, fixed = TRUE)]
.say("  conditionalPanels keyed on main_tabs: ", length(.conds))
for (c_ in .conds) .say("    ", c_)

.problems <- character(0)
if (!identical(sort(.conds), sort(.EXPECT_CONDS))) {
  .problems <- c(.problems,
                 paste0("expected exactly [", paste(.EXPECT_CONDS, collapse = "] and ["),
                        "], got [", paste(.conds, collapse = "] ["), "]"))
}

.ids <- .collect_attr(.ui, "id")
for (o in .EXPECT_OUTPUTS) {
  present <- o %in% .ids
  .say("  uiOutput(\"", o, "\") present: ", present)
  if (!present) .problems <- c(.problems, paste0("no element with id=\"", o, "\""))
}
.check("sidebar gating", .problems)

# ------------------------------------------------------ series palette -------
#
# The two colours double as axis TITLE and TICK LABEL colours, which is small
# text, so they must clear WCAG AA for normal text (4.5:1) against the white
# page -- not the 3:1 a chart mark alone would need. They must also stay
# distinguishable from each other. Both properties are easy to break with a
# well-meaning tweak to a hex value, and neither failure is visible to whoever
# makes the tweak, so they are asserted here rather than trusted.

.rel_lum <- function(hex) {
  v  <- c(substr(hex, 2, 3), substr(hex, 4, 5), substr(hex, 6, 7))
  ch <- strtoi(v, 16L) / 255
  f  <- ifelse(ch <= 0.03928, ch / 12.92, ((ch + 0.055) / 1.055)^2.4)
  sum(f * c(0.2126, 0.7152, 0.0722))
}
.contrast <- function(hex, other = "#FFFFFF") {
  l <- sort(c(.rel_lum(hex), .rel_lum(other)), decreasing = TRUE)
  (l[1] + 0.05) / (l[2] + 0.05)
}

.say("")
.say("series palette clears AA for small text on white")

.problems <- character(0)
if (!exists("MULTI_SERIES_COLORS")) {
  .problems <- "MULTI_SERIES_COLORS not found -- is R/multi_series_plot.R sourced?"
} else {
  if (!identical(sort(names(MULTI_SERIES_COLORS)), c("a", "b"))) {
    .problems <- c(.problems, paste0("expected slots a and b, got [",
                                     paste(names(MULTI_SERIES_COLORS), collapse = ", "), "]"))
  }
  for (s in names(MULTI_SERIES_COLORS)) {
    hx <- MULTI_SERIES_COLORS[[s]]
    if (!grepl("^#[0-9A-Fa-f]{6}$", hx)) {
      .problems <- c(.problems, paste0("slot ", s, ": '", hx, "' is not a 6-digit hex colour"))
      next
    }
    cr <- .contrast(hx)
    .say("    slot ", s, " ", hx, "  contrast vs white ", sprintf("%.2f:1", cr),
         "  (", MULTI_SERIES_SIDES[[s]], " axis)")
    if (cr < 4.5) {
      .problems <- c(.problems,
                     sprintf("slot %s (%s) is %.2f:1 against white -- below the 4.5:1 AA floor for the tick labels it colours",
                             s, hx, cr))
    }
  }
  if (length(unique(unname(MULTI_SERIES_COLORS))) < length(MULTI_SERIES_COLORS)) {
    .problems <- c(.problems, "the two slots share a colour -- the axis mapping would be unreadable")
  }
}
.check("series palette", .problems)

# --------------------------------------------- the new tab has its content ---

# Was a check for the step-2 placeholder copy. Now that the renderer is wired
# in, it checks for the plot output itself -- and that the placeholder is GONE,
# so a half-reverted edit cannot leave both in the tab. The note above the plot
# is rendered server-side, and only when both slots match, so its content
# cannot be seen from here; only that its uiOutput exists.
.say("")
.say("Compare Variables tab holds the plot output")
.plot_present <- "multi_series_plot" %in% .ids
.note_present <- "multi_same_note"   %in% .ids
.say("  plotlyOutput(\"multi_series_plot\") present: ", .plot_present)
.say("  uiOutput(\"multi_same_note\") present: ", .note_present)
.problems <- character(0)
if (!.plot_present) .problems <- c(.problems, "the Compare Variables tab has no plot output")
if (!.note_present) .problems <- c(.problems, "the same-variable note output is missing")
if (grepl("Under construction", .html, fixed = TRUE)) {
  .problems <- c(.problems, "the step-2 placeholder copy is still in the tab")
}
.check("tab content", .problems)

# ================================================================= verdict ===

.n_total <- .n_pass + .n_fail
.say("")
.rule()
if (.n_fail == 0L) {
  .say("VERDICT: PASS -- ", .n_pass, " of ", .n_total, " checks.")
  .say("The UI builds, the tabset is addressable as \"", .EXPECT_TABSET_ID,
       "\", and the ", length(.EXPECT_TABS), " tabs are in the expected order.")
} else {
  .say("VERDICT: FAIL -- ", .n_fail, " of ", .n_total, " checks failed:")
  for (nm in .fail_names) .say("  ", nm)
  .say("Do not commit. Paste this transcript back.")
}
.rule()

.logfile <- file.path(.OUT_DIR,
                      paste0("test_ui_structure_",
                             format(Sys.time(), "%Y%m%d_%H%M%S"), ".txt"))
writeLines(.transcript, .logfile)
cat("\nTranscript written to", .logfile, "\n")
