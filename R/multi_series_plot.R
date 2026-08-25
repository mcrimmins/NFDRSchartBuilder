# ==============================================================================
# R/multi_series_plot.R
#
# Presentation layer for the "Compare Variables" tab: the two series colours,
# and (from the renderer step onward) plot_multi_series() itself.
#
# Split out from app.R for the same reason build_daily_series() is a plain
# function rather than a reactive -- anything buried inside renderPlotly()
# cannot be called from tests/, so it can never be checked without launching
# the app. The colours live here rather than in app.R because the sidebar
# swatches and the plot axes MUST agree, and the only way to guarantee that is
# for both to read the same constant.
#
# shiny is namespace-qualified per CLAUDE.md: R/*.R must never attach a package,
# because a sourced file lands ahead of shiny on the search path and can mask
# its exports.
# ==============================================================================

# ==============================================================================
# The two series colours
# ==============================================================================
# These are not decorative. On a dual-axis chart the colour IS the mapping from
# a line to the axis it should be read against, so the same two values colour
# the line, the axis title and the axis tick labels
# (docs/MULTI_VARIABLE_SCOPE.md section 3, first mitigation).
#
# Because they colour tick LABELS -- small text -- they have to clear WCAG AA
# for normal text against the white page, not merely the 3:1 that a chart mark
# would need. Measured against #FFFFFF:
#
#   #0072B2  5.19:1   slot A, left axis
#   #B84A00  5.23:1   slot B, right axis
#
# The pair is derived from the Okabe-Ito colourblind-safe set; the orange is
# darkened from Okabe-Ito's #D55E00 (3.87:1, which fails AA for small text)
# until it clears 4.5:1. Separation survives the darkening: worst-case CVD
# separation is dE 22.2 under protanopia and 28.7 under tritanopia, against a
# target of 8, and 28.5 for normal vision.
#
# If either value changes, re-check BOTH properties -- contrast against white
# and CVD separation from the other slot. A colour that only looks different on
# your monitor is not a mapping.
#
# Colour is never the only cue: each dropdown is labelled "(left axis)" or
# "(right axis)" in text, and the swatch below is aria-hidden so a screen
# reader gets the label rather than a decorative square.
MULTI_SERIES_COLORS <- c(a = "#0072B2", b = "#B84A00")

# Axis side each slot owns. Kept beside the colours so the two cannot drift.
MULTI_SERIES_SIDES <- c(a = "left", b = "right")

# ==============================================================================
# multi_series_swatch_label
# ==============================================================================
# A selectInput label carrying a small colour chip, so the sidebar control and
# the axis it drives are visibly the same thing.
#
#   text   the label, which must name the axis in words as well
#   color  one of MULTI_SERIES_COLORS
multi_series_swatch_label <- function(text, color) {
  shiny::tagList(
    shiny::tags$span(
      style = paste0("display:inline-block; width:0.75em; height:0.75em; ",
                     "margin-right:0.45em; border-radius:2px; ",
                     "vertical-align:baseline; background-color:", color, ";"),
      `aria-hidden` = "true"
    ),
    text
  )
}
