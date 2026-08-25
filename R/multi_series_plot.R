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

# ==============================================================================
# multi_series_axis_title
# ==============================================================================
# Mirrors the y-label rule the other two tabs use: the computed variables carry
# their statistic in their own name ("Daily Total Precipitation"), so appending
# the Daily Statistic to them would read as nonsense ("... (mean)" on a total).
multi_series_axis_title <- function(variable, daily_stat, label_fn = function(v) v) {
  if (variable %in% c("precip_total", "burn_period", "precip_cum")) {
    label_fn(variable)
  } else {
    paste0(label_fn(variable), " (", daily_stat, ")")
  }
}

# ==============================================================================
# plot_multi_series
# ==============================================================================
# Two variables for one year, each against its own y-axis.
#
# A PLAIN FUNCTION returning a plotly object, not a renderPlotly() body, for the
# same reason build_daily_series() is a plain function: anything inside
# renderPlotly() needs a live Shiny session to run, so it could never be tested.
# tests/test_multi_series_plot.R builds the object and inspects its layout.
#
# NATIVE plotly, not ggplotly(). Every other plot in this app is a ggplot piped
# through ggplotly(), but ggplot2 supports at most one secondary axis and only
# as a FIXED TRANSFORM of the primary -- which is precisely the thing section 3
# of the scope doc forbids. There is no way to build this chart with ggplot2.
#
# ------------------------------------------------------------------------------
# THE THREE MITIGATIONS -- do not "tidy" these away
# ------------------------------------------------------------------------------
# Two y-scales on one plot is the most commonly cited chart mistake, because the
# alignment between the scales is arbitrary: slide one axis and the apparent
# relationship between the lines changes. This chart is defensible only with all
# three of the following in place, and each is one line that looks removable.
#
#   1. The axis title AND tick labels are drawn in the series colour. The colour
#      is the mapping from a line to the scale it should be read against. Take it
#      away and the reader has to guess.
#   2. Gridlines come from the LEFT axis only. Two sets of gridlines implies the
#      two scales are aligned to each other. They are not.
#   3. Nothing forces the scales to share zero or share breaks. There is
#      deliberately no rangemode = "tozero", no scaleanchor, no matches, and no
#      hand-set tick spacing. Anything that makes the two axes LOOK commensurate
#      re-creates the false-correlation problem that unified hover is here to
#      solve.
#
# Unified hover is the fourth leg: it gives exact values for both series at a
# date, so the chart is read rather than projected onto an arbitrary axis.
#
# ------------------------------------------------------------------------------
#   daily          output of build_multi_series() -- already station-averaged
#                  and cropped, one row per variable/month_day/year/record_type
#   variables      one or two variable names. De-duplicated: the same variable
#                  twice is a single series on a single axis, by design.
#   plot_year      the year to draw
#   daily_stat     used only to build the axis titles
#   show_forecast  draw the record_type == "F" segment
#   smooth         draw the smoothed line bold with the raw series behind it
#   smooth_spec    human-readable filter description for the x-axis label
#   label_fn       variable name -> display name; app.R passes
#                  pretty_variable_name(). Injected rather than called directly
#                  so this file has no dependency on app.R.
plot_multi_series <- function(daily, variables, plot_year, daily_stat,
                              show_forecast = TRUE,
                              smooth        = FALSE,
                              smooth_spec   = NULL,
                              label_fn      = function(v) v,
                              colors        = MULTI_SERIES_COLORS,
                              watermark     = TRUE) {

  variables <- unique(as.character(variables))

  if (length(variables) < 1L) {
    stop("plot_multi_series(): need at least one variable.")
  }
  # The DATA layer takes any N; this renderer is the part that caps at two.
  # A third series needs a third scale, and three arbitrary scales on one plot
  # is past defending -- that is the percentile-overlay renderer's job.
  if (length(variables) > 2L) {
    stop("plot_multi_series(): this renderer draws at most two variables, got ",
         length(variables), " (", paste(variables, collapse = ", "), "). ",
         "build_multi_series() accepts any number; the dual-axis renderer does not.")
  }

  slots  <- c("a", "b")[seq_along(variables)]
  yr     <- as.integer(plot_year)
  single <- length(variables) == 1L

  p <- plotly::plot_ly()

  for (i in seq_along(variables)) {

    v    <- variables[i]
    col  <- colors[[slots[i]]]
    yref <- if (i == 1L) "y" else "y2"
    nm   <- multi_series_axis_title(v, daily_stat, label_fn)

    rows <- daily[daily$variable == v & daily$year == yr, , drop = FALSE]
    obs  <- rows[rows$record_type == "O", , drop = FALSE]
    fcst <- rows[rows$record_type == "F", , drop = FALSE]
    obs  <- obs[order(obs$month_day), , drop = FALSE]
    fcst <- fcst[order(fcst$month_day), , drop = FALSE]

    if (nrow(obs) > 0L) {
      if (isTRUE(smooth)) {
        # Raw series stays in the series colour rather than going grey: with two
        # variables on screen, two grey ghosts would be indistinguishable from
        # each other. Opacity separates raw from smoothed instead.
        p <- plotly::add_trace(
          p, x = obs$month_day, y = obs$value, yaxis = yref,
          type = "scatter", mode = "lines",
          line = list(color = col, width = 1),
          opacity = 0.35, showlegend = FALSE,
          name = paste0(nm, " (daily)"),
          hovertemplate = paste0(nm, " daily: %{y:.2f}<extra></extra>"))

        sm <- obs[!is.na(obs$value_smooth), , drop = FALSE]
        if (nrow(sm) > 0L) {
          p <- plotly::add_trace(
            p, x = sm$month_day, y = sm$value_smooth, yaxis = yref,
            type = "scatter", mode = "lines",
            line = list(color = col, width = 2.5),
            name = nm,
            hovertemplate = paste0(nm, ": %{y:.2f}<extra></extra>"))
        }
      } else {
        p <- plotly::add_trace(
          p, x = obs$month_day, y = obs$value, yaxis = yref,
          type = "scatter", mode = "lines",
          line = list(color = col, width = 2),
          name = nm,
          hovertemplate = paste0(nm, ": %{y:.2f}<extra></extra>"))
      }
    }

    if (isTRUE(show_forecast) && nrow(fcst) > 0L) {
      # Forecast is the SAME colour with a dashed line: colour carries variable
      # identity, dash carries observed-vs-forecast. Giving forecast its own hue
      # would break the one mapping the dual axis depends on.
      p <- plotly::add_trace(
        p, x = fcst$month_day, y = fcst$value, yaxis = yref,
        type = "scatter", mode = "lines",
        line = list(color = col, width = 2, dash = "dash"),
        name = paste0(nm, " forecast"),
        hovertemplate = paste0(nm, " forecast: %{y:.2f}<extra></extra>"))
    }
  }

  x_title <- if (is.null(smooth_spec)) {
    "Month-Day"
  } else {
    paste0("Month-Day   (bold line: ", smooth_spec,
           "; faded line: unsmoothed daily values)")
  }

  # Mitigation 1 and 2 live here. Mitigation 3 is the ABSENCE of anything else.
  y1 <- list(
    title     = list(text = multi_series_axis_title(variables[1], daily_stat, label_fn),
                     font = list(color = colors[["a"]])),
    tickfont  = list(color = colors[["a"]]),
    showgrid  = TRUE,
    gridcolor = "#E9E9E9",
    zeroline  = FALSE
  )

  lay <- list(
    hovermode = "x unified",
    # tickformat = "%b" is NOT cosmetic. month_day carries a SYNTHETIC year --
    # build_daily_series() builds it as as.Date(format(date, "2024-%m-%d")) so
    # that every year can be overlaid on one axis. Left to format itself, the
    # axis reads "Jan 2024" on a chart of 2026 data: a confidently wrong year.
    # The ggplot tabs hide it with scale_x_date(date_labels = "%b"); this is the
    # native-plotly equivalent. dtick = "M1" matches their date_breaks.
    xaxis     = list(title = x_title, showgrid = TRUE, gridcolor = "#E9E9E9",
                     tickformat = "%b", dtick = "M1"),
    yaxis     = y1,
    legend    = list(orientation = "h", x = 0, y = -0.18),
    margin    = list(r = 80)
  )

  if (!single) {
    lay$yaxis2 <- list(
      title      = list(text = multi_series_axis_title(variables[2], daily_stat, label_fn),
                        font = list(color = colors[["b"]])),
      tickfont   = list(color = colors[["b"]]),
      overlaying = "y",
      side       = "right",
      showgrid   = FALSE,   # mitigation 2 -- one grid, not two
      zeroline   = FALSE
    )
  }

  if (isTRUE(watermark)) {
    lay$annotations <- list(list(
      x = 0.5, y = 0.5, text = "<b>EXPERIMENTAL</b>",
      xref = "paper", yref = "paper",
      xanchor = "center", yanchor = "middle", showarrow = FALSE,
      font = list(size = 80, color = "rgba(255, 0, 0, 0.15)")
    ))
  }

  do.call(plotly::layout, c(list(p), lay))
}
