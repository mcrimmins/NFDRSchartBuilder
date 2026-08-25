# ==============================================================================
# R/daily_series.R
#
# The daily-aggregation chain, extracted from app.R, plus the rolling filter
# that feeds the smoothed overlay.
#
# Until now the chain existed as three verbatim copies -- in climatology_plot,
# plotly_climatology_plot, and the download_plot_data handler -- so any change
# to variable dispatch or filtering had to land in all three or the two plots
# and the CSV export would silently disagree. It now lives here once.
#
# build_daily_series() is a PLAIN FUNCTION, not a reactive, on purpose. A
# reactive cannot be called outside a Shiny session, so tests/ would have to
# carry its own copy of the chain and would end up testing itself rather than
# the app. app.R wraps this in a one-line reactive (daily_series).
#
# ------------------------------------------------------------------------------
# ORDERING CONTRACT -- read before changing the pipeline
# ------------------------------------------------------------------------------
# The month-range crop MUST be the LAST step. Everything that looks backwards or
# forwards along the calendar -- the precip_cum running total and the rolling
# smoother -- has to see the full uncropped year, or it starts from the crop
# boundary instead of January 1 and silently produces wrong numbers at the
# left-hand edge of the plot.
#
# Concretely, for a March-June selection:
#   crop first  : cumulative precip restarts at 0 on Mar 1, and a 7-day mean has
#                 no data before Mar 1 so the first three days are NA or, worse,
#                 computed from a truncated window.
#   crop last   : both carry in correctly from Jan 1, and the crop simply drops
#                 rows that were already computed right.
#
# The station average sits ahead of the crop for the same reason: the smoother
# operates on the multi-station average, so that average has to exist before the
# crop happens. This is value-neutral -- month(month_day) is by construction
# identical to month(date) -- and tests/test_daily_series.R proves it against
# the frozen pre-refactor baselines.
#
# dplyr verbs are left unqualified here, matching R/fems_download.R. app.R
# attaches dplyr well before sourcing this file. lubridate, stats and tibble
# are qualified.
# ==============================================================================

# Moved here from app.R (unchanged) so this file is self-contained and the
# test scripts do not have to duplicate it.
safe_summary1 <- function(x, fun) {
  if (all(is.na(x))) NA else fun(x, na.rm = TRUE)
}

# ==============================================================================
# roll_apply -- the rolling filter
# ==============================================================================
# A small base-R roller rather than zoo::rollapply or slider, so nothing new has
# to be installed on Posit Connect. stats::filter would cover mean and sum but
# not median, so one helper covers all three.
#
#   x          numeric vector, already in calendar order with no gaps
#   n          window width in days. 1 is the identity.
#   fun        "mean", "sum" or "median"
#   align      "center" (window straddles the day) or "right" (window ends on
#              the day -- the trailing/lagging option)
#   min_frac   fraction of the window that must be non-NA for a value to be
#              produced. See the note on NA handling below.
#
# EDGE BEHAVIOUR: the ends are padded with NA. There are no partial windows --
# a value is either computed from a full-width window or it is not computed at
# all. Consequences worth knowing:
#   - a centered window of n days stops (n-1)/2 days short of BOTH ends, so on
#     the current-year line the smoothed series ends short of today. A 31-day
#     centered mean ends 15 days back.
#   - a trailing window reaches today, at the cost of lagging behind it.
# This is deliberate: a shrinking window at the edges is noisiest exactly where
# people read the chart hardest, and quietly so.
#
# Centered windows must be odd, so the window is symmetric about the day. The
# UI enforces this with an odd-only slider; the stop() here is a backstop.
#
# NA HANDLING inside the window follows the SAME rule as the edges: min_frac
# defaults to 1, so a value is computed only from a full window of real
# observations. One rule covers all three functions, with no special case for
# sum -- which would otherwise understate a total assembled from a short window.
#
# This costs almost nothing in practice: the 2005-2025 record for station 21202
# is exactly 7,670 rows against a calendar count of 7,670, i.e. not one missing
# day in 21 years. Where a station does have outages, the outage leaves a
# visible gap in the smoothed line rather than a quietly wrong number. Lowering
# it (0.75 tolerates one missing day in seven) trades that honesty for
# continuity, and with a 31-day window one missing day blanks 31 days of line.
roll_apply <- function(x, n, fun = "mean", align = "center", min_frac = 1) {

  n <- as.integer(n)
  if (is.na(n) || n <= 1L) return(as.numeric(x))

  align <- match.arg(align, c("center", "right"))
  if (align == "center" && n %% 2L == 0L) {
    stop("roll_apply(): a centered window must be an odd number of days, got ", n)
  }

  f <- switch(fun,
              mean   = function(v) mean(v, na.rm = TRUE),
              sum    = function(v) sum(v, na.rm = TRUE),
              median = function(v) stats::median(v, na.rm = TRUE),
              stop("roll_apply(): unknown function '", fun, "'"))

  N    <- length(x)
  need <- max(1L, as.integer(ceiling(min_frac * n)))

  if (align == "center") {
    half   <- (n - 1L) %/% 2L
    starts <- seq_len(N) - half
    ends   <- seq_len(N) + half
  } else {
    starts <- seq_len(N) - (n - 1L)
    ends   <- seq_len(N)
  }

  vapply(seq_len(N), function(i) {
    s <- starts[i]; e <- ends[i]
    if (s < 1L || e > N) return(NA_real_)      # NA padding, no partial windows
    v <- x[s:e]
    if (sum(!is.na(v)) < need) return(NA_real_)
    as.numeric(f(v))
  }, numeric(1))
}

# ==============================================================================
# smooth_fun_allowed
# ==============================================================================
# Which rolling filters make sense for the variable(s) currently being plotted.
#
# A rolling SUM of an already-cumulative series is meaningless -- precip_cum is
# a running total, so summing a window of it produces a number with no physical
# reading. It is taken off the menu rather than left there to be picked by
# accident.
#
# This is a plain function, and it lives here rather than in app.R, for the same
# reason the aggregation chain does: it is a DATA-semantics rule, it has to hold
# for every tab that offers smoothing, and a rule buried in an observeEvent
# cannot be tested. The UI labels stay in app.R -- this returns bare function
# names and lets the caller map them to whatever the menu says.
#
# Takes a vector because the Compare Variables tab plots two variables at once
# and the single filter control applies to both: if EITHER is cumulative, the
# rolling sum is wrong for that one, so it comes off for both.
smooth_fun_allowed <- function(variables) {
  fns <- c("mean", "sum", "median")
  if ("precip_cum" %in% variables) setdiff(fns, "sum") else fns
}

# ==============================================================================
# build_daily_series
# ==============================================================================
# Aggregate hourly station data to a daily series for one variable.
#
#   all_data       joined NFDRS + weather frame, as held in all_data_cache()
#   variable       selected variable name, including the computed ones
#                  ("precip_total", "precip_cum", "burn_period")
#   daily_stat     "mean", "min", "max" or "1300LST"
#   month_range    length-2 integer vector, inclusive calendar month bounds
#   smooth         TRUE to populate value_smooth
#   smooth_fun     "mean", "sum" or "median"
#   smooth_window  window width in days
#   smooth_align   "center" or "right"
#   smooth_min_frac  see roll_apply()
#
# Returns a tibble of month_day, year, record_type, value, value_smooth --
# averaged across whatever stations are present in all_data, then cropped to
# month_range. value_smooth is all-NA when smooth = FALSE.
#
# The smoother runs on EVERY (year, record_type) group, not just the current
# year, so the column is there if the climatology mean ever wants it too. Only
# the current-year observed line consumes it today.
#
# Windows do not cross the year boundary. Each year is smoothed independently,
# so early-January values are NA for a centered window -- the same rule as
# every other edge, applied consistently across the 21 climatology years.
build_daily_series <- function(all_data, variable, daily_stat, month_range,
                               smooth          = FALSE,
                               smooth_fun      = "mean",
                               smooth_window   = 7,
                               smooth_align    = "center",
                               smooth_min_frac = 1) {

  # --- 1. per-station daily aggregate ----------------------------------------
  # Advanced aggregation handling that intercepts custom variables
  stn_data <- all_data %>%
    group_by(station_id, date, record_type) %>%
    summarise(
      value = if (variable %in% c("precip_total", "precip_cum")) {
        safe_summary1(precipitation, sum)
      } else if (variable == "burn_period") {
        safe_summary1(relativeHumidity < 20, sum)
      } else if (daily_stat == "1300LST") {
        safe_summary1(.data[[variable]][hour == 13], mean)
      } else {
        safe_summary1(.data[[variable]], match.fun(daily_stat))
      },
      .groups = "drop"
    ) %>%
    mutate(year = as.integer(format(date, "%Y")), month_day = as.Date(format(date, "2024-%m-%d")))

  # --- 2. cumulative sum, per station, on the UNCROPPED year -----------------
  if (variable == "precip_cum") {
    stn_data <- stn_data %>%
      arrange(date) %>%
      group_by(station_id, year, record_type) %>%
      # coalesce() turns NAs into 0s so they don't break the running total
      mutate(value = cumsum(coalesce(value, 0))) %>%
      ungroup()
  }

  # --- 3. average across stations, still uncropped ---------------------------
  daily <- stn_data %>%
    group_by(month_day, year, record_type) %>%
    summarise(value = mean(value, na.rm = TRUE), .groups = "drop")

  # --- 4. rolling filter, still uncropped ------------------------------------
  if (isTRUE(smooth) && !is.na(smooth_window) && smooth_window > 1) {

    # Roll over a GAP-FREE calendar. A window over row positions is not a window
    # over days if the station dropped out: without this, "7-day mean" quietly
    # means "the 7 rows I happen to have". The filled days carry NA into the
    # window (subject to min_frac) and are dropped again below, so the returned
    # rows are exactly the rows that came in.
    daily <- daily %>%
      mutate(.real_date = as.Date(paste0(year, format(month_day, "-%m-%d")))) %>%
      group_by(year, record_type) %>%
      group_modify(function(.x, .key) {
        grid <- tibble::tibble(.real_date = seq(min(.x$.real_date, na.rm = TRUE),
                                                max(.x$.real_date, na.rm = TRUE),
                                                by = "day"))
        j <- left_join(grid, .x, by = ".real_date")
        j$value_smooth <- roll_apply(j$value, smooth_window, smooth_fun,
                                     smooth_align, smooth_min_frac)
        # drop the filled days -- they were scaffolding for the window only
        filter(j, !is.na(month_day))
      }) %>%
      ungroup() %>%
      select(month_day, year, record_type, value, value_smooth) %>%
      # group_modify returns rows grouped by year/record_type; restore the
      # month_day, year, record_type ordering that summarise() produced, so the
      # smoothed and unsmoothed paths return rows in the same order.
      arrange(month_day, year, record_type)

  } else {
    daily$value_smooth <- NA_real_
  }

  # --- 5. crop to the selected calendar months -- ALWAYS LAST ----------------
  # month(month_day) is identical to month(date) by construction: month_day is
  # built as as.Date(format(date, "2024-%m-%d")), so only the year is replaced.
  daily %>%
    filter(lubridate::month(month_day) >= month_range[1] &
             lubridate::month(month_day) <= month_range[2])
}
