# ==============================================================================
# R/daily_series.R
#
# The daily-aggregation chain, extracted from app.R.
#
# Until now this chain existed as three verbatim copies -- in climatology_plot,
# plotly_climatology_plot, and the download_plot_data handler -- so any change
# to variable dispatch or filtering had to land in all three or the two plots
# and the CSV export would silently disagree. It now lives here once.
#
# This is a PLAIN FUNCTION, not a reactive, on purpose. A reactive cannot be
# called outside a Shiny session, so tests/ would have to carry its own copy of
# the chain and would end up testing itself rather than the app. app.R wraps
# this in a one-line reactive (daily_series) that supplies the inputs.
#
# ORDERING CONTRACT: the precip_cum cumulative sum MUST run before the
# month-range filter, so accumulation starts January 1 rather than at the crop
# boundary. See CLAUDE.md.
#
# dplyr verbs are left unqualified here, matching R/fems_download.R, so that
# the body stays byte-identical to the three blocks it replaces. app.R attaches
# dplyr well before sourcing this file. lubridate is qualified, as it already
# was in the original.
# ==============================================================================

# Moved here from app.R (unchanged) so this file is self-contained and the
# test scripts do not have to duplicate it.
safe_summary1 <- function(x, fun) {
  if (all(is.na(x))) NA else fun(x, na.rm = TRUE)
}

# Aggregate hourly station data to a daily series for one variable.
#
#   all_data     joined NFDRS + weather frame, as held in all_data_cache()
#   variable     selected variable name, including the computed ones
#                ("precip_total", "precip_cum", "burn_period")
#   daily_stat   "mean", "min", "max" or "1300LST"
#   month_range  length-2 integer vector, inclusive calendar month bounds
#
# Returns a tibble of month_day, year, record_type, value -- averaged across
# whatever stations are present in all_data.
build_daily_series <- function(all_data, variable, daily_stat, month_range) {

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

  # Calculate Cumulative Sum BEFORE the month filter crops the data!
  if (variable == "precip_cum") {
    stn_data <- stn_data %>%
      arrange(date) %>%
      group_by(station_id, year, record_type) %>%
      # coalesce() turns NAs into 0s so they don't break the running total
      mutate(value = cumsum(coalesce(value, 0))) %>%
      ungroup()
  }

  # Apply calendar month filtering!
  stn_data <- stn_data %>%
    filter(lubridate::month(date) >= month_range[1] & lubridate::month(date) <= month_range[2])

  stn_data %>%
    group_by(month_day, year, record_type) %>%
    summarise(value = mean(value, na.rm = TRUE), .groups = "drop")
}
