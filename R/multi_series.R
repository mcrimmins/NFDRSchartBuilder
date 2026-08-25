# ==============================================================================
# R/multi_series.R
#
# build_multi_series() -- the data layer for the "Compare Variables" tab.
#
# A loop over build_daily_series(), row-bound with a `variable` column. It adds
# NOTHING to the aggregation itself: every guarantee build_daily_series()
# already carries -- the cumsum-before-crop rule, the 1300 LST path, station
# averaging, the gap-free rolling window, NA edge padding -- comes along
# unchanged, because this function does not reimplement any of it.
#
# That is the entire design, and tests/test_multi_series.R exists to prove it:
# each per-variable slice of the returned frame must be BIT-IDENTICAL to a
# standalone build_daily_series() call for that variable. If that test ever
# fails, this file has grown logic it was not supposed to have.
#
# Returns: variable, month_day, year, record_type, value, value_smooth.
#
# ------------------------------------------------------------------------------
# N VARIABLES, NOT TWO
# ------------------------------------------------------------------------------
# The tab renders exactly two series on two y-axes, but that cap lives in the
# RENDERER, not here. This function takes any number of variables so that a
# percentile-overlay renderer for three or four is a second renderer over the
# same data rather than a rewrite of the data layer. The tests cover N = 1, 2
# and 3 for that reason, not because the UI asks for it.
#
# ------------------------------------------------------------------------------
# DUPLICATES ARE COLLAPSED HERE, ON PURPOSE
# ------------------------------------------------------------------------------
# `variables` is de-duplicated before the loop. Picking the same variable in
# both dropdowns is allowed by design (docs/MULTI_VARIABLE_SCOPE.md section 8),
# and because the Daily Statistic control is shared, the two series would then
# be identical by construction. Computing the same aggregation twice is pure
# waste, and handing the renderer two identical blocks invites it to draw them
# against two independently scaled axes -- where an identical series reads as a
# lagged relationship, which is the exact failure mode the whole dual-axis
# design is trying to avoid.
#
# So the collapse happens once, here: ask for c("erc", "erc") and one block
# comes back. The renderer then sees a single variable and draws a single axis
# with no special case of its own. The user-facing note explaining what
# happened belongs in the UI, which can compare the two inputs directly.
#
# ------------------------------------------------------------------------------
# WHY THE AVAILABILITY CHECK IS HERE AND NOT IN build_daily_series()
# ------------------------------------------------------------------------------
# build_daily_series() takes one variable that the caller picked from a menu
# built out of the columns actually fetched, so it can reasonably assume the
# column is there. This function is fed by two independent dropdowns, and a
# stale selection surviving a re-fetch under a different fuel model is a real
# way to arrive here naming a variable the frame does not carry. Left
# unchecked, `.data[[variable]]` inside the summarise() fails deep in dplyr with
# a message that names neither the variable nor the tab. So the check is up
# front, it names the offending variable, and it prints what is actually
# available -- a short frame returned silently would be far worse.
#
# dplyr verbs are left unqualified here, matching R/daily_series.R and
# R/fems_download.R; app.R attaches dplyr well before sourcing this file.
# tibble is qualified.
# ==============================================================================

# Which raw column a variable needs present in all_data. The three computed
# variables are derived rather than read, so they name their source instead of
# themselves. Kept as a separate function so the test can exercise the mapping
# without going through a full aggregation.
multi_series_source_column <- function(variable) {
  switch(variable,
         precip_total = "precipitation",
         precip_cum   = "precipitation",
         burn_period  = "relativeHumidity",
         variable)
}

# ==============================================================================
# build_multi_series
# ==============================================================================
#   all_data       joined NFDRS + weather frame, as held in all_data_cache()
#   variables      character vector of variable names, any length >= 1.
#                  De-duplicated, order otherwise preserved.
#   daily_stat     "mean", "min", "max" or "1300LST" -- shared across all
#                  variables, per the scope decision. Precipitation and Burn
#                  Period keep their automatic overrides inside
#                  build_daily_series().
#   month_range    length-2 integer vector, inclusive calendar month bounds
#   smooth ...     passed straight through to build_daily_series(), unchanged,
#                  so every variable gets the same filter settings.
build_multi_series <- function(all_data, variables, daily_stat, month_range,
                               smooth          = FALSE,
                               smooth_fun      = "mean",
                               smooth_window   = 7,
                               smooth_align    = "center",
                               smooth_min_frac = 1) {

  if (!is.character(variables) || length(variables) == 0L || anyNA(variables)) {
    stop("build_multi_series(): `variables` must be a non-empty character ",
         "vector with no NAs.")
  }

  # See the header: same variable twice collapses to one block.
  variables <- unique(variables)

  needed  <- vapply(variables, multi_series_source_column, character(1),
                    USE.NAMES = FALSE)
  absent  <- !(needed %in% names(all_data))
  if (any(absent)) {
    numeric_cols <- names(all_data)[vapply(all_data, is.numeric, logical(1))]
    stop("build_multi_series(): variable(s) not available in the fetched data: ",
         paste(variables[absent], collapse = ", "),
         ". Missing source column(s): ",
         paste(unique(needed[absent]), collapse = ", "),
         ". Numeric columns present: ",
         paste(sort(numeric_cols), collapse = ", "))
  }

  # build_daily_series() reads .data[[variable]][hour == 13] on this path, so
  # the column has to exist before the loop rather than failing on variable 2.
  if (identical(daily_stat, "1300LST") && !("hour" %in% names(all_data))) {
    stop("build_multi_series(): daily_stat = \"1300LST\" needs an `hour` ",
         "column in all_data, which is not present.")
  }

  blocks <- lapply(variables, function(v) {
    one <- build_daily_series(all_data, v, daily_stat, month_range,
                              smooth          = smooth,
                              smooth_fun      = smooth_fun,
                              smooth_window   = smooth_window,
                              smooth_align    = smooth_align,
                              smooth_min_frac = smooth_min_frac)
    tibble::add_column(one, variable = v, .before = 1L)
  })

  bind_rows(blocks)
}
