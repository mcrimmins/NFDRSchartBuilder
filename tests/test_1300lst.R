# ==============================================================================
# test_1300lst.R -- Was the app's "1300LST" statistic ever 1 PM local time?
# Branch: fems-graphql-api
#
# Run from the project root:  source("tests/test_1300lst.R")
#
# The new GraphQL path derives `hour` from display_hour_lst, which is station
# local wall-clock time. Confirmed correct: across 21 years at SAGUARO, mean
# temperature bottoms at hour 6 and peaks at hour 15.
#
# The old CSV path derived `hour` by parsing a timestamp as UTC. Whether that
# produced local or UTC hours depends on which column the grep in
# download_nfdrs_data() happened to match, which we cannot tell by reading the
# code alone. So: run both paths over the same window and compare where the
# diurnal temperature curve peaks. Air temperature peaks mid-afternoon local,
# always. Any other peak means that path's `hour` is not local.
#
# The previous attempt at this failed because it extracted only
# download_weather_data(), which calls fetch_in_year_chunks() and
# harmonize_chunk_types(). This version pulls the whole helper block.
#
# Transcript -> tests/logs/test_1300lst_log.txt
# ==============================================================================

library(dplyr)
library(readr)
library(lubridate)
library(purrr)
library(tibble)
library(httr)

source("R/fems_download.R")

STATION  <- "21202"          # SAGUARO, MST (UTC-7), no DST
DAYS     <- 30
LOG_FILE <- "tests/logs/test_1300lst_log.txt"

s <- Sys.Date() - DAYS
e <- Sys.Date()

hr <- function(ch = "-") cat(strrep(ch, 78), "\n", sep = "")

sink(LOG_FILE, split = TRUE)
cat("1300LST check --", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("Station ", STATION, " (MST, UTC-7), window ", format(s), " to ", format(e),
    "\n\n", sep = "")

peak_of <- function(df, label) {
  o <- df[df$record_type == "O" & !is.na(df$temperature), ]
  if (nrow(o) == 0) { cat("  ", label, ": no observed rows\n", sep = ""); return(NULL) }
  bh <- tapply(o$temperature, o$hour, mean, na.rm = TRUE)
  cat("  ", label, " -- mean temperature by hour:\n", sep = "")
  print(round(bh, 1))
  cat("    peak hour = ", names(bh)[which.max(bh)],
      "   trough hour = ", names(bh)[which.min(bh)], "\n\n", sep = "")
  bh
}

# ---- NEW path ---------------------------------------------------------------
hr(); cat("NEW path (GraphQL, hour from display_hour_lst)\n"); hr()
new_bh <- tryCatch({
  d <- fems_download_weather(STATION, s, e, verbose = FALSE)
  peak_of(d, "new")
}, error = function(err) { cat("  error: ", conditionMessage(err), "\n"); NULL })

# ---- OLD path ---------------------------------------------------------------
# Extract the entire helper block from app.R: harmonize_chunk_types() through
# download_weather_data(), which is everything between those two markers.
hr(); cat("OLD path (CSV endpoint, hour from parsed timestamp)\n"); hr()

old_bh <- tryCatch({
  src <- readLines("app.R", warn = FALSE)

  start <- grep("^harmonize_chunk_types <- function", src)
  stop_ <- grep("^pretty_variable_name <- function", src)
  if (length(start) == 0 || length(stop_) == 0) {
    stop("could not locate the helper block in app.R -- has it been rewritten already?")
  }

  block <- src[start[1]:(stop_[1] - 1)]
  cat("  Extracted ", length(block), " lines from app.R (lines ", start[1],
      "-", stop_[1] - 1, ")\n", sep = "")

  # Load into a private environment so these definitions cannot shadow the
  # new fems_* functions in the global environment.
  old_env <- new.env(parent = globalenv())
  eval(parse(text = paste(block, collapse = "\n")), envir = old_env)

  cat("  Defined: ", paste(ls(old_env), collapse = ", "), "\n\n", sep = "")

  d <- old_env$download_weather_data(STATION, s, e)
  if (is.null(d)) stop("old download returned NULL (CSV endpoint may be refusing anonymous requests now)")
  peak_of(d, "old")
}, error = function(err) { cat("  error: ", conditionMessage(err), "\n\n"); NULL })

# ---- verdict ----------------------------------------------------------------
hr("="); cat("VERDICT\n"); hr("=")

if (is.null(new_bh) || is.null(old_bh)) {
  cat("  Could not compare -- one of the two paths did not return data.\n")
  if (!is.null(new_bh)) {
    cat("  The new path peaks at hour ", names(new_bh)[which.max(new_bh)],
        ", which is consistent with local time.\n", sep = "")
  }
} else {
  pn <- as.integer(names(new_bh)[which.max(new_bh)])
  po <- as.integer(names(old_bh)[which.max(old_bh)])
  shift <- (po - pn) %% 24

  cat("  new peak hour : ", pn, "\n", sep = "")
  cat("  old peak hour : ", po, "\n", sep = "")
  cat("  offset        : ", shift, " hours\n\n", sep = "")

  if (shift == 0) {
    cat("  The two paths agree. '1300LST' has always meant 1 PM local, and the\n")
    cat("  migration changes nothing for that statistic.\n")
  } else if (shift == 7) {
    cat("  The old path is 7 hours ahead -- exactly the MST offset. Its `hour`\n")
    cat("  was UTC, so the existing '1300LST' option has actually been showing\n")
    cat("  0600 local. The migration corrects it.\n\n")
    # Quantify: what does the correction do to the plotted value?
    if (!is.null(new_bh) && all(c("13", "6") %in% names(new_bh))) {
      cat("  Impact on temperature at this station and season:\n")
      cat("    value the old option really showed (hour 6) : ",
          round(new_bh[["6"]], 1), " F\n", sep = "")
      cat("    value it will show after the fix (hour 13)  : ",
          round(new_bh[["13"]], 1), " F\n", sep = "")
      cat("    shift                                       : ",
          round(new_bh[["13"]] - new_bh[["6"]], 1), " F\n", sep = "")
    }
  } else {
    cat("  Unexpected offset. Worth looking at directly before concluding\n")
    cat("  anything -- it is not a clean timezone shift.\n")
  }
}

cat("\n  Note: this compares temperature because its diurnal cycle is\n")
cat("  unambiguous. Every other hourly variable inherits the same `hour`,\n")
cat("  so whatever is true here is true for all of them.\n")

hr("=")
cat("Transcript: ", LOG_FILE, "\n", sep = "")
sink(NULL)
