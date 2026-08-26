# ==============================================================================
# R/export_naming.R
#
# Naming helpers shared by BOTH CSV download handlers -- the Static tab's and
# the Compare Variables tab's.
#
# Its own small file rather than a corner of app.R because a filename builder is
# exactly the kind of thing that is only ever discovered to be broken when a
# user cannot find, or cannot open, the file they just downloaded. Here a test
# can hand it a station called "MT. LEMMON #2" and check what comes out.
# ==============================================================================

# ==============================================================================
# station_slug
# ==============================================================================
# A filesystem-safe fragment naming the stations behind an export.
#
# Downloads land in one flat folder. Without the station in the name, two
# exports of the same variable and year from DIFFERENT stations collide, and the
# second silently overwrites the first -- or, worse, does not, and there are two
# files that cannot be told apart later.
#
#   names         station names, in the order they should appear
#   ids           station ids, used only as a fallback when names are missing
#   max_stations  how many to spell out before summarising the rest. Multi-
#                 station selections are averaged into one series, so all of
#                 them are genuinely part of the file's identity -- but a
#                 twelve-station filename is unusable, so the tail becomes a
#                 count. The full list always goes in the FILE, in the
#                 Station_Names column, so nothing is actually lost.
#
# Punctuation and spaces are stripped rather than substituted: "MT. LEMMON"
# becomes "MTLEMMON", not "MT_LEMMON_", because trailing and doubled separators
# are what make generated filenames ugly. Names are joined with "-".
station_slug <- function(names, ids = NULL, max_stations = 3) {

  clean <- function(x) {
    x <- as.character(x)
    x <- x[!is.na(x)]
    x <- trimws(x)
    x[nzchar(x)]
  }

  x <- clean(names)
  if (length(x) == 0L) x <- clean(ids)
  if (length(x) == 0L) return("stations")

  n_extra <- length(x) - max_stations
  x <- utils::head(x, max_stations)

  x <- gsub("[^A-Za-z0-9]+", "", x)
  x <- x[nzchar(x)]
  if (length(x) == 0L) return("stations")

  out <- paste(x, collapse = "-")
  if (n_extra > 0L) out <- paste0(out, "-plus", n_extra)
  out
}
