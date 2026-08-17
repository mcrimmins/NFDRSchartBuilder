# ==============================================================================
# fems_api.R -- Minimal client for the FEMS Read-Only GraphQL API
# Author: Mike Crimmins (crimmins@arizona.edu) / Claude
# Branch: fems-graphql-api
#
# Endpoint : https://fems.fs2c.usda.gov/api/ext-climatology/graphql
# Auth     : HTTP Basic -- username = FEMS account username,
#                          password = FEMS API key
#
# Credentials are read from .Renviron (never hardcode them):
#   FEMS_USER=crimmins@arizona.edu
#   FEMS_KEY=<your api key>
#
# NOTE: .Renviron is only read when R starts. After editing it, restart the
#       R session (RStudio: Session > Restart R, or Ctrl+Shift+F10).
#
# Usage:
#   source("fems_api.R")
#   d <- fems_gql(query = "...", variables = list(stationIds = "21202"))
# ==============================================================================

library(httr)
library(jsonlite)
library(tibble)
library(dplyr)

FEMS_ENDPOINT <- "https://fems.fs2c.usda.gov/api/ext-climatology/graphql"

# ------------------------------------------------------------------
# Credentials
# ------------------------------------------------------------------
fems_credentials <- function() {
  user <- Sys.getenv("FEMS_USER", unset = "")
  key  <- Sys.getenv("FEMS_KEY",  unset = "")

  if (!nzchar(user)) {
    stop("FEMS_USER is not set. Add `FEMS_USER=crimmins@arizona.edu` to ",
         ".Renviron and restart R.", call. = FALSE)
  }
  if (!nzchar(key)) {
    stop("FEMS_KEY is not set. Add `FEMS_KEY=<your key>` to .Renviron and ",
         "restart R.", call. = FALSE)
  }
  list(user = user, key = key)
}

# Safe to print - shows that a key is loaded without revealing it.
fems_mask_key <- function(key) {
  if (!nzchar(key)) return("<empty>")
  n <- nchar(key)
  if (n <= 8) return(strrep("*", n))
  paste0(substr(key, 1, 4), strrep("*", n - 8), substr(key, n - 3, n))
}

# ------------------------------------------------------------------
# Core POST
#
# Returns the parsed `data` object from the GraphQL response.
# Throws with a useful message on HTTP failure or GraphQL-level errors
# (GraphQL returns HTTP 200 with an `errors` array, so status alone is
# not enough to tell success from failure).
# ------------------------------------------------------------------
fems_gql <- function(query,
                     variables   = list(),
                     endpoint    = FEMS_ENDPOINT,
                     timeout_sec = 300,
                     verbose     = FALSE) {

  creds <- fems_credentials()

  vars_json <- if (length(variables) == 0) {
    "{}"
  } else {
    as.character(jsonlite::toJSON(variables, auto_unbox = TRUE,
                                  null = "null", na = "null", digits = NA))
  }

  body <- paste0(
    '{"query":',     as.character(jsonlite::toJSON(query, auto_unbox = TRUE)),
    ',"variables":', vars_json, '}'
  )

  if (isTRUE(verbose)) {
    message("POST ", endpoint)
    message("  variables: ", vars_json)
  }

  res <- httr::POST(
    url    = endpoint,
    config = httr::authenticate(creds$user, creds$key, type = "basic"),
    httr::content_type_json(),
    httr::accept_json(),
    httr::user_agent("NFDRSChartBuilder/R (crimmins@arizona.edu)"),
    httr::timeout(timeout_sec),
    body   = body,
    encode = "raw"
  )

  status <- httr::status_code(res)
  txt    <- httr::content(res, as = "text", encoding = "UTF-8")

  if (status %in% c(401, 403)) {
    stop("FEMS API rejected the credentials (HTTP ", status, "). Check that ",
         "FEMS_USER is the username the API key was generated under, that ",
         "FEMS_KEY is current (generating a new key invalidates the old one), ",
         "and that the account has the FEMS API or FEMS Admin role.\n",
         "Server said: ", substr(txt, 1, 500), call. = FALSE)
  }
  if (status >= 400) {
    stop("FEMS API HTTP ", status, ": ", substr(txt, 1, 1000), call. = FALSE)
  }

  parsed <- tryCatch(
    jsonlite::fromJSON(txt, simplifyVector = FALSE),
    error = function(e) {
      stop("FEMS API returned a non-JSON body (HTTP ", status, "): ",
           substr(txt, 1, 500), call. = FALSE)
    }
  )

  if (!is.null(parsed$errors) && length(parsed$errors) > 0) {
    msgs <- vapply(parsed$errors, function(e) {
      m <- e$message
      if (is.null(m)) "<no message>" else as.character(m)
    }, character(1))
    stop("GraphQL error(s):\n  - ", paste(msgs, collapse = "\n  - "),
         call. = FALSE)
  }

  parsed$data
}

# ------------------------------------------------------------------
# Convert a GraphQL `data` array (list of records) to a tibble.
# NULLs become NA; nested objects are kept as list-columns.
# ------------------------------------------------------------------
fems_as_tibble <- function(rows) {
  if (is.null(rows) || length(rows) == 0) return(tibble::tibble())

  flat <- lapply(rows, function(r) {
    r <- lapply(r, function(v) {
      if (is.null(v))                 return(NA)
      if (is.list(v) || length(v) > 1) return(list(v))
      v
    })
    tibble::as_tibble(r)
  })

  dplyr::bind_rows(flat)
}

# ------------------------------------------------------------------
# Paged fetch.
#
# FEMS GraphQL responses carry `_metadata { page per_page total_count
# page_count }`. Pages appear to be 0-indexed (the guide's examples use
# "page": 0). This walks pages until page_count is reached.
#
#   query        : GraphQL document declaring $page and $perPage
#   variables    : everything except page/perPage
#   root         : name of the top-level field, e.g. "nfdrsObs"
#   per_page     : records per request
# ------------------------------------------------------------------
fems_gql_paged <- function(query,
                           variables,
                           root,
                           per_page    = 5000,
                           max_pages   = 1000,
                           page_var    = "page",
                           per_page_var = "perPage",
                           pause       = 0.2,
                           verbose     = TRUE) {

  collected <- list()
  page      <- 0
  n_pages   <- NA_integer_
  total     <- NA_integer_

  repeat {
    vars <- variables
    vars[[page_var]]     <- page
    vars[[per_page_var]] <- per_page

    d <- fems_gql(query, vars)
    node <- d[[root]]

    if (is.null(node)) {
      stop("Response has no field named '", root, "'. Got: ",
           paste(names(d), collapse = ", "), call. = FALSE)
    }

    meta <- node[["_metadata"]]
    if (!is.null(meta)) {
      n_pages <- if (is.null(meta$page_count))  NA_integer_ else as.integer(meta$page_count)
      total   <- if (is.null(meta$total_count)) NA_integer_ else as.integer(meta$total_count)
    }

    rows <- node[["data"]]
    if (!is.null(rows) && length(rows) > 0) {
      collected[[length(collected) + 1]] <- rows
    }

    if (isTRUE(verbose)) {
      message("  ", root, " page ", page,
              if (!is.na(n_pages)) paste0("/", max(n_pages - 1, 0)) else "",
              " -> ", length(rows), " rows",
              if (!is.na(total)) paste0(" (total_count ", total, ")") else "")
    }

    page <- page + 1
    if (is.na(n_pages) || page >= n_pages) break
    if (page >= max_pages) {
      warning("Hit max_pages (", max_pages, ") for '", root,
              "'; result is truncated.", call. = FALSE)
      break
    }
    Sys.sleep(pause)
  }

  out <- fems_as_tibble(unlist(collected, recursive = FALSE))
  attr(out, "total_count") <- total
  attr(out, "page_count")  <- n_pages
  out
}
