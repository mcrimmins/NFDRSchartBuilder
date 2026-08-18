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
# PERFORMANCE NOTE (2026-08-17)
# -----------------------------
# The first version of this file parsed responses with
# fromJSON(simplifyVector = FALSE) and then built one tibble per record before
# bind_rows()-ing them. For a 190,000-row period-of-record pull that meant
# constructing 190,000 tibbles in R, which dominated the total fetch time.
# Responses are now simplified by jsonlite in C (simplify = TRUE), so a page
# arrives as a ready-made data frame and paging just binds a handful of them.
# Set simplify = FALSE only when you need the raw nested list (e.g. for a
# query returning nested objects rather than flat scalar columns).
# ==============================================================================

# NOTE: do NOT attach httr or jsonlite here.
#
# app.R attaches shiny first, then sources this file. Attaching jsonlite at
# that point puts it ahead of shiny on the search path, and jsonlite exports
# validate() -- which masks shiny::validate() and breaks every
# validate(need(...)) guard in the app with "is.character(txt) is not TRUE".
# httr is attached by app.R already and is left alone for the same reason.
#
# Every httr and jsonlite call in this file is namespace-qualified, so nothing
# needs attaching. dplyr and tibble are attached because fems_download.R uses
# their verbs and the pipe unqualified, and app.R attaches them anyway.
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
                     timeout_sec = 600,
                     simplify    = TRUE,
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
    jsonlite::fromJSON(txt, simplifyVector = simplify, flatten = simplify),
    error = function(e) {
      stop("FEMS API returned a non-JSON body (HTTP ", status, "): ",
           substr(txt, 1, 500), call. = FALSE)
    }
  )

  # `errors` is a list of objects when simplify = FALSE and a data frame
  # when simplify = TRUE, so handle both shapes.
  errs <- parsed$errors
  if (!is.null(errs) && length(errs) > 0) {
    msgs <- if (is.data.frame(errs)) {
      as.character(errs$message)
    } else {
      vapply(errs, function(e) {
        m <- e$message
        if (is.null(m)) "<no message>" else as.character(m)
      }, character(1))
    }
    stop("GraphQL error(s):\n  - ", paste(msgs, collapse = "\n  - "),
         call. = FALSE)
  }

  parsed$data
}

# ------------------------------------------------------------------
# Coerce a GraphQL `data` payload to a tibble.
#
# With simplify = TRUE (the default) jsonlite has already produced a data
# frame, so this is a cheap passthrough. The list-of-records branch is kept
# for simplify = FALSE callers.
# ------------------------------------------------------------------
fems_as_tibble <- function(rows) {
  if (is.null(rows)) return(tibble::tibble())

  if (is.data.frame(rows)) {
    if (nrow(rows) == 0) return(tibble::tibble())
    return(tibble::as_tibble(rows))
  }

  if (length(rows) == 0) return(tibble::tibble())

  flat <- lapply(rows, function(r) {
    r <- lapply(r, function(v) {
      if (is.null(v))                  return(NA)
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
# page_count }`. Pages are 0-indexed. This walks pages until page_count is
# reached and binds the result once at the end.
#
# ALWAYS pass an explicit sortBy/sortOrder in `variables`: nfdrsObs and
# weatherObs return opposite default orders, and paging an unpinned sort can
# drop or duplicate records across page boundaries.
#
#   query        : GraphQL document declaring $page and $perPage
#   variables    : everything except page/perPage
#   root         : name of the top-level field, e.g. "nfdrsObs"
#   per_page     : records per request
#   progress     : optional function(page, page_count, rows_so_far)
# ------------------------------------------------------------------
fems_gql_paged <- function(query,
                           variables,
                           root,
                           per_page     = 25000,
                           max_pages    = 1000,
                           page_var     = "page",
                           per_page_var = "perPage",
                           pause        = 0,
                           progress     = NULL,
                           verbose      = TRUE) {

  collected <- list()
  page      <- 0
  n_pages   <- NA_integer_
  total     <- NA_integer_
  n_rows    <- 0L

  repeat {
    vars <- variables
    vars[[page_var]]     <- page
    vars[[per_page_var]] <- per_page

    d    <- fems_gql(query, vars, simplify = TRUE)
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
    nr   <- if (is.data.frame(rows)) nrow(rows) else length(rows)
    if (nr > 0) {
      collected[[length(collected) + 1]] <- rows
      n_rows <- n_rows + nr
    }

    if (isTRUE(verbose)) {
      message("  ", root, " page ", page,
              if (!is.na(n_pages)) paste0("/", max(n_pages - 1, 0)) else "",
              " -> ", nr, " rows",
              if (!is.na(total)) paste0(" (of ", total, ")") else "")
    }
    if (is.function(progress)) progress(page, n_pages, n_rows)

    page <- page + 1
    if (is.na(n_pages) || page >= n_pages) break
    if (page >= max_pages) {
      warning("Hit max_pages (", max_pages, ") for '", root,
              "'; result is truncated.", call. = FALSE)
      break
    }
    if (pause > 0) Sys.sleep(pause)
  }

  out <- if (length(collected) == 0) {
    tibble::tibble()
  } else if (is.data.frame(collected[[1]])) {
    tibble::as_tibble(dplyr::bind_rows(collected))
  } else {
    fems_as_tibble(unlist(collected, recursive = FALSE))
  }

  attr(out, "total_count") <- total
  attr(out, "page_count")  <- n_pages
  out
}
