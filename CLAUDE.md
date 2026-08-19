# Project instructions — NFDRSv4 Chart Builder

R/Shiny app that visualizes daily fire weather indices, meteorological
variables, and percentile climatologies from NFDRS, for RAWS stations
nationwide. Maintained by Mike Crimmins, University of Arizona. Deployed to
Posit Connect at viz.datascience.arizona.edu.

Read `README.md` for the layout and `docs/FEMS_API_MIGRATION_NOTES.md` for the
API field mapping.

---

## How we work together

**Claude cannot run R.** There is no R in Claude's environment, and the bridge
to Mike's machine has no network access. So Claude writes code and test
scripts; Mike runs them in RStudio and pastes the log back. Write test scripts
that print a readable transcript to `tests/logs/` and state a verdict — they
are the only feedback loop.

**Verify refactors by diff, not by assertion.** When rewriting part of a large
file, extract the regions that should be untouched from `git show HEAD:<file>`
and confirm they appear verbatim in the new version, then report the numbers.
"I only changed X" is not evidence; 833 of 1220 lines proven byte-identical is.

**Mike runs git himself.** Claude's device bridge cannot delete files, so every
git command Claude runs leaves `.git/index.lock`, `.git/HEAD.lock`, and stray
`.git/objects/tmp_obj_*` behind, which block the next command. Claude may stage
and inspect, but should hand Mike the commit command.

**Mike's terminal is Windows Command Prompt.** Give single-line commands. No
`>`, `|`, `&`, or `^` outside quotes — cmd reads `->` in a commit message as a
redirect and will create a junk file. For multi-paragraph commit messages use
repeated `-m` flags rather than a multi-line string. `rm`, heredocs, and
`$(...)` are not available; use `del` and `rmdir /s /q`.

**RStudio does not clear the global environment on restart.** Session > Restart
R restores the workspace from `.Rproj.user/` independently of `.RData`. To
truly clear leftovers, `rm(list = ls())`.

---

## Hard-won gotchas — check these first when something breaks

**Namespace masking is the most likely cause of a weird Shiny error.**
Two real failures:

- `library(jsonlite)` in a file sourced by `app.R` masked `shiny::validate()`,
  because app.R attaches shiny first and the sourced file lands ahead of it on
  the search path. Every `validate(need(...))` guard failed with
  `is.character(txt) is not TRUE`. **In `R/*.R`, namespace-qualify
  (`jsonlite::fromJSON`, `httr::GET`) instead of attaching.**
- A test helper named `hr()` masked `shiny::hr()`, giving
  `unused argument (style = ...)`. **Prefix helpers in `tests/` with a dot
  (`.hr`)** so they cannot collide and stay out of `ls()`.

When a Shiny error names a function shiny exports, run `find("<name>")` before
reading app code.

**Line endings.** The repo is normalized to LF via `.gitattributes`. Working
files stay CRLF on Windows, so `git diff` may show a file as modified with no
real change — check with `git diff -w --numstat` before assuming.

---

## FEMS API constraints

Data comes from the authenticated FEMS Read-Only **GraphQL** API at
`https://fems.fs2c.usda.gov/api/ext-climatology/graphql`. HTTP Basic auth:
username = FEMS account username, password = API key. Both live in `.Renviron`
as `FEMS_USER` / `FEMS_KEY`, and must also be set as environment variables on
Posit Connect. Regenerating a key in the FEMS UI invalidates the previous one —
a sudden 401 means check that first.

- **GraphQL returns HTTP 200 on failure.** Always inspect the `errors` array,
  never just the status code.
- **Schema introspection is disabled** in production (Apollo). The PDF in
  `docs/` is the only reference and contains at least one typo (`ndfr_date`).
  Trust only field names empirically confirmed to return data.
- **Local station time is single-station only.** A multi-station query with
  `dateTimeFormat: "LocalStationTime"` is rejected. Local time is what makes
  `date`/`hour` correct, so `R/fems_download.R` loops stations one at a time.
  Do not batch this back into one request — batching also saves nothing,
  because pagination is row-based and one station already exceeds a page.
- **Always send `sortBy` / `sortOrder` when paging.** The two feeds return
  opposite default orders, and paging an unpinned sort can drop or duplicate
  rows.
- There is **no date-range cap** — a 21-year request returns ~190,000 rows fine.
- `vapor_press_def` is in **pascals**; the app works in kPa.
- `station_id` comes back as integer — coerce to character for joins.
- Join on `display_hour_lst`, not `observation_time_lst`: the two feeds differ
  by seconds in the latter.

---

## Code conventions and known debt

**`R/fems_download.R` has a design contract:** its functions return exactly the
column names `app.R` already uses (`energyReleaseComponent`,
`relativeHumidity`, `precipitation`, `vpd`, `hdw`, …). Keep it that way so data
changes stay confined to the download layer.

**The daily aggregation chain is duplicated verbatim in three places** —
`climatology_plot`, `plotly_climatology_plot`, and the `download_plot_data`
handler. Any change to variable dispatch or filtering must land in all three or
the CSV export and the two plots will disagree. Extracting it into a shared
`reactive()` is the obvious refactor and has not been done.

**Within that chain, the `precip_cum` cumulative sum must run BEFORE the
month-range filter**, so accumulation starts January 1 rather than at the crop
boundary.

**The 2005–2025 climatology baseline is hardcoded in five places**: three
plot/export blocks, the summary table, and the About tab text.

**The About tab is stale** — it says "What's New (Spring 2025)" and describes an
"18-year baseline" for what is a 21-year window.

`station_metadata` carries a `tz` column (Olson names, 20 distinct zones) that
nothing currently uses.

---

## Testing

Run from the project root so relative paths resolve:

```r
source("tests/test_fems_api.R")        # auth + query smoke tests
source("tests/test_fems_api_2.R")      # API behavior probes
source("tests/test_fems_api_3.R")      # download benchmark + contract validation
source("tests/test_fems_api_4.R")      # network/parse split, page size
source("tests/test_1300lst.R")         # local-hour verification
source("tests/test_migration_impact.R")# how far chart values move
```

These hit the live API and are not automated. Several pull a full period of
record and take minutes. `.rscignore` keeps `tests/`, `archive/`, `docs/`, and
`scripts/` out of the Connect deployment bundle.
