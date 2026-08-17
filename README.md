# NFDRSv4 Chart Builder

Interactive R/Shiny application for visualizing daily fire weather indices,
meteorological variables, and percentile climatologies from the National Fire
Danger Rating System (NFDRS).

University of Arizona — <https://cales.arizona.edu/climate/>
Contact: Mike Crimmins, <crimmins@arizona.edu>

---

## Layout

```
app.R                  The application. Single file: helpers, UI, server.
R/                     Modules sourced by app.R.
  fems_api.R             GraphQL client for the FEMS Read-Only API.
  fems_download.R        NFDRS + weather download and assembly.
data/                  Runtime and source data.
  station_metadata_FEMS3_042225.csv        read at app start
  FEMS_3.0_RAWS_Master_Station_List_and_Metadata.csv   upstream source
www/                   Static assets served by Shiny (logos).
tests/                 API connectivity, benchmark, and validation scripts.
  logs/                  Test transcripts. Not tracked.
scripts/               Maintenance and one-off utilities, not part of the app.
  processMetaData.R      regenerates the station metadata CSV
  usageAPI.R             pulls Posit Connect usage metrics
  getMTBSpts_func.R      MTBS fire perimeter lookup (not yet wired in)
docs/                  Reference material.
archive/               Superseded app versions, kept for reference only.
```

`app.R` and `www/` must stay at the repository root — Shiny and Posit Connect
both depend on that.

## Running locally

Open `NFDRSChartBuilder.Rproj` in RStudio so the working directory is the
project root, then run the app. All paths in the project are relative to the
root, including the ones inside `tests/`.

## Credentials

Data comes from the FEMS Read-Only API, which requires authentication. Put
these in `.Renviron` at the project root (gitignored) and restart R:

```
FEMS_USER=<your FEMS account username>
FEMS_KEY=<your FEMS API key>
```

Generate the key from the FEMS UI under the hamburger menu → API Key. Note that
generating a new key immediately invalidates the previous one.

The same two variables must be set as environment variables on Posit Connect
before deploying, or the app will fail to fetch.

## Tests

Run from the project root:

```r
source("tests/test_fems_api.R")      # auth + query smoke tests
source("tests/test_fems_api_2.R")    # API behavior probes
source("tests/test_fems_api_3.R")    # download layer benchmark + validation
source("tests/test_fems_api_4.R")    # network/parse split, page size
source("tests/test_1300lst.R")       # old vs new local-hour handling
```

These hit the live API and are not automated — they print a transcript to
`tests/logs/` for reading. `test_fems_api_3.R` and `test_fems_api_4.R` pull a
full period of record and take a few minutes.

## Data source

All observation and forecast data comes from the USDA Forest Service FEMS API
at `https://fems.fs2c.usda.gov/api/ext-climatology/graphql`. Station metadata
updates come from <https://www.wildfire.gov/node/3473> or
<https://fems.fs2c.usda.gov/download>.

See `docs/FEMS_API_MIGRATION_NOTES.md` for the field mapping between the
retired CSV endpoints and the current GraphQL queries.
