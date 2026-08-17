# FEMS GraphQL Migration — Field Mapping & Findings

Branch `fems-graphql-api`. Verified against the live API on 2026-08-17 with
station 21202 (SAGUARO), fuel model Y.

## Status

| Stage | Result |
|---|---|
| Credentials load from `.Renviron` | PASS |
| Basic auth against `/api/ext-climatology/graphql` | PASS |
| Schema introspection | **BLOCKED** — Apollo Server has `introspection: false` in production |
| `stationMetaData` | PASS |
| `nfdrsObs` (22 fields) | PASS |
| `weatherObs` (18 fields) | PASS |

Introspection being disabled means the PDF is our only schema reference, so
every field name below is one we have *empirically confirmed returns data* —
not one transcribed from the guide. Anything not on these lists is unverified.

---

## 1. NFDRS: `download-nfdr` CSV → `nfdrsObs`

Query arguments that work:

```
fuelModels: String!        "Y"                     (required)
stationIds: String         "21202"
startDateRange: Date       "2026-08-14"            (plain YYYY-MM-DD)
endDateRange: Date         "2026-08-17"
hasHistoricData: TriState  "ALL"
dateTimeFormat             "LocalStationTime"
page: Int                  0                       (0-indexed)
per_page: Int              5000                    (variable is $perPage,
                                                    argument is per_page)
```

| app.R column (post-rename) | GraphQL field | Note |
|---|---|---|
| `station_id` | `station_id` | — |
| `record_type` | `nfdr_type` | Already `"O"` / `"F"`. The `substr(toupper(NFDRType),1,1)` step goes away. |
| `date` / `hour` | `display_hour_lst` | See §3 — this is the big one. |
| `kbdi` | `kbdi` | — |
| `oneHR_TL_FuelMoisture` | `one_hr_tl_fuel_moisture` | — |
| `tenHR_TL_FuelMoisture` | `ten_hr_tl_fuel_moisture` | — |
| `hundredHR_TL_FuelMoisture` | `hun_hr_tl_fuel_moisture` | note `hun_`, not `hundred_` |
| `thousandHR_TL_FuelMoisture` | `thou_hr_tl_fuel_moisture` | note `thou_` |
| `ignitionComponent` | `ignition_component` | — |
| `spreadComponent` | `spread_component` | — |
| `energyReleaseComponent` | `energy_release_component` | — |
| `burningIndex` | `burning_index` | — |
| `herbaceousLFI_fuelMoisture` | `herbaceous_lfi_fuel_moisture` | — |
| `woodyLFI_fuelMoisture` | `woody_lfi_fuel_moisture` | — |
| `gsi` | `gsi` | — |
| — | `quality_code` | New. Not currently used; worth considering as a filter. |
| — | `fuel_model`, `fuel_model_version` | Useful for the plot subtitle. |

The guide lists `ndfr_date` and `nfdr_time`; those were **not** requested in the
passing query and the `ndfr_` spelling looks like a doc typo. Don't rely on them.

---

## 2. Weather: `download-weather` CSV → `weatherObs`

Note the different date arguments — these are `DateTime!` (required, with time
and `Z`), not `Date`:

```
startDateTimeRange: DateTime!   "2026-08-14T00:00:00Z"
endDateTimeRange: DateTime!     "2026-08-17T23:59:59Z"
```

| app.R column (post-rename) | GraphQL field | Note |
|---|---|---|
| `record_type` | `observation_type` | `"O"` / `"F"`. Different field name than NFDRS's `nfdr_type`. |
| `temperature` | `temperature` | °F |
| `relativeHumidity` | `relative_humidity` | % |
| `precipitation` | `hourly_precip` | in |
| `windSpeed` | `wind_speed` | mph |
| `windDirection` | `wind_direction` | degrees, 0–360 |
| `gustSpeed` | `peak_gust_speed` | mph |
| `gustDirection` | `peak_gust_dir` | **UNITS UNCONFIRMED** — see §4 |
| `solarRadiation` | `sol_rad` | W/m² |
| `vpd` | `vapor_press_def` | **Pa, not kPa** — see §5 |
| `dewpoint` | *(none)* | Keep computing locally. |
| `hdw` | *(none)* | Keep computing locally. |
| — | `snow_flag` | New. |

All the `rename_with(~..., matches(...))` normalization in
`download_weather_data()` becomes unnecessary — GraphQL field names are fixed by
the query, so there's no per-request naming drift to defend against.

---

## 3. Local time is now free — and this fixes `1300LST`

The API returns four time fields:

```
observation_time      2026-08-14T07:08:04.000Z          UTC, actual obs instant
observation_time_lst  2026-08-14T00:08:00.000-07:00     station local, actual
display_hour          2026-08-14T07:00:00.000Z          UTC, rounded to hour
display_hour_lst      2026-08-14T00:00:00.000-07:00     station local, rounded
```

`display_hour_lst` is exactly what the daily aggregation needs: a clean,
station-local hour stamp. So:

```r
date <- as.Date(substr(display_hour_lst, 1, 10))
hour <- as.integer(substr(display_hour_lst, 12, 13))
```

No timezone library, no `lutz` lookup, no parsing ambiguity.

**Worth checking against the current app.** `download_nfdrs_data()` picks its
time column with `grep("observation_time_lst|observationtime|datetime", ...)`
and takes the *first column position* that matches — then parses it as UTC when
the string ends in `Z`. Depending on which column FEMS put first in the CSV,
`hour` may have been a UTC hour rather than a local one. At SAGUARO (MST, −7)
that would make the "1300LST" statistic actually 6 AM local. Before we swap
anything, it's worth pulling one day through both paths and comparing — if the
old numbers were UTC-based, the migration will visibly shift that statistic and
we should say so rather than have it look like a regression.

---

## 4. `peak_gust_dir` — open question

Sample rows showed `wind_direction` at 250 / 60 / 320 while `peak_gust_dir` was
28 / 34 / 22 for the same hours. That pattern (0–36 vs 0–360) is consistent with
gust direction being stored in **tens of degrees**, a common RAWS/WIMS
convention. Probe E in `test_fems_api_2.R` checks the observed range over 30
days. If the max lands near 36, the field needs `× 10` before plotting on a
degree axis.

---

## 5. `vapor_press_def` is the app's own formula, in Pa

Confirmed numerically against three rows:

| T (°F) | RH (%) | app `vpd` (kPa) | × 1000 | API `vapor_press_def` |
|---|---|---|---|---|
| 106 | 17 | 6.4937 | 6493.7 | 6493 |
| 105 | 17 | 6.3054 | 6305.4 | 6305 |
| 105 | 18 | 6.2294 | 6229.4 | 6229 |

Agreement is under 1 Pa, so FEMS uses the same Magnus-Tetens coefficients the
app does and reports the result in pascals.

Two consequences. The local `vpd` calculation can be replaced with
`vapor_press_def / 1000` — and because `hdw` is derived from `vpd`, HDW comes
along unchanged. And whichever way we go, existing charts stay continuous; this
is not a silent units change.

---

## 6. Architecture implications

**Likely deletions.** If probe A shows multi-year requests are accepted, then
`fetch_in_year_chunks()` and `harmonize_chunk_types()` both go away. They exist
solely to work around the CSV endpoints' 1-year cap and readr's per-request type
guessing. GraphQL returns typed JSON and paginates, so neither problem exists.
That is roughly 130 lines of the trickiest code in the app.

**Pagination replaces chunking**, with one caveat: in the first test `nfdrsObs`
returned oldest-first and `weatherObs` newest-first. Paging a result set whose
sort order isn't pinned can drop or duplicate rows across pages, so the download
layer should always send an explicit `sortBy` / `sortOrder`. Probe B confirms
which sort arguments are accepted.

**The join gets cleaner.** Both queries return `station_id` + `display_hour_lst`
+ a type field, so the join key becomes
`station_id / date / hour / record_type` computed from a single unambiguous
source on both sides.

**Possible later wins**, not part of this migration: `stationMetaData` could
replace the static `station_metadata_FEMS3_042225.csv` (it returns `time_zone`
and `time_zone_offset` too, which is what `processMetaData.R` uses `lutz` for),
and `percentileLevels` / `percentileAvgMinMax` could replace locally computed
climatology ribbons.

---

## 7. Deployment note

`.Renviron` is gitignored, so `FEMS_USER` and `FEMS_KEY` must be set as
environment variables on Posit Connect (both `nfdrschartbuilder-dev` and
`nfdrschartbuilder`) before a deploy that depends on them. The app currently
fetches anonymously and will break on deploy if the vars are missing.

Also: regenerating the key in the FEMS UI **invalidates the previous key**. If
the deployed app suddenly 401s, that's the first thing to check.
