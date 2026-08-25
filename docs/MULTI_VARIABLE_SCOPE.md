# Scope — multi-variable comparison tab

Status: scoped 2026-08-18, no code written. Decisions marked **[settled]** came
from Mike; **[open]** ones still need a call.

---

## 1. What we're building

A new **interactive (plotly) tab** **[settled]** that plots **two variables
together for the selected year**, each against its own y-axis.

- Variables: two separate dropdowns, each drawn from the same list the main
  Variable selector uses. Each carries a colour swatch matching its axis.
- Daily statistic: the **existing global control** **[settled]** — no
  per-variable statistic in v1. Precipitation and Burn Period keep their
  automatic overrides.
- Year: `input$plot_year`, as on the other tabs. Observed and forecast both
  drawn.
- Month range: the existing slider applies.
- Smoothing: comes along free — `build_daily_series()` already produces
  `value_smooth`, so the existing sidebar controls work on this tab with no
  extra code.

**Not in scope for v1:** climatology ribbons (see §4), CSV export, a static
ggplot version, per-variable statistics, more than two variables on screen.

---

## 2. Why two and not four

The original ask was up to four variables with "additional y-axes as needed."
Two things pushed against it:

1. **Two y-scales on one plot is the most commonly cited chart mistake.** The
   alignment between the scales is arbitrary, so the chart manufactures a
   correlation that is not in the data. Slide one axis and the apparent
   relationship changes.
2. **ggplot2 supports at most one secondary axis**, and only as a fixed
   transform of the primary. Four axes is not buildable in the static tab at
   all.

Deciding to go **plotly-only** substantially defuses (1). The danger in a
dual-axis chart is readers estimating values by projecting onto an arbitrary
axis; with unified hover giving exact numbers at a date, they read rather than
project. Meteograms are also the canonical form in fire weather — the audience
has read temperature-and-RH-on-one-plot for their whole careers.

**The architecture does not cap at two.** `build_multi_series()` accepts any
number of variables; the dual-axis renderer happens to take exactly two. A
percentile-overlay renderer for three or four is a second renderer over the same
data, not a rewrite. Starting at two forecloses nothing.

---

## 3. Three things the dual axis needs to stay honest

All cheap in plotly, all non-optional:

- **Colour the axis titles and tick labels to match their series.** Standard
  meteogram practice; removes any ambiguity about which line reads against
  which side.
- **Draw gridlines from one axis only.** Two grids implies the scales are
  aligned, which they are not.
- **Do not force the scales to share zero or share breaks.** Anything that makes
  the two axes *look* commensurate reintroduces exactly the problem hover was
  meant to solve.

---

## 4. No climatology ribbons on this tab

Two variables cannot both show percentile bands without recreating the alignment
problem in fill form — two overlapping translucent ribbons on independent scales
is worse than two lines, not better. This tab is a pure current-year comparison;
the main Static and Interactive tabs remain the climatology view.

---

## 5. Code shape

**New file `R/multi_series.R`:**

```r
build_multi_series <- function(all_data, variables, daily_stat, month_range, ...)
```

A loop over `build_daily_series()`, row-bound with a `variable` column. Accepts
any number of variables. Passes smoothing arguments straight through.

**No change to `R/daily_series.R` or to any existing tab.** That is the whole
point: the smoothing refactor made `build_daily_series()` a plain function
taking one variable, so N variables is a loop, and everything it already
guarantees — the cumsum-before-crop rule, the 1300 LST path, station averaging,
gap handling — comes along unchanged.

**New reactive in `app.R`**, separate from `daily_series()` (which is bound to
`input$variable` and stays as it is).

**New `tabPanel`** with a `plotlyOutput`, plus two variable dropdowns in the
sidebar, shown only when that tab is active.

---

## 6. Testing

`tests/test_multi_series.R`, run the same way as the others:

1. **Composition equivalence.** `build_multi_series()` for two variables returns
   exactly what two separate `build_daily_series()` calls return, per variable,
   bit-identical. This is the test that matters — it pins that the loop adds
   nothing and loses nothing.
2. Column contract: `variable, month_day, year, record_type, value, value_smooth`.
3. One variable and three variables both work — no hidden assumption of exactly
   two in the data layer.
4. Smoothing arguments reach each variable: `value_smooth` populated for both
   when smoothing is on, all-NA when off.
5. A variable with no data in the fetched frame fails cleanly rather than
   silently returning a short frame.

Runs against the frozen `tests/logs/baseline_rawdata.rds`, so no network.

---

## 7. Sandbox and promotion

This feature adds a file and a tab and modifies nothing existing, which makes it
the cleanest possible sandbox candidate — **it works identically under either
the branch model or the feature-flag model**, so that decision does not have to
be made first.

- **Flag model:** `experimental/multi_series_tab.R`, sourced only when
  `NFDRS_EXPERIMENTAL` is set; the tab wrapped in `if (EXPERIMENTAL)`.
  Promotion = move the file into `R/`, remove the conditional.
- **Branch model:** `feat/multi-variable` cut from main, deployed to the sandbox
  app. Promotion = merge that branch into main.

Either way promotion is close to trivial, because nothing existing has to be
reconciled.

---

## 8. Open items -- all settled 2026-08-25

1. **[settled]** Dropdown defaults: slot A initializes to whatever the main
   Variable selector currently shows, read ONCE when the selectors are first
   built rather than bound live -- a live binding would make this tab jump
   under the user when they change the main selector. Slot B defaults to
   Relative Humidity, falling back to the first available variable that is not
   slot A.
2. **[settled]** Same variable in both slots: allowed, but detected. With the
   shared global Daily Statistic the two series are identical by construction,
   so a dual-axis render would be a no-op dressed up as a comparison -- worse,
   the arbitrary scale offset would make an identical series look like a lagged
   relationship. The renderer collapses to a single series on a single axis and
   shows a short note. No dropdown filtering: choices shifting under the user is
   a worse cost than a harmless transient state mid-change.
3. **[settled]** Month range: the existing shared slider. A second range control
   would desynchronize this tab from every other one for no gain.
4. **[settled]** CSV export: v1.1 -- not v1, not never. tidyr is already
   attached so `pivot_wider` makes it cheap, but it is a separate deliverable
   from the chart and folding it in widens the first review pass.
5. **[settled]** Tab name: **"Compare Variables"**. Does not hard-code the
   count, so the 3-4 variable percentile-overlay renderer of section 2 can
   arrive under the same tab without a rename.

---

## 9. Three things section 5 missed, found when reading the code

1. **The renderer cannot be `ggplotly()`.** Every plot in the app today is a
   ggplot piped through `ggplotly()`, but ggplot2 supports at most one secondary
   axis and only as a fixed transform of the primary -- which is exactly what
   section 3 forbids. This tab must be written in native `plot_ly()` with
   `yaxis2`. It will be the first native plotly code in the codebase.

2. **"Modifies nothing existing" is not quite true.** Showing the two dropdowns
   only when this tab is active requires an `id` on the `tabsetPanel` in the
   main panel, so a `conditionalPanel` can key off `input.main_tabs`. One line,
   but a real edit to `app.R`.

3. **The `precip_cum` rolling-sum guard is variable-aware.** The
   `observeEvent(input$variable, ...)` in `app.R` strips "Rolling sum" from the
   filter menu when Cumulative Precipitation is selected, because a rolling sum
   of an already-cumulative series is meaningless. If either new slot holds
   `precip_cum`, that guard has to see it. This is an interaction with existing
   code and gets its own build step and its own diff.

---

## 10. Sandbox decision -- `feat/multi-variable` branch [settled 2026-08-25]

**Chosen: the branch model, no feature flag.** This matches the model already
written down in `reference_deploy_and_git` -- main stable and always
deployable, short-lived feature branches off main, merged back and deleted.
A flag was considered and rejected as a second mechanism layered on top of a
discipline that has to hold anyway.

**What the flag would have mitigated, and what replaces it.** Republish deploys
the working directory, not a git ref. With the branch checked out, ANY Republish
sends the in-progress tab wherever it is pointed; a flag would have made that
inert on prod because `NFDRS_EXPERIMENTAL` is unset there. Without it, the rule
has to hold by hand:

> **Branch checked out -> only ever Republish to the dev app. Prod deploys
> happen only from a clean `main`.**

**Checkout hazard, assessed.** This branch only ADDS `R/multi_series.R`,
`R/multi_series_plot.R` and `tests/test_multi_series.R`, and modifies `app.R`.
Returning to `main` deletes three files but no directories, and `R/` survives
either way because it holds other files. The 2026-08-18 failure was a
*directory* removal ("Deletion of directory 'R' failed"), so that specific trap
does not apply. Close the R session before switching anyway -- RStudio holding a
handle on a sourced file was the other suspected cause and is independent of
OneDrive.

**File layout** (unchanged by this decision -- the reasoning is testability, not
sandboxing):

- `R/multi_series.R` -- `build_multi_series()`.
- `R/multi_series_plot.R` -- `plot_multi_series()`, a plain function returning a
  plotly object. Same reasoning that made `build_daily_series()` a plain
  function rather than a reactive: a renderer buried inside `renderPlotly()`
  cannot be called outside a Shiny session, so it could never be tested.

The `experimental/` directory sketched in section 7 is not used.

**Promotion:** merge `feat/multi-variable` into `main`, delete the branch local
and remote. Per `reference_deploy_and_git`, run
`git log --oneline feat/multi-variable..main` before merging -- this repo's
branches have diverged silently before.
