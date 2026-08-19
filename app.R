# ==============================================================================
# Application: NFDRS Chart Builder App ----  DEVELOPMENT VERSION
# Author: Mike Crimmins (crimmins@arizona.edu) / Gemini Pro / Claude
# Date: April 2026 --- UPDATED Aug 13 2026
# Description: An interactive R/Shiny application for visualizing daily fire
#              weather indices, meteorological variables, and percentile
#              climatologies from the National Fire Danger Rating System (NFDRS).
#              Includes tools for both historical baseline comparisons and
#              Year-to-Date (YTD) anomaly tracking.
#
# Data Sources:
#   - API endpoints: https://fems.fs2c.usda.gov/api/
#   - Metadata updates: https://www.wildfire.gov/node/3473 or
#                       https://fems.fs2c.usda.gov/download
#
# Organization: University of Arizona (https://cales.arizona.edu/climate/)
# ==============================================================================
library(shiny)
library(leaflet)
library(dplyr)
library(tidyr)
library(ggplot2)
library(purrr)
library(tibble)
library(bslib)
library(plotly)
library(RColorBrewer)
library(DT)
library(rlang)
library(httr)
library(readr)
library(lubridate)
library(grid)
# -----------------------------
# Helper Functions
# -----------------------------
#####
# safe_summary1() moved to R/daily_series.R so the aggregation chain and its
# helper travel together and tests/ can source them without app.R.
##### API downloads -----
# ------------------------------------------------------------------
# Data access now lives in R/fems_download.R, which queries the FEMS
# Read-Only GraphQL API (authenticated) instead of the old public CSV
# download endpoints.
#
# Two functions were retired along with those endpoints:
#
#   fetch_in_year_chunks()  -- the CSV endpoints capped requests at one year
#       per station, so the range had to be split into calendar years and
#       reassembled. The GraphQL API has no such cap: a 21-year request for
#       one station returns 184,098 rows without complaint. Nothing to chunk.
#
#   harmonize_chunk_types() -- existed only because readr guessed column
#       types independently for each CSV request, so a column that was empty
#       in one year came back logical and collided with the same column as
#       numeric in another. JSON is typed, so the problem cannot arise.
#
# The new functions return the same column names the rest of this file
# already uses, so nothing downstream of all_data_cache() changed.
#
# Requires FEMS_USER and FEMS_KEY in .Renviron, and the same two as
# environment variables on Posit Connect. See README.md.
# ------------------------------------------------------------------
source("R/fems_download.R")
source("R/daily_series.R")

# Start of the fetch window. 2005 rather than the previous 2004 because the
# climatology baseline, the plot-year input, and every downstream filter all
# begin at 2005 -- the 2004 rows were downloaded and then discarded.
FETCH_START_DATE <- "2005-01-01"
#####
pretty_variable_name <- function(var) {
  # Special bypass for our custom computed variables
  if(var == "precip_total") return("Precip Total")
  if(var == "precip_cum") return("Cumulative Precip")
  if(var == "burn_period") return("Burn Period")
  
  gsub("([a-z])([A-Z])", "\\1 \\2", var) |>
    tools::toTitleCase()
}
reverse_fill_vars <- c(
  "oneHR_TL_FuelMoisture",
  "tenHR_TL_FuelMoisture",
  "hundredHR_TL_FuelMoisture",
  "thousandHR_TL_FuelMoisture",
  "woodyLFI_fuelMoisture",
  "herbaceousLFI_fuelMoisture",
  "relativeHumidity",
  "precip_cum","precip_total"
)
# -----------------------------
# Load Station Metadata
# -----------------------------
station_metadata <- read.csv("data/station_metadata_FEMS3_042225.csv", stringsAsFactors = FALSE)
# -----------------------------
# UI
# -----------------------------
ui <- fluidPage(
  tags$head(
    HTML("
      <script async src='https://www.googletagmanager.com/gtag/js?id=G-JJ5NSHGJE6'></script>
      <script>
        window.dataLayer = window.dataLayer || [];
        function gtag(){dataLayer.push(arguments);}
        gtag('js', new Date());
        gtag('config', 'G-JJ5NSHGJE6');
      </script>
    "),
    tags$style(HTML("
      .shiny-output-error-validation {
        color: #6c757d; font-size: 24px; text-align: center; margin-top: 150px; font-weight: 500;
      }
      @keyframes pulse {
        0% { transform: scale(1); box-shadow: 0 0 0 0 rgba(217, 83, 79, 0.7); }
        70% { transform: scale(1.02); box-shadow: 0 0 0 10px rgba(217, 83, 79, 0); }
        100% { transform: scale(1); box-shadow: 0 0 0 0 rgba(217, 83, 79, 0); }
      }
      .btn-warning-pulse {
        animation: pulse 2s infinite;
        background-color: #d9534f !important;
        border-color: #d43f3a !important;
        color: white !important;
      }
    "))
  ),
  theme = bs_theme(bootswatch = "lumen"),
  tags$div(
    style = "display: flex; align-items: center; justify-content: space-between; margin-bottom: 20px;",
    tags$h1("NFDRSv4 Chart Builder -- Experimental"),
    tags$img(src = "UAlogo.jpg", height = "60px")
  ),
  sidebarLayout(
    sidebarPanel(
      # --- DATA FETCHING GROUP ---
      leafletOutput("station_map", height = 300), # Reduced height to save vertical space
      
      uiOutput("station_selector"),
      div(style = "font-size: 0.85em; margin-top: -10px; margin-bottom: 15px; color: #555; line-height: 1.3;",
          "Click the map or dropdown to add/remove stations. ", tags$br(),
          tags$b("Note:"), " Selecting multiple stations will aggregate their data into a single average."),
      
      selectInput("fuel_model", "Fuel Model", choices = c("Y","Z")),
      
      # Fetch button moved directly below API parameters
      uiOutput("dynamic_fetch_button"),
      
      # Visual separator
      hr(style = "border-top: 2px solid #b0b0b0; margin-top: 25px; margin-bottom: 20px;"),
      
      # --- PLOT SETTINGS GROUP ---
      h4("📈 Plot Settings", style = "margin-bottom: 15px; font-weight: bold; font-size: 1.1em; color: #444;"),
      
      uiOutput("variable_selector"),
      
      selectInput("daily_stat", "Daily Statistic",
                  choices = c("mean", "min", "max", "1300LST")),
      
      sliderInput("month_range", "Plot Month Range",
                  min = 1, max = 12, value = c(1, 12), step = 1),
      
      numericInput("plot_year", "Plot Year (2005-Present)",
                   value = as.numeric(format(Sys.Date(), "%Y")),
                   min = 2005, max = as.numeric(format(Sys.Date(), "%Y"))),
      
      checkboxInput("show_forecast", "Plot Current Forecast", value = TRUE),
      
      # --- SMOOTHED OVERLAY ---
      checkboxInput("smooth_on", "Overlay a smoothed line", value = FALSE),
      
      conditionalPanel(
        condition = "input.smooth_on == true",
        div(
          style = "margin-left: 12px; padding-left: 12px; border-left: 3px solid #ddd;",
          
          selectInput("smooth_fun", "Filter",
                      choices = c("Rolling mean" = "mean",
                                  "Rolling sum" = "sum",
                                  "Rolling median" = "median"),
                      selected = "mean"),
          
          radioButtons("smooth_align", "Window position",
                       choices = c("Centered" = "center", "Trailing" = "right"),
                       selected = "center", inline = TRUE),
          
          # Odd widths only, so a centered window is symmetric about its day.
          sliderInput("smooth_window", "Window (days)",
                      min = 3, max = 61, value = 7, step = 2),
          
          uiOutput("smooth_note")
        )
      ),
      
      hr(style = "margin-top: 20px; margin-bottom: 20px;"),
      
      # --- FOOTER ---
      div(
        style = "text-align: center; color: #666; font-size: 0.9em;",
        tags$img(src = "BP_app_logos.png", style = "width: 100%; height: auto; margin-bottom: 10px;"),
        tags$p("Contact: Mike Crimmins, crimmins@arizona.edu", style = "margin-bottom: 2px;"),
        tags$a(href="https://cales.arizona.edu/climate/", target="_blank", "https://cales.arizona.edu/climate/")
      )
    ),
    mainPanel(
      uiOutput("stale_data_banner"),
      
      tabsetPanel(
        #tabPanel("Static Plot", plotOutput("climatology_plot", height = "700px")),
        tabPanel("Static Plot",
                 plotOutput("climatology_plot", height = "600px"),
                 br(),
                 div(style = "text-align: center; margin-top: 10px; margin-bottom: 20px;",
                     downloadButton("download_plot_data", "Download Plot Data (CSV)", class = "btn-primary")
                 )
        ),
        tabPanel("Interactive Plot",
                 div(
                   plotlyOutput("plotly_climatology_plot", height = "700px"),
                   br(),
                   checkboxInput("show_hist_years", "Show Historic Years", value = FALSE)
                 )
        ),
        tabPanel("Summary Stats", DTOutput("summary_table")),
        tabPanel("About",
                 div(style = "padding: 20px; max-width: 900px;",
                     
                     h3("🔥 NFDRSv4 Chart Builder"),
                     p("An interactive tool for visualizing daily fire weather indices, meteorological variables, and percentile climatologies from the National Fire Danger Rating System (NFDRS). This is an experimental product -- corrections and suggestions are welcome through the Submit Feedback tab."),
                     
                     hr(style = "margin-top: 20px; margin-bottom: 20px;"),
                     
                     h4("🛠️ How to Use"),
                     tags$ol(
                       tags$li("Select one or more stations from the map or the dropdown. Multiple stations are averaged into a single series."),
                       tags$li("Choose a fuel model (Y or Z) and click ", tags$b("Fetch Station Data"), ". If you change the station or fuel model afterwards, the button turns red until you fetch again."),
                       tags$li("Pick a variable. The list reflects what was fetched, and covers NFDRS indices (ERC, BI, KBDI, fuel moistures) as well as weather variables (temperature, RH, wind, VPD, HDW)."),
                       tags$li("Pick a daily statistic and a year. ", tags$b("Note:"), " Precipitation and Burn Period ignore the statistic and compute daily totals or hour counts instead."),
                       tags$li("Use the ", tags$b("Month Range"), " slider to focus on a season. The Summary Stats table recalculates to match."),
                       tags$li("Optionally tick ", tags$b("Overlay a smoothed line"), " to lay a rolling filter over the selected year. How it behaves at the ends of the series is described below.")
                     ),
                     
                     hr(style = "margin-top: 20px; margin-bottom: 20px;"),
                     
                     h4("📊 Data & Methodology"),
                     p("All observations and forecasts come from the ",
                       tags$a(href="https://fems.fs2c.usda.gov/api/", target="_blank", "USDA Forest Service FEMS API"), "."),
                     tags$ul(
                       tags$li(tags$b("Historical baseline:"), " Percentile ribbons, dashed thresholds and the normal average use a 21-year baseline, ", tags$b("2005 to 2025"), "."),
                       tags$li(tags$b("Derived metrics:"), " Standard NFDRS indices come from FEMS. Burn Period (hours per day with RH below 20%) and cumulative precipitation are computed here from the hourly weather feed."),
                       tags$li(tags$b("Smoothed line:"), " The optional overlay applies a rolling mean, sum or median to the selected year's observed values only -- the climatology mean and the percentile bands are never smoothed. The unsmoothed daily series stays visible as a thin grey line, and the filter in use is named in the plot subtitle."),
                       tags$li(tags$b("Smoothing at the ends:"), " A smoothed value appears only where a complete window of observations exists. A centered window therefore stops short of the most recent day by half its width, so a 31-day centered mean ends about 15 days back. Choose a trailing window if you need the line to reach today, accepting that it lags behind a change. Gaps in the record are left blank rather than filled from a partial window.")
                     ),
                     
                     p(style = "color: #888; font-size: 0.85em; margin-top: 20px;",
                       "Last updated August 2026.")
                 )
        ),
        
        tabPanel("Submit Feedback", icon = icon("comment-dots"),
                 div(style = "max-width: 800px; margin: 0 auto; padding-top: 30px;",
                     
                     h3("📝 App Feedback & Bug Reports", style = "text-align: center; color: #444;"),
                     p("Please use the form below to share any issues you encounter, features you'd like to see, or general feedback about the NFDRSv4 Chart Builder.",
                       style = "text-align: center; color: #666; margin-bottom: 20px;"),
                     
                     # Paste the URL from your Google Form inside the quotes below!
                     # Only copy the URL part (the part inside src="...") from the Google Embed code
                     tags$iframe(
                       src = Sys.getenv("FEEDBACK_FORM_URL"),
                       width = "100%",
                       height = "800px",
                       frameborder = "0",
                       marginheight = "0",
                       marginwidth = "0",
                       style = "border: none; border-radius: 8px; box-shadow: 0px 4px 10px rgba(0,0,0,0.1);"
                     )
                 )
        )
        
      )
    )
  )
)
# -----------------------------
# Server
# -----------------------------
server <- function(input, output, session) {
  
  default_station_id <- "21202"
  selected_stations <- reactiveVal(default_station_id)
  
  data_cache <- reactiveValues()
  weather_data_cache <- reactiveValues()
  all_data_cache <- reactiveVal(NULL)
  
  fetched_fuel_model <- reactiveVal(NULL)
  data_outdated <- reactiveVal(FALSE)
  
  observeEvent(c(input$station_ids, input$fuel_model), {
    if (!is.null(all_data_cache())) {
      data_outdated(TRUE)
    }
  }, ignoreInit = TRUE)
  
  output$dynamic_fetch_button <- renderUI({
    if (data_outdated()) {
      actionButton("fetch_data", "⚠️ FETCH NEW DATA",
                   class = "btn-warning-pulse", width = "100%",
                   style = "font-weight: bold; font-size: 16px; padding: 12px;")
    } else {
      actionButton("fetch_data", "Fetch Station Data",
                   class = "btn-primary", width = "100%",
                   style = "font-weight: bold;")
    }
  })
  
  output$stale_data_banner <- renderUI({
    if (data_outdated()) {
      div(class = "alert alert-warning",
          style = "font-size: 16px; font-weight: bold; text-align: center; margin-bottom: 15px;",
          "⚠️ Station selection or Fuel Model has changed. The charts below are showing old data. Click 'Fetch New Data' to update.")
    }
  })
  
  output$station_selector <- renderUI({
    selectizeInput("station_ids", "Selected Station(s)",
                   choices = setNames(station_metadata$station_id, station_metadata$station_name),
                   selected = selected_stations(),
                   multiple = TRUE,
                   options = list(placeholder = "Click to select one or more..."))
  })
  
  output$station_map <- renderLeaflet({
    leaflet(station_metadata) |>
      addProviderTiles(providers$Esri.WorldTopoMap, group="topomap") |>
      addCircleMarkers(
        lng = ~longitude, lat = ~latitude,
        layerId = ~station_id,
        label = ~station_name,
        radius = 5, color = "blue", fillOpacity = 0.5
      )
  })
  
  observeEvent(input$station_map_marker_click, {
    click_id <- input$station_map_marker_click$id
    current <- selected_stations()
    new_selection <- if (click_id %in% current) setdiff(current, click_id) else c(current, click_id)
    selected_stations(new_selection)
    updateSelectInput(session, "station_ids", selected = new_selection)
  })
  
  observeEvent(input$station_ids, {
    selected_stations(input$station_ids)
    sel_data <- station_metadata %>% filter(station_id %in% input$station_ids)
    
    leafletProxy("station_map") %>%
      clearMarkers() %>%
      addCircleMarkers(
        data = station_metadata,
        lng = ~longitude, lat = ~latitude,
        layerId = ~station_id, label = ~station_name,
        radius = 5, color = ~ifelse(station_id %in% input$station_ids, "red", "blue"),
        fillOpacity = 0.5
      )
    
    if (nrow(sel_data) == 1) {
      leafletProxy("station_map") %>% setView(lng = sel_data$longitude, lat = sel_data$latitude, zoom = 7)
    } else if (nrow(sel_data) > 1) {
      leafletProxy("station_map") %>% fitBounds(min(sel_data$longitude), min(sel_data$latitude), max(sel_data$longitude), max(sel_data$latitude))
    }
  })
  
  observeEvent(input$fetch_data, {
    req(input$fuel_model)
    stns <- selected_stations()
    req(length(stns) > 0)

    fm <- input$fuel_model

    # Work out what is actually missing. The two feeds are cached separately
    # on purpose: weather does not depend on fuel model, so switching Y <-> Z
    # re-fetches only the NFDRS side.
    need_nfdrs <- stns[vapply(stns, function(id)
      is.null(data_cache[[paste(id, fm, sep = "_")]]), logical(1))]
    need_wx <- stns[vapply(stns, function(id)
      is.null(weather_data_cache[[id]]), logical(1))]

    to_fetch <- union(need_nfdrs, need_wx)
    failed   <- character(0)

    # Friendly names for the progress readout.
    station_label <- function(id) {
      nm <- station_metadata$station_name[match(id, station_metadata$station_id)]
      if (is.na(nm)) id else nm
    }

    withProgress(message = "Downloading & assembling data...", value = 0, {

      # Stations are downloaded ONE AT A TIME, not batched. FEMS rejects a
      # local-station-time query covering more than one station, and local
      # time is what makes date/hour correct. Fetching serially also means
      # each station is cached the moment it completes, so a failure part way
      # through does not discard the stations already retrieved.
      n <- length(to_fetch)
      for (i in seq_along(to_fetch)) {
        id  <- to_fetch[i]
        nm  <- station_label(id)
        base <- (i - 1) / n

        setProgress(value = base,
                    detail = paste0(nm, " (", i, " of ", n, "): NFDRS"))

        ok <- TRUE

        if (id %in% need_nfdrs) {
          df <- tryCatch(
            fems_download_nfdrs(id, FETCH_START_DATE, Sys.Date() + 7,
                                fuel_model = fm, verbose = FALSE),
            error = function(e) {
              showNotification(paste0("NFDRS download failed for ", nm, ": ",
                                      conditionMessage(e)),
                               type = "error", duration = 10)
              NULL
            })
          if (is.null(df) || nrow(df) == 0) {
            ok <- FALSE
          } else {
            data_cache[[paste(id, fm, sep = "_")]] <- df
          }
        }

        if (ok && id %in% need_wx) {
          setProgress(value = base + 0.5 / n,
                      detail = paste0(nm, " (", i, " of ", n, "): weather"))
          df <- tryCatch(
            fems_download_weather(id, FETCH_START_DATE, Sys.Date() + 7,
                                  verbose = FALSE),
            error = function(e) {
              showNotification(paste0("Weather download failed for ", nm, ": ",
                                      conditionMessage(e)),
                               type = "error", duration = 10)
              NULL
            })
          if (is.null(df) || nrow(df) == 0) {
            ok <- FALSE
          } else {
            weather_data_cache[[id]] <- df
          }
        }

        if (!ok) failed <- c(failed, id)
      }

      setProgress(value = 1, detail = "Assembling")

      # A station is usable only if BOTH feeds are present, since the plots
      # read NFDRS indices and weather variables from the same joined frame.
      usable <- stns[vapply(stns, function(id)
        !is.null(data_cache[[paste(id, fm, sep = "_")]]) &&
        !is.null(weather_data_cache[[id]]), logical(1))]
      failed <- setdiff(stns, usable)

      if (length(usable) == 0) {
        showNotification("No data could be retrieved for the selected station(s).",
                         type = "error", duration = 10)
        return(NULL)
      }
      if (length(failed) > 0) {
        showNotification(paste("No data for station(s):",
                               paste(vapply(failed, station_label, character(1)),
                                     collapse = ", "),
                               "- charts show the remaining station(s)."),
                         type = "warning", duration = 10)
      }

      all_nfdrs <- bind_rows(lapply(usable, function(id)
        data_cache[[paste(id, fm, sep = "_")]]))
      all_weather <- bind_rows(lapply(usable, function(id)
        weather_data_cache[[id]]))

      all_data <- left_join(all_nfdrs, all_weather,
                            by = c("station_id", "date", "hour", "record_type"))

      all_data_cache(all_data)
      fetched_fuel_model(fm)
      data_outdated(FALSE)
    })
  })
  
  ##### var labels ----
  nfdrs_labels <- c("Energy Release Component (ERC)" = "energyReleaseComponent", "Burning Index (BI)" = "burningIndex",
                    "Ignition Component (IC)" = "ignitionComponent", "Spread Component (SC)" = "spreadComponent",
                    "Keetch-Byram Drought Index" = "kbdi", "1-hr Fuel Moisture" = "oneHR_TL_FuelMoisture",
                    "10-hr Fuel Moisture" = "tenHR_TL_FuelMoisture", "100-hr Fuel Moisture" = "hundredHR_TL_FuelMoisture",
                    "1000-hr Fuel Moisture" = "thousandHR_TL_FuelMoisture", "Live Woody Fuel Moisture" = "woodyLFI_fuelMoisture",
                    "Live Herbaceous Fuel Moisture" = "herbaceousLFI_fuelMoisture", "Growing Season Index" = "gsi")
  
  # NEW: Added custom aggregations to Weather variables list
  weather_var_labels <- c("Temperature (°F)" = "temperature",
                          "Dewpoint (°F)" = "dewpoint",
                          "Relative Humidity (%)" = "relativeHumidity",
                          "Wind Speed (mph)" = "windSpeed", "Wind Gust (mph)" = "gustSpeed",
                          "Wind Direction (°)" = "windDirection", "Gust Direction (°)" = "gustDirection",
                          "Solar Radiation (W/m²)" = "solarRadiation", "Hourly Precipitation (in)" = "precipitation",
                          "Vapor Pressure Deficit (kPa)" = "vpd", "Hot-Dry-Windy Index" = "hdw",
                          "Daily Total Precipitation (in)" = "precip_total",
                          "Cumulative Precipitation (in)" = "precip_cum",
                          "Burn Period (Hours RH < 20%)" = "burn_period")
  
  weather_vars <- unname(weather_var_labels)
  
  output$variable_selector <- renderUI({
    req(all_data_cache())
    raw_available <- names(all_data_cache())[sapply(all_data_cache(), is.numeric)]
    
    # Inject computed variables into available list if their base variables exist
    if ("precipitation" %in% raw_available) raw_available <- c(raw_available, "precip_total", "precip_cum")
    if ("relativeHumidity" %in% raw_available) raw_available <- c(raw_available, "burn_period")
    
    display_vars <- c(nfdrs_labels, weather_var_labels)
    display_vars <- display_vars[display_vars %in% raw_available]
    selectInput("variable", "Select Variable", choices = display_vars, selected = display_vars[1])
  })
  
  # ---------------------------------------------------------------------
  # Shared daily series
  # ---------------------------------------------------------------------
  # One source of truth for the daily aggregation. Deliberately a one-line
  # wrapper: all the logic is in build_daily_series() (R/daily_series.R) so
  # that tests/ can call it without a Shiny session.
  daily_series <- reactive({
    req(all_data_cache(), input$variable, input$daily_stat, input$month_range)
    build_daily_series(
      all_data_cache(), input$variable, input$daily_stat, input$month_range,
      smooth        = isTRUE(input$smooth_on),
      smooth_fun    = input$smooth_fun    %||% "mean",
      smooth_window = input$smooth_window %||% 7,
      smooth_align  = input$smooth_align  %||% "center"
    )
  })
  
  # A rolling SUM of an already-cumulative series is meaningless, so it is taken
  # off the menu for Cumulative Precipitation rather than left there to be
  # picked by accident.
  observeEvent(input$variable, {
    full    <- c("Rolling mean" = "mean", "Rolling sum" = "sum", "Rolling median" = "median")
    allowed <- if (identical(input$variable, "precip_cum")) full[full != "sum"] else full
    keep    <- if (isTRUE(input$smooth_fun %in% allowed)) input$smooth_fun else "mean"
    updateSelectInput(session, "smooth_fun", choices = allowed, selected = keep)
  }, ignoreNULL = TRUE)
  
  # Spell out what the current settings cost, so nobody has to work out for
  # themselves why the smoothed line stops short of today.
  output$smooth_note <- renderUI({
    req(input$smooth_window, input$smooth_align)
    lag <- (as.integer(input$smooth_window) - 1L) %/% 2L
    msg <- if (identical(input$smooth_align, "center")) {
      paste0("Centered: the line stops ", lag, " days short of each end, today included. ",
             "Edges are left blank rather than computed from a partial window.")
    } else {
      paste0("Trailing: the window ends on the plotted day, so the line reaches today ",
             "but lags a turn by about ", lag, " days.")
    }
    extra <- if (identical(input$variable, "precip_cum")) {
      " Rolling sum is unavailable here -- the series is already a running total."
    } else ""
    div(style = "font-size: 0.8em; color: #666; line-height: 1.35; margin-top: -8px;",
        msg, extra)
  })
  
  # Human-readable description of the active filter, e.g. "33-day centered
  # median", or NULL when smoothing is off. Lives in the subtitle of the static
  # plot and the axis label of the interactive one, so a saved image says what
  # was done to the line without the legend having to carry it.
  smooth_spec <- reactive({
    if (!isTRUE(input$smooth_on)) return(NULL)
    paste0(input$smooth_window %||% 7, "-day ",
           if (identical(input$smooth_align %||% "center", "center")) "centered" else "trailing",
           " ", input$smooth_fun %||% "mean")
  })
  
  # Plot rendering
  output$climatology_plot <- renderPlot({
    validate(need(!is.null(all_data_cache()), "Welcome! Please select your station(s) and click 'Fetch Station Data' to generate the chart."))
    req(input$variable, input$daily_stat, input$plot_year, fetched_fuel_model())
    
    all_data <- all_data_cache()
    
    plotted_station_ids <- unique(all_data$station_id)
    station_names <- station_metadata %>%
      filter(station_id %in% plotted_station_ids) %>%
      pull(station_name) %>% unique()
    station_label <- paste(station_names, collapse = ", ")
    
    # Daily aggregation -- shared with the interactive plot and the CSV
    # export. Lived here as one of three verbatim copies until now.
    all_data_sig <- daily_series()
    
    all_data_hist <- all_data_sig %>% filter(record_type == "O")
    
    historical_years <- all_data_hist %>% filter(year >= 2005 & year <= 2025) %>%
      summarise(start_year = min(year, na.rm = TRUE), end_year = max(year, na.rm = TRUE))
    
    clim_df <- all_data_hist %>% filter(year >= 2005 & year <= 2025) %>%
      group_by(month_day) %>%
      summarise(min = min(value, na.rm = TRUE), max = max(value, na.rm = TRUE),
                mean = mean(value, na.rm = TRUE), median = median(value, na.rm = TRUE), .groups = "drop")
    
    df_hist <- all_data_hist %>% filter(year >= 2005 & year <= 2025)
    
    p90_global <- quantile(df_hist$value, 0.90, na.rm = TRUE)
    p97_global <- quantile(df_hist$value, 0.97, na.rm = TRUE)
    p50_global <- quantile(df_hist$value, 0.50, na.rm = TRUE)
    p25_global <- quantile(df_hist$value, 0.25, na.rm = TRUE)
    
    ribbon_df <- df_hist %>%
      group_by(month_day) %>%
      summarise(q0 = quantile(value, 0.00, na.rm = TRUE), q33 = quantile(value, 0.33, na.rm = TRUE),
                q66 = quantile(value, 0.66, na.rm = TRUE), q90 = quantile(value, 0.90, na.rm = TRUE),
                q97 = quantile(value, 0.97, na.rm = TRUE), q100 = quantile(value, 1.00, na.rm = TRUE), .groups = "drop")
    
    ribbon_data <- tibble(
      range = c("0–33%", "33–66%", "66–90%", "90–97%", "97–100%"),
      ymin = c("q0", "q33", "q66", "q90", "q97"),
      ymax = c("q33", "q66", "q90", "q97", "q100")
    ) %>% pmap_dfr(function(range, ymin, ymax) {
      ribbon_df %>% transmute(month_day, ymin = .data[[ymin]], ymax = .data[[ymax]], range = range)
    })
    
    df_current_obs <- all_data_sig %>% filter(year == input$plot_year, record_type == "O") %>%
      group_by(month_day) %>% summarise(value = mean(value, na.rm = TRUE),
                                        value_smooth = first(value_smooth), .groups = "drop")
    df_current_fcst <- all_data_sig %>% filter(year == input$plot_year, record_type == "F") %>%
      group_by(month_day) %>% summarise(value = mean(value, na.rm = TRUE), .groups = "drop")
    
    default_fill <- c("0–33%" = "#cce5ff", "33–66%" = "#e6f2ff", "66–90%" = "#ffe0b2", "90–97%" = "#ffcc80", "97–100%" = "#ff9933")
    moisture_fill <- c("0–33%" = "#ff9933", "33–66%" = "#ffcc80", "66–90%" = "#ffe0b2", "90–97%" = "#e6f2ff", "97–100%" = "#cce5ff")
    fill_values <- if (input$variable %in% reverse_fill_vars) moisture_fill else default_fill
    
    # The legend key stays short; the filter is spelled out in the subtitle so
    # it does not squeeze the plot panel.
    obs_label <- paste0(input$plot_year, " Observed")
    
    color_mapping <- setNames(c("blue", "orangered", "forestgreen"),
                              c("Mean", obs_label, paste0(input$plot_year, " Forecast")))
    
    # Hide the daily_stat suffix for our total/count variables
    y_axis_label <- if (input$variable %in% c("precip_total", "burn_period", "precip_cum")) {
      pretty_variable_name(input$variable)
    } else {
      paste(pretty_variable_name(input$variable), "(", input$daily_stat, ")")
    }
    
    p <- ggplot() +
      geom_ribbon(data = ribbon_data, aes(x = month_day, ymin = ymin, ymax = ymax, fill = range), alpha = 0.7) +
      geom_line(data = clim_df, aes(x = month_day, y = mean, color = "Mean"), linewidth = 0.6) +
      geom_hline(yintercept = c(p25_global, p50_global, p90_global, p97_global), color = "gray40", linetype = "dashed") +
      annotate("text", x = min(ribbon_data$month_day, na.rm = T) + 2, y = p90_global, label = "90%", hjust = 0, vjust = -0.5, size = 3, color = "gray40") +
      annotate("text", x = min(ribbon_data$month_day, na.rm = T) + 2, y = p97_global, label = "97%", hjust = 0, vjust = -0.5, size = 3, color = "gray40") +
      annotate("text", x = min(ribbon_data$month_day, na.rm = T) + 2, y = p50_global, label = "50%", hjust = 0, vjust =-0.5, size = 3, color = "gray40") +
      annotate("text", x = min(ribbon_data$month_day, na.rm = T) + 2, y = p25_global, label = "25%", hjust = 0, vjust = -0.5, size = 3, color = "gray40") +
      scale_x_date(date_labels = "%b", date_breaks = "1 month", expand = expansion(mult = c(0, 0))) +
      scale_fill_manual("Daily %tile Range", values = fill_values) +
      scale_color_manual(name = NULL, values = color_mapping) +
      labs(
        title = if (input$variable %in% weather_vars) {
          names(weather_var_labels)[match(input$variable, weather_var_labels)]
        } else {
          paste0(pretty_variable_name(input$variable), " (Fuel Model ", fetched_fuel_model(), ")")
        },
        subtitle = paste0(station_label, " | ", input$plot_year,
                          " vs Climatology (", historical_years$start_year, "–", historical_years$end_year, ")",
                          if (is.null(smooth_spec())) "" else paste0(" | ", smooth_spec())),
        x = "Month-Day",
        y = y_axis_label,
        caption = if (is.null(smooth_spec())) {
          "EXPERIMENTAL PRODUCT -- University of Arizona -- Data from FEMS-API"
        } else {
          paste0("Thin grey line: unsmoothed daily values.  ",
                 "EXPERIMENTAL PRODUCT -- University of Arizona -- Data from FEMS-API")
        }
      ) +
      #... your existing ggplot code ... +
      annotation_custom(
        textGrob("EXPERIMENTAL",
                 gp = gpar(col = "red", alpha = 0.15, fontsize = 80, fontface = "bold"))
      )+
      theme_bw(base_size = 14)
    
    if (nrow(df_current_obs) > 0) {
      if (isTRUE(input$smooth_on)) {
        # The raw series stays visible underneath so the smoothing reads as
        # smoothing. Neutral grey, and its colour is set OUTSIDE aes() so it
        # neither fights the orange percentile bands nor adds a legend row.
        p <- p + geom_line(data = df_current_obs,
                           aes(x = month_day, y = value),
                           colour = "gray30", linewidth = 0.35, alpha = 0.65) +
                 geom_line(data = filter(df_current_obs, !is.na(value_smooth)),
                           aes(x = month_day, y = value_smooth, color = obs_label),
                           linewidth = 1.4)
      } else {
        p <- p + geom_line(data = df_current_obs, aes(x = month_day, y = value, color = obs_label), linewidth = 1.2)
      }
    }
    if (input$show_forecast && nrow(df_current_fcst) > 0) {
      p <- p + geom_line(data = df_current_fcst, aes(x = month_day, y = value, color = paste0(input$plot_year, " Forecast")), linewidth = 1.2, linetype = "solid")
    }
    
    return(p)
  })
  
  ##### plotly version
  output$plotly_climatology_plot <- renderPlotly({
    validate(need(!is.null(all_data_cache()), "Welcome! Please select your station(s) and click 'Fetch Station Data' to generate the chart."))
    req(input$variable, input$daily_stat, input$plot_year, fetched_fuel_model())
    
    # Daily aggregation -- shared with the static plot and the CSV export.
    all_data_sig <- daily_series()
    
    all_data_hist <- all_data_sig %>% filter(record_type == "O")
    
    df_hist_all <- all_data_hist %>% filter(year >= 2005 & year <= 2025) %>%
      mutate(year_str = as.character(year)) %>% group_by(year_str, month_day) %>%
      summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
      mutate(text = paste("Year:", year_str, "<br>Date:", format(month_day, "%b-%d"), "<br>Value:", round(value, 1)))
    
    clim_df <- df_hist_all %>% group_by(month_day) %>% summarise(mean = mean(value, na.rm = TRUE), .groups = "drop") %>%
      mutate(text = paste("Date:", format(month_day, "%b-%d"), "<br>Mean:", round(mean, 1)))
    
    df_current_obs <- all_data_sig %>% filter(year == input$plot_year, record_type == "O") %>%
      group_by(month_day) %>% summarise(value = mean(value, na.rm = TRUE),
                                        value_smooth = first(value_smooth), .groups = "drop") %>%
      mutate(text    = paste0("Date: ", format(month_day, "%b-%d"),
                              "<br>Observed: ", round(value, 1)),
             text_sm = paste0("Date: ", format(month_day, "%b-%d"),
                              "<br>Smoothed: ", round(value_smooth, 1)))
    
    df_current_fcst <- all_data_sig %>% filter(year == input$plot_year, record_type == "F") %>%
      group_by(month_day) %>% summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
      mutate(text = paste("Date:", format(month_day, "%b-%d"), "<br>Forecast:", round(value, 1)))
    
    ribbon_df <- df_hist_all %>% group_by(month_day) %>%
      summarise(q0 = quantile(value, 0.00, na.rm = TRUE), q33 = quantile(value, 0.33, na.rm = TRUE),
                q66 = quantile(value, 0.66, na.rm = TRUE), q90 = quantile(value, 0.90, na.rm = TRUE),
                q97 = quantile(value, 0.97, na.rm = TRUE), q100 = quantile(value, 1.00, na.rm = TRUE), .groups = "drop")
    
    ribbon_data <- tibble(
      range = c("0–33%", "33–66%", "66–90%", "90–97%", "97–100%"),
      ymin = c("q0", "q33", "q66", "q90", "q97"), ymax = c("q33", "q66", "q90", "q97", "q100")
    ) %>% pmap_dfr(function(range, ymin, ymax) {
      ribbon_df %>% transmute(
        month_day,
        ymin = .data[[ymin]], ymax = .data[[ymax]],
        range = range,
        text = paste0("Range: ", range, "<br>Bounds: ", round(.data[[ymin]], 1), " to ", round(.data[[ymax]], 1))
      )
    })
    
    default_fill <- c("0–33%" = "#cce5ff", "33–66%" = "#e6f2ff", "66–90%" = "#ffe0b2", "90–97%" = "#ffcc80", "97–100%" = "#ff9933")
    moisture_fill <- c("0–33%" = "#ff9933", "33–66%" = "#ffcc80", "66–90%" = "#ffe0b2", "90–97%" = "#e6f2ff", "97–100%" = "#cce5ff")
    fill_values <- if (input$variable %in% reverse_fill_vars) moisture_fill else default_fill
    
    n_hist_years <- length(unique(df_hist_all$year_str))
    hist_colors <- colorRampPalette(brewer.pal(8, "Dark2"))(n_hist_years)
    obsYr <- paste0(input$plot_year, " Observed")
    fcstYr <- paste0(input$plot_year, " Forecast")
    
    color_values <- c(setNames(hist_colors, unique(df_hist_all$year_str)), "Mean" = "blue",
                      setNames("orangered", obsYr), setNames("forestgreen", fcstYr))
    
    y_axis_label <- if (input$variable %in% c("precip_total", "burn_period", "precip_cum")) {
      pretty_variable_name(input$variable)
    } else {
      paste(pretty_variable_name(input$variable), "(", input$daily_stat, ")")
    }
    
    p <- ggplot() +
      geom_ribbon(data = ribbon_data, aes(x = month_day, ymin = ymin, ymax = ymax, fill = range, text = text, group = range), alpha = 0.7)
    
    if (isTruthy(input$show_hist_years)) {
      p <- p + geom_line(data = df_hist_all, aes(x = month_day, y = value, group = year_str, color = year_str, text = text), linewidth = 0.5, alpha = 0.4)
    }
    
    p <- p +
      geom_line(data = clim_df, aes(x = month_day, y = mean, group = 1, color = "Mean", text = text), linewidth = 0.75) +
      geom_hline(yintercept = quantile(df_hist_all$value, c(0.25, 0.5, 0.9, 0.97), na.rm = TRUE), linetype = "dashed", color = "gray40") +
      scale_x_date(date_labels = "%b", date_breaks = "1 month", expand = expansion(mult = c(0, 0))) +
      scale_fill_manual("Daily %tile Range", values = fill_values) +
      scale_color_manual("Legend", values = color_values) +
      theme_bw(base_size = 14) +
      labs(x = if (is.null(smooth_spec())) "Month-Day" else
             paste0("Month-Day   (bold line: ", smooth_spec(),
                    "; grey line: unsmoothed daily values)"),
           y = y_axis_label)
    
    if (nrow(df_current_obs) > 0) {
      if (isTRUE(input$smooth_on)) {
        # Raw series in neutral grey, outside the colour scale so it adds no
        # legend row. The two traces carry DIFFERENT hover text so the unified
        # tooltip reads "Observed" once and "Smoothed" once, not both twice.
        p <- p + geom_line(data = df_current_obs,
                           aes(x = month_day, y = value, group = 1, text = text),
                           colour = "gray30", linewidth = 0.35, alpha = 0.65) +
                 geom_line(data = filter(df_current_obs, !is.na(value_smooth)),
                           aes(x = month_day, y = value_smooth, group = 1, color = !!obsYr, text = text_sm),
                           linewidth = 1.3)
      } else {
        p <- p + geom_line(data = df_current_obs, aes(x = month_day, y = value, group = 1, color = !!obsYr, text = text), linewidth = 1)
      }
    }
    if (input$show_forecast && nrow(df_current_fcst) > 0) {
      p <- p + geom_line(data = df_current_fcst, aes(x = month_day, y = value,group = 1, color = !!fcstYr, text = text), linewidth = 1, linetype = "solid")
    }
    
    #ggplotly(p, tooltip = "text") %>% layout(hovermode = "x unified")
    
    # added watermark
    ggplotly(p, tooltip = "text") %>%
      layout(
        hovermode = "x unified",
        annotations = list(
          list(
            x = 0.5,
            y = 0.5,
            text = "<b>EXPERIMENTAL</b>",
            xref = "paper",
            yref = "paper",
            xanchor = "center",
            yanchor = "middle",
            showarrow = FALSE,
            font = list(
              size = 80,
              color = "rgba(255, 0, 0, 0.15)"
            )
          )
        )
      )
    
  })
  
  output$summary_table <- renderDT({
    validate(need(!is.null(all_data_cache()), "No data loaded yet. Click 'Fetch Station Data' to populate this table."))
    req(input$plot_year, input$daily_stat)
    
    all_data <- all_data_cache()
    
    var_list <- c(unname(nfdrs_labels), unname(weather_var_labels))
    # Standard base variables that we will apply the normal summary statistic to
    standard_vars <- intersect(var_list, names(all_data))
    
    df_obs <- all_data %>% filter(record_type == "O")
    
    # 1. Calculate Standard Variables across stations FIRST
    if (input$daily_stat == "1300LST") {
      standard_daily <- df_obs %>%
        filter(hour == 13) %>%
        group_by(date) %>%
        summarise(across(all_of(standard_vars), ~safe_summary1(.x, mean)), .groups = "drop")
    } else {
      summary_fun <- match.fun(input$daily_stat)
      standard_daily <- df_obs %>%
        group_by(date) %>%
        summarise(across(all_of(standard_vars), ~safe_summary1(.x, summary_fun)), .groups = "drop")
    }
    
    # 2. Calculate Custom Computed Variables across all 24 hours
    special_daily <- df_obs %>%
      group_by(date) %>%
      summarise(
        precip_total = if("precipitation" %in% names(df_obs)) safe_summary1(precipitation, sum) else NA,
        burn_period = if("relativeHumidity" %in% names(df_obs)) safe_summary1(relativeHumidity < 20, sum) else NA,
        .groups = "drop"
      )
    
    # 3. Combine them back together
    daily_data <- standard_daily %>%
      left_join(special_daily, by = "date") %>%
      mutate(year = as.integer(format(date, "%Y")), month_day = as.Date(format(date, "2024-%m-%d"))) %>%
      filter(lubridate::month(date) >= input$month_range[1] & lubridate::month(date) <= input$month_range[2])
    
    # Identify final available variables for table pivoting
    available_vars_for_table <- standard_vars
    if ("precipitation" %in% names(df_obs)) available_vars_for_table <- c(available_vars_for_table, "precip_total")
    if ("relativeHumidity" %in% names(df_obs)) available_vars_for_table <- c(available_vars_for_table, "burn_period")
    
    # Filter to current configured plot year
    df_current <- daily_data %>% filter(year == input$plot_year)
    validate(need(nrow(df_current) > 0, paste("No observed data available for the year", input$plot_year, "in the selected month range.")))
    
    # Grab both the MIN and MAX actual dates from the subsetted data
    max_month_day <- max(df_current$month_day, na.rm = TRUE)
    min_actual_date <- min(df_current$date, na.rm = TRUE)
    max_actual_date <- max(df_current$date, na.rm = TRUE)
    
    daily_data_ytd <- daily_data %>% filter(month_day <= max_month_day)
    
    baseline <- daily_data_ytd %>%
      filter(year >= 2005 & year <= 2025) %>%
      summarise(across(all_of(available_vars_for_table), ~mean(.x, na.rm = TRUE))) %>%
      pivot_longer(everything(), names_to = "Variable", values_to = "Historical_Mean")
    
    current_ytd <- daily_data_ytd %>%
      filter(year == input$plot_year) %>%
      summarise(across(all_of(available_vars_for_table), ~mean(.x, na.rm = TRUE))) %>%
      pivot_longer(everything(), names_to = "Variable", values_to = "Current_Period")
    
    table_data <- inner_join(baseline, current_ytd, by = "Variable") %>%
      mutate(
        Anomaly = Current_Period - Historical_Mean,
        `% of Normal` = (Current_Period / Historical_Mean) * 100
      ) %>%
      mutate(Variable = sapply(Variable, pretty_variable_name)) %>%
      mutate(across(where(is.numeric), ~round(.x, 2))) %>%
      rename(
        `Historical Mean (2005-2025)` = Historical_Mean,
        !!paste(input$plot_year, "Period Mean") := Current_Period
      )
    
    # Update caption to show the exact date range (Start - End)
    datatable(table_data,
              options = list(pageLength = 25, dom = 't', scrollX = TRUE),
              rownames = FALSE,
              caption = htmltools::tags$caption(
                style = 'caption-side: top; text-align: left; font-size: 16px; font-weight: bold; color: #333;',
                paste0("Selected Period Anomalies (", format(min_actual_date, "%b %d, %Y"), " - ", format(max_actual_date, "%b %d, %Y"),
                       ") based on Daily ", tools::toTitleCase(input$daily_stat))
              )) %>%
      formatStyle('Anomaly', color = styleInterval(0, c('blue', 'red')), fontWeight = 'bold') %>%
      formatString('% of Normal', suffix = '%')
  })
  
  # ---------------------------------------------------------------------
  # Download Handler for Plot Data
  # ---------------------------------------------------------------------
  output$download_plot_data <- downloadHandler(
    filename = function() {
      # The smoothing spec goes in the name so two downloads with different
      # windows do not overwrite each other in the Downloads folder.
      sm <- if (isTRUE(input$smooth_on)) {
        paste0("_smooth", input$smooth_window,
               if (identical(input$smooth_align, "center")) "c" else "t",
               substr(input$smooth_fun, 1, 3))
      } else ""
      paste0("NFDRS_", input$variable, "_", input$daily_stat, "_", input$plot_year, sm, ".csv")
    },
    content = function(file) {
      # Require the data to be fetched first
      req(all_data_cache(), input$variable, input$daily_stat, input$plot_year)
      
      # 1. Base aggregation -- now shared with both plots via daily_series().
      all_data_sig <- daily_series()
      
      # 2. Build the output columns
      # Historical Data (2005-2025)
      df_hist <- all_data_sig %>%
        filter(record_type == "O", year >= 2005, year <= 2025) %>%
        group_by(month_day) %>%
        summarise(
          Historical_Mean = round(mean(value, na.rm = TRUE), 2),
          Min_0 = round(quantile(value, 0.00, na.rm = TRUE), 2),
          Pct_33 = round(quantile(value, 0.33, na.rm = TRUE), 2),
          Pct_66 = round(quantile(value, 0.66, na.rm = TRUE), 2),
          Pct_90 = round(quantile(value, 0.90, na.rm = TRUE), 2),
          Pct_97 = round(quantile(value, 0.97, na.rm = TRUE), 2),
          Max_100 = round(quantile(value, 1.00, na.rm = TRUE), 2),
          .groups = "drop"
        )
      
      # Current Year Observed. The smoothed column is always present so the CSV
      # schema does not change between downloads; it is simply empty when
      # smoothing is switched off.
      df_obs <- all_data_sig %>%
        filter(year == input$plot_year, record_type == "O") %>%
        group_by(month_day) %>%
        summarise(Current_Observed = round(mean(value, na.rm = TRUE), 2),
                  Current_Observed_Smoothed = round(first(value_smooth), 2),
                  .groups = "drop")
      
      # Current Year Forecast
      df_fcst <- all_data_sig %>%
        filter(year == input$plot_year, record_type == "F") %>%
        group_by(month_day) %>%
        summarise(Current_Forecast = round(mean(value, na.rm = TRUE), 2), .groups = "drop")
      
      # 3. Merge together and format for export
      final_data <- df_hist %>%
        left_join(df_obs, by = "month_day") %>%
        left_join(df_fcst, by = "month_day") %>%
        mutate(Date = format(month_day, "%b-%d")) %>%
        select(Date, Historical_Mean, Min_0, Pct_33, Pct_66, Pct_90, Pct_97, Max_100, Current_Observed, Current_Observed_Smoothed, Current_Forecast)
      
      # Write the CSV
      write.csv(final_data, file, row.names = FALSE, na = "")
    }
  )
  
}
#shinyApp(ui = ui, server = server)
# Change your bottom line from this:
shinyApp(ui = ui, server = server)
# To this:
#bslib::run_with_themer(shinyApp(ui = ui, server = server))