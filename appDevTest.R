# ==============================================================================
# Application: NFDRS Chart Builder App ----  DEVELOPMENT VERSION
# Author: Mike Crimmins (crimmins@arizona.edu) / Gemini Pro
# Date: April 2025
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

# -----------------------------
# Helper Functions
# -----------------------------
#####

# Safe summary function to avoid warnings
safe_summary1 <- function(x, fun) {
  if (all(is.na(x))) NA else fun(x, na.rm = TRUE)
}

##### API downloads -----
download_nfdrs_data <- function(station_id, start_date, end_date,
                                fuel_model = "Y", dataset = "all") {
  
  base_url <- "https://fems.fs2c.usda.gov/api/climatology/download-nfdr"
  start_iso <- paste0(start_date, "T00:00:00Z")
  end_iso <- paste0(end_date, "T23:59:59Z")
  
  query_params <- list(
    stationIds = station_id,
    startDate = start_iso,
    endDate = end_iso,
    dataFormat = "csv",
    dataset = dataset,
    fuelModels = fuel_model
  )
  
  url <- httr::modify_url(base_url, query = query_params)
  message("Fetching NFDRS data from: ", url)
  
  tryCatch({
    res <- httr::GET(url)
    httr::stop_for_status(res)
    
    raw_text <- httr::content(res, "text", encoding = "UTF-8")
    df <- readr::read_csv(I(raw_text), show_col_types = FALSE)
    
    if(nrow(df) == 0) return(NULL)
    
    time_col_idx <- grep("observation_time_lst|observationtime|datetime", names(df), ignore.case = TRUE)
    if (length(time_col_idx) == 0) {
      stop("No valid time column found. API returned: ", paste(names(df), collapse = ", "))
    }
    time_col <- names(df)[time_col_idx[1]]
    
    if (inherits(df[[time_col]], "POSIXt")) {
      df$parsed_time <- df[[time_col]]
    } else {
      time_str <- as.character(df[[time_col]])
      if (any(grepl("Z$", time_str, ignore.case = TRUE))) {
        df$parsed_time <- lubridate::ymd_hms(time_str, tz = "UTC", truncated = 3, quiet = TRUE)
      } else {
        df$parsed_time <- lubridate::ymd_hms(time_str, truncated = 3, quiet = TRUE) 
      }
    }
    
    df <- df %>%
      mutate(
        station_id = as.character(station_id),
        date = as.Date(parsed_time),
        hour = as.integer(format(parsed_time, "%H")),
        record_type = substr(toupper(NFDRType), 1, 1) 
      ) %>%
      select(-parsed_time) %>%
      rename(any_of(c(
        energyReleaseComponent = "EnergyReleaseComponent",
        burningIndex = "BurningIndex",
        ignitionComponent = "IgnitionComponent",
        spreadComponent = "SpreadComponent",
        kbdi = "KBDI",
        oneHR_TL_FuelMoisture = "OneHR_TL_FuelMoisture",
        tenHR_TL_FuelMoisture = "TenHR_TL_FuelMoisture",
        hundredHR_TL_FuelMoisture = "HundredHR_TL_FuelMoisture",
        thousandHR_TL_FuelMoisture = "ThousandHR_TL_FuelMoisture",
        woodyLFI_fuelMoisture = "WoodyLFI_FuelMoisture",
        herbaceousLFI_fuelMoisture = "HerbaceousLFI_fuelMoisture",
        gsi = "GSI"
      )))
    
    return(df)
    
  }, error = function(e) {
    warning("Failed to download or parse NFDRS data: ", conditionMessage(e))
    return(NULL)
  })
}
#####

#####
# weather download function
download_weather_data <- function(station_id, start_date, end_date) {
  
  base_url <- "https://fems.fs2c.usda.gov/api/climatology/download-weather"
  start_iso <- paste0(start_date, "T00:00:00Z")
  end_iso <- paste0(end_date, "T23:59:59Z")
  
  query_params <- list(
    stationIds = station_id,
    startDate = start_iso,
    endDate = end_iso,
    dataFormat = "csv",
    dataIncrement = "hourly",
    dataset = "all", 
    stationtypes = "RAWS(SATNFDRS)"
  )
  
  url <- httr::modify_url(base_url, query = query_params)
  message("Fetching Weather data from: ", url)
  
  tryCatch({
    res <- httr::GET(url)
    httr::stop_for_status(res)
    
    raw_text <- httr::content(res, "text", encoding = "UTF-8")
    df <- readr::read_csv(I(raw_text), show_col_types = FALSE)
    
    if(nrow(df) == 0) return(NULL)
    
    df <- df %>%
      rename_with(~"observationTime", matches("DateTime|ObservationTime", ignore.case = TRUE)) %>%
      rename_with(~"temperature", contains("Temperature")) %>%
      rename_with(~"relativeHumidity", matches("Relative Humidity|RelativeHumidity", ignore.case = TRUE)) %>%
      rename_with(~"precipitation", contains("Precipitation")) %>%
      rename_with(~"windSpeed", matches("Wind Speed|WindSpeed", ignore.case = TRUE)) %>%
      rename_with(~"windDirection", matches("Wind Azimuth|WindAzimuth", ignore.case = TRUE)) %>%
      rename_with(~"gustSpeed", matches("Gust Speed|GustSpeed", ignore.case = TRUE)) %>%
      rename_with(~"gustDirection", matches("Gust Azimuth|GustAzimuth", ignore.case = TRUE)) %>%
      rename_with(~"solarRadiation", matches("Solar Radiation|SolarRadiation", ignore.case = TRUE))
    
    if (inherits(df$observationTime, "POSIXt")) {
      df$parsed_time <- df$observationTime
    } else {
      time_str <- as.character(df$observationTime)
      if (any(grepl("Z$", time_str, ignore.case = TRUE))) {
        df$parsed_time <- lubridate::ymd_hms(time_str, tz = "UTC", truncated = 3, quiet = TRUE)
      } else {
        df$parsed_time <- lubridate::ymd_hms(time_str, truncated = 3, quiet = TRUE) 
      }
    }
    
    df <- df %>%
      mutate(
        station_id = as.character(station_id),
        date = as.Date(parsed_time),
        hour = as.integer(format(parsed_time, "%H")),
        record_type = substr(toupper(ObservationType), 1, 1) 
      ) %>%
      select(-parsed_time) %>%
      select(-matches("flag|ObservationType", ignore.case = TRUE)) 
    
    if(all(c("temperature", "relativeHumidity", "windSpeed") %in% names(df))) {
      df <- df %>%
        mutate(
          temp_c = (temperature - 32) * 5 / 9,
          vpd = (1 - relativeHumidity / 100) * (0.6108 * exp((17.27 * temp_c) / (temp_c + 237.3))),
          hdw = (windSpeed * 0.44704) * (vpd * 10)
        ) %>%
        select(-temp_c)
    }
    
    return(df)
    
  }, error = function(e) {
    warning("Weather data parse failed: ", conditionMessage(e))
    return(NULL)
  })
}
#####

pretty_variable_name <- function(var) {
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
  "relativeHumidity"
)

# -----------------------------
# Load Station Metadata
# -----------------------------
station_metadata <- read.csv("station_metadata_FEMS3_042225.csv", stringsAsFactors = FALSE)

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
      /* Optional pulsing animation for the warning button */
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
  theme = bs_theme(bootswatch = "journal"),
  tags$div(
    style = "display: flex; align-items: center; justify-content: space-between; margin-bottom: 20px;",
    tags$h1("NFDRS Chart Builder"),
    tags$img(src = "UAlogo.jpg", height = "60px") 
  ),
  sidebarLayout(
    sidebarPanel(
      leafletOutput("station_map", height = 400),
      uiOutput("station_selector"),
      selectInput("fuel_model", "Fuel Model", choices = c("Y","Z")),
      uiOutput("variable_selector"),
      selectInput("daily_stat", "Daily Statistic", 
                  choices = c("mean", "min", "max", "1300LST")),
      
      numericInput("plot_year", "Plot Year (2005-Present)",
                   value = as.numeric(format(Sys.Date(), "%Y")),
                   min = 2005, max = as.numeric(format(Sys.Date(), "%Y"))),
      
      br(),
      # dynamic UI for the fetch button
      uiOutput("dynamic_fetch_button"),
      br(), 
      checkboxInput("show_forecast", "Plot Current Forecast", value = TRUE),
      br(),
      
      div(
        style = "text-align: center;",  
        tags$img(src = "BP_app_logos.png", style = "width: 100%; height: auto;"),
        tags$p("Contact: Mike Crimmins, crimmins@arizona.edu"),
        tags$p("https://cales.arizona.edu/climate/")
      )
    ),
    mainPanel(
      # High-visibility warning banner
      uiOutput("stale_data_banner"),
      
      tabsetPanel(
        tabPanel("Static Plot", plotOutput("climatology_plot", height = "700px")),
        tabPanel("Interactive Plot",
                 div(
                   plotlyOutput("plotly_climatology_plot", height = "700px"),
                   br(),
                   checkboxInput("show_hist_years", "Show Historic Years", value = FALSE)
                 )
        ),
        tabPanel("YTD Summary", DTOutput("summary_table")),
        tabPanel("About", 
                 div(style = "padding: 20px;",
                     h3("🔥 NFDRS Chart Builder"),
                     p("The NFDRS Chart Builder is an interactive tool for visualizing daily fire weather indices, meteorological variables, and percentile climatologies from the National Fire Danger Rating System (NFDRS)."),
                     h4("🛠️ How to Use"),
                     tags$ol(
                       tags$li("Use the map or dropdown to select one or more stations (select/deselect stations using map or dropdown)."),
                       tags$li("Select a fuel model available through FEMS (Y or Z)."),
                       tags$li("Choose a variable to analyze. Options dynamically update based on the fetched data and include NFDRS indices (e.g., ERC, BI, KBDI, Fuel Moistures) as well as weather variables (e.g., Temperature, RH, Wind Speed, VPD, HDW)."),
                       tags$li("Pick a daily summary statistic (mean, min, or max) and a target year."),
                       tags$li("Click 'Fetch Station Data' to download from the API. ", 
                               tags$b("Note: "), "If you change your station or fuel model later, the button will turn red and warn you to fetch the newly requested data to keep the charts accurate."),
                       tags$li("View visualizations in either the static or interactive plot tabs, or check the ", 
                               tags$b("Summary Stats"), " tab for a Year-to-Date (YTD) anomaly table comparing the current year to the historical baseline (2005–2022).")
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
  
  # NEW: We track what was ACTUALLY fetched to keep plot titles honest
  fetched_fuel_model <- reactiveVal(NULL) 
  data_outdated <- reactiveVal(FALSE) 
  
  # Trigger warning if they change inputs that require a fetch
  observeEvent(c(input$station_ids, input$fuel_model), {
    if (!is.null(all_data_cache())) {
      data_outdated(TRUE)
    }
  }, ignoreInit = TRUE)
  
  # Dynamic Fetch Button Rendering
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
  
  # Highly visible alert banner above the charts
  output$stale_data_banner <- renderUI({
    if (data_outdated()) {
      div(class = "alert alert-warning", 
          style = "font-size: 16px; font-weight: bold; text-align: center; margin-bottom: 15px;",
          "⚠️ Station selection or Fuel Model has changed. The charts below are showing old data. Click 'Fetch New Data' to update.")
    }
  })
  
  output$station_selector <- renderUI({
    selectInput("station_ids", "Selected Stations",
                choices = setNames(station_metadata$station_id, station_metadata$station_name),
                selected = selected_stations(), multiple = TRUE)
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
    
    withProgress(message = "Downloading & assembling data...", {
      all_nfdrs <- map_dfr(stns, function(id) {
        key <- paste(id, input$fuel_model, sep = "_")
        if (!is.null(data_cache[[key]])) {
          data_cache[[key]]
        } else {
          df <- download_nfdrs_data(id, "2000-01-01", Sys.Date() + 7, input$fuel_model)
          if (is.null(df)) {
            showNotification(paste("Failed to fetch NFDRS data for station", id), type = "error", duration = 6)
            return(tibble()) 
          }
          df$station_id <- id
          data_cache[[key]] <- df
          df
        }
      })
      
      all_weather <- map_dfr(stns, function(id) {
        if (!is.null(weather_data_cache[[id]])) {
          weather_data_cache[[id]]
        } else {
          df <- download_weather_data(id, "2000-01-01", Sys.Date() + 7)
          if (is.null(df)) {
            showNotification(paste("Failed to fetch weather data for station", id), type = "error", duration = 6)
            return(tibble())
          }
          df$station_id <- id
          weather_data_cache[[id]] <- df
          df
        }
      })
      
      if (nrow(all_nfdrs) == 0 || nrow(all_weather) == 0) {
        showNotification("One or more datasets returned empty. Cannot process station.", type = "error")
        return(NULL)
      }
      
      all_data <- left_join(all_nfdrs, all_weather, by = c("station_id", "date", "hour", "record_type"))
      
      # SUCCESS: Save data, lock in the fuel model, and clear the warning
      all_data_cache(all_data)
      fetched_fuel_model(input$fuel_model)
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
  
  weather_var_labels <- c("Temperature (°F)" = "temperature", "Relative Humidity (%)" = "relativeHumidity",
                          "Wind Speed (mph)" = "windSpeed", "Wind Gust (mph)" = "gustSpeed",
                          "Wind Direction (°)" = "windDirection", "Gust Direction (°)" = "gustDirection",
                          "Solar Radiation (W/m²)" = "solarRadiation", "Hourly Precipitation (in)" = "precipitation",
                          "Vapor Pressure Deficit (kPa)" = "vpd", "Hot-Dry-Windy Index" = "hdw")
  
  weather_vars <- unname(weather_var_labels)
  
  output$variable_selector <- renderUI({
    req(all_data_cache())
    available_vars <- names(all_data_cache())[sapply(all_data_cache(), is.numeric)]
    display_vars <- c(nfdrs_labels, weather_var_labels)
    display_vars <- display_vars[display_vars %in% available_vars]
    selectInput("variable", "Select Variable", choices = display_vars, selected = display_vars[1])
  })
  
  # Plot rendering
  output$climatology_plot <- renderPlot({
    validate(need(!is.null(all_data_cache()), "Welcome! Please select your station(s) and click 'Fetch Station Data' to generate the chart."))
    req(input$variable, input$daily_stat, input$plot_year, fetched_fuel_model())
    
    all_data <- all_data_cache()
    #summary_fun <- match.fun(input$daily_stat)
    
    # -------------------------------------------------------------
    # NEW FIX: Extract names safely from downloaded data, NOT UI inputs
    # -------------------------------------------------------------
    plotted_station_ids <- unique(all_data$station_id)
    station_names <- station_metadata %>%
      filter(station_id %in% plotted_station_ids) %>%
      pull(station_name) %>% unique()
    station_label <- paste(station_names, collapse = ", ")
    
    # stn_data <- all_data %>%
    #   group_by(station_id, date, record_type) %>%
    #   summarise(value = safe_summary1(.data[[input$variable]], summary_fun), .groups = "drop") %>%
    #   mutate(year = as.integer(format(date, "%Y")), month_day = as.Date(format(date, "2024-%m-%d")))

    # -------------------------------------------------------------
    # Aggregate data based on selected daily statistic
    # -------------------------------------------------------------
    if (input$daily_stat == "1300LST") {
      stn_data <- all_data %>%
        filter(hour == 13) %>% # Simply grab 13:00 for both Observed AND Forecast
        group_by(station_id, date, record_type) %>%
        # Use mean just to safely collapse single values
        summarise(value = safe_summary1(.data[[input$variable]], mean), .groups = "drop") %>%
        mutate(year = as.integer(format(date, "%Y")), month_day = as.Date(format(date, "2024-%m-%d")))
    } else {
      summary_fun <- match.fun(input$daily_stat)
      stn_data <- all_data %>%
        group_by(station_id, date, record_type) %>%
        summarise(value = safe_summary1(.data[[input$variable]], summary_fun), .groups = "drop") %>%
        mutate(year = as.integer(format(date, "%Y")), month_day = as.Date(format(date, "2024-%m-%d")))
    }
    
     
    all_data_sig <- stn_data %>%
      group_by(date, record_type) %>%
      summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
      mutate(year = as.integer(format(date, "%Y")), month_day = as.Date(format(date, "2024-%m-%d")))
    
    all_data_hist <- all_data_sig %>% filter(record_type == "O")
    
    historical_years <- all_data_hist %>% filter(year >= 2005 & year <= 2022) %>%
      summarise(start_year = min(year, na.rm = TRUE), end_year = max(year, na.rm = TRUE))
    
    # clim_df <- all_data_hist %>% filter(year != input$plot_year) %>%
    #   group_by(month_day) %>%
    #   summarise(min = min(value, na.rm = TRUE), max = max(value, na.rm = TRUE),
    #             mean = mean(value, na.rm = TRUE), median = median(value, na.rm = TRUE), .groups = "drop")
    # NEW:
    clim_df <- all_data_hist %>% filter(year >= 2005 & year <= 2022) %>%
      group_by(month_day) %>%
      summarise(min = min(value, na.rm = TRUE), max = max(value, na.rm = TRUE),
                mean = mean(value, na.rm = TRUE), median = median(value, na.rm = TRUE), .groups = "drop")
    
        
    df_hist <- all_data_hist %>% filter(year >= 2005 & year <= 2022)
    
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
      group_by(month_day) %>% summarise(value = mean(value, na.rm = TRUE), .groups = "drop")
    df_current_fcst <- all_data_sig %>% filter(year == input$plot_year, record_type == "F") %>%
      group_by(month_day) %>% summarise(value = mean(value, na.rm = TRUE), .groups = "drop")
    
    default_fill <- c("0–33%" = "#cce5ff", "33–66%" = "#e6f2ff", "66–90%" = "#ffe0b2", "90–97%" = "#ffcc80", "97–100%" = "#ff9933")
    moisture_fill <- c("0–33%" = "#ff9933", "33–66%" = "#ffcc80", "66–90%" = "#ffe0b2", "90–97%" = "#e6f2ff", "97–100%" = "#cce5ff")
    fill_values <- if (input$variable %in% reverse_fill_vars) moisture_fill else default_fill
    
    color_mapping <- setNames(c("blue", "orangered", "forestgreen"), 
                              c("Mean", paste0(input$plot_year, " Observed"), paste0(input$plot_year, " Forecast")))
    
    p <- ggplot() +
      geom_ribbon(data = ribbon_data, aes(x = month_day, ymin = ymin, ymax = ymax, fill = range), alpha = 0.7) +
      geom_line(data = clim_df, aes(x = month_day, y = mean, color = "Mean"), linewidth = 0.6) +
      geom_hline(yintercept = c(p25_global, p50_global, p90_global, p97_global), color = "gray40", linetype = "dashed") +
      annotate("text", x = as.Date("2024-01-03"), y = p90_global, label = "90%", hjust = 0, vjust = -0.5, size = 3, color = "gray40") +
      annotate("text", x = as.Date("2024-01-03"), y = p97_global, label = "97%", hjust = 0, vjust = -0.5, size = 3, color = "gray40") +
      annotate("text", x = as.Date("2024-01-03"), y = p50_global, label = "50%", hjust = 0, vjust =-0.5, size = 3, color = "gray40") +
      annotate("text", x = as.Date("2024-01-03"), y = p25_global, label = "25%", hjust = 0, vjust = -0.5, size = 3, color = "gray40") +
      scale_x_date(date_labels = "%b-%d", date_breaks = "1 month",expand = expansion(mult = c(0, 0))) +
      scale_fill_manual("Daily %tile Range", values = fill_values) +
      scale_color_manual(name = NULL, values = color_mapping) +
      labs(
        title = if (input$variable %in% weather_vars) {
          names(weather_var_labels)[match(input$variable, weather_var_labels)]
        } else {
          # Use fetched_fuel_model() instead of input$fuel_model
          paste0(pretty_variable_name(input$variable), " (Fuel Model ", fetched_fuel_model(), ")")
        },
        subtitle = paste0(station_label, " | ", input$plot_year,
                          " vs Climatology (", historical_years$start_year, "–", historical_years$end_year, ")"),
        x = "Month-Day",
        y = paste(pretty_variable_name(input$variable), "(", input$daily_stat, ")"),
        caption = "Data from FEMS-API"
      ) +
      theme_bw(base_size = 14)
    
    if (nrow(df_current_obs) > 0) {
      p <- p + geom_line(data = df_current_obs, aes(x = month_day, y = value, color = paste0(input$plot_year, " Observed")), linewidth = 1.2)
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
    
    all_data <- all_data_cache()
    #summary_fun <- match.fun(input$daily_stat)
    
    # stn_data <- all_data %>%
    #   group_by(station_id, date, record_type) %>%
    #   summarise(value = safe_summary1(.data[[input$variable]], summary_fun), .groups = "drop") %>%
    #   mutate(year = as.integer(format(date, "%Y")), month_day = as.Date(format(date, "2024-%m-%d")))

    # -------------------------------------------------------------
    # Aggregate data based on selected daily statistic
    # -------------------------------------------------------------
    if (input$daily_stat == "1300LST") {
      stn_data <- all_data %>%
        filter(hour == 13) %>% # Simply grab 13:00 for both Observed AND Forecast
        group_by(station_id, date, record_type) %>%
        # Use mean just to safely collapse single values
        summarise(value = safe_summary1(.data[[input$variable]], mean), .groups = "drop") %>%
        mutate(year = as.integer(format(date, "%Y")), month_day = as.Date(format(date, "2024-%m-%d")))
    } else {
      summary_fun <- match.fun(input$daily_stat)
      stn_data <- all_data %>%
        group_by(station_id, date, record_type) %>%
        summarise(value = safe_summary1(.data[[input$variable]], summary_fun), .groups = "drop") %>%
        mutate(year = as.integer(format(date, "%Y")), month_day = as.Date(format(date, "2024-%m-%d")))
    }
    
    all_data_sig <- stn_data %>%
      group_by(date, record_type) %>%
      summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
      mutate(year = as.integer(format(date, "%Y")), month_day = as.Date(format(date, "2024-%m-%d")))
    
    all_data_hist <- all_data_sig %>% filter(record_type == "O")
    
    # df_hist_all <- all_data_hist %>% filter(year != input$plot_year) %>%
    #   mutate(year_str = as.character(year)) %>% group_by(year_str, month_day) %>%
    #   summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
    #   mutate(text = paste("Year:", year_str, "<br>Date:", format(month_day, "%b-%d"), "<br>Value:", round(value, 1)))
    
    # NEW:
    df_hist_all <- all_data_hist %>% filter(year >= 2005 & year <= 2022) %>%
      mutate(year_str = as.character(year)) %>% group_by(year_str, month_day) %>%
      summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
      mutate(text = paste("Year:", year_str, "<br>Date:", format(month_day, "%b-%d"), "<br>Value:", round(value, 1)))
    
    clim_df <- df_hist_all %>% group_by(month_day) %>% summarise(mean = mean(value, na.rm = TRUE), .groups = "drop") %>%
      mutate(text = paste("Date:", format(month_day, "%b-%d"), "<br>Mean:", round(mean, 1)))
    
    df_current_obs <- all_data_sig %>% filter(year == input$plot_year, record_type == "O") %>%
      group_by(month_day) %>% summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
      mutate(text = paste("Date:", format(month_day, "%b-%d"), "<br>Observed:", round(value, 1)))
    
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
        ymin = .data[[ymin]], 
        ymax = .data[[ymax]], 
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
    
    p <- ggplot() +
      geom_ribbon(data = ribbon_data, aes(x = month_day, ymin = ymin, ymax = ymax, fill = range, text = text, group = range), alpha = 0.7)
    
    if (isTruthy(input$show_hist_years)) {
      p <- p + geom_line(data = df_hist_all, aes(x = month_day, y = value, group = year_str, color = year_str, text = text), linewidth = 0.5, alpha = 0.4)
    }
    
    p <- p +
      geom_line(data = clim_df, aes(x = month_day, y = mean, group = 1, color = "Mean", text = text), linewidth = 0.75) +
      #geom_line(data = clim_df, aes(x = month_day, y = mean, color = "Mean", text = text), linewidth = 0.75) +
      geom_hline(yintercept = quantile(df_hist_all$value, c(0.25, 0.5, 0.9, 0.97), na.rm = TRUE), linetype = "dashed", color = "gray40") +
      scale_x_date(date_labels = "%b-%d", date_breaks = "1 month", expand = expansion(mult = c(0, 0))) +
      scale_fill_manual("Daily %tile Range", values = fill_values) +
      scale_color_manual("Legend", values = color_values) +
      theme_bw(base_size = 14) + 
      labs(x = "Month-Day", y = paste(pretty_variable_name(input$variable), "(", input$daily_stat, ")"))
    
    if (nrow(df_current_obs) > 0) {
      p <- p + geom_line(data = df_current_obs, aes(x = month_day, y = value, group = 1, color = !!obsYr, text = text), linewidth = 1)
      #p <- p + geom_line(data = df_current_obs, aes(x = month_day, y = value, color = !!obsYr, text = text), linewidth = 1)
    }
    if (input$show_forecast && nrow(df_current_fcst) > 0) {
      p <- p + geom_line(data = df_current_fcst, aes(x = month_day, y = value, group = 1, color = !!fcstYr, text = text), linewidth = 1, linetype = "solid")
      #p <- p + geom_line(data = df_current_fcst, aes(x = month_day, y = value, color = !!fcstYr, text = text), linewidth = 1, linetype = "solid")
    }
    
    ggplotly(p, tooltip = "text") %>% layout(hovermode = "x unified")
  })
  
  output$summary_table <- renderDT({
    validate(need(!is.null(all_data_cache()), "No data loaded yet. Click 'Fetch Station Data' to populate this table."))
    req(input$plot_year, input$daily_stat)
    
    all_data <- all_data_cache()
    
    # Notice: summary_fun <- match.fun(input$daily_stat) has been REMOVED from here!
    
    # Create a master list of variables to process
    var_list <- c(unname(nfdrs_labels), unname(weather_var_labels))
    available_vars <- intersect(var_list, names(all_data))
    
    # 1. Filter to Observed data only
    df_obs <- all_data %>% filter(record_type == "O")
    
    # 2. Determine the YTD cut-off based on the current plot year
    df_current <- df_obs %>% filter(year(date) == input$plot_year)
    
    validate(need(nrow(df_current) > 0, paste("No observed data available for the year", input$plot_year)))
    
    max_date_current <- max(df_current$date, na.rm = TRUE)
    cutoff_yday <- lubridate::yday(max_date_current)
    
    # 3. Aggregate hourly to daily values for ALL years, up to the cut-off day
    if (input$daily_stat == "1300LST") {
      daily_data <- df_obs %>%
        mutate(yday = lubridate::yday(date), year = year(date)) %>%
        filter(yday <= cutoff_yday, hour == 13) %>%
        group_by(year, date) %>%
        summarise(across(all_of(available_vars), ~safe_summary1(.x, mean)), .groups = "drop")
    } else {
      # It is safely placed inside the else block here instead!
      summary_fun <- match.fun(input$daily_stat)
      daily_data <- df_obs %>%
        mutate(yday = lubridate::yday(date), year = year(date)) %>%
        filter(yday <= cutoff_yday) %>%
        group_by(year, date) %>%
        summarise(across(all_of(available_vars), ~safe_summary1(.x, summary_fun)), .groups = "drop")
    }
    
    # 4. Calculate Historical Baseline (2005 - 2022)
    baseline <- daily_data %>%
      filter(year >= 2005 & year <= 2022) %>%
      summarise(across(all_of(available_vars), ~mean(.x, na.rm = TRUE))) %>%
      pivot_longer(everything(), names_to = "Variable", values_to = "Historical_Mean")
    
    # 5. Calculate Current Year YTD
    current_ytd <- daily_data %>%
      filter(year == input$plot_year) %>%
      summarise(across(all_of(available_vars), ~mean(.x, na.rm = TRUE))) %>%
      pivot_longer(everything(), names_to = "Variable", values_to = "Current_YTD")
    
    # 6. Combine, calculate anomalies, and format the table
    table_data <- inner_join(baseline, current_ytd, by = "Variable") %>%
      mutate(
        Anomaly = Current_YTD - Historical_Mean,
        `% of Normal` = (Current_YTD / Historical_Mean) * 100
      ) %>%
      # Prettify variable names using your existing helper function
      mutate(Variable = sapply(Variable, pretty_variable_name)) %>%
      # Round numeric columns for clean display
      mutate(across(where(is.numeric), ~round(.x, 2))) %>%
      rename(
        `Historical Mean (2005-2022)` = Historical_Mean,
        !!paste(input$plot_year, "YTD Mean") := Current_YTD
      )
    
    # 7. Render with DT formatting
    datatable(table_data, 
              options = list(pageLength = 25, dom = 't', scrollX = TRUE), 
              rownames = FALSE,
              caption = htmltools::tags$caption(
                style = 'caption-side: top; text-align: left; font-size: 16px; font-weight: bold; color: #333;',
                paste("Year-to-Date Anomalies (Jan 1 -", format(max_date_current, "%b %d"), 
                      ") based on Daily", tools::toTitleCase(input$daily_stat))
              )) %>%
      formatStyle(
        'Anomaly',
        # Simple color formatting: Red for positive anomalies, Blue for negative
        color = styleInterval(0, c('blue', 'red')),
        fontWeight = 'bold'
      ) %>%
      formatString('% of Normal', suffix = '%')
  })
  
}

shinyApp(ui = ui, server = server)