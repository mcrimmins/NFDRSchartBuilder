# NFDRS Chart Builder App
# MAC 04/15/25

# update metadata file from https://www.wildfire.gov/node/3473 or
# https://fems.fs2c.usda.gov/download

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
# new API download function - updated 3/30/26 to handle new API structure,
# timestamp formats, add error handling, dynamic time parsing, and FORECASTS
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
    
    # Fix: Wrap the content in I() to satisfy newer versions of readr
    raw_text <- httr::content(res, "text", encoding = "UTF-8")
    df <- readr::read_csv(I(raw_text), show_col_types = FALSE)
    
    if(nrow(df) == 0) return(NULL)
    
    # Fix: Dynamically find the time column, ignoring case sensitivity
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
    
    # Fix: Wrap the content in I()
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
# Make sure your CSV file is in the same directory
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
    
    # NEW: Custom CSS to make the validation messages large, centered, and gray
    tags$style(HTML("
      .shiny-output-error-validation {
        color: #6c757d;           /* A nice bootstrap gray instead of red */
        font-size: 24px;          /* Make the text much larger */
        text-align: center;       /* Center it horizontally */
        margin-top: 150px;        /* Push it down into the middle of the chart area */
        font-weight: 500;         /* Make it slightly bold */
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
      selectInput("daily_stat", "Daily Statistic", choices = c("mean", "min", "max")),
      numericInput("plot_year", "Plot Year (2005-Present)",
                   value = as.numeric(format(Sys.Date(), "%Y")),
                   min = 2005, max = as.numeric(format(Sys.Date(), "%Y"))),
      
      # NEW: Action button and Forecast Toggle
      br(),
      actionButton("fetch_data", "Fetch Station Data", class = "btn-primary", width = "100%", style = "font-weight: bold;"),
      br(), br(),
      checkboxInput("show_forecast", "Plot Forecasts (Dashed Line)", value = TRUE),
      br(),
      
      div(
        style = "text-align: center;",  
        tags$img(src = "BP_app_logos.png", style = "width: 100%; height: auto;"),
        tags$p("Contact: Mike Crimmins, crimmins@arizona.edu"),
        tags$p("https://cales.arizona.edu/climate/")
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Static Plot", plotOutput("climatology_plot", height = "700px")),
        tabPanel("Interactive Plot",
                 div(
                   plotlyOutput("plotly_climatology_plot", height = "700px"),
                   br(),
                   checkboxInput("show_hist_years", "Show Historic Years", value = FALSE)
                 )
        ),
        tabPanel("Summary Stats", DTOutput("summary_table")),
        tabPanel("About", 
                 div(style = "padding: 20px;",
                     h3("🔥 NFDRS Chart Builder"),
                     p("The NFDRS Chart Builder is an interactive tool for visualizing daily fire weather indices and percentile climatologies from the National Fire Danger Rating System (NFDRS)."),
                     p("Users can explore historical patterns and current and past year observations of key fire weather indices across selected stations and fuel models."),
                     h4("🛠️ How to Use"),
                     tags$ol(
                       tags$li("Use the map or dropdown to select one or more stations (select/deselect stations using map or dropdown)."),
                       tags$li("Select a fuel model available through FEMS (V,W,X,Y,Z)."),
                       tags$li("Choose a variable such as ERC, BI, IC, SC, or KBDI."),
                       tags$li("Pick a daily summary statistic (mean, min, or max) and a target year."),
                       tags$li("Click 'Fetch Station Data' to download from the API."),
                       tags$li("View results in either a static or interactive plot tab.")
                     ),
                     h4("🔍 Features"),
                     tags$ul(
                       tags$li("Select one or more stations to create daily composites (SIGs)"),
                       tags$li("Daily summaries of hourly data using min, max or mean values"),
                       tags$li("7-day FEMS API Forecast Integration"),
                       tags$li("Interactive map with zoom, pan and snap capabilities")
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
        radius = 5,
        color = "blue",
        fillOpacity = 0.5
      )
  })
  
  observeEvent(input$station_map_marker_click, {
    click_id <- input$station_map_marker_click$id
    current <- selected_stations()
    new_selection <- if (click_id %in% current) {
      setdiff(current, click_id)
    } else {
      c(current, click_id)
    }
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
        layerId = ~station_id,
        label = ~station_name,
        radius = 5,
        color = ~ifelse(station_id %in% input$station_ids, "red", "blue"),
        fillOpacity = 0.5
      )
    
    if (nrow(sel_data) == 1) {
      leafletProxy("station_map") %>%
        setView(lng = sel_data$longitude, lat = sel_data$latitude, zoom = 7)
    } else if (nrow(sel_data) > 1) {
      leafletProxy("station_map") %>%
        fitBounds(
          lng1 = min(sel_data$longitude),
          lat1 = min(sel_data$latitude),
          lng2 = max(sel_data$longitude),
          lat2 = max(sel_data$latitude)
        )
    }
  })
  
  # Trigger download ONLY when the Fetch button is clicked
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
      
      # Ensure neither download failed completely before joining
      if (nrow(all_nfdrs) == 0 || nrow(all_weather) == 0) {
        showNotification("One or more datasets returned empty. Cannot process station.", type = "error")
        return(NULL)
      }
      
      # Join dynamically using record_type to avoid Obs/Fcst duplication issues
      all_data <- left_join(
        all_nfdrs, all_weather,
        by = c("station_id", "date", "hour", "record_type")
      )
      
      all_data_cache(all_data)
    })
  })
  
  ##### var labels ----
  nfdrs_labels <- c(
    "Energy Release Component (ERC)" = "energyReleaseComponent",
    "Burning Index (BI)"             = "burningIndex",
    "Ignition Component (IC)"        = "ignitionComponent",
    "Spread Component (SC)"          = "spreadComponent",
    "Keetch-Byram Drought Index"     = "kbdi",
    "1-hr Fuel Moisture"             = "oneHR_TL_FuelMoisture",
    "10-hr Fuel Moisture"            = "tenHR_TL_FuelMoisture",
    "100-hr Fuel Moisture"           = "hundredHR_TL_FuelMoisture",
    "1000-hr Fuel Moisture"          = "thousandHR_TL_FuelMoisture",
    "Live Woody Fuel Moisture"       = "woodyLFI_fuelMoisture",
    "Live Herbaceous Fuel Moisture"  = "herbaceousLFI_fuelMoisture",
    "Growing Season Index"           = "gsi"
  )
  
  weather_var_labels <- c(
    "Temperature (°F)" = "temperature",
    "Relative Humidity (%)" = "relativeHumidity",
    "Wind Speed (mph)" = "windSpeed",
    "Wind Gust (mph)" = "gustSpeed",
    "Wind Direction (°)" = "windDirection",
    "Gust Direction (°)" = "gustDirection",
    "Solar Radiation (W/m²)" = "solarRadiation",
    "Hourly Precipitation (in)" = "precipitation",
    "Vapor Pressure Deficit (kPa)" = "vpd",
    "Hot-Dry-Windy Index" = "hdw"
  )
  
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
    #req(all_data_cache(), input$variable, input$daily_stat, input$plot_year)
    # NEW: Friendly UI message when data is missing
    validate(
      need(!is.null(all_data_cache()), "Welcome! Please select your station(s) and click 'Fetch Station Data' to generate the chart.")
    )
    req(input$variable, input$daily_stat, input$plot_year)
    
    
    all_data <- all_data_cache()
    
    summary_fun <- match.fun(input$daily_stat)
    
    # Process base stn data (Keep record_type intact for splitting later)
    stn_data <- all_data %>%
      group_by(station_id, date, record_type) %>%
      summarise(value = safe_summary1(.data[[input$variable]], summary_fun), .groups = "drop") %>%
      mutate(year = as.integer(format(date, "%Y")),
             month_day = as.Date(format(date, "2024-%m-%d")))
    
    # Average across stations for each day & record_type
    all_data_sig <- stn_data %>%
      group_by(date, record_type) %>%
      summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
      mutate(year = as.integer(format(date, "%Y")),
             month_day = as.Date(format(date, "2024-%m-%d")))
    
    # HISTORICAL DATA: ONLY use Observations (O), never Forecasts (F)
    all_data_hist <- all_data_sig %>% filter(record_type == "O")
    
    historical_years <- all_data_hist %>% filter(year >= 2005 & year <= 2022) %>%
      summarise(start_year = min(year, na.rm = TRUE), end_year = max(year, na.rm = TRUE))
    
    clim_df <- all_data_hist %>% filter(year != input$plot_year) %>%
      group_by(month_day) %>%
      summarise(min = min(value, na.rm = TRUE),
                max = max(value, na.rm = TRUE),
                mean = mean(value, na.rm = TRUE),
                median = median(value, na.rm = TRUE), .groups = "drop")
    
    df_hist <- all_data_hist %>% filter(year >= 2005 & year <= 2022)
    
    p90_global <- quantile(df_hist$value, 0.90, na.rm = TRUE)
    p97_global <- quantile(df_hist$value, 0.97, na.rm = TRUE)
    p50_global <- quantile(df_hist$value, 0.50, na.rm = TRUE)
    p25_global <- quantile(df_hist$value, 0.25, na.rm = TRUE)
    
    ribbon_df <- df_hist %>%
      group_by(month_day) %>%
      summarise(q0 = quantile(value, 0.00, na.rm = TRUE),
                q33 = quantile(value, 0.33, na.rm = TRUE),
                q66 = quantile(value, 0.66, na.rm = TRUE),
                q90 = quantile(value, 0.90, na.rm = TRUE),
                q97 = quantile(value, 0.97, na.rm = TRUE),
                q100 = quantile(value, 1.00, na.rm = TRUE),
                .groups = "drop")
    
    ribbon_data <- tibble(
      range = c("0–33%", "33–66%", "66–90%", "90–97%", "97–100%"),
      ymin = c("q0", "q33", "q66", "q90", "q97"),
      ymax = c("q33", "q66", "q90", "q97", "q100")
    ) %>% pmap_dfr(function(range, ymin, ymax) {
      ribbon_df %>% transmute(month_day,
                              ymin = .data[[ymin]],
                              ymax = .data[[ymax]],
                              range = range)
    })
    
    # SPLIT CURRENT YEAR INTO OBSERVED AND FORECAST
    df_current_obs <- all_data_sig %>% 
      filter(year == input$plot_year, record_type == "O") %>%
      group_by(month_day) %>%
      summarise(value = mean(value, na.rm = TRUE), .groups = "drop")
    
    df_current_fcst <- all_data_sig %>% 
      filter(year == input$plot_year, record_type == "F") %>%
      group_by(month_day) %>%
      summarise(value = mean(value, na.rm = TRUE), .groups = "drop")
    
    station_names <- station_metadata %>%
      filter(station_id %in% selected_stations()) %>%
      pull(station_name) %>% unique()
    station_label <- paste(station_names, collapse = ", ")
    
    default_fill <- c("0–33%" = "#cce5ff", "33–66%" = "#e6f2ff", "66–90%" = "#ffe0b2",
                      "90–97%" = "#ffcc80", "97–100%" = "#ff9933")
    moisture_fill <- c("0–33%" = "#ff9933", "33–66%" = "#ffcc80", "66–90%" = "#ffe0b2",
                       "90–97%" = "#e6f2ff", "97–100%" = "#cce5ff")
    fill_values <- if (input$variable %in% reverse_fill_vars) moisture_fill else default_fill
    
    color_mapping <- setNames(c("blue", "orangered", "forestgreen"), 
                              c("Mean", paste0(input$plot_year, " Observed"), paste0(input$plot_year, " Forecast")))
    
    p <- ggplot() +
      geom_ribbon(data = ribbon_data,
                  aes(x = month_day, ymin = ymin, ymax = ymax, fill = range), alpha = 0.7) +
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
          paste0(pretty_variable_name(input$variable), " (Fuel Model ", input$fuel_model, ")")
        },
        subtitle = paste0(station_label, " | ", input$plot_year,
                          " vs Climatology (", historical_years$start_year, "–", historical_years$end_year, ")"),
        x = "Month-Day",
        y = paste(pretty_variable_name(input$variable), "(", input$daily_stat, ")"),
        caption = "Data from FEMS-API"
      ) +
      theme_bw(base_size = 14)
    
    # Plot observations
    if (nrow(df_current_obs) > 0) {
      p <- p + geom_line(data = df_current_obs, aes(x = month_day, y = value, color = paste0(input$plot_year, " Observed")), linewidth = 1.2)
    }
    # Plot Forecasts if toggled
    if (input$show_forecast && nrow(df_current_fcst) > 0) {
      p <- p + geom_line(data = df_current_fcst, aes(x = month_day, y = value, color = paste0(input$plot_year, " Forecast")), linewidth = 1.2, linetype = "solid")
    }
    
    return(p)
  })
  
  ##### plotly version
  output$plotly_climatology_plot <- renderPlotly({
    #req(all_data_cache(), input$variable, input$daily_stat, input$plot_year)
    # NEW: Friendly UI message when data is missing
    validate(
      need(!is.null(all_data_cache()), "Welcome! Please select your station(s) and click 'Fetch Station Data' to generate the chart.")
    )
    req(input$variable, input$daily_stat, input$plot_year)
    
    
    all_data <- all_data_cache()
    
    summary_fun <- match.fun(input$daily_stat)
    
    stn_data <- all_data %>%
      group_by(station_id, date, record_type) %>%
      summarise(value = safe_summary1(.data[[input$variable]], summary_fun), .groups = "drop") %>%
      mutate(year = as.integer(format(date, "%Y")), month_day = as.Date(format(date, "2024-%m-%d")))
    
    all_data_sig <- stn_data %>%
      group_by(date, record_type) %>%
      summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
      mutate(year = as.integer(format(date, "%Y")), month_day = as.Date(format(date, "2024-%m-%d")))
    
    all_data_hist <- all_data_sig %>% filter(record_type == "O")
    
    df_hist_all <- all_data_hist %>%
      filter(year != input$plot_year) %>%
      mutate(year_str = as.character(year)) %>%
      group_by(year_str, month_day) %>%
      summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
      mutate(text = paste("Year:", year_str, "<br>Date:", format(month_day, "%b-%d"), "<br>Value:", round(value, 1)))
    
    clim_df <- df_hist_all %>%
      group_by(month_day) %>%
      summarise(mean = mean(value, na.rm = TRUE), .groups = "drop") %>%
      mutate(text = paste("Date:", format(month_day, "%b-%d"), "<br>Mean:", round(mean, 1)))
    
    # SPLIT CURRENT YEAR
    df_current_obs <- all_data_sig %>%
      filter(year == input$plot_year, record_type == "O") %>%
      group_by(month_day) %>%
      summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
      mutate(text = paste("Date:", format(month_day, "%b-%d"), "<br>Observed:", round(value, 1)))
    
    df_current_fcst <- all_data_sig %>%
      filter(year == input$plot_year, record_type == "F") %>%
      group_by(month_day) %>%
      summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
      mutate(text = paste("Date:", format(month_day, "%b-%d"), "<br>Forecast:", round(value, 1)))
    
    ribbon_df <- df_hist_all %>%
      group_by(month_day) %>%
      summarise(
        q0 = quantile(value, 0.00, na.rm = TRUE), q33 = quantile(value, 0.33, na.rm = TRUE),
        q66 = quantile(value, 0.66, na.rm = TRUE), q90 = quantile(value, 0.90, na.rm = TRUE),
        q97 = quantile(value, 0.97, na.rm = TRUE), q100 = quantile(value, 1.00, na.rm = TRUE),
        .groups = "drop"
      )
    
    ribbon_data <- tibble(
      range = c("0–33%", "33–66%", "66–90%", "90–97%", "97–100%"),
      ymin = c("q0", "q33", "q66", "q90", "q97"),
      ymax = c("q33", "q66", "q90", "q97", "q100")
    ) %>% pmap_dfr(function(range, ymin, ymax) {
      ribbon_df %>% transmute(month_day, ymin = .data[[ymin]], ymax = .data[[ymax]], range = range)
    })
    
    default_fill <- c("0–33%" = "#cce5ff", "33–66%" = "#e6f2ff", "66–90%" = "#ffe0b2", "90–97%" = "#ffcc80", "97–100%" = "#ff9933")
    moisture_fill <- c("0–33%" = "#ff9933", "33–66%" = "#ffcc80", "66–90%" = "#ffe0b2", "90–97%" = "#e6f2ff", "97–100%" = "#cce5ff")
    fill_values <- if (input$variable %in% reverse_fill_vars) moisture_fill else default_fill
    
    n_hist_years <- length(unique(df_hist_all$year_str))
    hist_colors <- colorRampPalette(brewer.pal(8, "Dark2"))(n_hist_years)
    
    obsYr <- paste0(input$plot_year, " Observed")
    fcstYr <- paste0(input$plot_year, " Forecast")
    
    color_values <- c(
      setNames(hist_colors, unique(df_hist_all$year_str)),
      "Mean" = "blue",
      setNames("orangered", obsYr),
      setNames("forestgreen", fcstYr)
    )
    
    p <- ggplot() +
      geom_ribbon(data = ribbon_data, aes(x = month_day, ymin = ymin, ymax = ymax, fill = range), alpha = 0.7)
    
    if (isTruthy(input$show_hist_years)) {
      p <- p + geom_line(data = df_hist_all, aes(x = month_day, y = value, group = year_str, color = year_str, text = text), linewidth = 0.5, alpha = 0.4)
    }
    
    p <- p +
      geom_line(data = clim_df, aes(x = month_day, y = mean, group = 1, color = "Mean", text = text), linewidth = 0.75) +
      geom_hline(yintercept = quantile(df_hist_all$value, c(0.25, 0.5, 0.9, 0.97), na.rm = TRUE), linetype = "dashed", color = "gray40") +
      scale_x_date(date_labels = "%b-%d", date_breaks = "1 month", expand = expansion(mult = c(0, 0))) +
      scale_fill_manual("Daily %tile Range", values = fill_values) +
      scale_color_manual("Legend", values = color_values) +
      theme_bw(base_size = 14) + 
      labs(x = "Month-Day", y = paste(pretty_variable_name(input$variable), "(", input$daily_stat, ")"))
    
    if (nrow(df_current_obs) > 0) {
      p <- p + geom_line(data = df_current_obs, aes(x = month_day, y = value, group = 1, color = !!obsYr, text = text), linewidth = 1)
    }
    if (input$show_forecast && nrow(df_current_fcst) > 0) {
      p <- p + geom_line(data = df_current_fcst, aes(x = month_day, y = value, group = 1, color = !!fcstYr, text = text), linewidth = 1, linetype = "solid")
    }
    
    ggplotly(p, tooltip = "text") %>% layout(hovermode = "x unified")
  })
  
  # Basic Summary Table 
  output$summary_table <- renderDT({
    #req(all_data_cache(), input$variable, input$plot_year)
    # NEW: Friendly UI message when data is missing
    validate(
      need(!is.null(all_data_cache()), "No data loaded yet. Click 'Fetch Station Data' to populate this table.")
    )
    req(input$variable, input$plot_year)
    
    df <- all_data_cache() %>%
      filter(year(date) == input$plot_year) %>%
      select(date, hour, station_id, record_type, !!sym(input$variable)) %>%
      arrange(desc(date), desc(hour))
    datatable(df, options = list(pageLength = 10, scrollX = TRUE))
  })
  
}

shinyApp(ui = ui, server = server)