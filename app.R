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

# -----------------------------
# Helper Functions
# -----------------------------
#####

# Safe summary function to avoid warnings
safe_summary1 <- function(x, fun) {
  if (all(is.na(x))) NA else fun(x, na.rm = TRUE)
}
# safe_summary <- function(x, fun) {
#   fun <- rlang::as_function(fun)  # Ensures anonymous/lambda support
#   if (all(is.na(x))) return(NA)
#   
#   # Check if the function has a formal argument named 'na.rm'
#   if ("na.rm" %in% names(formals(fun))) {
#     fun(x, na.rm = TRUE)
#   } else {
#     fun(x)
#   }
# }

# updated download_nfdrs_data function with dynamic timezone

download_nfdrs_data <- function(station_id, start_date, end_date,
                                fuel_model = "Y", dataset = "observation") {
  
  # Find the local timezone from station metadata
  local_tz <- station_metadata %>%
    filter(station_id == !!station_id) %>%
    pull(tz) %>%
    unique()
  
  # Fallback if timezone not found
  if (length(local_tz) == 0 || is.na(local_tz)) {
    warning(paste("Timezone not found for station", station_id, "- defaulting to UTC"))
    local_tz <- "UTC"
  }
  
  base_url <- "https://fems.fs2c.usda.gov/api/climatology/download-nfdr"
  start_iso <- paste0(start_date, "T00:00:00Z")
  end_iso <- paste0(end_date, "T23:59:59Z")
  query <- list(
    stationIds = station_id,
    startDate = start_iso,
    endDate = end_iso,
    dataFormat = "csv",
    dataset = dataset,
    fuelModels = fuel_model
  )
  url <- paste0(base_url, "?", paste0(names(query), "=", query, collapse = "&"))
  message("Fetching data from: ", url)
  
  tryCatch({
    df <- read.csv(url, stringsAsFactors = FALSE)
    df$observationTime_UTC <- as.POSIXct(df$observationTime, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
    df$observationTime_local <- format(df$observationTime_UTC, tz = local_tz, usetz = FALSE)
    df$observationTime_local <- as.POSIXct(df$observationTime_local, tz = local_tz)
    #df$date <- as.Date(df$observationTime_local)
    df$date <- as.Date(format(df$observationTime_local, tz = local_tz))
    df$hour <- as.integer(format(df$observationTime_local, "%H"))
    df$station_id <- station_id # added station id back in 
    return(df)
  }, error = function(e) {
    warning("Failed to download or parse the data: ", conditionMessage(e))
    return(NULL)
  })
}

#####

#####
# weather download function

download_weather_data <- function(station_id, start_date, end_date) {
  local_tz <- station_metadata %>%
    filter(station_id == !!station_id) %>%
    pull(tz) %>%
    unique()
  
  if (length(local_tz) == 0 || is.na(local_tz)) {
    warning(paste("Timezone not found for station", station_id, "- defaulting to UTC"))
    local_tz <- "UTC"
  }
  
  base_url <- "https://fems.fs2c.usda.gov/api/climatology/download-weather"
  start_iso <- paste0(start_date, "T00:00:00Z")
  end_iso <- paste0(end_date, "T23:59:59Z")
  
  query <- list(
    stationIds = station_id,
    startDate = start_iso,
    endDate = end_iso,
    dataFormat = "csv",
    dataIncrement = "hourly",
    dataset = "observation",
    stationtypes = "RAWS(SATNFDRS)"
  )
  
  url <- paste0(base_url, "?", paste0(names(query), "=", query, collapse = "&"))
  message("Fetching weather data from: ", url)
  
  tryCatch({
    df <- read.csv(url, stringsAsFactors = FALSE)
    
    # Clean up column names: remove units and spaces
    #names(df) <- gsub("\\(.*?\\)", "", names(df))     # Remove units in parentheses
    #names(df) <- gsub("[[:space:]]+", "", names(df))  # Remove whitespace
    df$station_id <- station_id # added station id back in 
    
    # Rename columns to app-compatible format
    df <- df %>%
      rename(
        #station_id       = StationId,
        observationTime  = DateTime,
        temperature      = Temperature.F.,
        relativeHumidity = RelativeHumidity...,
        precipitation    = Precipitation.in.,
        windSpeed        = WindSpeed.mph.,
        windDirection    = WindAzimuth.degrees.,
        gustSpeed        = GustSpeed.mph.,
        gustDirection    = GustAzimuth.degrees.,
        solarRadiation   = SolarRadiation.W.m2.
      )
    
    
    # Convert timestamps
    df$observationTime_UTC <- as.POSIXct(df$observationTime, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
    df$observationTime_local <- as.POSIXct(format(df$observationTime_UTC, tz = local_tz, usetz = FALSE), tz = local_tz)
    df$date <- as.Date(df$observationTime_local)
    df$hour <- as.integer(format(df$observationTime_local, "%H"))
    
    # Drop unused flag columns
    df <- df %>% select(-starts_with("Tflag"), -starts_with("RHflag"), -starts_with("PCPflag"),
                        -starts_with("WSflag"), -starts_with("WAflag"), -starts_with("SRflag"),
                        -starts_with("GSflag"), -starts_with("GAflag"), -SnowFlag, -ObservationType)
    
    # add in VPD
    # Convert temperature to Celsius
    df$temp_c <- (df$temperature - 32) * 5 / 9
    
    # Calculate Vapor Pressure Deficit (VPD)
    df$vpd <- with(df, {
      es <- 0.6108 * exp((17.27 * temp_c) / (temp_c + 237.3))
      (1 - relativeHumidity / 100) * es
    })
    
    # Calculate hourly HDW using VPD in hPa and wind speed in m/s (inline conversion)
    df$hdw<- (df$windSpeed * 0.44704) * (df$vpd*10)
    
    # Optional: remove intermediate temp_c if not needed
    df <- df %>% select(-temp_c)
    
    
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
# Load Station Metadata -- created using processMetaData.R
# -----------------------------
#station_metadata <- read.csv("FEMS_3.0_RAWS_Master_Station_List_and_Metadata.csv", stringsAsFactors = FALSE)
station_metadata <- read.csv("station_metadata_FEMS3_042225.csv", stringsAsFactors = FALSE)

# -----------------------------
# UI
# -----------------------------
ui <- fluidPage(
  # google analytics tracking code
  tags$head(
    # Google Analytics (GA4)
    HTML("
      <!-- Global site tag (gtag.js) - Google Analytics -->
      <script async src='https://www.googletagmanager.com/gtag/js?id=G-JJ5NSHGJE6'></script>
      <script>
        window.dataLayer = window.dataLayer || [];
        function gtag(){dataLayer.push(arguments);}
        gtag('js', new Date());
        gtag('config', 'G-JJ5NSHGJE6');
      </script>
    ")
  ),
  # set theme
  theme = bs_theme(bootswatch = "journal"),
  # Add the logo at the top of the page
  tags$div(
    style = "display: flex; align-items: center; justify-content: space-between; margin-bottom: 20px;",
    tags$h1("NFDRS Chart Builder"),
    tags$img(src = "UAlogo.jpg", height = "60px")  # Adjust the height as needed
  ),
  # Footer with logo
  # tags$div(
  #   style = "position: fixed; bottom: 0; width: 100%; text-align: center; padding: 10px; background-color: #f8f9fa;",
  #   tags$img(src = "BP_app_logos.png", height = "60px") # Adjust src to point to your logo file and set the desired height
  # ),
  #titlePanel("NFDRS Chart Builder"),
  sidebarLayout(
    sidebarPanel(
      leafletOutput("station_map", height = 400),
      uiOutput("station_selector"),
      #selectInput("fuel_model", "Fuel Model", choices = c("Y", "V", "W","X","Z")),
      selectInput("fuel_model", "Fuel Model", choices = c("Y","Z")),
      uiOutput("variable_selector"),
      selectInput("daily_stat", "Daily Statistic", choices = c("mean", "min", "max")),
      numericInput("plot_year", "Plot Year (2005-Present)",
                   value = as.numeric(format(Sys.Date(), "%Y")),
                   min = 2005, max = as.numeric(format(Sys.Date(), "%Y"))),
      
      # Container div for logo and contact info
      div(
        style = "text-align: center;",  # center them (optional)
        
        # Logo image (assumes file is in www/my_logo.png)
        tags$img(src = "BP_app_logos.png", style = "width: 100%; height: auto;"),
        
        # Contact info
        tags$p("Contact: Mike Crimmins, crimmins@arizona.edu"),
        tags$p("https://cales.arizona.edu/climate/")
      )
      
    ),
    # mainPanel(
    #   plotOutput("climatology_plot", height = "700px")
    # )
    mainPanel(
      tabsetPanel(
        tabPanel("Static Plot", plotOutput("climatology_plot", height = "700px")),
        #tabPanel("Interactive Plot", plotlyOutput("plotly_climatology_plot", height = "700px")),
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
                     p("Users can explore historical patterns and current and past year observations of key fire weather indices across selected stations and fuel models.
                       Data access provided by the Fire Environment Mapping System", tags$a(href = "https://fems.fs2c.usda.gov/", "(FEMS)")),
                     
                     h4("🛠️ How to Use"),
                     tags$ol(
                       tags$li("Use the map or dropdown to select one or more stations (select/deselect stations using map or dropdown)."),
                       tags$li("Select a fuel model available through FEMS (V,W,X,Y,Z)."),
                       tags$li("Choose a variable such as ERC, BI, IC, SC, or KBDI."),
                       tags$li("Pick a daily summary statistic (mean, min, or max) and a target year."),
                       tags$li("View results in either a static or interactive plot tab.")
                     ),
                     h4("🔍 Features"),
                     tags$ul(
                       tags$li("Select one or more stations to create daily composites (SIGs)"),
                       tags$li("Daily summaries of hourly data using min, max or mean values"),
                       tags$li("Data caching by station and fuel model for performance"),
                       tags$li("Static map can be saved by right clicking and selecting 'Save Image As...'"),
                       tags$li("Interactive map with zoom, pan and snap capabilities")
                     ),
                     hr(),
                     h4("📬 Contact"),
                     p("For questions or feedback about this tool, please contact:",
                       tags$br(),
                       tags$b("Mike Crimmins"), tags$br(),
                       "Email: ", tags$a(href = "mailto:crimmins@arizona.edu", "crimmins@arizona.edu"), tags$br(),
                       "Web: ", tags$a(href = "https://cales.arizona.edu/climate", "https://cales.arizona.edu/climate"), tags$br(),
                       "GitHub: ", tags$a(href = "https://github.com/mcrimmins/NFDRSchartBuilder",
                                          "https://github.com/mcrimmins/NFDRSchartBuilder")
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
  #selected_stations <- reactiveVal(character(0))
  selected_stations <- reactiveVal(default_station_id)
  
  data_cache <- reactiveValues()
  weather_data_cache <- reactiveValues()
  all_data_cache <- reactiveVal(NULL)
  
  output$station_selector <- renderUI({
    selectInput("station_ids", "Selected Stations",
                choices = setNames(station_metadata$station_id, station_metadata$station_name),
                selected = selected_stations(), multiple = TRUE)
  })
  
  # Render map
  output$station_map <- renderLeaflet({
    leaflet(station_metadata) |>
      #addTiles() |>
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
  
  # Handle map marker click
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
  
  # Sync dropdown selection -> update reactive value and map
  # observeEvent(input$station_ids, {
  #   selected_stations(input$station_ids)
  #   leafletProxy("station_map") |>
  #     clearMarkers() |>
  #     addCircleMarkers(
  #       data = station_metadata,
  #       lng = ~longitude, lat = ~latitude,
  #       layerId = ~station_id,
  #       label = ~station_name,
  #       radius = 5,
  #       color = ~ifelse(station_id %in% input$station_ids, "red", "blue"),
  #       fillOpacity = 0.7
  #     )
  # })
  
  ####
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
    
    # Zoom to selected stations
    if (nrow(sel_data) == 1) {
      leafletProxy("station_map") %>%
        setView(lng = sel_data$longitude, lat = sel_data$latitude, zoom = 7)
    } else if (nrow(sel_data) > 1) {
      # leafletProxy("station_map") %>%
      #   setView(lng = sel_data$longitude[1], lat = sel_data$latitude[1], zoom = 7)
      leafletProxy("station_map") %>%
        fitBounds(
          lng1 = min(sel_data$longitude),
          lat1 = min(sel_data$latitude),
          lng2 = max(sel_data$longitude),
          lat2 = max(sel_data$latitude)
        )
    }
  })
  ####
  
  
  
  # Dropdown UI for selected stations
  output$station_selector <- renderUI({
    selectInput("station_ids", "Selected Stations",
                choices = setNames(station_metadata$station_id, station_metadata$station_name),
                selected = selected_stations(), multiple = TRUE)
  })
  
  # Trigger download if fuel model or selection changes
  observeEvent(list(selected_stations(), input$fuel_model), {
    req(input$fuel_model)
    stns <- selected_stations()
    req(length(stns) > 0)
    
    withProgress(message = "Downloading & assembling data...", {
      all_nfdrs <- map_dfr(stns, function(id) {
        key <- paste(id, input$fuel_model, sep = "_")
        if (!is.null(data_cache[[key]])) {
          data_cache[[key]]
        } else {
          df <- download_nfdrs_data(id, "2000-01-01", Sys.Date(), input$fuel_model)
          
          if (is.null(df)) {
            showNotification(paste("Failed to fetch NFDRS data for station", id),
                             type = "error", duration = 6)
            return(tibble())  # Safely return empty data
          }
          
          df$station_id <- id
          data_cache[[key]] <- df
          df
        }
      })
      
      # all_weather <- map_dfr(stns, function(id) {
      #   if (!is.null(weather_data_cache[[id]])) {
      #     weather_data_cache[[id]]
      #   } else {
      #     df <- download_weather_data(id, "2000-01-01", Sys.Date())
      #     if (!is.null(df)) {
      #       df$station_id <- id
      #       weather_data_cache[[id]] <- df
      #     }
      #     df
      #   }
      # })
      
      all_weather <- map_dfr(stns, function(id) {
        if (!is.null(weather_data_cache[[id]])) {
          weather_data_cache[[id]]
        } else {
          df <- download_weather_data(id, "2000-01-01", Sys.Date())
          
          # 👉 Add this block to notify if download failed
          if (is.null(df)) {
            showNotification(paste("Failed to fetch weather data for station", id),
                             type = "error", duration = 6)
            return(tibble())  # Return empty tibble to avoid breaking map_dfr
          }
          
          df$station_id <- id
          weather_data_cache[[id]] <- df
          df
        }
      })
      
      all_data<- left_join(
        all_nfdrs, all_weather,
        by = c("station_id", "observationTime_local", "date", "hour")
      )
      
      all_data_cache(all_data)
    })
  })
  
  
  # Update variable selection once data is ready
  # output$variable_selector <- renderUI({
  #   req(all_data_cache())
  #   vars <- names(all_data_cache())[sapply(all_data_cache(), is.numeric)]
  #   selectInput("variable", "Select Variable", choices = vars, selected = vars[1])
  # })
  
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
  
  # for plotting
  weather_vars <- unname(weather_var_labels)
  
  #####
  
  # Update variable selector based on available data
  output$variable_selector <- renderUI({
    req(all_data_cache())
    
    # Combine with your NFDRS variable labels
    available_vars <- names(all_data_cache())[sapply(all_data_cache(), is.numeric)]
    display_vars <- c(nfdrs_labels, weather_var_labels)
    display_vars <- display_vars[display_vars %in% available_vars]
    
    selectInput("variable", "Select Variable", choices = display_vars, selected = display_vars[1])
  })
  
  
  # Plot rendering
  output$climatology_plot <- renderPlot({
    req(all_data_cache(), input$variable, input$daily_stat, input$plot_year)
    all_data <- all_data_cache()
    
    summary_fun <- match.fun(input$daily_stat)
    stn_data <- all_data %>%
      group_by(station_id, date) %>%
      summarise(value = safe_summary1(.data[[input$variable]], summary_fun), .groups = "drop") %>%
      mutate(year = as.integer(format(date, "%Y")),
             month_day = as.Date(format(date, "2024-%m-%d")))
    
    all_data <- stn_data %>%
      group_by(date) %>%
      summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
      mutate(year = as.integer(format(date, "%Y")),
             month_day = as.Date(format(date, "2024-%m-%d")))
    
    #historical_years <- all_data %>% filter(year != input$plot_year) %>%
    #  summarise(start_year = min(year, na.rm = TRUE), end_year = max(year, na.rm = TRUE))
    historical_years <- all_data %>% filter(year >= 2005 & year <= 2022) %>%
      summarise(start_year = min(year, na.rm = TRUE), end_year = max(year, na.rm = TRUE))
    
    clim_df <- all_data %>% filter(year != input$plot_year) %>%
      group_by(month_day) %>%
      summarise(min = min(value, na.rm = TRUE),
                max = max(value, na.rm = TRUE),
                mean = mean(value, na.rm = TRUE),
                median = median(value, na.rm = TRUE), .groups = "drop")
    
    df_current <- all_data %>% filter(year == input$plot_year) %>%
      group_by(month_day) %>%
      summarise(value = mean(value, na.rm = TRUE), .groups = "drop")
    
    #df_hist <- all_data %>% filter(year != input$plot_year)
    df_hist <- all_data %>% filter(year >= 2005 & year <= 2022)
    
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
    
    station_names <- station_metadata %>%
      filter(station_id %in% selected_stations()) %>%
      pull(station_name) %>% unique()
    
    # station_label <- if (length(station_names) <= 3) {
    #   paste(station_names, collapse = ", ")
    # } else {
    #   paste0(length(station_names), " stations: ", paste(station_names[1:3], collapse = ", "), ", …")
    # }
    station_label <- paste(station_names, collapse = ", ")
    
    # remapped fill for moisture-type variables
    default_fill <- c("0–33%" = "#cce5ff", "33–66%" = "#e6f2ff", "66–90%" = "#ffe0b2",
                      "90–97%" = "#ffcc80", "97–100%" = "#ff9933")
    moisture_fill <- c("0–33%" = "#ff9933", "33–66%" = "#ffcc80", "66–90%" = "#ffe0b2",
                       "90–97%" = "#e6f2ff", "97–100%" = "#cce5ff")
    fill_values <- if (input$variable %in% reverse_fill_vars) moisture_fill else default_fill
    
    ggplot() +
      geom_ribbon(data = ribbon_data,
                  aes(x = month_day, ymin = ymin, ymax = ymax, fill = range), alpha = 0.7) +
      geom_line(data = clim_df, aes(x = month_day, y = mean, color = "Mean"), linewidth = 0.6) +
      geom_line(data = df_current, aes(x = month_day, y = value, color = paste0(input$plot_year, " Observed")), linewidth = 1.2) +
      geom_hline(yintercept = c(p25_global, p50_global, p90_global, p97_global), color = "gray40", linetype = "dashed") +
      annotate("text", x = as.Date("2024-01-03"), y = p90_global,
               label = "90%", hjust = 0, vjust = -0.5, size = 3, color = "gray40") +
      annotate("text", x = as.Date("2024-01-03"), y = p97_global,
               label = "97%", hjust = 0, vjust = -0.5, size = 3, color = "gray40") +
      annotate("text", x = as.Date("2024-01-03"), y = p50_global,
               label = "50%", hjust = 0, vjust =-0.5, size = 3, color = "gray40") +
      annotate("text", x = as.Date("2024-01-03"), y = p25_global,
               label = "25%", hjust = 0, vjust = -0.5, size = 3, color = "gray40") +
      scale_x_date(date_labels = "%b-%d", date_breaks = "1 month",expand = expansion(mult = c(0, 0))) +
      # scale_fill_manual("Daily %tile Range",
      #                   values = c("0–33%" = "#cce5ff", "33–66%" = "#e6f2ff", "66–90%" = "#ffe0b2", 
      #                              "90–97%" = "#ffcc80", "97–100%" = "#ff9933")) +
      scale_fill_manual("Daily %tile Range",
                        values = fill_values) +
      scale_color_manual(name = NULL,
                         values = setNames(c("blue", "orangered"), c("Mean", paste0(input$plot_year, " Observed")))) +
      labs(
        #title = paste0(pretty_variable_name(input$variable), " (Fuel Model ", input$fuel_model, ")"),
        title = if (input$variable %in% weather_vars) {
          names(weather_var_labels)[match(input$variable, weather_var_labels)]
        } else {
          paste0(pretty_variable_name(input$variable), " (Fuel Model ", input$fuel_model, ")")
        },
        subtitle = paste0(station_label, " | ", input$plot_year,
                          " vs Climatology (", historical_years$start_year, "–", historical_years$end_year, ")"),
        x = "Month-Day",
        y = paste(pretty_variable_name(input$variable), "(", input$daily_stat, ")"),
        caption = paste0("EXPERIMENTAL PRODUCT | University of Arizona - Climate Science Applications Program | Data from FEMS-API | Observations through ",
                         df_current$month_day[nrow(df_current)])
      ) +
      theme_bw(base_size = 14)
  })
  
  
  ##### plotly version
  # Plot rendering
  
  output$plotly_climatology_plot <- renderPlotly({
    req(all_data_cache(), input$variable, input$daily_stat, input$plot_year)
    all_data <- all_data_cache()
    
    summary_fun <- match.fun(input$daily_stat)
    
    stn_data <- all_data %>%
      group_by(station_id, date) %>%
      summarise(value = safe_summary1(.data[[input$variable]], summary_fun), .groups = "drop") %>%
      mutate(
        year = as.integer(format(date, "%Y")),
        month_day = as.Date(format(date, "2024-%m-%d"))
      )
    
    all_data <- stn_data %>%
      group_by(date) %>%
      summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
      mutate(
        year = as.integer(format(date, "%Y")),
        month_day = as.Date(format(date, "2024-%m-%d"))
      )
    
    df_hist_all <- all_data %>%
      filter(year != input$plot_year) %>%
      mutate(year_str = as.character(year)) %>%
      group_by(year_str, month_day) %>%
      summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
      mutate(
        text = paste("Year:", year_str, "<br>Date:", format(month_day, "%b-%d"), "<br>Value:", round(value, 1))
      )
    
    clim_df <- df_hist_all %>%
      group_by(month_day) %>%
      summarise(mean = mean(value, na.rm = TRUE), .groups = "drop") %>%
      mutate(
        text = paste("Date:", format(month_day, "%b-%d"), "<br>Mean:", round(mean, 1))
      )
    
    df_current <- all_data %>%
      filter(year == input$plot_year) %>%
      group_by(month_day) %>%
      summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
      mutate(
        text = paste("Date:", format(month_day, "%b-%d"), "<br>Value:", round(value, 1))
      )
    
    ribbon_df <- df_hist_all %>%
      group_by(month_day) %>%
      summarise(
        q0 = quantile(value, 0.00, na.rm = TRUE),
        q33 = quantile(value, 0.33, na.rm = TRUE),
        q66 = quantile(value, 0.66, na.rm = TRUE),
        q90 = quantile(value, 0.90, na.rm = TRUE),
        q97 = quantile(value, 0.97, na.rm = TRUE),
        q100 = quantile(value, 1.00, na.rm = TRUE),
        .groups = "drop"
      )
    
    ribbon_data <- tibble(
      range = c("0–33%", "33–66%", "66–90%", "90–97%", "97–100%"),
      ymin = c("q0", "q33", "q66", "q90", "q97"),
      ymax = c("q33", "q66", "q90", "q97", "q100")
    ) %>% pmap_dfr(function(range, ymin, ymax) {
      ribbon_df %>%
        transmute(
          month_day,
          ymin = .data[[ymin]],
          ymax = .data[[ymax]],
          range = range
        )
    })
    
    default_fill <- c("0–33%" = "#cce5ff", "33–66%" = "#e6f2ff", "66–90%" = "#ffe0b2",
                      "90–97%" = "#ffcc80", "97–100%" = "#ff9933")
    moisture_fill <- c("0–33%" = "#ff9933", "33–66%" = "#ffcc80", "66–90%" = "#ffe0b2",
                       "90–97%" = "#e6f2ff", "97–100%" = "#cce5ff")
    fill_values <- if (input$variable %in% reverse_fill_vars) moisture_fill else default_fill
    
    # color_values <- c(
    #   setNames(rep("gray60", length(unique(df_hist_all$year_str))), unique(df_hist_all$year_str)),
    #   "Mean" = "blue",
    #   "Observed" = "orangered"
    # )
    
    # How many historic years
    n_hist_years <- length(unique(df_hist_all$year_str))
    
    # Generate distinct colors
    hist_colors <- colorRampPalette(brewer.pal(8, "Dark2"))(n_hist_years)
    
    # Combine
    # color_values <- c(
    #   setNames(hist_colors, unique(df_hist_all$year_str)),   # historic years
    #   "Mean" = "blue",                                        # mean line
    #   "Observed" = "orangered"                                # observed current year
    # )
    
    # obs yr label
    obsYr<-paste0(input$plot_year, " Observed")
    
    # Combine
    color_values <- c(
      setNames(hist_colors, unique(df_hist_all$year_str)),   # historic years
      "Mean" = "blue",                                        # mean line
      setNames("orangered", obsYr)                                # observed current year
    )
    
    p <- ggplot() +
      geom_ribbon(data = ribbon_data,
                  aes(x = month_day, ymin = ymin, ymax = ymax, fill = range), alpha = 0.7)
    
    if (isTruthy(input$show_hist_years)) {
      p <- p + geom_line(
        data = df_hist_all,
        aes(x = month_day, y = value, group = year_str, color = year_str, text = text),
        linewidth = 0.5, alpha = 0.4
      )
    }
    
    p <- p +
      geom_line(data = clim_df,
                aes(x = month_day, y = mean, group = 1, color = "Mean", text = text),
                linewidth = 0.75) +
      geom_line(data = df_current,
                aes(x = month_day, y = value, group = 1, color = paste0(input$plot_year, " Observed"), text = text),
                linewidth = 1) +
      geom_hline(yintercept = quantile(df_hist_all$value, c(0.25, 0.5, 0.9, 0.97), na.rm = TRUE),
                 linetype = "dashed", color = "gray40") +
      scale_x_date(date_labels = "%b-%d", date_breaks = "1 month", expand = expansion(mult = c(0, 0))) +
      scale_fill_manual("Daily %tile Range", values = fill_values) +
      scale_color_manual("Legend", values = color_values) +
      labs(
        #title = paste0(pretty_variable_name(input$variable), " (Fuel Model ", input$fuel_model, ")"),
        title = if (input$variable %in% weather_vars) {
          names(weather_var_labels)[match(input$variable, weather_var_labels)]
        } else {
          paste0(pretty_variable_name(input$variable), " (Fuel Model ", input$fuel_model, ")")
        },
        subtitle = paste0("Station(s): ", paste(selected_stations(), collapse = ", "), " | Year: ", input$plot_year),
        x = "Month-Day",
        y = paste(pretty_variable_name(input$variable), "(", input$daily_stat, ")"),
        caption = paste0("Data from FEMS | Updated through ", Sys.Date())
      ) +
      theme_bw(base_size = 14)
    
    # ggplotly(p, tooltip = "text") %>%
    #   layout(legend = list(title = list(text = "Daily Values")))
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        title = list(font = list(size = 14)),
        xaxis = list(
          title = list(font = list(size = 12)),
          tickfont = list(size = 11)
        ),
        yaxis = list(
          title = list(font = list(size = 12)),
          tickfont = list(size = 11)
        ),
        legend = list(
          #title = list(text = "Daily Values"),
          title = list(text = "<span style='font-size:10pt;'>Daily Values</span>"),
          font = list(size = 10)
        )
      )
    
  })
  
  #####
  
  ##### add in summary datatable
  
  # Render summary table
  output$summary_table <- renderDT({
    req(all_data_cache())
    data <- all_data_cache()
    
    # Filter to non-plot year
    #df_hist <- data %>% filter(format(date, "%Y") != input$plot_year)
    df_hist <- data
    
    # Variables to exclude from summary
    exclude_vars <- c("stationId", "hour")
    
    # Numeric columns excluding unwanted ones
    numeric_vars <- names(df_hist)[sapply(df_hist, is.numeric) & !(names(df_hist) %in% exclude_vars)]
    
    # Safe summary helper
    safe_summary <- function(x, fun) {
      fun <- rlang::as_function(fun)  # Ensures anonymous/lambda support
      if (all(is.na(x))) return(NA)
      
      # Check if the function has a formal argument named 'na.rm'
      if ("na.rm" %in% names(formals(fun))) {
        fun(x, na.rm = TRUE)
      } else {
        fun(x)
      }
    }
    
    # Calculate summaries
    summary_df <- map_dfr(numeric_vars, function(var) {
      vec <- df_hist[[var]]
      tibble(
        Variable = var,
        Min    = safe_summary(vec, ~ min(., na.rm = TRUE)),
        Max    = safe_summary(vec, ~ max(., na.rm = TRUE)),
        Mean   = safe_summary(vec, ~ mean(., na.rm = TRUE)),
        Median = safe_summary(vec, ~ median(., na.rm = TRUE)),
        Q25    = safe_summary(vec, ~ quantile(., 0.25, na.rm = TRUE)),
        Q50    = safe_summary(vec, ~ quantile(., 0.50, na.rm = TRUE)),
        Q75    = safe_summary(vec, ~ quantile(., 0.75, na.rm = TRUE)),
        Q90    = safe_summary(vec, ~ quantile(., 0.90, na.rm = TRUE)),
        Q97    = safe_summary(vec, ~ quantile(., 0.97, na.rm = TRUE))
      )
    })
    
    
    # Map readable labels using nfdrs and weather label dictionaries
    label_map <- c(nfdrs_labels, weather_var_labels)
    summary_df$Variable <- ifelse(
      summary_df$Variable %in% label_map,
      names(label_map)[match(summary_df$Variable, label_map)],
      summary_df$Variable
    )
    
    # Round all numeric columns
    summary_df[-1] <- lapply(summary_df[-1], function(x) round(x, 1))
    
    # Dynamic period title
    years <- range(as.integer(format(df_hist$date, "%Y")), na.rm = TRUE)
    
    # Return datatable with styled caption
    DT::datatable(
      summary_df,
      rownames = FALSE,
      options = list(pageLength = 25, scrollX = TRUE, dom = 't'),
      caption = htmltools::tags$caption(
        style = 'caption-side: top; text-align: left; font-weight: bold;',
        paste0("Summary Statistics (", years[1], "–", years[2], ")")
      )
    )
  })
  #####
  
}

# Run the app
shinyApp(ui, server)

