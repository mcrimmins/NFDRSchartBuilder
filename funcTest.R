library(dplyr)
library(readr)
library(lubridate)
library(httr)

download_nfdrs_data <- function(station_id, start_date, end_date,
                                fuel_model = "Y", dataset = "observation") {
  
  # 1. Construct the URL safely using httr
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
  
  # Let httr handle the URL encoding natively
  url <- modify_url(base_url, query = query_params)
  message("Fetching data from: ", url)
  
  tryCatch({
    # 2. Read CSV (suppress messages for clean console output)
    df <- read_csv(url, show_col_types = FALSE)
    
    if(nrow(df) == 0) {
      warning("API returned an empty dataset for station: ", station_id)
      return(NULL)
    }
    
    # 3. Identify the correct time column (FEMS sometimes uses ObservationTime or observation_time_lst)
    time_col <- if("observation_time_lst" %in% names(df)) "observation_time_lst" else "ObservationTime"
    
    if (!(time_col %in% names(df))) {
      stop("Could not find a valid time column in the API response.")
    }
    
    # 4. THE CHECK & PARSE
    # Check if read_csv already did the hard work and parsed it as a datetime
    if (inherits(df[[time_col]], "POSIXt")) {
      df$parsed_time <- df[[time_col]]
      
    } else {
      # If it's a character string, parse it manually
      time_str <- as.character(df[[time_col]])
      
      # The 'truncated = 3' argument tells lubridate not to panic if "00:00:00" is missing
      if (any(grepl("Z$", time_str, ignore.case = TRUE))) {
        df$parsed_time <- ymd_hms(time_str, tz = "UTC", truncated = 3, quiet = TRUE)
      } else {
        df$parsed_time <- ymd_hms(time_str, truncated = 3, quiet = TRUE) 
      }
    }
    
    # 5. Build your final columns and standardize names
    df <- df %>%
      mutate(
        station_id = station_id,
        date = as.Date(parsed_time),
        # Extract the hour directly from the parsed datetime object
        hour = as.integer(format(parsed_time, "%H"))
      ) %>%
      # Clean up the temporary column
      select(-parsed_time) %>%
      # Rename new API PascalCase columns to match the old Shiny app lowerCamelCase expectations
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
        herbaceousLFI_fuelMoisture = "HerbaceousLFI_fuelMoisture"
      )))
    
    return(df)
    
  }, error = function(e) {
    warning("Failed to download or parse the data: ", conditionMessage(e))
    return(NULL)
  })
}