# get MTBS fire points in bounding box
# https://data-usfs.hub.arcgis.com/datasets/81284bfaf86a4fa2a7d49c74424ffe1e_62/api
# https://data-usfs.hub.arcgis.com/datasets/81284bfaf86a4fa2a7d49c74424ffe1e_62/about

library(httr)
library(jsonlite)
library(dplyr)

get_fire_history_json <- function(xmin, ymin, xmax, ymax) {
  base_url <- "https://apps.fs.usda.gov/arcx/rest/services/EDW/EDW_MTBS_01/MapServer/62/query"
  
  params <- list(
    where = "1=1",
    outFields = "*",
    geometry = paste(xmin, ymin, xmax, ymax, sep = ","),
    geometryType = "esriGeometryEnvelope",
    inSR = "4326",
    spatialRel = "esriSpatialRelContains",
    outSR = "4326",
    f = "json"
  )
  
  response <- GET(base_url, query = params)
  
  if (status_code(response) != 200) {
    stop("Request failed with status code: ", status_code(response))
  }
  
  content_text <- content(response, as = "text", encoding = "UTF-8")
  parsed_content <- fromJSON(content_text)
  
  # ✅ Check for empty 'features'
  if (length(parsed_content$features) == 0) {
    stop("No fire history records found in the specified bounding box.")
  }
  
  # Flatten attributes
  fire_df <- parsed_content[["features"]][["attributes"]]
  
  # Convert IG_DATE to POSIXct if present
  if ("IG_DATE" %in% names(fire_df)) {
    fire_df <- fire_df %>%
      mutate(IG_DATE = as.POSIXct(IG_DATE / 1000, origin = "1970-01-01", tz = "UTC"))
  }
  
  return(fire_df)
}

fire_df <- get_fire_history_json(-111.544, 32.089, -110.366, 32.496)
fire_df <- get_fire_history_json(-111, 32, -111, 32)

