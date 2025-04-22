# process station metadata
# update metadata file from https://www.wildfire.gov/node/3473 or
# https://fems.fs2c.usda.gov/download
# MAC 04/22/25

# load libraries
library(lutz)

# station metadata file name
file<-"FEMS_3.0_RAWS_Master_Station_List_and_Metadata.csv"

# load file
station_metadata <- read.csv(file, stringsAsFactors = FALSE)

# thin out file to critical variables
station_metadata <- station_metadata[,c("station_id", "station_name", "latitude", "longitude", "elevation")]

# Get Olson time zone name
station_metadata$tz <- lutz::tz_lookup_coords(station_metadata$latitude, station_metadata$longitude, method = "accurate")

# save df to csv
write.csv(station_metadata, file = "station_metadata_FEMS3_042225.csv", row.names = FALSE)