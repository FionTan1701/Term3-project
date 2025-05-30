library(tidyverse)
library(dplyr)
library(terra)
library(sf)
library(gstat)    
library(lubridate)
library(data.table) # usefule for reading in large csv files

#set directory
setwd("~/Term3-project/")

# Read sit catchment -----------------------------------------------------------

stw<- st_read("Data/STW/stw_catchment_FINAL.shp", crs= 27700)

stw<- st_transform(stw, crs= 27700)

stw.df <- st_drop_geometry(stw)

# area of stw catchment
stw$stw_area<- st_area(stw)


# Reading and processing data as rasters  --------------------------------------

era5.folder<- "Data/Covariates/raw/era5"

# list all grib files
grib.files<- list.files(era5.folder, pattern= "\\.grib", full.names=T)

# crop to extenys

# read in each grib file
raster_list <- lapply(grib.files, rast)

# stack rasters
raster_stack <- rast(grib.files)

# Project and crop raster to extent of England  --------------------------------

# get england shapefile and project data to its extent
england<- st_read("Data/shapefiles/england/england_crop.shp")


# project raster to england so they have the same 
rast <- project(raster_stack, crs(england))
#april <- project(april, crs(england))

# crop raster to england
cropped_raster <- crop(rast, england)


vars.df <- as.data.frame(cropped_raster, xy = TRUE)


# Split into variables  --------------------------------------------------------

# temperature
temp.df <- cbind(vars.df[,c(1,2)], vars.df[,grepl("temp", names(vars.df))])

# rain
rain.df<- cbind(vars.df[,c(1,2)], vars.df[,grepl("precipitation", names(vars.df))])

write.csv(rain.df, "rain_df.csv")
write.csv(temp.df, "temp_df.csv")

# Convert wide to long  --------------------------------------------------------

names <- c("temp", "rain")

result_list <- list()

# Loop over each variable name
for (var_name in names) {
  # Dynamically refer to the correct dataframe (e.g., temp.df for temp, rain.df for rain)
  df <- get(paste0(var_name, ".df"))
  
  # Perform the pivot and transformation
  long_df <- df %>%
    pivot_longer(cols = !c("x", "y"), names_to = "Time", values_to = var_name) %>%
    mutate(Time = ifelse(str_detect(Time, "\\.\\d+$"),
                         as.numeric(str_extract(Time, "\\d+$")),
                         0))
  
  # Store the result in the list
  result_list[[var_name]] <- long_df
}

temp_long_df <- result_list[["temp"]]
rain_long_df <- result_list[["rain"]]


write.csv(rain_long_df, "rain_long_df.csv")
write.csv(temp_long_df, "temp_long_df.csv")

# Convert numeric hour to proper date-time format ------------------------------

start_date <- as.Date("2021-05-01")
result_list<- list() # re intialise empty list

for (var_name in names){
  # get dataframe
  df<- get(paste0(var_name, "_long_df"))
  
  # get date time as one variable
  df<- df %>%
    mutate(DateTime= start_date + hours(Time))
  
  # store results
  result_list[[var_name]]<- df
}
temp<- result_list[["temp"]]
rain <- result_list[["rain"]]

# Group by day and calculate daily mean of each variable -----------------------

# temp
temp <- temp %>%
  mutate(coordinates = paste(x, y, sep = ","))

daily_temp <- temp %>%
  mutate(Date = as.Date(DateTime)) %>%
  group_by(Date, coordinates, x, y) %>%
  summarise(dailytemp = mean(temp))

# rain
rain <- rain %>%
  mutate(coordinates = paste(x, y, sep = ","))

daily_rain <- rain %>%
  mutate(Date = as.Date(DateTime)) %>%
  group_by(Date, coordinates, x, y) %>%
  summarise(dailyrain = mean(rain))

write.csv(daily_temp, "daily_temp.csv")
write.csv(daily_rain, "daily_rain.csv")

# Interpolating using inverse distance weighting -------------------------------

## IDW used due to the coarse resolution of the grid for the era5 data, leading
## to some STWs and LSOAs having missing values


## Temperature

# create time index called day

daily_temp$Date <- as.Date(daily_temp$date) 
daily_temp$day <- as.numeric(daily_temp$date - min(daily_temp$date)) + 1


# initialise empty lists
IDW <- list()
LIDW.grid <- list()
sitedata.df <- list()

for (i in 1:337){
  cat("Processing day", i, "of", "337", "\n")
  # non missing
  temp.day <- subset(daily_temp, day == i & !is.na(dailytemp))
  temp.day.sf <- st_as_sf(temp.day, coords = c("x","y"), crs = 27700)
  
  # all
  all.temp.day<- subset(daily_temp, day == i)
  all.day.sf <- st_as_sf(all.temp.day, coords = c("x","y"), crs = 27700)
  
  # model on non-missing values
  g <- gstat(formula = dailytemp ~ 1, data = temp.day.sf, nmax = 8,
             set = list(idp = 0))
  
  # predict on whole data (including missing)
  LIDW.grid[[i]] <- cbind(st_drop_geometry(all.day.sf), predict(g, all.day.sf)[,c(1,2)])
  
}
#####run from here

# combine into one data frame
temp.LIDW.df<- do.call(rbind, LIDW.grid)

# select and rename variables as needed
temp.LIDW.df<- temp.LIDW.df %>%
  separate(coordinates, into = c("x", "y"), sep = ",") %>%
  dplyr::select(var1.pred, Date, day, x, y) %>%
  rename(dailytemp=var1.pred)


write.csv(temp.LIDW.df, "temp_LIDW_df.csv")

## Rain
# create time index called day
daily_rain$Date <- as.Date(daily_rain$Date) 
daily_rain$day <- as.numeric(daily_rain$Date - min(daily_rain$Date)) + 1

# initialise empty lists
IDW <- list()
LIDW.grid <- list()
sitedata.df <- list()

for (i in 1:337){
  cat("Processing day", i, "of", "337")
  # non missing
  rain.day <- subset(daily_rain, day == i & !is.na(dailyrain))
  rain.day.sf <- st_as_sf(rain.day, coords = c("x","y"), crs = 27700)
  
  # all
  all.rain.day<- subset(daily_rain, day == i)
  all.day.sf <- st_as_sf(all.rain.day, coords = c("x","y"), crs = 27700)
  
  # model on non-missing values
  g <- gstat(formula = dailyrain ~ 1, data = rain.day.sf, nmax = 8,
             set = list(idp = 0))
  
  # predict on whole data (including missing)
  LIDW.grid[[i]] <- cbind(st_drop_geometry(all.day.sf), predict(g, all.day.sf)[,c(1,2)])
  
}

# combine into one data frame
rain.LIDW.df<- do.call(rbind, LIDW.grid)

# change variable names for calrity and separate coordinates column
rain.LIDW.df<- rain.LIDW.df %>%
  separate(coordinates, into = c("x", "y"), sep = ",") %>%
  dplyr::select(var1.pred, Date, day, x, y) %>%
  rename(dailyrain=var1.pred)


write.csv(rain.LIDW.df, "rain_LIDW_df.csv")

# Extracting from STW catchments -----------------------------------------------

## Temperature
temp.day <- list()
for (i in 1:337) {
  cat("Processing day", i, "of", "337", "\n")
  # Subset the daily_temp dataframe for the current date
  temp<- subset(temp.LIDW.df, day == i)[, c("x", "y", "dailytemp")]
  
  # Create a raster from the subset data
  temp.r <- rast(temp, type = "xyz", crs = crs(england))
  
  # Extract the temperature data for the polygons in stw
  extracted_temp <- terra::extract(temp.r, stw, fun = mean, na.rm = TRUE)
  
  extracted_temp$day <- i
  
  # Combine the extracted data with stw
  temp.day[[i]] <- cbind(stw.df, extracted_temp)
}


# combine results to get one dataframe
temp.day.df <- do.call(rbind, temp.day)

write.csv(temp.day.df, "temp_day_df.csv")

## Rainfall

rain.day <- list()
for (i in 1:337) {
  cat("Processing day", i, "of", "337", "\n")
  # Subset the daily_rain dataframe for the current date
  rain<- subset(rain.LIDW.df, day == i)[, c("x", "y", "dailyrain")]
  
  # Create a raster from the subset data
  rain.r <- rast(rain, type = "xyz", crs = crs(england))
  
  # Extract the rainerature data for the polygons in stw
  extracted_rain <- terra::extract(rain.r, stw, fun = mean, na.rm = TRUE)
  
  extracted_rain$day <- i
  
  # Combine the extracted data with stw
  rain.day[[i]] <- cbind(stw.df, extracted_rain)
}

# combine and save
# combine results to get one dataframe
rain.day.df <- do.call(rbind, rain.day)

write.csv(rain.day.df, "rain_day_df.csv")