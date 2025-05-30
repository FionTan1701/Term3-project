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

write.csv(vars.df,"vars_df.csv")