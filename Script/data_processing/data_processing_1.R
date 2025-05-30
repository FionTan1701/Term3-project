#=================================================== 
###  Packages
#===================================================

library(tidyverse)
library(dplyr)
library(terra)
library(sf)
library(gstat)    
library(lubridate)
library(data.table) # usefule for reading in large csv files

#set directory
setwd("~/Term3-project/")

#=================================================== 
###  Land cover
#===================================================

# land cover raster
#land_cover<- raster("data/raw/Covariates/Land cover/land_cover_100m_2018.tif")

land_cover_v<- st_read("Data/Covariates/raw/land cover/U2018_CLC2018_V2020_20u1_gdb.gdb")

land_cover_v<- st_transform(land_cover_v, crs= 27700)

# crop to study area

# england shapefile
england<- st_read("Data/shapefiles/england/england_crop.shp")

england<- st_transform(england, crs= 27700)

land_cvr_crop<- st_crop(land_cover_v, england)

names(land_cvr_crop)


# Group by urban, vegetation, industrial and other

# 111, 112 = Urban
#urbanicity_codes <- c(111,112)

# 311, 312, 313, 321, 322, 323,324, 331, 333, 334, 335 =Vegetation

# 121, 122, 123 = Industrial

# Other

#agriculture_codes <- c(211, 212, 213, 221, 222, 223, 231, 241, 242, 243, 244) 


land_cvr_crop$land_cat <- ifelse(land_cvr_crop$Code_18 %in% c(111, 112), "urbanicity",
                                 ifelse(land_cvr_crop$Code_18 %in% c(211:213, 221:223, 231, 241:244), "agriculture", "other"))
            

# stw catchmnet shp
stw<- st_read("Data/STW/stw_catchment_FINAL.shp", crs= 27700)

stw<- st_transform(stw, crs= 27700)

# area of stw catchment
stw$stw_area<- st_area(stw)


# get area of intersection
intersection <- st_intersection(land_cvr_crop, stw)

intersection$area_land<- st_area(intersection)

# e.g. proportion urban
prop_urb <- intersection %>%
  filter(land_cat == "urbanicity") %>%
  group_by(site_code) %>%
  summarise(prop_urb = sum(area_land) / first(stw_area)) %>%
  ungroup()

prop_agri <- intersection %>%
  filter(land_cat == "agriculture") %>%
  group_by(site_code) %>%
  summarise(prop_agri = sum(area_land) / first(stw_area)) %>%
  ungroup()

prop_urb<- st_drop_geometry(prop_urb)
prop_agri<- st_drop_geometry(prob_agri)

write.csv(prop_urb, "~/Term3-project/Data/cleaned_covariates/stw_prop_urb.csv", row.names =  FALSE )



#=================================================== 
###  Lockdown stage
#===================================================
nov_df <- read.csv("Data/Norovirus/nov_wide_week.csv")


nov_df <- rownames_to_column(nov_wide_week, var = "week_date")
nov_df <- as.Date(nov_df$week_date)
nov_df <- as.data.frame(nov_df) %>% 
  rename(week_date = nov_df)

                                     
# lockdown
nov_df <- nov_df %>%
  mutate(lockdown_phase = case_when(
    week_date >= as.Date("2021-05-27") & week_date <= as.Date("2021-07-18") ~ "step3",
    week_date >= as.Date("2021-07-19") & week_date <= as.Date("2021-12-07") ~ "step4",
    week_date >= as.Date("2021-12-08") & week_date <= as.Date("2022-01-26") ~ "planB",
    week_date >= as.Date("2022-01-27") & week_date <= as.Date("2022-03-17") ~ "lifting"))
#write.csv(nov_df,"~/Term3-project/Data/Norovirus/nov_df.csv")

# recode as dummy variable(one-hot encode)
nov_df<- nov_df %>%
  mutate(lockdown_phase = as.character(lockdown_phase)) %>%
  pivot_wider(names_from   = lockdown_phase, 
              values_from  = lockdown_phase, 
              values_fn    = length, 
              values_fill  = 0, 
              names_prefix = "lockdown_") %>%
  mutate(across(starts_with("lockdown_"), ~factor(.x, levels=c(0,1))))

# make sure to convert to numeric
nov_df$lockdown_step3 <- as.numeric(as.character(nov_df$lockdown_step3))
nov_df$lockdown_step4 <- as.numeric(as.character(nov_df$lockdown_step4))
nov_df$lockdown_planB <- as.numeric(as.character(nov_df$lockdown_planB))
nov_df$lockdown_lifting <- as.numeric(as.character(nov_df$lockdown_lifting))
nov_df$lockdown_NA <- as.numeric(as.character(nov_df$lockdown_NA))

write.csv(nov_df,"~/Term3-project/Data/cleaned_covariates/lockdown_stage.csv", row.names = FALSE)

#=================================================== 
###  index of multiple deprivation processing 
#===================================================

imd_df <- read.csv("Data/Covariates/lsoa/lsoa_imd.csv")

imd_df <- imd_df %>% select(LSOA21CD, IMD_score,IMD_rank)

stw_sf <- st_read("Data/STW/stw_catchment_FINAL.shp")
lsoa_sf <- st_read("Data/LSOA/LSOA2021_boundaries/LSOA2021_boundaries.shp")
pop_df<- read.csv("Data/Covariates/lsoa/population_2021.csv")

pop_df <- pop_df %>%
  mutate(population = as.numeric(gsub(",", "", population)))

lsoa_sf <- lsoa_sf %>%
  left_join(pop_df %>% select(LSOA21CD, population), by = "LSOA21CD")

lsoa_stw_intersection <- st_intersection(lsoa_sf, stw_sf )

lsoa_stw_intersection <- lsoa_stw_intersection %>%
  mutate(intersection_area = as.numeric(st_area(.)))

imd_lsoa_stw <- lsoa_stw_intersection %>%
  left_join(
    lsoa_sf %>% st_drop_geometry() %>% select(LSOA21CD, Shape__Are),
    by = "LSOA21CD"
  ) %>%
  left_join(
    imd_df,
    by = "LSOA21CD"
  )

imd_lsoa_stw <- imd_lsoa_stw %>%
  mutate(area_prop = as.numeric(intersection_area) / Shape__Are.x,
         pop_in_stw= population * area_prop,
  )

stw_imd <- imd_lsoa_stw %>%
  group_by(site_code) %>%
  summarise(
    weighted_imd_score = sum(IMD_score * pop_in_stw, na.rm = TRUE) /
      sum(pop_in_stw, na.rm = TRUE),
    weighted_imd_rank = sum(IMD_rank * pop_in_stw, na.rm = TRUE) /
      sum(pop_in_stw, na.rm = TRUE),
    .groups = "drop"
  )

stw_imd <- stw_imd %>%
  st_drop_geometry()

write.csv(stw_imd, "~/Term3-project/Data/cleaned_covariates/stw_imd.csv", row.names = FALSE)


#=================================================== 
###  ERA5 (rain and temperature) processing 
#===================================================


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
april <- project(april, crs(england))

# crop raster to england
cropped_raster <- crop(rast, england)


vars.df <- as.data.frame(cropped_raster, xy = TRUE)


# Split into variables  --------------------------------------------------------

# temperature
temp.df <- cbind(vars.df[,c(1,2)], vars.df[,grepl("temp", names(vars.df))])

# rain
rain.df<- cbind(vars.df[,c(1,2)], vars.df[,grepl("precipitation", names(vars.df))])

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

# Interpolating using inverse distance weighting -------------------------------

## IDW used due to the coarse resolution of the grid for the era5 data, leading
## to some STWs and LSOAs having missing values


## Temperature

# create time index called day

#daily_temp<-daily_temp_lsoa

daily_temp <- daily_temp %>%
  left_join(
    lsoa_sf %>% 
      st_drop_geometry() %>%                    # Drop geometry to avoid spatial column conflicts
      select(LSOA21CD, BNG_E, BNG_N),           # Keep only needed columns
    by = "LSOA21CD"
  )


daily_temp <- daily_temp %>%
  rename(
    x = BNG_E,
    y = BNG_N
  )

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

