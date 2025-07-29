################################################################
## LSOA Predictions
################################################################

library(fmesher)
library(INLA)
library(Matrix)
library(Metrics)
library(sf)
library(scoringRules)
library(tibble)
library(dplyr)
library(sf)
library(sp)
library(fmesher)
library(lubridate)
library(terra)
library(readxl)
library(tidyr)
library(raster)
library(stringr)
library(inlabru)
library(zoo)
library(data.table)
options(saveWorkspace = FALSE)
#=================================================== 
###  Data
#===================================================

## LSOA 

# shapefile
lsoa<- st_read("data/lsoa_crop.shp") # lsoa shapefile
lsoa<- lsoa[grepl("^E0", lsoa$LSOA21CD), ] # 318

# school, carehome, imd, bame
cov<- fread("data/lsoa.cov.csv") # some covariates at the centroid grid
cov<- cov[grepl("^E0", cov$LSOA21CD), ]


cov <- cov %>%
  dplyr::select(LSOA21CD, school_den, carehom_den, scale_imd, scale_bame, Easting, Northing) %>%
  unique()


# land cover
land_cover<- read.csv("data/covariates/lsoa/land_cover_cat_lsoa.csv")

# mobility
mob<- fread("data/covariates/lsoa/lsoa_mob_raw.csv")

mob <- mob %>%
  group_by(LSOA21CD) %>%
  arrange(date, .by_group = TRUE) %>%
  mutate(mob_7day = rollmean(mobility, k = 7, fill = NA, align = "right"))

# temperature
temp<- fread("data/covariates/lsoa/daily_temp_lsoa_2.csv")


# rainfall
rain<- fread("data/covariates/lsoa/daily_rain_lsoa_2.csv")

## Add covariates to data frame

# create, week beginning
start<- as.Date("2021-05-24")
end<- as.Date("2022-03-28")

weeks <- seq(from = start, to = end, by = "week")

# Create a data.table with weeks
weeks_df <- data.table(week = weeks)

# Add a numerical week variable
weeks_df[, week_num := 1:.N]

weeks_df$date<- weeks_df$week
weeks_df$week= as.numeric(weeks_df$week_num)
# combinations of weeks and lsoa
cov_df <- as.data.frame(cov)


# Define weeks as a tibble
weeks_tbl <- tibble(week = 1:45)

# Convert cov_df to tibble
cov_tbl <- as_tibble(cov_df)  # Remove week column if needed

# Expand all combinations
grid <- crossing(weeks_tbl, cov_tbl)

# add dates
grid<- grid %>%
left_join(weeks_df, by= "week")

# add temporal covariates
grid<- grid %>%
  left_join(temp %>%
              mutate(date= as.Date(temp$date)) %>%
              dplyr::select(LSOA21CD, date, temp_7day_avg, temp_cat),
            by= c("LSOA21CD", "date"))

grid<- grid %>%
  left_join(rain %>%
              mutate(date= as.Date(rain$date)) %>%
              dplyr::select(LSOA21CD, date, rain_7day_avg, rain_cat),
            by= c("LSOA21CD", "date"))

grid<- grid %>%
  left_join(mob %>%
              mutate(date= as.Date(date)) %>%
              dplyr::select(LSOA21CD, date, mob_7day),
            by= c("LSOA21CD", "date"))

# convert rain and temperature into dummy variables
# recode temperature and rain as dummy variables
grid <- grid %>%
  mutate(temp_cat = as.character(temp_cat)) %>%
  pivot_wider(
    names_from = temp_cat,  # Create dummy variables from temp_cat
    values_from = temp_cat, 
    values_fn = length,  # Count occurrences
    values_fill = list(temp_cat = 0),  # Fill with 0s
    names_prefix = "temp_"  # Prefix for new dummy variable names
  ) %>%
  mutate(across(starts_with("temp_"), ~ factor(.x, levels = c(0, 1))))

grid <- grid %>%
  mutate(rain_cat = as.character(rain_cat)) %>%
  pivot_wider(
    names_from = rain_cat,  # Create dummy variables from rain_cat
    values_from = rain_cat, 
    values_fn = length,  # Count occurrences
    values_fill = list(rain_cat = 0),  # Fill with 0s
    names_prefix = "rain_"  # Prefix for new dummy variable names
  ) %>%
  mutate(across(starts_with("rain_"), ~ factor(.x, levels = c(0, 1))))  # Convert to factor


# land cover 
grid<- grid %>%
  left_join(land_cover %>%
              dplyr::select(LSOA21CD, prop_urb, prop_agri),
            by= "LSOA21CD")

# lockdown

grid <- grid %>%
  mutate(lockdown_phase = case_when(
    date >= as.Date("2021-05-24") & date <= as.Date("2021-07-18") ~ "step3",
    date >= as.Date("2021-07-19") & date <= as.Date("2021-12-07") ~ "step4",
    date >= as.Date("2021-12-08") & date <= as.Date("2022-01-26") ~ "planB",
    date >= as.Date("2022-01-27") & date <= as.Date("2022-03-28") ~ "lifting"))

# recode temperature and rain as dummy variables
grid<- grid %>%
  mutate(lockdown_phase = as.character(lockdown_phase)) %>%
  pivot_wider(names_from   = lockdown_phase, 
              values_from  = lockdown_phase, 
              values_fn    = length, 
              values_fill  = 0, 
              names_prefix = "lockdown_") %>%
  mutate(across(starts_with("lockdown_"), ~factor(.x, levels=c(0,1))))

grid$carehome_den<- grid$carehom_den
# scale covariates
covariates_to_scale <-  c("school_den", "carehome_den", "mob_7day", "prop_agri", "prop_urb")
scale_covariates <- function(df, covariates_to_scale) {
  # Define a custom scaling function
  scale <- function(x) {
    (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
  }
  
  # Apply the custom scaling function across specified covariates
  df <- df %>% mutate(across(all_of(covariates_to_scale), scale, .names = "scale_{.col}"))
  
  return(df)
}

grid<- scale_covariates(grid, covariates_to_scale)

# if variable is _cat pass through as.numeric(as.character())

grid <- grid %>%
  mutate_at(vars(contains("temp")), ~ as.numeric(as.character(.))) %>%
  mutate_at(vars(contains("rain")), ~ as.numeric(as.character(.))) %>%
  mutate_at(vars(contains("lockdown")), ~ as.numeric(as.character(.)))

# convert to sf object
grid_sf<-st_as_sf(grid, coords= c("Easting", "Northing"), crs= 27700)
grid_sf<- st_transform(grid_sf,  crs = "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +units=km +no_defs")


## Spatial mean
space_agg <- samples_df %>%
  group_by(LSOA21CD) %>%
  summarise(across(as.numeric(starts_with("sample_")), \(x) mean(x, na.rm = TRUE)))



# Gather the data into long format, perform calculations, and spread it back
space_long <- space_agg %>%
  pivot_longer(cols = starts_with("sample_"), names_to = "sample", values_to = "value") %>%
  group_by(LSOA21CD) %>%
  summarise(
    calculate_stats(value),
    .groups = 'drop'
  )

head(space_long)

# save
write.csv(space_long,  "outputs/predictions/M224_lsoa_pred.csv") # aggregated predictions)





  
  