library(lubridate)
library(dplyr)
library(tidyr)
library(sf)

setwd("~/Term3-project")
mobility_df <- read.csv("Data/Covariates/lsoa/lsoa_mob_raw.csv")
stw_sf <- st_read("Data/STW/stw_catchment_FINAL.shp")
lsoa_sf <- st_read("Data/LSOA/LSOA2021_boundaries/LSOA2021_boundaries.shp")
pop_df<- read.csv("Data/Covariates/lsoa/population_2021.csv")

mobility_weekly <- mobility_df %>%
  mutate(week = floor_date(as.Date(date), "week")) %>%
  group_by(LSOA21CD, week) %>%
  summarise(mobility_avg = mean(mobility, na.rm = TRUE), .groups = "drop")

summary(mobility_weekly)
###mobility df includes lsoa of wales

######## Count number of unique weeks per LSOA21CD
lsoa_week_counts <- mobility_weekly %>%
  group_by(LSOA21CD) %>%
  summarise(n_weeks = n_distinct(week)) %>%
  ungroup()

# View result
print(lsoa_week_counts)

#########

#########################################################################
lsoa_sf <- lsoa_sf %>%
  mutate(lsoa_area = as.numeric(st_area(.))) 

#join population_df to each lsoa
lsoa_sf <- lsoa_sf %>%
  left_join(pop_df %>% select(LSOA21CD, population), by = "LSOA21CD") 

###create a week column in lsoa to join with mobility weekly
weeks <- unique(mobility_weekly$week)

###################################################################

#### intersect lsoa_sf and stw_Sf

#Intersect STW catchments with LSOAs
intersections <- st_intersection(stw_sf, lsoa_sf)

intersections <- intersections %>%
  mutate(intersect_area = st_area(.))

#create combinations of lsoa and week
lsoa_stw_weeks_2 <- intersections %>%
  distinct(LSOA21CD, .keep_all = TRUE) %>%
  select(LSOA21CD,geometry) %>%
  crossing(week = weeks) %>%
  left_join(intersections, by = "LSOA21CD")

lsoa_stw_mobility <- lsoa_stw_weeks_2 %>%
  left_join(mobility_weekly, by = c("LSOA21CD", "week"))


lsoa_stw_mobility <- lsoa_stw_mobility %>%
  mutate(population = as.numeric(gsub(",", "", population)))

#Calculate weighted values
lsoa_stw_mobility <-  lsoa_stw_mobility %>%
  mutate(area_prop = as.numeric(intersect_area) / Shape__Are,
         pop_in_catchment = population * area_prop,
         weighted_mobility = mobility_avg * pop_in_catchment)

stw_mobility_weekly <- lsoa_stw_mobility %>%
  group_by(site_code, week) %>%
  summarise(
    pop_weighted_mobility = sum(weighted_mobility, na.rm = TRUE) / sum(pop_in_catchment, na.rm = TRUE),
    .groups = "drop"
  )

write.csv(stw_mobility_weekly,"~/Term3-project/Data/cleaned_covariates/stw_mobility_weekly.csv")
