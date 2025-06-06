library(lubridate)
library(dplyr)
library(tidyr)
library(sf)

setwd("~/Term3-project")
mobility_df <- read.csv("Data/Covariates/lsoa/lsoa_mob_raw.csv")
stw_sf <- st_read("Data/STW/stw_catchment_FINAL.shp")
lsoa_sf <- st_read("Data/LSOA/LSOA2021_boundaries/LSOA2021_boundaries.shp")
pop_df<- read.csv("Data/Covariates/lsoa/population_2021.csv")

#weekly average
mobility_weekly <- mobility_df %>%
  mutate(week = floor_date(as.Date(date), unit = "week", week_start = 1)) %>%
  group_by(LSOA21CD, week) %>%
  summarise(mobility_avg = mean(mobility, na.rm = TRUE), .groups = "drop")

#7-days rolling average
mobility_rolling_df <- mobility_df %>%
  arrange(LSOA21CD, date) %>%
  group_by(LSOA21CD) %>%
  mutate(
    mobility_rolling_7day = rollmean(mobility, 7, fill = NA, align = "center"),
    weekday = wday(date, label = TRUE) 
  ) %>%
  ungroup() %>%
  filter(weekday == "Mon")


summary(mobility_weekly)
summary(mobility_rolling_df)
###mobility df includes lsoa of wales

######## Count number of unique weeks per LSOA21CD
lsoa_week_counts <- mobility_weekly %>%
  group_by(LSOA21CD) %>%
  summarise(n_weeks = n_distinct(week)) %>%
  ungroup()

print(lsoa_week_counts)

#########

#########################################################################
lsoa_sf <- lsoa_sf %>%
  mutate(lsoa_area = as.numeric(st_area(.))) 

#join population_df to each lsoa
lsoa_sf <- lsoa_sf %>%
  left_join(pop_df %>% select(LSOA21CD, population), by = "LSOA21CD") 

###create a week column in lsoa to join with mobility weekly
weeks <- unique(mobility_rolling_df$date)

###################################################################

#### intersect lsoa_sf and stw_Sf

#Intersect STW catchments with LSOAs
intersections <- st_intersection(stw_sf, lsoa_sf)

intersections <- intersections %>%
  mutate(intersect_area = st_area(.))

#create combinations of lsoa and week
lsoa_stw_weeks<- intersections %>%
  distinct(LSOA21CD, .keep_all = TRUE) %>%
  select(LSOA21CD,geometry) %>%
  crossing(week = weeks) %>%
  left_join(intersections, by = "LSOA21CD")

lsoa_stw_weeks <- lsoa_stw_weeks%>%
  rename(date = "week")

lsoa_stw_mobility <- lsoa_stw_weeks %>%
  left_join(mobility_rolling_df, by = c("LSOA21CD", "date"))


lsoa_stw_mobility <- lsoa_stw_mobility %>%
  mutate(population = as.numeric(gsub(",", "", population)))

#Calculate weighted values
lsoa_stw_mobility <-  lsoa_stw_mobility %>%
  mutate(area_prop = as.numeric(intersect_area) / Shape__Are,
         pop_in_catchment = population * area_prop)

stw_mobility_weekly <- lsoa_stw_mobility %>%
  group_by(site_code, date) %>%
  summarise(
    weighted_mobility = sum(mobility_rolling_7day * pop_in_catchment, na.rm = TRUE) /
      sum(pop_in_catchment, na.rm = TRUE),
    .groups = "drop"
  )



write.csv(stw_mobility_weekly,"~/Term3-project/Data/cleaned_covariates/stw_mobility_weekly.csv", row.names =  FALSE)
test <- read.csv("~/Term3-project/Data/cleaned_covariates/stw_mobility_weekly.csv")
