library(tidyverse)
library(dplyr)
library(sf)

setwd("/rds/general/user/ft824/home/Term3-project/Data")

stw_sf<- st_read("STW/stw_catchment_FINAL.shp")
ethnicity <- read.csv("Covariates/lsoa/ethnicity.csv")
lsoa_sf<-st_read("LSOA2021_boundaries/LSOA2021_boundaries.shp")

ethnicity_summary <- ethnicity %>%
  mutate(ethnicity_group = if_else(
    grepl("^White:", `Ethnic.group..20.categories.`), 
    "White", 
    "Non-White"
  )) %>%
  group_by(`Lower.layer.Super.Output.Areas.Code`, ethnicity_group) %>%
  summarise(total = sum(Observation, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from = ethnicity_group, 
    values_from = total,
    values_fill = 0
  ) %>%
  mutate(
    total_population = White + `Non-White`,
    prop_non_white = `Non-White` / total_population
  )


lsoa_sf <- lsoa_sf %>%
  mutate(lsoa_area = as.numeric(st_area(.)))

lsoa_stw_intersection <- st_intersection(lsoa_sf, stw_sf)

# Add overlap area
lsoa_stw_intersection <- lsoa_stw_intersection %>%
  mutate(intersection_area = as.numeric(st_area(.)))

lsoa_stw <- lsoa_stw_intersection %>%
  left_join(select(lsoa_sf, LSOA21CD, lsoa_area), by = "LSOA21CD") %>%
  left_join(
    ethnicity_summary %>%
      rename(LSOA21CD = `Lower.layer.Super.Output.Areas.Code`),
    by = "LSOA21CD"
  )

lsoa_stw <- lsoa_stw %>%
  mutate(
    pop_fraction = intersection_area / lsoa_area,
    pop_in_stw = total_population * pop_fraction
  )

stw_ethnicity <- lsoa_stw %>%
  group_by(site_code) %>%
  summarise(
    weighted_prop_non_white = sum(prop_non_white * pop_in_stw, na.rm = TRUE) /
      sum(pop_in_stw, na.rm = TRUE),
    .groups = "drop"
  )


saveRDS(stw_ethnicity,"stw_ethnic_summary.rds")

