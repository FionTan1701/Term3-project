library(tidyverse)
library(sf)

setwd("/rds/general/user/ft824/home/Term3-project/Data")


stw_sf <- st_read("STW/stw_catchment_FINAL.shp")
lsoa_sf <- st_read("LSOA2021_boundaries/LSOA2021_boundaries.shp")

#load school location density csv
school_df <- read.csv("Covariates/lsoa/lsoa_school_density.csv")  

# Add area to LSOAs
lsoa_sf <- lsoa_sf %>%
  mutate(lsoa_area = as.numeric(st_area(.)))

# Spatial intersection of LSOAs and STW catchments
lsoa_stw_intersection <- st_intersection(lsoa_sf, stw_sf) %>%
  mutate(intersection_area = as.numeric(st_area(.)))

# Join population and care home data
lsoa_stw <- lsoa_stw_intersection %>%
  left_join(
    lsoa_sf %>%
      st_drop_geometry() %>%
      select(LSOA21CD, lsoa_area),
    by = "LSOA21CD"
  ) %>%
  left_join(
    school_df %>% 
      select(LSOA21CD, school_den, population),
    by = "LSOA21CD"
  )

# Calculate weighted care home density
lsoa_stw <- lsoa_stw %>%
  mutate(
    pop_fraction = intersection_area / lsoa_area.y,
    pop_in_stw = population * pop_fraction
  )

stw_school <- lsoa_stw %>%
  group_by(site_code) %>%
  summarise(
    weighted_school_density = sum(school_den * pop_in_stw, na.rm = TRUE) / 
                                sum(pop_in_stw, na.rm = TRUE),
    .groups = "drop"
  )

# Save result
write_csv(stw_school, "/rds/general/user/ft824/home/Term3-project/Data/stw_school_density.csv")
