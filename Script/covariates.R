library(tidyverse)
library(dplyr)
library(sf)

setwd("/rds/general/user/ft824/home/Term3-project/Data")

stw_sf<- st_read("STW/stw_catchment_FINAL.shp")
ethnicity <- read.csv("Covariates/lsoa/ethnicity.csv")

lsoa_sf<-st_read("LSOA2021_boundaries/LSOA2021_boundaries.shp")


# Step 1: Make sure shapefiles have matching CRS
stw_sf <- st_transform(stw_sf, crs = st_crs(lsoa_sf))

# Step 2: Merge ethnicity CSV with LSOA shapefile
# Rename LSOA code column if needed to match
lsoa_sf <- lsoa_sf %>%
  left_join(ethnic_data, by = c("LSOA_code_column" = "Lower.layer.Super.Output.Areas.Code"))

# Step 3: Intersect LSOAs with STW catchments
lsoa_stw_intersection <- st_intersection(lsoa_sf, stw_sf)

# Step 4: Add intersection area (used as a proxy weight)
lsoa_stw_intersection$area <- st_area(lsoa_stw_intersection)

# Step 5: For each STW catchment, compute area-weighted ethnic group counts

# Let's assume you have one column called `Observation` and one called `Ethnic.group`
# Pivot to wide first if you have multiple rows per LSOA per ethnic group
ethnic_wide <- ethnic_data %>%
  pivot_wider(names_from = `Ethnic.group..20.categories.`, values_from = Observation)  # adjust names if needed

# Join the wide ethnic data to LSOA geometries
lsoa_sf <- lsoa_sf %>%
  left_join(ethnic_wide, by = c("LSOA_code_column" = "Lower.layer.Super.Output.Areas.Code"))

# Re-intersect with STW after joining wide ethnicity data
lsoa_stw_intersection <- st_intersection(lsoa_sf, stw_sf)

# Calculate area per polygon
lsoa_stw_intersection$area <- as.numeric(st_area(lsoa_stw_intersection))

# Now group by STW and compute area-weighted totals for each ethnic group
ethnic_vars <- names(ethnic_wide)[-1]  # drop LSOA code

stw_ethnic_summary <- lsoa_stw_intersection %>%
  group_by(site_code) %>%
  summarise(across(all_of(ethnic_vars), ~weighted.mean(., w = area, na.rm = TRUE), .names = "wmean_{.col}"),
            .groups = "drop")

saveRDS(stw_ethnic_summary,"stw_ethnic_summary.rds")
