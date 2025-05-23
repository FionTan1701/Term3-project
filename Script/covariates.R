library(tidyverse)
library(dplyr)
library(sf)

setwd("/rds/general/user/ft824/home/Term3-project/Data")
setwd("~/Term3-project/Data")

stw_sf<- st_read("STW/stw_catchment_FINAL.shp")
ethnicity <- read.csv("Covariates/lsoa/ethnicity.csv")
lsoa_sf<-st_read("LSOA/LSOA2021_boundaries/LSOA2021_boundaries.shp")
pop_df<- read.csv("Covariates/lsoa/population_2021.csv")

pop_df <- pop_df %>%
  mutate(population = as.numeric(gsub(",", "", population)))


ethnicity_summary <- ethnicity %>%
  mutate(ethnicity_group = if_else(
    grepl("^White:", `Ethnic.group..20.categories.`),
    "White", "Non-White"
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
  ) %>%
  rename(LSOA21CD = `Lower.layer.Super.Output.Areas.Code`)

# ---- STEP 2: Prepare LSOA sf with population ----

lsoa_sf <- lsoa_sf %>%
  left_join(pop_df %>% select(LSOA21CD, population), by = "LSOA21CD")
  

# ---- STEP 3: Intersect LSOAs with STW catchments ----
lsoa_stw_intersection <- st_intersection(
  lsoa_sf %>% select(LSOA21CD, geometry),
  stw_sf %>% select(site_code, geometry)
)

# ---- STEP 4: Compute intersection area ----
lsoa_stw_intersection <- lsoa_stw_intersection %>%
  mutate(intersection_area = as.numeric(st_area(.)))

# ---- STEP 5: Join LSOA area and ethnicity info ----
lsoa_stw <- lsoa_stw_intersection %>%
  left_join(
    lsoa_sf %>% st_drop_geometry() %>% select(LSOA21CD, lsoa_area),
    by = "LSOA21CD"
  ) %>%
  left_join(
    ethnicity_summary,
    by = "LSOA21CD"
  )

# ---- STEP 6: Calculate population fractions ----
lsoa_stw <- lsoa_stw %>%
  mutate(area_prop = as.numeric(intersection_area) / lsoa_area,
         pop_in_stw= population * area_prop,
  )


# ---- STEP 7: Compute weighted proportion of non-white ----
stw_ethnicity <- lsoa_stw %>%
  group_by(site_code) %>%
  summarise(
    weighted_prop_non_white = sum(prop_non_white * pop_in_stw, na.rm = TRUE) /
                              sum(pop_in_stw, na.rm = TRUE),
    .groups = "drop"
  )


write_csv(stw_ethnicity, "/rds/general/user/ft824/home/Term3-project/Data/stw_ethnicity_v2.csv")