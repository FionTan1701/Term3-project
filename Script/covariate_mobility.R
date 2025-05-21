library(lubridate)
library(dplyr)

setwd("~/Term3-project")
mobility_df <- read_csv("Data/Covariates/lsoa/lsoa_mob_raw.csv")
stw_sf <- st_read("Data/STW/stw_catchment_FINAL.shp")
lsoa_sf <- st_read("Data/LSOA/LSOA2021_boundaries/LSOA2021_boundaries.shp")

mobility_weekly <- mobility_df %>%
  mutate(week = floor_date(date, "week")) %>%
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

lsoa_sf <- lsoa_sf %>%
  mutate(lsoa_area = as.numeric(st_area(.)))  # this creates 'lsoa_area'

lsoa_stw <- st_intersection(lsoa_sf, stw_sf) %>%
  mutate(intersection_area = as.numeric(st_area(.))) %>%
  left_join(
    lsoa_sf %>%
      st_drop_geometry() %>%
      select(LSOA21CD, lsoa_area),
    by = "LSOA21CD"
  ) %>%
  mutate(area_fraction = intersection_area / lsoa_area.y )

###create a week column in lsoa to join with mobility weekly
weeks <- unique(mobility_weekly$week)


#create all combinations of LSOA-STW + weeks
lsoa_stw_weeks <- expand.grid(
  LSOA21CD = lsoa_stw$LSOA21CD,
  week = weeks,
  stringsAsFactors = FALSE
) %>%
  left_join(lsoa_stw, by = "LSOA21CD")

# Step 3: join with mobility
lsoa_stw_mobility <- lsoa_stw_weeks %>%
  left_join(mobility_weekly, by = c("LSOA21CD", "week")) %>%
  mutate(weighted_mobility = mobility_avg * area_fraction)




# Join weekly mobility
lsoa_stw_mobility <- lsoa_stw %>%
  left_join(mobility_weekly, by = c("LSOA21CD", "week"))%>%
  mutate(weighted_mobility = mobility_avg * area_fraction)

# Aggregate to STW
stw_weekly_mobility <- lsoa_stw_mobility %>%
  group_by(site_code, week) %>%
  summarise(mobility_avg = sum(weighted_mobility, na.rm = TRUE), .groups = "drop")

# How many LSOAs in each?
n_distinct(lsoa_stw$LSOA21CD)
n_distinct(mobility_weekly$LSOA21CD)
length(lsoa_stw$LSOA21CD)
length(unique(mobility_weekly$LSOA21CD))


# Check how many got dropped
lsoa_stw_mobility %>%
  filter(is.na(mobility_avg)) %>%
  count()
