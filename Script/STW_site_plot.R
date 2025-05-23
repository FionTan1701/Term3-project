library(sf)
library(ggplot2)


setwd("~/Term3-project/Script")

stw_sf <- st_read("../Data/STW/stw_catchment_FINAL.shp")
lsoa_sf <- st_read("../Data/LSOA/LSOA2021_boundaries/LSOA2021_boundaries.shp")

# Plot all STW catchment areas
ggplot(data = stw_sf) +
  geom_sf(fill = "lightblue", color = "darkblue", alpha = 0.6) +
  theme_minimal() +
  labs(title = "STW Catchment Areas with Norovirus Monitoring Sites",
       caption = "Each polygon represents a wastewater catchment area")

#version 2 stw site
ggplot() + 
  geom_sf(data = stw_sf, colour = "blue", fill ="white") +
  coord_sf() +
  theme_bw() +
  theme_minimal() +
  labs(title = "STW Catchment Areas with Norovirus Monitoring Sites")

# site catchment over LSOA
ggplot() +
  geom_sf(data = lsoa_sf , fill = "grey90", colour ="white", size = 0.1) +
  geom_sf(data = stw_sf, fill = "lightblue", colour = "darkblue", alpha = 0.4) +
  theme_minimal() +
  labs(title = " STW catchments over LSOA boundaries",
      caption = " LSOA boundaries with STW catchments overlaid")

data <- read_csv("Data/Norovirus/site_info.csv")
stw_data <- data %>% filter(site_type == "SEWAGE TREATMENT WORKS")

stw_sf <- st_as_sf(
  stw_data,
  coords = c("Easting", "Northing"),
  crs = 27700  # British National Grid
)

stw_wgs84 <- st_transform(stw_sf, crs = 4326)

ggplot() +
  geom_sf(data = stw_wgs84, color = "blue", size = 1) +
  labs(title = "Sewage Treatment Works Locations in England") +
  theme_minimal()


