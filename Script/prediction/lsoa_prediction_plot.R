library(ggplot2)
library(dplyr)
library(tidyr)
library(sf)
library(viridis)
library(readr)
library(RColorBrewer)

setwd("~/Term3-project")

# read data
grid <- read_csv("outputs/prediction/lsoa_prediction.csv")

# shapefile
england<- st_read("Data/shapefiles/england/england_crop.shp")
england<- st_transform(england, crs= 27700)

# LSOA boundaries
lsoa_boundaries <- st_read("Data/LSOA/LSOA2021_boundaries/LSOA2021_boundaries.shp") %>%
  select(LSOA21CD, geometry)

lsoa_boundaries<- lsoa_boundaries[grepl("^E0", lsoa_boundaries$LSOA21CD), ]

unique_weeks <- sort(unique(grid$date_index))

lsoa_expanded <- expand_grid(LSOA21CD = lsoa_boundaries$LSOA21CD,
                             date_index = unique_weeks) %>%
  left_join(lsoa_boundaries, by = "LSOA21CD") %>%
  st_as_sf()

## inla prediction


grid$date <-  as.Date("2021-05-24") + 7*(grid$date_index - 1)
grid$date <- format(grid$date, "%d/%m/%Y")
grid$date_label <- factor(grid$date, levels = unique(grid$date))

grid_sf <-st_as_sf(grid, coords= c("Easting", "Northing"), crs= 27700)
grid_sf <- st_transform(grid_sf,  crs = "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +units=km +no_defs")

lsoa_joined_inla <- lsoa_expanded %>%
  left_join(grid, by = c("LSOA21CD" = "LSOA21CD", "date_index"))

## RF prediction
# RF grid prediction plot
grid_RF <- read_csv("outputs/prediction/rf_lsoa_prediction.csv")

#create date and data_label for plotting
grid_RF$date <-  as.Date("2021-05-24") + 7*(grid_RF$date_index - 1)
grid_RF$date <- format(grid_RF$date, "%d/%m/%Y")
grid_RF$date_label <- factor(grid_RF$date, levels = unique(grid_RF$date))

lsoa_joined_RF <- lsoa_expanded %>%
  left_join(grid_RF, by = c("LSOA21CD" = "LSOA21CD", "date_index"))

# create limits for the outcome so the plots are in the same scale
global_min <- min(grid$pred_mean, grid_RF$predicted)
global_max <- max(grid$pred_mean, grid_RF$predicted)



i <- seq(1,45,5)

subset_grid <- lsoa_joined_inla %>%
  filter(date_index == i)


pdf("Figures/prediction_plot/lsoa_boundary_pred/lsoa_boundary_pred_inla_every5week_blue.pdf",width = 8 , height = 10)
p<- ggplot(subset_grid) +
  geom_sf(aes(fill = pred_mean), color = NA) +
  scale_fill_gradient(low = "#deebf7", high = "#3182bd", limits = c(global_min, global_max)) +
  theme_bw() +
  facet_wrap(~ date_label, ncol = 3) +
  labs(title = "Predictions (INLA) by LSOA every 5 weeks", fill = "Posterior Mean")+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )
print(p)

dev.off()

subset_grid <- lsoa_joined_RF %>%
    filter(date_index == i)

pdf("Figures/prediction_plot/lsoa_boundary_pred/lsoa_boundary_pred_RF_every5week_blue.pdf",width = 8 , height = 10)
p<- ggplot(subset_grid) +
  geom_sf(aes(fill = predicted), color = NA) +
  scale_fill_gradient(low = "#deebf7", high = "#3182bd", limits = c(global_min, global_max)) +
  theme_bw() +
  facet_wrap(~ date_label, ncol = 3) +
  labs(title = "Predictions (RF) by LSOA every 5 weeks", fill = "Predicted Value")+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )
print(p)

dev.off()