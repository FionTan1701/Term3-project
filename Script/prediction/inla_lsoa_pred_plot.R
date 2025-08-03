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



#i <- seq(1,5)

subset_grid <- lsoa_joined_inla %>%
  filter(date_index == 1)


p <- ggplot(subset_grid, aes(x = Easting, y = Northing, color = pred_mean)) +
    geom_point(size = 0.3, alpha = 0.8) +
  scale_color_gradient(low = "#deebf7", high = "dodgerblue4", limits = c(global_min, global_max)) +
    geom_sf(data = england, fill = NA, color ="grey", inherit.aes = FALSE)+
    coord_sf() +
    theme_bw() +
    #facet_wrap(~ date_label, ncol = 5)+
    labs(title = paste("Spatial Distribution of INLA Predictions by Week"),
         color = "Posterior Mean") +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
#print(p)


p2<- ggplot(subset_grid) +
  geom_sf(aes(fill = pred_mean), color = NA) +
  scale_color_gradient(low = "#deebf7", high = "dodgerblue4", limits = c(global_min, global_max)) +
  theme_bw() +
  facet_wrap(~ date_label, ncol = 3) +
  labs(title = "Weekly Predictions (INLA) by LSOA", fill = "Posterior Mean")+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )
#print(p2)


plot_list <- seq(1,45,9)


for (j in plot_list) {
  subset_week <- seq(j,j+8,1)
  subset_grid <- lsoa_joined_inla %>% filter (date_index %in% subset_week)

  pdf(paste("Figures/prediction_plot/lsoa_boundary_pred/lsoa_boundary_pred_inla",j,"-",j+8,"blue.pdf"),width = 8, height = 10)

  p2<- ggplot(subset_grid) +
  geom_sf(aes(fill = pred_mean), color = NA) +
  scale_color_gradient(low = "#deebf7", high = "dodgerblue4", limits = c(global_min, global_max)) +
  theme_bw() +
  facet_wrap(~ date_label, ncol = 3) +
  labs(title = "Weekly Predictions (INLA) by LSOA", fill = "Posterior Mean")+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )
  print(p2)

  dev.off()
}




pdf("Figures/prediction_plot/lsoa_boundary_pred_RF.pdf", width = 8, height = 10)

for (j in plot_list) {
  subset_week <- seq(j,j+8,1)
  subset_grid <- lsoa_joined_RF %>% filter (date_index %in% subset_week)

  pdf(paste("Figures/prediction_plot/lsoa_boundary_pred/lsoa_boundary_pred_RF",j,"-",j+8,"blue.pdf"),width = 8, height = 10)

  p3<- ggplot(subset_grid) +
  geom_sf(aes(fill = predicted), color = NA) +
  #scale_fill_viridis_c(option = "plasma", na.value = "lightgrey",limits = c(global_min, global_max)) +
  scale_color_gradient(low = "#deebf7", high = "dodgerblue4", limits = c(global_min, global_max)) +
  theme_bw() +
  facet_wrap(~ date_label, ncol = 3) +
  labs(title = "Weekly Predictions (RF) by LSOA", fill = "Predicted Value")+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )
  print(p3)

  dev.off()
}


