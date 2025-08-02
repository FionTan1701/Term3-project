library(ggplot2)
library(dplyr)
library(sf)
library(viridis)

setwd("~/Term3-project")

grid <- read.csv("outputs/prediction/lsoa_prediction.csv")

grid$date <-  as.Date("2021-05-24") + 7*(grid$date_index - 1)
grid$date <- format(grid$date, "%d/%m/%Y")
grid$date_label <- factor(grid$date, levels = unique(grid$date))

grid_sf <-st_as_sf(grid, coords= c("Easting", "Northing"), crs= 27700)
grid_sf <- st_transform(grid_sf,  crs = "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +units=km +no_defs")

pdf("outputs/prediction/lsoa_prediction_plot.pdf",width = 10, height =8)
ggplot(grid_sf) +
  geom_sf(aes(color = pred_mean), size = 0.3) +
  geom_df(data = england, fill =NA , color = "grey", inherit.aes = FALSE) +
  scale_color_viridis(option = "C") +
  facet_wrap(~ date_label) +
  labs(title = "Posterior Mean by Week",
       color = "Posterior Mean") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
  panel.grid.minor = element_blank())

  dev.off()