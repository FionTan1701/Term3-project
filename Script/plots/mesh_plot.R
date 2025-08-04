library(tidyverse) 
library(lubridate)
library(INLA)
library(inlabru)
library(sp)
library(fmesher)
library(corrr)
library(sf)
library(ggplot2)
library(viridis)
library(ggforce)
library(reshape2)
library(readr)
library(fmesher)

setwd("~/Term3-project")

## read data -------------------------------------------------------------------
nov_df <- read.csv("Data/final_data/processed_final.csv")
pred_grid <- read.csv("Data/prediction_data/lsoa_grid_prediction.csv")

# shapefile
england<- st_read("Data/shapefiles/england/england_crop.shp")
england<- st_transform(england, crs= 27700)


## scale------------------------------------------------------------------------

covariates_to_scale <-  c("school_density", "carehome_density", "imd_score", "BAME", "mobility", "rain_rolling_7day","temp_rolling_7day", "prop_urb")
scale_covariates <- function(df, covariates_to_scale) {
  # Define a custom scaling function
  scale <- function(x) {
    (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
  }
  
  # Apply the custom scaling function across specified covariates
  df <- df %>% mutate(across(all_of(covariates_to_scale), scale, .names = "scale_{.col}"))
  
  return(df)
}

nov_df<- scale_covariates(nov_df, covariates_to_scale)
nov_df<- as.data.frame(nov_df)

# convert df into sf and units m to km
nov <- st_as_sf(nov_df, coords= c("Easting", "Northing"), crs= 27700)
nov <- st_transform(nov,  crs = "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +units=km +no_defs")

### mesh(est) ------------------------------------------------------------------------

max.edge = diff(range(st_coordinates(nov)[,1]))/(3*5)
bound.outer = diff(range(st_coordinates(nov)[,1]))/3
coords<- as.matrix((st_coordinates(nov)))
domain <- inla.nonconvex.hull(coords)

mesh<- fm_mesh_2d_inla(boundary = domain,
                       loc=coords,
                       max.edge = c(1,2)*max.edge,
                       offset= c(max.edge, bound.outer),
                       cutoff= max.edge/5,
                       crs= st_crs(nov))


#prepare dataframe for mesh
mesh_sf <- fm_as_sfc(mesh)

#prepare site coordinates
site_coords <- unique(st_coordinates(nov)) %>%
  as.data.frame() %>%
  rename(x = X, y = Y)

ggplot() +
  # England shapefile
  geom_sf(data = england, fill = NA, color = "black", size = 0.6) +
  coord_sf() 


  # Mesh triangles
  ggplot() +
  geom_polygon(data = mesh_triangles, aes(x = x, y = y, group = triangle_id),
               fill = NA, color = "grey60", alpha = 0.6) +
  # Site points
  geom_point(data = site_coords, aes(x = x, y = y), color = "red", size = 1) +
  theme_minimal() +
  labs(title = "INLA Mesh with Site Locations and England Boundary",
       x = "Easting", y = "Northing")

plot(mesh)

ggplot() +
  geom_sf(data = mesh_sf$triangles, fill = NA, color = "grey50", size = 0.2) +
  geom_sf(data = mesh_sf$boundary, fill = NA, color = "blue", linetype = "solid", size = 0.4) +
  geom_sf(data = england, fill = NA, color = "black") +
  geom_point(data = site_coords, aes(x = x,y=y), color = "red", size =1) +
  coord_sf() +
  theme_minimal()

ggplot() +
  geom_polygon(data = mesh_sf$triangles, fill = NA, color = "grey50") +
  geom_polygon(data = mesh_sf$boundary, fill = NA, color = "blue", linetype = "solid", linewidth = 0.4) +
  coord_sf()



plot(mesh, col = NA, border = 'lightgrey', main = "Mesh with Sites")

# Site points as an sf POINT object

sites_sf <- st_as_sf(site_coords, coords = c("x", "y"))

plot(sites_sf, col = "black", pch = 19, add = TRUE, cex = 0.5)
plot(st_geometry(england), add = TRUE, border = "red", lwd = 2)
