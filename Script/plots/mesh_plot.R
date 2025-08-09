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
domain <- inla.nonconvex.hull(coords, convex = 0.2)
domain <-inla.nonconvex.hull(coords)


mesh<- fm_mesh_2d_inla(boundary = domain,
                       loc=coords,
                       max.edge = c(1,2)*max.edge,
                       offset= c(bound.outer, max.edge*2),
                       cutoff= max.edge/5,
                       crs= st_crs(nov))



#Base plot
st_crs(mesh)
st_crs(sites_sf)
st_crs(england)



plot(mesh, col = NA, border = 'lightgrey', main = "Mesh with Sites")

england_km <- st_transform(england, crs = mesh$crs)
rgb_val <- col2rgb("lavenderblush1") / 255  # normalize to 0–1
plot(st_geometry(england_km), add = TRUE, col = rgb(rgb_val[1], rgb_val[2], rgb_val[3], alpha = 0.5),border = "red", lwd = 1)

# Site points as an sf POINT object
sites_sf <- st_as_sf(site_coords, coords = c("x", "y"))

plot(sites_sf, col = "black", pch = 19, add = TRUE, cex = 0.5)



