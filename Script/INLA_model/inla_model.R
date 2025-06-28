#Load packages
library(tidyverse)
library(INLA)
library(inlabru)
library(sp)
library(fmesher)
library(sf)
library(ggplot2)

#set directory
setwd("~/Term3-project")

#read data
nov_df <- read.csv("Data/final_df3.csv")

#create s_index
nov_df <- nov_df %>%
  arrange(site_code) %>%
  mutate(site_code= as.factor(site_code)) %>%
  mutate(s_index=as.numeric(site_code)) %>%
  mutate(site_code= as.character(site_code))

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

# england crop
nov_df <- st_as_sf(nov_df, coords= c("Easting", "Northing"), crs= 27700)

england<- st_read("Data/shapefiles/england/england_crop.shp")
england<- st_transform(england, crs= st_crs(nov_df))

#mesh construction

nov_df <- st_as_sf(nov_df, coords= c("Easting", "Northing"), crs= 27700)
nov_df <- st_transform(nov_df,  crs = "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +units=km +no_defs")

coords<- unique(st_coordinates(nov_df))
colnames(coords) <- c("Easting", "Northing")  
coords_df <- as.data.frame(coords)            

#domain <- inla.nonconvex.hull(coords, concave = -0.05, convex = -0.02, resolution=c(200,200))
domain <- inla.nonconvex.hull(coords)

#max.edge = 15
max.edge = diff(range(st_coordinates(nov_df)[,1]))/(3*5)
bound.outer = diff(range(st_coordinates(nov_df)[,1]))/3

mesh<- fm_mesh_2d_inla(boundary = domain,
                       loc=coords,
                       max.edge = c(1,2)*max.edge,
                       offset= c( max.edge, bound.outer),
                       cutoff= max.edge/5,
                       crs= st_crs(nov_df))

pdf("model6_mesh.pdf", width = 14, height = 10)
                       
plot(mesh)
print(max.edge)
print(bound.outer)


ggplot() +
  gg(mesh) +
  geom_point(data = coords_df , aes(Easting, Northing)) +
  theme_void()


dev.off()



