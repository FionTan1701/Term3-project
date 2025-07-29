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

setwd("~/Term3-project")
nov_df <- read.csv("Data/processed_final.csv")


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



#prediction grid of England---------------------------------------------------
england <- st_read("Data/shapefiles/england/england_crop.shp")
england <- st_transform(england, crs = 27700)

bb <- st_bbox(england)
x <- seq(bb$xmin - 1, bb$xmax + 1, length.out = 50)
y <- seq(bb$ymin - 1, bb$ymax + 1, length.out = 50)
dp <- as.matrix(expand.grid(x, y))

p <- st_as_sf(data.frame(x = dp[, 1], y = dp[, 2]),coords = c("x", "y"))
st_crs(p) <- st_crs(27700)
ind <- st_intersects(england, p)
dp <- dp[ind[[1]], ]

pdf("prediction_grid.pdf", width = 8, height = 8)
plot(dp, asp = 1)
dev.off()


