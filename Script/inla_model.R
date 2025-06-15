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


#mesh construction

nov_df <- st_as_sf(nov_df, coords= c("Easting", "Northing"), crs= 27700)
nov_df <- st_transform(nov_df,  crs = "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +units=km +no_defs")

coords<- unique(st_coordinates(nov_df))
colnames(coords) <- c("Easting", "Northing")  
coords_df <- as.data.frame(coords)            

#domain <- inla.nonconvex.hull(coords, concave = -0.05, convex = -0.02, resolution=c(200,200))
domain <- inla.nonconvex.hull(coords)

max.edge = 15
bound.outer = diff(range(st_coordinates(nov_df)[,1]))/3

mesh<- fm_mesh_2d_inla(boundary = domain,
                       loc=coords,
                       max.edge = c(1,2)*max.edge,
                       offset= c(max.edge, bound.outer),
                       cutoff= max.edge/5,
                       crs= st_crs(nov_df))

pdf("mesh2.pdf", width = 14, height = 10)
                       
plot(mesh)

ggplot() +
  gg(mesh) +
  geom_point(data = coords_df , aes(Easting, Northing)) +
  theme_void()

dev.off()

#SPDE model
spde.cv = inla.spde2.pcmatern(mesh = mesh.cv, alpha = 2, 
                              prior.range = c(1, 0.01),   # p(range < 1) = 0.01
                              prior.sigma = c(2, 0.01)) # p(sigma > 2) = 0.01

spde.cv$n.spde #n. of mesh vertices


spde <- inla.spde2.pcmatern(mesh = mesh, alpha = 2,
                            prior.range = c(5, 0.05),  # p(range < 5) = 0.05
                            prior.sigma = c(5, 0.05))  # p(sigma > 2) = 0.05
print(paste("no. of mesh vertices",spde$n.spde)) #n. of mesh vertices

##Create index set
n_week <- length(unique(nov_df$date_index))
s.index<- inla.spde.make.index(name="spatial.field",
                               n.spde=spde$n.spde,
                               n.group= n_week) 
length(s.index$spatial.field)

## A matrices
#train
coordinates.alldays.cv = nov_df[c("Easting", "Northing")] %>% as.matrix()

Ae = inla.spde.make.A(mesh = mesh,
                      loc = coordinates.alldays.cv,
                      group = nov_df$date_index,
                      n.group = n_week)
coords.train<- as.matrix(st_coordinates(train))

print(dim(Ae))

print(nrow(nov_df) )
print(length(s_index$spatial.field))

#validation
coords.val<- as.matrix(st_coordinates(val))

n_week2<- length(unique(val$date_index))

A.val <- inla.spde.make.A(mesh=mesh,
                          loc=coords.val,
                          group= val$date_index,
                          n.group= n_week2)

print(dim(A.val))

#train stack

stack.train <- inla.stack(
  data = list(Log10_NoV_norm = nov_df$Log10_NoV_norm), 
  A = list(A.train, 1), 
  effects = list(
    c(s.index, list(Intercept = 1)),
    list(
      week = nov_df$date_index,
      site_code = nov_df$site_code,
      lockdown_step3 = nov_df$lockdown_step3,
      lockdown_step4 = nov_df$lockdown_step4,
      lockdown_lifting = nov_df$lockdown_lifting,
      lockdown_planB = nov_df$lockdown_planB,
      scale_school_density = nov_df$scale_school_density,
      scale_carehome_density = nov_df$scale_carehome_density,
      scale_mobility = nov_df$scale_mobility,
      scale_BAME = nov_df$scale_BAME,
      scale_imd_score= nov_df$scale_imd_score,
      scale_prop_urb= nov_df$scale_prop_urb,
      scale_rain_rolling_7day = nov_df$scale_rain_rolling_7day,
      scale_temp_rolling_7day = nov_df$scale_temp_rolling_7day
    )
  ),
  tag = "train"
)

#validation stack

# validation stack

stack.val <- inla.stack(
  data = list(Log10_NoV_norm = NA), 
  A = list(A.val, 1), 
  effects = list(
    c(s.index, list(Intercept = 1)),
    list(
      week = val$date_index,
      site_code = val$site_code,
      lockdown_step3 = val$lockdown_step3,
      lockdown_step4 = val$lockdown_step4,
      lockdown_lifting = val$lockdown_lifting,
      lockdown_planB = val$lockdown_planB,
      scale_school_density = val$scale_school_density,
      scale_carehome_density = val$scale_carehome_density,
      scale_mobility = val$scale_mobility,
      scale_BAME= val$scale_BAME,
      scale_imd_score= val$scale_imd_score,
      scale_prop_urb= val$scale_prop_urb,
      scale_rain_rolling_7day = val$scale_rain_rolling_7day,
      scale_temp_rolling_7day = val$scale_temp_rolling_7day
    )
  ),
  tag = "val"
)


