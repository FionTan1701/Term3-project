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

#mesh construction
##############


pdf("mesh2.pdf", width = 14, height = 10)

coords<- unique(st_coordinates(nov_df))

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
plot(mesh)

ggplot() +
  gg(mesh) +
  geom_point(data = coords, aes(Longitude, Latitude)) 

dev.off()

##############

