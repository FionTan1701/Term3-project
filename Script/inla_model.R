#Load packages
library(tidyverse)
library(INLA)
library(inlabru)
library(viridis)
library(sp)
library(fmesher)
library(GGally)

#set directory
setwd("~/Term3-project")

#read data
nov_df <- read.csv("/Data/final_df1.csv")

#mesh construction

coords.cv = unique(nov_df[c("Easting", "Northing")])
boundary.cv = inla.convex.hull(as.matrix(coords.cv[,1:2]))
mesh.cv = inla.mesh.2d(boundary = boundary.cv, max.edge = c(0.8, 1.3), cutoff = 0.1)
plot(mesh.cv)