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

setwd("~/Term3-project")

## read data -------------------------------------------------------------------
nov_df <- read.csv("Data/final_data/processed_final.csv")
pred_grid <- read.csv("Data/prediction_data/lsoa_grid_prediction.csv")


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

#grid <-st_as_sf(pred_grid, coords= c("Easting", "Northing"), crs= 27700)
#grid <- st_transform(grid,  crs = "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +units=km +no_defs")
grid <- pred_grid %>%
  rename(lockdown_step3 = lockdown_step3,
      lockdown_step4 = lockdown_step4,
      lockdown_lifting = lockdown_lifting,
      lockdown_planB = lockdown_planB,
      scale_school_density = scale_school_den,
      scale_carehome_density = scale_carehome_den,
      scale_mobility = scale_mob_7day,
      scale_BAME = scale_bame,
      scale_imd_score= scale_imd,
      scale_prop_urb= scale_prop_urb,
      scale_rain_rolling_7day = scale_rain_7day_avg,
      scale_temp_rolling_7day = scale_temp_7day_avg,
      date_index = week
    )

## Set prior and formula--------------------------------------------------------

prec_overall_time <- c(10, 0.05)
prec_overall_space <- c(10, 0.05)

intercept_prior <- list(mean.intercept = 0, prec.intercept = 0.0001)
fixed_effects_prior <- list(mean = 0, prec = 0.0001)


rho_hyper = list(theta = list(prior = "pccor0", param = c(0.1, 0.9)))
pc_prec <- list(prec = list(prior = "pc.prec", param = c(10, 0.05)))

formula<-  as.formula('nov_3week ~ -1 + Intercept + lockdown_step3 + lockdown_step4 + lockdown_planB + lockdown_lifting +
    scale_school_density + scale_carehome_density + scale_mobility + scale_BAME + scale_imd_score + scale_prop_urb +
    scale_rain_rolling_7day + scale_temp_rolling_7day + 
    f(site_code, model="iid", hyper= pc_prec) + f(week, model= "iid", hyper= pc_prec) + 
    f(spatial.field, model=spde, group=spatial.field.group, control.group=list(model="iid", hyper=pc_prec))')


## mesh(est) ------------------------------------------------------------------------

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

## SPDE(est) -------------------------------------------------------------------------

spde <- inla.spde2.pcmatern(mesh = mesh, alpha = 2,
                            prior.range = c(5, 0.05),
                            prior.sigma = c(5, 0.05))

# create index
n_week<- length(unique(nov$date_index))
s.index<- inla.spde.make.index(name="spatial.field",
                               group = nov$date_index,
                               n.spde=spde$n.spde,
                               n.group= n_week) 
## A matrix (est)---------------------------------------------------------------

A_est <- inla.spde.make.A(mesh = mesh,
                          loc = coords,
                          group = nov$date_index,
                          n.group = n_week)

dim(A_est)

## estimation stack ------------------------------------------------------------

stack_est <- inla.stack(
  tag = "est",
  data = list(nov_3week = nov$nov_3week),
  A = list(A_est,1),
  effects = list(
    c(s.index, list(Intercept = 1)),
    list(
      week = nov$date_index,
      site_code = nov$site_code,
      lockdown_step3 = nov$lockdown_step3,
      lockdown_step4 = nov$lockdown_step4,
      lockdown_lifting = nov$lockdown_lifting,
      lockdown_planB = nov$lockdown_planB,
      scale_school_density = nov$scale_school_density,
      scale_carehome_density = nov$scale_carehome_density,
      scale_mobility = nov$scale_mobility,
      scale_BAME = nov$scale_BAME,
      scale_imd_score= nov$scale_imd_score,
      scale_prop_urb= nov$scale_prop_urb,
      scale_rain_rolling_7day = nov$scale_rain_rolling_7day,
      scale_temp_rolling_7day = nov$scale_temp_rolling_7day
    )
  )
)

## prediction stack ------------------------------------------------------------

sc <- 1/1000
coords.lsoa<- as.matrix(cbind(grid[,"Easting"], grid[,"Northing"])*sc)
#coords.lsoa<- st_coordinates(grid)

nrow(coords.lsoa) == nrow(grid)


group_pred <- grid$date_index
n_week_pred <- length(unique(grid$date_index))

## A matrix (pred) -------------------------------------------------------------

A_pred = inla.spde.make.A(
  mesh =  mesh,
  loc = coords.lsoa,
  group = group_pred,
  n.group = n_week_pred)

dim(A_pred)

## create index (pred) ---------------------------------------------------------

s.index_pred = inla.spde.make.index(
  name = "spatial.field",
  n.spde = spde$n.spde,
  n.group = n_week_pred)

## prediction stack-------------------------------------------------------------

stack_pred <- inla.stack(
  tag = "pred",
  data = list(nov_3week = NA),
  A = list(A_pred,1),
  effects = list(
    c(s.index_pred, list(Intercept = 1)),
    list(
      week = grid$date_index,
      LSOA21CD = grid$LSOA21CD,
      lockdown_step3 = grid$lockdown_step3,
      lockdown_step4 = grid$lockdown_step4,
      lockdown_lifting = grid$lockdown_lifting,
      lockdown_planB = grid$lockdown_planB,
      scale_school_density = grid$scale_school_density,
      scale_carehome_density = grid$scale_carehome_density,
      scale_mobility = grid$scale_mobility,
      scale_BAME = grid$scale_BAME,
      scale_imd_score= grid$scale_imd_score,
      scale_prop_urb= grid$scale_prop_urb,
      scale_rain_rolling_7day = grid$scale_rain_rolling_7day,
      scale_temp_rolling_7day = grid$scale_temp_rolling_7day
    )
  )
)

## join stack ------------------------------------------------------------------

join_stack <- inla.stack(stack_est, stack_pred)

## prediction model 

pred.lsoa <- inla(formula,
                  data = inla.stack.data(join_stack),
                  family = "gaussian",
                  control.predictor = list(A = inla.stack.A(join_stack), compute = TRUE, link = 1),
                  control.compute = list(dic = TRUE, cpo = TRUE, waic = TRUE,
                                         mlik = TRUE, return.marginals.predictor = TRUE, config = TRUE,
                                         openmp.strategy = "default", smtp = "taucs"),
                  control.fixed = list(mean = 0, prec = 0.0001,mean.intercept = 0, prec.intercept = 0.0001), 
                  control.family = list(hyper = list(prec = list(param = c(1, 0.2)))),
                  verbose= TRUE)

#save(pred.lsoa, file="outputs/prediction/lsoa_pred.RData") 
#saveRDS(pred.lsoa, "outputs/prediction/lsoa_pred.rds")

grid_sf <-st_as_sf(pred_grid, coords= c("Easting", "Northing"), crs= 27700)
grid_sf <- st_transform(grid,  crs = "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +units=km +no_defs")
#grid_sf$nov_3week <- pred.lsoa$summary.fitted.values$mean

ggplot(grid_sf) + 
  geom_sf(aes(color = cluster))
