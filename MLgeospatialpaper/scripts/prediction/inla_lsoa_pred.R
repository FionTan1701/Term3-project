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

#setwd("~/Term3-project")

## read data -------------------------------------------------------------------

nov_df<- read.csv("../../data/data/nov_df_full.csv")
pred_grid <- read.csv("../../data/lsoa_prediction_grid.csv")

#englang shapefile
england<- st_read("../../data/data/england_crop.shp")
england<- st_transform(england, crs= 27700)

## scale------------------------------------------------------------------------

covariates_to_scale <-  c("school_den", "carehome_den", "imd", "bame", "mob_7day", "prop_agri", "prop_urb")
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

# ensure no missing coordinates
coords_master <- nov_df %>%
  filter(!is.na(Easting) & !is.na(Northing)) %>%
  distinct(site_code, Easting, Northing)

nov_df <- nov_df %>%
  dplyr::select(-Easting, -Northing) %>%
  left_join(coords_master, by = "site_code")

# convert df into sf and units m to km
nov <- st_as_sf(nov_df, coords= c("Easting", "Northing"), crs= 27700)
nov <- st_transform(nov,  crs = "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +units=km +no_defs")

#grid <-st_as_sf(pred_grid, coords= c("Easting", "Northing"), crs= 27700)
#grid <- st_transform(grid,  crs = "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +units=km +no_defs")


grid <- pred_grid %>%
  rename(lockdown_step3 = lockdown_step3,
      lockdown_step4 = lockdown_step4,
      lockdown_planA = lockdown_lifting,
      lockdown_planB = lockdown_planB,
      scale_mob_7day = scale_mobility,
      temp_1 = weeklytemp_cat1,
      temp_2 = weeklytemp_cat2,
      temp_3 = weeklytemp_cat3,
      temp_4 = weeklytemp_cat4,
      temp_5 = weeklytemp_cat5,
      rain_1 = rain_cat_1,
      rain_2 = rain_cat_2,
      rain_3 = rain_cat_3,
      rain_4 = rain_cat_4,
      rain_5 = rain_cat_5,
    )

## Set prior and formula--------------------------------------------------------

prec_overall_time <- c(10, 0.05)
prec_overall_space <- c(10, 0.05)

intercept_prior <- list(mean.intercept = 0, prec.intercept = 0.0001)
fixed_effects_prior <- list(mean = 0, prec = 0.0001)


rho_hyper = list(theta = list(prior = "pccor0", param = c(0.1, 0.9)))
pc_prec <- list(prec = list(prior = "pc.prec", param = c(10, 0.05)))

formula<-  as.formula('avg_Log10_NoV ~ -1 + Intercept + lockdown_step3 + lockdown_step4 + lockdown_planB +
    scale_school_den + scale_carehome_den + scale_mob_7day + scale_bame + scale_imd + scale_prop_agri + scale_prop_urb +
    temp_2 + temp_3 + temp_4 + temp_5 +
    rain_2 + rain_3 + rain_4 + rain_5 +
                        f(site_code, model="iid", hyper= pc_prec) + f(week, model= "iid", hyper= pc_prec) +
                        f(spatial.field, model=spde, group=spatial.field.group, control.group=list(model="iid", hyper=pc_prec))')
  
## mesh(est) ------------------------------------------------------------------------

max.edge = 5
bound.outer = diff(range(st_coordinates(nov)[,1]))/5
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
all_weeks <- sort(unique(nov$week))

nov$week_index  <- match(nov$week, all_weeks)
grid$week_index <- match(grid$week, all_weeks)

n_week <- length(all_weeks)
s.index<- inla.spde.make.index(name="spatial.field",
                               group = nov$week,
                               n.spde=spde$n.spde,
                               n.group= n_week) 
## A matrix (est)---------------------------------------------------------------

A_est <- inla.spde.make.A(mesh = mesh,
                          loc = coords,
                          group = nov$week_index,
                          n.group = n_week)

dim(A_est)

## estimation stack ------------------------------------------------------------

stack_est <- inla.stack(
  tag = "est",
  data = list(avg_Log10_NoV = nov$avg_Log10_NoV),
  A = list(A_est,1,1),
  effects = list(
    s.index, list(Intercept = 1),
    list(
      week = nov$week,
      site_code = nov$site_code,
      lockdown_step3 = nov$lockdown_step3,
      lockdown_step4 = nov$lockdown_step4,
      lockdown_planA = nov$lockdown_planA,
      lockdown_planB = nov$lockdown_planB,
      scale_school_den = nov$scale_school_den,
      scale_carehome_den = nov$scale_carehome_den,
      scale_mob_7day = nov$scale_mob_7day,
      scale_bame = nov$scale_bame,
      scale_imd= nov$scale_imd,
      scale_prop_urb= nov$scale_prop_urb,
      scale_prop_agri= nov$scale_prop_agri,
        temp_2 = nov$temp_2,
        temp_3 = nov$temp_3,
        temp_4 = nov$temp_4,
        temp_5 = nov$temp_5,
        rain_2 = nov$rain_2,
        rain_3 = nov$rain_3,
        rain_4 = nov$rain_4,
        rain_5 = nov$rain_5
    )
  )
)

## prediction stack ------------------------------------------------------------

sc <- 1/1000
coords.lsoa<- as.matrix(cbind(grid[,"Easting"], grid[,"Northing"])*sc)
#coords.lsoa<- st_coordinates(grid)

nrow(coords.lsoa) == nrow(grid)


group_pred <- grid$week_index
n_week_pred <- length(unique(grid$week))

## A matrix (pred) -------------------------------------------------------------

A_pred = inla.spde.make.A(
  mesh =  mesh,
  loc = coords.lsoa,
  group = group_pred,
  n.group = n_week)

dim(A_pred)

## create index (pred) ---------------------------------------------------------

s.index_pred = inla.spde.make.index(
  name = "spatial.field",
  n.spde = spde$n.spde,
  n.group = n_week)

## prediction stack-------------------------------------------------------------

stack_pred <- inla.stack(
  tag = "pred",
  data = list(avg_Log10_NoV = NA),
  A = list(A_pred,1,1),
  effects = list(
    s.index_pred, list(Intercept = 1),
    list(
      week = grid$week,
      LSOA21CD = grid$LSOA21CD,
      lockdown_step3 = grid$lockdown_step3,
      lockdown_step4 = grid$lockdown_step4,
      lockdown_planA = grid$lockdown_planA,
      lockdown_planB = grid$lockdown_planB,
      scale_school_den = grid$scale_school_den,
      scale_carehome_den = grid$scale_carehome_den,
      scale_mob_7day = grid$scale_mob_7day,
      scale_bame = grid$scale_bame,
      scale_imd= grid$scale_imd,
      scale_prop_urb= grid$scale_prop_urb,
      scale_prop_agri = grid$scale_prop_agri,
      temp_2 = grid$temp_2,
      temp_3 = grid$temp_3,
      temp_4 = grid$temp_4,
      temp_5 = grid$temp_5,
      rain_2 = grid$rain_2,
      rain_3 = grid$rain_3,
      rain_4 = grid$rain_4,
      rain_5 = grid$rain_5
      
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

index  <- inla.stack.index(stack = join_stack, tag ="pred")$data
grid<- data.frame(grid)

grid$pred_mean <-pred.lsoa$summary.fitted.values[index, "mean"]
grid$pred_ll <- pred.lsoa$summary.fitted.values[index, "0.025quant"]
grid$pred_ul <- pred.lsoa$summary.fitted.values[index, "0.975quant"]


write.csv(grid, "../../data/lsoa_prediction.csv", row.names = FALSE)
