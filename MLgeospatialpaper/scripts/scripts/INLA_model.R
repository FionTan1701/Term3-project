
# packages
library(fmesher)
library(INLA)
library(Matrix)
library(MatrixModels)
library(Metrics)
library(sf)
library(scoringRules)
library(tibble)
library(dplyr)
library(sf)
library(sp)
library(fmesher)
library(lubridate)
library(terra)
library(readxl)
library(tidyr)
library(raster)
library(stringr)
library(inlabru)
library(zoo)
library(INLAspacetime)
library(blockCV)

options(saveWorkspace = FALSE)
inla.setOption(num.threads= "6")

# functions
extract.predicted = function(val_pred) {
# Initialize vectors to store results
prediction.vector.tot = NULL
coverage = NULL
ci_amplitude = NULL

# Loop through each row of val_pred
for (i in 1:nrow(val_pred)) {
  # Extract mean, standard deviation, and quantiles
  mean_val = val_pred$mean[i]
  sd_val = val_pred$sd[i]
  q0.025 = val_pred$q0.025[i]
  q0.975 = val_pred$q0.975[i]
  
  # Generate predictions
  prediction.vector = rnorm(n.pred, mean = mean_val, sd = sd_val)
  prediction.vector.tot = c(prediction.vector.tot, prediction.vector)  # Collect all predictions
  
  # Calculate confidence interval amplitude
  ci_amplitude = c(ci_amplitude, q0.975 - q0.025)
  
  # Calculate coverage
  coverage = c(coverage, COV(mean_val, lower = q0.025, upper = q0.975))
}

# Compute summary metrics
predicted = c(
  mean(prediction.vector.tot),  # Mean of all predictions
  round(sum(coverage[!is.na(coverage)]) / length(coverage[!is.na(coverage)]), 4),  # Average coverage
  mean(ci_amplitude),  # Mean of CI amplitudes
  var(prediction.vector.tot)  # Variance of all predictions
)

# Name the elements of the result vector
names(predicted) = c(
  "mean_pred",
  "coverage",
  "ci_amplitude",
  "pmcc"
)

return(predicted)
}


# functions
MSE <- function(z, zhat) {
z <- as.matrix(z)
zhat <- as.matrix(zhat)
x <- c(z - zhat)
u <- x[!is.na(x)]
# round(sqrt(sum(u^2)/length(u)), 4) # cannot be rooted now because we take the average later
round(sum(u^2)/length(u), 4)
}
MAE <- function(z, zhat) {
z <- as.matrix(z)
zhat <- as.matrix(zhat)
x <- abs(c(zhat - z))
u <- x[!is.na(x)]
round(sum(u)/length(u), 4)
}
MAPE <- function(z, zhat) {
z <- as.matrix(z)
zhat <- as.matrix(zhat)
x <- abs(c(zhat - z))/z
u <- x[!is.na(x)]
u <- u[!is.infinite(u)]
round(sum(u)/length(u) * 100, 4)
}
BIAS <- function(z, zhat) {
z <- as.matrix(z)
zhat <- as.matrix(zhat)
x <- c(zhat - z)
u <- x[!is.na(x)]
round(sum(u)/length(u), 4)
}
pBIAS <- function(z, zhat) {
z <- as.matrix(z)
zhat <- as.matrix(zhat)
x <- c(zhat - z)/z
u <- x[!is.na(x)]
u <- u[!is.infinite(u)]
round(sum(u)/length(u) * 100, 4)
}
CORR <- function(z, zhat) {
z <- as.matrix(z)
zhat <- as.matrix(zhat)
round(cor(z,zhat,use="pairwise.complete.obs", method="spearman"), 4)
}
COV <- function(z, lower=NULL, upper=NULL, coverage=NULL) {
if(!is.null(lower) && !is.null(upper)){
  z <- as.matrix(z)
  lower <- as.matrix(lower)
  upper <- as.matrix(upper)
  x <- z>=lower & z<=upper
  u <- x[!is.na(x)]
  round(sum(u)/length(u) * 100, 4)
}else if(!is.null(coverage)){
  round(mean(coverage, na.rm = T),4)
}
}
FRAC2 <- function(z, zhat) {
z <- as.matrix(z)
zhat <- as.matrix(zhat)
x <- z/zhat>=0.5 & z/zhat<=2
u <- x[!is.na(x)]
round(sum(u)/length(u) * 100, 4)
}

PMCC <- function(z, zhat) {
  z <- as.matrix(z)
  zhat <- as.matrix(zhat)
  x <- c(z-zhat)^2
  gof <- sum(x[!is.na(x)])
  pmcc <- gof
  round(pmcc, 4)
}

#=================================================== 
###  Data
#===================================================

nov_df<- read.csv("../../data/data/nov_df_reduced.csv")

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

coords_master <- nov_df %>%
  filter(!is.na(Easting) & !is.na(Northing)) %>%
  distinct(site_code, Easting, Northing)

nov_df <- nov_df %>%
  dplyr::select(-Easting, -Northing) %>%
  left_join(coords_master, by = "site_code")


# set as sf object
nov<- st_as_sf(nov_df, coords= c("Easting", "Northing"), crs= 27700)
nov<- st_transform(nov,  crs = "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +units=km +no_defs")

nov<- nov %>%
  arrange(site_code) %>%
  mutate(site_code= as.factor(site_code)) %>%
  mutate(s_index=as.numeric(site_code)) %>%
  mutate(site_code= as.character(site_code))

nov$Week_date<- as.Date(nov$Week_date)

nov<- nov %>%
  arrange(Week_date) %>%
  mutate(f_index= as.numeric(Week_date))

nov <- nov %>%
  mutate(avg_Log10_NoV = ifelse(GI_ND | GII_ND, NA, avg_Log10_NoV)) # non detects as NA

#=================================================== 
###  Priors
#===================================================

pc_prec <- list(prec = list(prior = "pc.prec", param = c(10, 0.05)))


#=================================================== 
###  Mesh and SPDE set up
#===================================================

max.edge = 5
bound.outer = diff(range(st_coordinates(nov)[,1]))/5

coords<- unique(st_coordinates(nov))

domain <- inla.nonconvex.hull(coords)


mesh<- fm_mesh_2d_inla(boundary = domain,
                     loc=coords,
                     max.edge = c(1,2)*max.edge,
                     offset= c(max.edge, bound.outer),
                     cutoff= max.edge/5,
                     crs= st_crs(nov))



## A matrices

n_week<- length(unique(nov$week))

coords.train<- as.matrix(st_coordinates(nov))

A<- inla.spde.make.A(mesh=mesh,
                           loc=coords.train,
                           group=nov$week,
                           n.group= n_week)
print(dim(A))


## SPDE

spde <- inla.spde2.pcmatern(mesh = mesh, alpha = 2,
                            prior.range = c(5, 0.5),
                            prior.sigma = c(5, 0.05),
                            constr= T)

s.index<- inla.spde.make.index(name="spatial.field",
                               n.spde=spde$n.spde,
                               n.group= n_week) 
length(s.index$spatial.field)

#=================================================== 
###  Modelling
#===================================================

## Stack

stack <- inla.stack(
  data = list(avg_Log10_NoV = nov$avg_Log10_NoV), 
  A = list(A, 1), 
  effects = list(
    c(s.index, list(Intercept = 1)),
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
      scale_bame= nov$scale_bame,
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



## Fit model

# formula
formula<-  as.formula('avg_Log10_NoV ~ -1 + Intercept + lockdown_step3 + lockdown_step4 + lockdown_planB +
  scale_school_den + scale_carehome_den + scale_mob_7day + scale_bame + scale_imd + scale_prop_agri + scale_prop_urb +
  temp_2 + temp_3 + temp_4 + temp_5 +
  rain_2 + rain_3 + rain_4 + rain_5 +
                      f(site_code, model="iid", hyper= pc_prec) + f(week, model= "iid", hyper= pc_prec)  +
                      f(spatial.field, model=spde, group=spatial.field.group, control.group=list(model="iid", hyper=pc_prec))')




## Fit the model
fit<- inla(
  formula,
  data=inla.stack.data(stack, spde=spde),
  family= "gaussian", 
  control.compute = list(
    dic = TRUE, 
    cpo = TRUE, 
    waic = TRUE,
    mlik = TRUE, 
    return.marginals = TRUE, 
    config = TRUE,
    openmp.strategy = "default", 
    smtp = "taucs"),
  control.predictor=list(A=inla.stack.A(stack),compute=T, link= 1),
  control.fixed = list(
    mean = 0, 
    prec = 0.0001,
    mean.intercept = 0, 
    prec.intercept = 0.0001
  ), 
  control.family = list(hyper = list(prec = list(param = c(1, 0.2)))),
  inla.mode="experimental"
)

print(summary(fit))

