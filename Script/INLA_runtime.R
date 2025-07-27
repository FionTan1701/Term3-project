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


setwd("~/Term3-project")
nov_df <- read.csv("Data/processed_final.csv")

## functions--------------------------------------------------------------------

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

## create s_index (numerical index for site code)-------------------------------

nov_df <- nov_df %>%
  arrange(site_code) %>%
  mutate(site_code= as.factor(site_code)) %>%
  mutate(s_index=as.numeric(site_code)) %>%
  mutate(site_code= as.character(site_code))

## create f_index (numerical index for week date)-------------------------------

nov <- nov_df
nov <- nov %>%
  mutate(one_week_date = as.Date(one_week_date, format = "%d/%m/%Y")) %>%  # Adjust format as needed
  arrange(one_week_date) %>%
  mutate(f_index = as.numeric(one_week_date))



## convert df to sf-------------------------------------------------------------

nov <- st_as_sf(nov_df, coords= c("Easting", "Northing"), crs= 27700)
nov <- st_transform(nov,  crs = "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +units=km +no_defs")

# create fold blocks------------------------------------------------------------
# unique sites

sites<- nov %>%
  dplyr::select(site_code) %>%
  unique()

# shapefile
england<- st_read("Data/shapefiles/england/england_crop.shp")
england<- st_transform(england, crs= st_crs(nov))

raster<- raster(england)


#=================================================== 
###  Priors
#===================================================

# prior specification
prec_overall_time <- c(10, 0.05)
prec_overall_space <- c(10, 0.05)

intercept_prior <- list(mean.intercept = 0, prec.intercept = 0.0001)
fixed_effects_prior <- list(mean = 0, prec = 0.0001)


rho_hyper = list(theta = list(prior = "pccor0", param = c(0.1, 0.9)))
pc_prec <- list(prec = list(prior = "pc.prec", param = c(10, 0.05)))



#=================================================== 
###  CV loop
#===================================================

# initialise dataframes
predictions<- data.frame()
samples<- data.frame()
all_interval_scores <- numeric()
metrics<- data.frame()
fit <- list()


max.edge = diff(range(st_coordinates(nov)[,1]))/(3*5)
bound.outer = diff(range(st_coordinates(nov)[,1]))/3

# initial values, from model

# initial values, from model
theta.ini <- c(
  log(11), # likelihood log precision
  log(7), # log precision site
  log(35), # log precision week
  log(0.5) # theta 1
) 


# Use entire dataset as training
train <- nov

# Build mesh, SPDE, A matrix, and stack using full dataset
coords.train <- as.matrix(st_coordinates(train))

formula<-  as.formula('nov_3week ~ -1 + Intercept + lockdown_step3 + lockdown_step4 + lockdown_planB + lockdown_lifting +
    scale_school_density + scale_carehome_density + scale_mobility + scale_BAME + scale_imd_score + scale_prop_urb +
    scale_rain_rolling_7day + scale_temp_rolling_7day +
                        f(site_code, model="iid", hyper= pc_prec) + f(week, model= "iid", hyper= pc_prec) +
                        f(spatial.field, model=spde, group=spatial.field.group, control.group=list(model="iid", hyper=pc_prec))')

# Build domain and mesh
domain <- inla.nonconvex.hull(coords.train)
mesh <- fm_mesh_2d_inla(
  boundary = domain,
  loc = coords.train,
  max.edge = c(1, 2) * max.edge,
  offset = c(max.edge, bound.outer),
  cutoff = max.edge / 5,
  crs = st_crs(nov)
)

# SPDE model
spde <- inla.spde2.pcmatern(mesh = mesh, alpha = 2,
                            prior.range = c(5, 0.05),
                            prior.sigma = c(5, 0.05))

# A matrix for full data
A.train <- inla.spde.make.A(mesh = mesh, loc = coords.train, group = train$date_index, n.group = length(unique(train$date_index)))

# SPDE index
s.index <- inla.spde.make.index(name = "spatial.field", n.spde = spde$n.spde, n.group = length(unique(train$date_index)))

# Stack
stack.train <- inla.stack(
  data = list(nov_3week = train$nov_3week), 
  A = list(A.train, 1),
  effects = list(
    c(s.index, list(Intercept = 1)),
    list(
      week = train$date_index,
      site_code = train$site_code,
      lockdown_step3 = train$lockdown_step3,
      lockdown_step4 = train$lockdown_step4,
      lockdown_lifting = train$lockdown_lifting,
      lockdown_planB = train$lockdown_planB,
      scale_school_density = train$scale_school_density,
      scale_carehome_density = train$scale_carehome_density,
      scale_mobility = train$scale_mobility,
      scale_BAME = train$scale_BAME,
      scale_imd_score = train$scale_imd_score,
      scale_prop_urb = train$scale_prop_urb,
      scale_rain_rolling_7day = train$scale_rain_rolling_7day,
      scale_temp_rolling_7day = train$scale_temp_rolling_7day
    )
  ),
  tag = "train"
)

# Timing the INLA model
start_time <- Sys.time()

fit_full <- inla(
  formula,
  data = inla.stack.data(stack.train, spde = spde),
  family = "gaussian",
  control.predictor = list(A = inla.stack.A(stack.train), compute = TRUE, link = 1),
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE, config = TRUE),
  control.fixed = list(mean = 0, prec = 0.0001, mean.intercept = 0, prec.intercept = 0.0001),
  control.family = list(hyper = list(prec = list(param = c(1, 0.2)))),
  inla.mode = "experimental"
)

end_time <- Sys.time()
runtime <- end_time - start_time
print(paste("INLA model runtime:", runtime))
print(as.numeric(difftime(end_time, start_time, units = "secs")))

