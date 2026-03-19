
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

# ensure no missing coordinates
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
###  CV blocks
#===================================================

# shapefile
england<- st_read("../../data/data/england_crop.shp")
england<- st_transform(england, crs= st_crs(nov))

raster<- raster(england)

sites<- nov %>%
  dplyr::select(site_code, geometry) %>%
  unique()

folds<- blockCV::cv_spatial(
  x= sites,
  r= raster,
  k= 10,
  seed= 12,
  plot= FALSE
)

fold_blocks<- folds$blocks

fold_blocks<- st_transform(fold_blocks, crs= st_crs(nov))

# assign folds to data

nov<- nov %>%
  st_intersection(fold_blocks)

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
# spde


## 146 site_codees -> create new s_index

# sort site code + region
nov<- nov %>%
  arrange(site_code, region)

nov<- nov %>%
  arrange(site_code) %>%
  mutate(site_code= as.factor(site_code)) %>%
  mutate(s_index=as.numeric(site_code)) %>%
  mutate(site_code= as.character(site_code))


nov <- nov %>%
  mutate(avg_Log10_NoV = ifelse(GI_ND | GII_ND, NA, avg_Log10_NoV))


#=================================================== 
###  Setting up k-fold CV
#===================================================



#=================================================== 
###  CV loop
#===================================================

# initialise dataframes
predictions<- data.frame()
samples<- data.frame()
all_interval_scores <- numeric()
metrics<- data.frame()
fit <- list()

# shapefile
england<- st_read("../../data/data/england_crop.shp")
england<- st_transform(england, crs= st_crs(nov))



max.edge = 5
bound.outer = diff(range(st_coordinates(nov)[,1]))/5

# initial values, from model

# initial values, from model
theta.ini <- c(
  log(11), # likelihood log precision
  log(7), # log precision site
  log(35), # log precision week
  log(0.5) # theta 1
) 



# Taking dimensions of bounding box

# Loop through each fold for cross-validation
for (k in 1:10) {
  
  # subset data
  train <- subset(nov, folds != k)  # All folds except k
  val <- subset(nov, folds == k)   # Only fold k
  
  val_data<- val$avg_Log10_NoV
  
  # Create mesh using coordinates from training data
  sc= 1/1000
  
  coords.train<- as.matrix(st_coordinates(train))
  
  coords<- unique(st_coordinates(nov))
  
  domain <- inla.nonconvex.hull(coords)
  
  
  mesh<- fm_mesh_2d_inla(boundary = domain,
                         loc=coords.train,
                         max.edge = c(1,2)*max.edge,
                         offset= c(max.edge, bound.outer),
                         cutoff= max.edge/5,
                         crs= st_crs(nov))
  
  
  
  ## A matrices
  
  n_week<- length(unique(nov$week))
  
  coords.train<- as.matrix(st_coordinates(train))
  
  A.train<- inla.spde.make.A(mesh=mesh,
                             loc=coords.train,
                             group=train$week,
                             n.group= n_week)
  print(dim(A.train))
  
  coords.val<- as.matrix(st_coordinates(val))
  
  A.val <- inla.spde.make.A(mesh=mesh,
                            loc=coords.val,
                            group= val$week,
                            n.group= n_week)
  
  print(dim(A.val))
  
  ## SPDE
  
  spde <- inla.spde2.pcmatern(mesh = mesh, alpha = 2,
                              prior.range = c(5, 0.5),
                              prior.sigma = c(5, 0.05),
                              constr= T)
  
  s.index<- inla.spde.make.index(name="spatial.field",
                                 n.spde=spde$n.spde,
                                 n.group= n_week) 
  length(s.index$spatial.field)
  
  
  ## Stacks
  
  # training stack
  stack.train <- inla.stack(
    data = list(avg_Log10_NoV = train$avg_Log10_NoV), 
    A = list(A.train, 1), 
    effects = list(
      c(s.index, list(Intercept = 1)),
      list(
        week = train$week,
        site_code = train$site_code,
        lockdown_step3 = train$lockdown_step3,
        lockdown_step4 = train$lockdown_step4,
        lockdown_planA = train$lockdown_planA,
        lockdown_planB = train$lockdown_planB,
        scale_school_den = train$scale_school_den,
        scale_carehome_den = train$scale_carehome_den,
        scale_mob_7day = train$scale_mob_7day,
        scale_bame= train$scale_bame,
        scale_imd= train$scale_imd,
        scale_prop_urb= train$scale_prop_urb,
        scale_prop_agri= train$scale_prop_agri,
        temp_2 = train$temp_2,
        temp_3 = train$temp_3,
        temp_4 = train$temp_4,
        temp_5 = train$temp_5,
        rain_2 = train$rain_2,
        rain_3 = train$rain_3,
        rain_4 = train$rain_4,
        rain_5 = train$rain_5
      )
    ),
    tag = "train"
  )
  
  # validation stack
  
  stack.val <- inla.stack(
    data = list(avg_Log10_NoV = NA), 
    A = list(A.val, 1), 
    effects = list(
      c(s.index, list(Intercept = 1)),
      list(
        week = val$week,
        site_code = val$site_code,
        lockdown_step3 = val$lockdown_step3,
        lockdown_step4 = val$lockdown_step4,
        lockdown_planA = val$lockdown_planA,
        lockdown_planB = val$lockdown_planB,
        scale_school_den = val$scale_school_den,
        scale_carehome_den = val$scale_carehome_den,
        scale_mob_7day = val$scale_mob_7day,
        scale_bame= val$scale_bame,
        scale_imd= val$scale_imd,
        scale_prop_urb= val$scale_prop_urb,
        scale_prop_agri= val$scale_prop_agri,
        temp_2 = val$temp_2,
        temp_3 = val$temp_3,
        temp_4 = val$temp_4,
        temp_5 = val$temp_5,
        rain_2 = val$rain_2,
        rain_3 = val$rain_3,
        rain_4 = val$rain_4,
        rain_5 = val$rain_5
      )
    ),
    tag = "val"
  )
  
  # join stacks
  
  join.stack <- inla.stack(stack.train, stack.val)
  
  ## Fit model
  
  # formula
  formula<-  as.formula('avg_Log10_NoV ~ -1 + Intercept + lockdown_step3 + lockdown_step4 + lockdown_planB +
    scale_school_den + scale_carehome_den + scale_mob_7day + scale_bame + scale_imd + scale_prop_agri + scale_prop_urb +
    temp_2 + temp_3 + temp_4 + temp_5 +
    rain_2 + rain_3 + rain_4 + rain_5 +
                        f(site_code, model="iid", hyper= pc_prec) + f(week, model= "iid", hyper= pc_prec) +
                        f(spatial.field, model=spde, group=spatial.field.group, control.group=list(model="iid", hyper=pc_prec))')
  print(paste("Fitting of fold", k, "in progress..."))
  
  
  ## Fit the model
  fit[[k]] <- inla(
    formula,
    data=inla.stack.data(join.stack, spde=spde),
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
    control.predictor=list(A=inla.stack.A(join.stack),compute=T, link= 1),
    control.fixed = list(
      mean = 0, 
      prec = 0.0001,
      mean.intercept = 0, 
      prec.intercept = 0.0001
    ), 
    control.family = list(hyper = list(prec = list(param = c(1, 0.2)))),
    inla.mode="experimental",
    verbose=TRUE
  )
  
  
  fit.fold<- fit[[k]]
  
  print(summary(fit.fold))
  
  ## Extract predictions
  
  index_inla_train = inla.stack.index(join.stack,"train")$data
  index_inla_val = inla.stack.index(join.stack,"val")$data
  
  
  results.train= fit.fold$summary.linear.predictor$mean[index_inla_train]
  predicted= fit.fold$summary.linear.predictor$mean[index_inla_val]
  
  train_data<- train$avg_Log10_NoV
  
 
  
  val$mean<-fit.fold$summary.linear.predictor$mean[index_inla_val]
  val$q0.025<-fit.fold$summary.linear.predictor$`0.025quant`[index_inla_val]
  val$q0.975<-fit.fold$summary.linear.predictor$`0.975quant`[index_inla_val]
  
  # append predictions and samples to dataframe
  
  val<- val %>%
    dplyr::select(week, site_code, region, avg_Log10_NoV, mean, q0.025, q0.975) %>%
    st_drop_geometry()
  
  predictions<- rbind(predictions, val)
  observed<-val$avg_Log10_NoV
  predicted<- val$mean
  
  ## Metrics
  
  
  mse<- MSE(observed, predicted)
  mae<- MAE(observed, predicted)
  mape<- MAPE(observed, predicted)
  bias<- BIAS(observed, predicted)
  pbias <- pBIAS(observed, predicted)
  corr <- CORR(observed, predicted)
  cov<- COV(val$avg_Log10_NoV, lower = val$q0.025, upper= val$q0.975)
  
  # store metrics
  metrics_fold <- data.frame(
    Fold = k,
    MSE = mse,
    MAE = mae,
    MAPE = mape,
    BIAS = bias,
    pBIAS = pbias,
    CORR = corr,
    COV = cov
  )
  
  # print metrics fold
  print(metrics_fold)
  
  # append to dataframe
  metrics<- rbind(metrics, metrics_fold)
  
  
  print(paste(k, "folds done"))
}

## CV metrics


# 95 % coverage probability
# Coverage
coverage_vector <- numeric(nrow(predictions))

for (i in 1:nrow(predictions)) {
  # Get the confidence interval bounds for the i-th observation
  lower_bound <- predictions$q0.025[i]
  upper_bound <- predictions$q0.975[i]
  
  # Get the observed value for the i-th observation
  observed <- predictions$avg_Log10_NoV[i]
  
  # Check if the observed value is within the confidence interval bounds
  coverage <- ifelse(observed >= lower_bound & observed <= upper_bound, 1, 0)
  
  # Store the coverage result (1 if within interval, 0 if outside) for the i-th observation
  coverage_vector[i] <- coverage
}

# Calculate the overall coverage probability
coverage_probability <- mean(coverage_vector, na.rm = TRUE) * 100
print(paste("Coverage Probability (%):", coverage_probability))

summary_metrics <- metrics %>%
  summarise(
    Mean_MSE = mean(MSE, na.rm = TRUE),
    Mean_MAE = mean(MAE, na.rm = TRUE),
    Mean_MAPE = mean(MAPE, na.rm = TRUE),
    Mean_BIAS = mean(BIAS, na.rm = TRUE),
    Mean_pBIAS = mean(pBIAS, na.rm = TRUE),
    Mean_CORR = mean(CORR, na.rm = TRUE),
    COV = coverage_probability,
  )

# Print the summary of metrics
print(summary_metrics)


