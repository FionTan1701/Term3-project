library(tidyverse) 
library(lubridate)
library(INLA)
library(inlabru)
library(sp)
library(fmesher)
library(corrr)
library(sf)

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

#max.edge = 15
max.edge = diff(range(st_coordinates(nov)[,1]))/(3*5)
bound.outer = diff(range(st_coordinates(nov)[,1]))/3

coords<- unique(st_coordinates(nov))

#domain <- inla.nonconvex.hull(coords, concave = -0.05, convex = -0.02, resolution=c(200,200))
domain <- inla.nonconvex.hull(coords)


mesh<- fm_mesh_2d_inla(boundary = domain,
                       loc=coords,
                       max.edge = c(1,2)*max.edge,
                       offset= c(max.edge, bound.outer),
                       cutoff= max.edge/5,
                       crs= st_crs(nov))


## SPDE

spde <- inla.spde2.pcmatern(mesh = mesh, alpha = 2,
                            prior.range = c(5, 0.05),
                            prior.sigma = c(5, 0.05))


## Create spatial index and A matrices

n_week<- length(unique(nov$date_index))
n_spatial <- mesh$n

#s.index contains
#(i) spatial.field index from 1 to n_spatial for n_week times
#(ii) spatial.field.group from 1 to n_week, each element replicated n_spatial times
s.index<- inla.spde.make.index(name="spatial.field",
                               n.spde=spde$n.spde,
                               n.group= n_week) 


coords<- as.matrix(st_coordinates(nov))

A.train <- inla.spde.make.A(mesh=mesh,
                           loc=coords,
                           group=nov$date_index,
                           n.group= n_week)

#matrix A has dimension number of observation * number of indices
print(dim(A.train))
print(nrow(nov))
print(length(s.index$spatial.field))

#estimation stack
stack.train <- inla.stack(
  data = list(nov_3week = nov$nov_3week), 
  A = list(A.train, 1), 
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
  ),
  tag = "train"
)

# Creat A matrix for validation prediction
#at the sae location

A.val <- inla.spde.make.A(mesh=mesh,
                            loc=coords,
                            group=nov$date_index,
                            n.group= n_week)

print(dim(A.val))

# validation stack

stack.val <- inla.stack(
  data = list(nov_3week = NA), 
  A = list(A.val, 1), 
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
      scale_BAME= nov$scale_BAME,
      scale_imd_score= nov$scale_imd_score,
      scale_prop_urb= nov$scale_prop_urb,
      scale_rain_rolling_7day = nov$scale_rain_rolling_7day,
      scale_temp_rolling_7day = nov$scale_temp_rolling_7day
    )
  ),
  tag = "val"
)


# join stacks

join.stack <- inla.stack(stack.train, stack.val)

# formula
formula<-  as.formula('nov_3week ~ -1 + Intercept + lockdown_step3 + lockdown_step4 + lockdown_planB + lockdown_lifting +
    scale_school_density + scale_carehome_density + scale_mobility + scale_BAME + scale_imd_score + scale_prop_urb +
    scale_rain_rolling_7day + scale_temp_rolling_7day +
                        f(site_code, model="iid", hyper= pc_prec) + f(week, model= "iid", hyper= pc_prec) +
                        f(spatial.field, model=spde, group=spatial.field.group, control.group=list(model="iid", hyper=pc_prec))')

## Fit the model
fit <- inla(
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
  control.predictor=list(A=inla.stack.A(join.stack),compute=TRUE, link= 1),
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

#saveRDS(fit, "outputs/model7_processed.rds")

index_inla_train <- inla.stack.index(join.stack,"train")$data

# correlation between the data response and the posterior mean of the predicted values 
#print(cor(nov$nov_3week, fit$summary.linear.predictor$mean[index_inla_train], use="complete.obs"))
print(paste("Correlation of obeserved and predicted:",cor(nov$nov_3week, fit$summary.fitted.values$mean[index_inla_train], use="complete.obs")))

lims <- range(c(nov$nov_3week, fit$summary.fitted.values$mean[index_inla_train]), na.rm = TRUE)

pdf("m7_processed_corrplotv2.pdf",width = 14, height = 10)

# plot(nov$nov_3week, fit$summary.linear.predictor$mean[index_inla_train])

plot(nov$nov_3week, fit$summary.fitted.values$mean[index_inla_train],
main ="Observed vs predicted (Posterior Mean)",
xlab = "Observed nov_3week",
ylab = "Predicted nov_3week",
xlim = lims,
ylim = lims)
abline(0,1,col="blue",lwd=2)

dev.off()