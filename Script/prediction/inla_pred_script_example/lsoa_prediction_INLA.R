################################################################
## LSOA Predictions
################################################################

library(fmesher)
library(INLA)
library(Matrix)
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


#=================================================== 
###  Process lsoa covariates
#===================================================

grid<- read.csv("data/grid.csv") # centroids of LSOAs 
nov_df<- read.csv("data/nov_cov_clean_2.csv")
nov_df <- subset(nov_df, week != 44)

#=================================================== 
###  Mobility
#===================================================

# lad 21 mobility csv
lad.21.mob<- read.csv("data/lad21mobility.csv")

# Categorise
lad.21.mob$mobility<- as.numeric(lad.21.mob$weighted_workplaces_change)

# crop to dates (lagged by 1 week)
dates<- unique(as.Date(nov_df$collection_date))
nov_df$lagged_date <- as.Date(nov_df$collection_date) - 7

lagged_dates<- unique(nov_df$lagged_date)

lad.21.mob <- subset(lad.21.mob, as.Date(date) %in% lagged_dates)
lad.21.mob$lagged_date<- lad.21.mob$date
lad.21.mob$date<- as.Date(lad.21.mob$lagged_date) + 7

lad.21.mob$mob_cat <- cut(lad.21.mob$mobility, 
                          breaks = quantile(lad.21.mob$mobility, probs = seq(0, 1, by = 0.2), na.rm= TRUE), 
                          include.lowest = TRUE, 
                          labels = FALSE)

lad.21.mob$mob_cat <- as.factor(lad.21.mob$mob_cat)

lad.21.mob<- cbind(lad.21.mob, model.matrix(~mob_cat - 1, lad.21.mob))

lad.21.mob$mob_cat1 <- as.numeric(as.character(lad.21.mob$mob_cat1))
lad.21.mob$mob_cat2 <- as.numeric(as.character(lad.21.mob$mob_cat2))
lad.21.mob$mob_cat3<- as.numeric(as.character(lad.21.mob$mob_cat3))
lad.21.mob$mob_cat4<- as.numeric(as.character(lad.21.mob$mob_cat4))
if ("mob_cat5" %in% names(lad.21.mob)) {
  lad.21.mob$mob_cat5 <- as.numeric(as.character(lad.21.mob$mob_cat5))
}


# lad 21 shapefile
lad.21<- st_read("data/LAD_DEC_2021_UK_BFC.shp")

# crop to england
lad.21<- lad.21 %>%
  filter(grepl("^E", LAD21CD))

# lsoa shapefile
lsoa.21<- st_read("data/LSOA_2021_EW_BFC_V8.shp")

# crop to england
lsoa.21<- lsoa.21 %>%
  filter(grepl("^E", LSOA21CD))


lsoa.lad.21<- st_intersection(lsoa.21, lad.21)


# add mobility data
lsoa.lad.21.mob <- lad.21.mob %>%
  dplyr::left_join(lsoa.lad.21 %>% st_drop_geometry() %>% dplyr::select(LSOA21CD, LAD21CD), by="LAD21CD")



lsoa.cov <- lsoa.lad.21.mob  %>%
  left_join(grid, by = "LSOA21CD")

#=================================================== 
###  Lockdown
#===================================================



# create lockdown variable
lsoa.cov <- lsoa.cov %>%
  mutate(lockdown_phase = case_when(
    date >= as.Date("2021-05-27") & date <= as.Date("2021-07-18") ~ "step3",
    date >= as.Date("2021-07-19") & date <= as.Date("2021-09-02") ~ "step4",
    date >= as.Date("2021-09-16") & date <= as.Date("2021-12-07") ~ "planA",
    date >= as.Date("2021-12-08") & date <= as.Date("2022-01-18") ~ "planB",
    date >= as.Date("2022-01-19") & date <= as.Date("2022-03-17") ~ "lifting"))

# recode as dummy variable
lsoa.cov<- lsoa.cov %>%
  mutate(lockdown_phase = as.character(lockdown_phase)) %>%
  pivot_wider(names_from   = lockdown_phase, 
              values_from  = lockdown_phase, 
              values_fn    = length, 
              values_fill  = 0, 
              names_prefix = "lockdown_") %>%
  mutate(across(starts_with("lockdown_"), ~factor(.x, levels=c(0,1))))

# make sure to convert to numeric
lsoa.cov$lockdown_step3 <- as.numeric(as.character(lsoa.cov$lockdown_step3))
lsoa.cov$lockdown_step4 <- as.numeric(as.character(lsoa.cov$lockdown_step4))
lsoa.cov$lockdown_planA <- as.numeric(as.character(lsoa.cov$lockdown_planA))
lsoa.cov$lockdown_planB <- as.numeric(as.character(lsoa.cov$lockdown_planB))

lsoa.cov$school_cat5<-0
lsoa.cov$carehome_cat5<-0

#=================================================== 
###  Week varable
#===================================================


# Calculate the week-beginning date (Monday) for each observation
lsoa.cov <- lsoa.cov %>%
  mutate(week_beginning = floor_date(date, unit = "week", week_start = 1))

# Define the study start date
study_start <- as.Date("2021-05-27")

# Calculate the study week number based on the week-beginning date
lsoa.cov <- lsoa.cov %>%
  mutate(week = as.integer((week_beginning - study_start) / 7) + 1)

head(lsoa.cov)
  
#=================================================== 
###  Estimation stack
#===================================================

inla.setOption(smtp = "taucs", inla.mode = "classic", num.threads = "256")
  
  ## Priors and formula
  
  # prior specification
  rho_hyper = list(theta = list(prior = "pccor1", param = c(0, 0.9)))
  prec_overall_time <- c(10, 0.05)
  prec_overall_space <- c(10, 0.05)
  prec_overall_region <- c(10, 0.05)
  
  formula<-  as.formula('avg_Log10_NoV ~ -1 + Intercept + mob_cat1 + mob_cat2 + mob_cat3 + mob_cat4 +
                      school_cat2 + school_cat3 + school_cat4 + school_cat5 + carehome_cat2 + carehome_cat3 + carehome_cat4 +
                      carehome_cat5 + lockdown_step3 + lockdown_step4 + lockdown_planA + lockdown_planB + f(site_code, model="iid", prior="pc.prec", param=prec_overall_space) + f(week, model= "iid") + f(spatial.field, model=spde, group=spatial.field.group, control.group=list(model="iid"))')
  
  ## Mesh
  
  sc= 1/1000
  coords<- unique(as.matrix(cbind(nov_df[,"Easting"], nov_df[,"Northing"])*sc))
  
  # mesh 
  max.edge = 50/3 # max.edge 1/5 of spatial range
  bound.outer = 50/3
  domain <- inla.nonconvex.hull(coords,
                                concave = -.07,convex = -0.05, resolution=c(100,100))
  
  mesh<- fm_mesh_2d_inla(boundary = domain,
                         loc=coords,
                         max.edge = c(30,60))
  mesh$n
  
  ## SPDE
  
  # define weights
  nov_df$week<- as.numeric(nov_df$week) # might need to have all weeks for all stations
  n_week <- length(unique(nov_df$week))
  
  # spde
  
  spde<- inla.spde2.pcmatern(mesh = mesh, alpha = 2,
                             prior.range=c(10,0.05),
                             prior.sigma= c(10,0.05),
                             constr= T)

  print(spde$n.spde)
  
  s.index<- inla.spde.make.index(name="spatial.field",
                                 n.spde=spde$n.spde,
                                 group= nov_df$week,
                                 n.group= n_week) 
  length(s.index$spatial.field)
  
  
  
  ## A matrix
  
  sc= 1/1000
  coords.all<- as.matrix(cbind(nov_df[,"Easting"], nov_df[,"Northing"])*sc)
  
  A.est<- inla.spde.make.A(mesh=mesh,
                       loc=coords.all,
                       group=nov_df$week,
                       n.group= n_week)
  dim(A.est)
  
  
  ## Stack
  nov_df$avg_Log10_NoV<- as.numeric(nov_df$avg_Log10_NoV)
  stack_est<- inla.stack(data = list(avg_Log10_NoV = nov_df$avg_Log10_NoV), 
                     A = list(A.est, 1), 
                     effects = list(c(s.index,
                                      list(Intercept = 1)),
                                    list(week = nov_df$week,
                                         site_code = nov_df$site_code,
                                         lockdown_step3 = nov_df$lockdown_step3,
                                         lockdown_step4 = nov_df$lockdown_step4,
                                         lockdown_planA = nov_df$lockdown_planA,
                                         lockdown_planB = nov_df$lockdown_planB,
                                         school_cat2 = nov_df$school_cat2,
                                         school_cat3 = nov_df$school_cat3,
                                         school_cat4 = nov_df$school_cat4,
                                         school_cat5 = nov_df$school_cat5,
                                         carehome_cat2 = nov_df$carehome_cat2,
                                         carehome_cat3 = nov_df$carehome_cat3,
                                         carehome_cat4 = nov_df$carehome_cat4,
                                         carehome_cat5 = nov_df$carehome_cat5,
                                         mob_cat1 = nov_df$mob_cat1,
                                         mob_cat2 = nov_df$mob_cat2,
                                         mob_cat3 = nov_df$mob_cat3,
                                         mob_cat4 = nov_df$mob_cat4)),
                     tag= "est")
  
  
  
#=================================================== 
###  Prediction stack
#===================================================

    
  sc= 1/1000
  coords.lsoa<- as.matrix(cbind(lsoa.cov[,"Easting"], lsoa.cov[,"Northing"])*sc)
  
  groupp= lsoa.cov$week
  n_week<- length(unique(lsoa.cov$week))
  ## A matrix
  
  A.pred = inla.spde.make.A(
    mesh =  mesh,
    loc = coords.lsoa,
    group = groupp,
    n.group = n_week)
  
  dim(A.pred)
  
  
  ## Create indexes
  s.index = inla.spde.make.index(
    name = "spatial.field",
    n.spde = spde$n.spde,
    n.group = n_week)
  
  ## Prediction stack
  
  stack_pred<- inla.stack(data = list(avg_Log10_NoV = NA), 
                         A = list(A.pred, 1), 
                         effects = list(c(s.index,
                                          list(Intercept = 1)),
                                        list(week = lsoa.cov$week,
                                             lockdown_step3 = lsoa.cov$lockdown_step3,
                                             lockdown_step4 = lsoa.cov$lockdown_step4,
                                             lockdown_planA = lsoa.cov$lockdown_planA,
                                             lockdown_planB = lsoa.cov$lockdown_planB,
                                             school_cat2 = lsoa.cov$school_cat2,
                                             school_cat3 = lsoa.cov$school_cat3,
                                             school_cat4 = lsoa.cov$school_cat4,
                                             school_cat5=lsoa.cov$school_cat5,
                                             carehome_cat2 = lsoa.cov$carehome_cat2,
                                             carehome_cat3 = lsoa.cov$carehome_cat3,
                                             carehome_cat4 = lsoa.cov$carehome_cat4,
                                             carehome_cat5 = lsoa.cov$carehome_cat5,
                                             mob_cat1 = lsoa.cov$mob_cat1,
                                             mob_cat2 = lsoa.cov$mob_cat2,
                                             mob_cat3 = lsoa.cov$mob_cat3,
                                             mob_cat4 = lsoa.cov$mob_cat4,
                                             LSOA21CD= lsoa.cov$LSOA21CD)),
                         tag= "pred")
  
  ## Joint stack
  
  joint.stack<- inla.stack(stack_est, stack_pred)
  

#=================================================== 
###  Model
#===================================================
  
  
pred.lsoa <- inla(formula,
                 data = inla.stack.data(joint.stack),
                 family = "gaussian",
                 control.predictor = list(A = inla.stack.A(joint.stack), compute = TRUE, link = 1),
                 control.compute = list(dic = TRUE, cpo = TRUE, waic = TRUE,
                                        mlik = TRUE, return.marginals.predictor = TRUE, config = TRUE,
                                        openmp.strategy = "default", smtp = "taucs"),
                 num.threads= 256,
                 verbose= TRUE)
  
  
#=================================================== 
###  Aggregation
#===================================================
  
  # extract sample.size values from each marginal of the fitted values
  posterior.samples = inla.posterior.sample(n = 100, result=pred.lsoa, intern = FALSE, use.improved.mean = TRUE, add.names = TRUE, seed = 0L)
  
  # extract the posterior latent for the validation sites and reshape in order to have a matrix of nrow(valid) x sample.size
  samples.fitted <- matrix(
    unlist(lapply(
      lapply(posterior.samples, "[[", "latent"), 
      "[", inla.stack.index(joint.stack, "pred")$data
    ), use.names = FALSE), 
    nrow = nrow(lsoa.cov), 
    byrow = FALSE
  )
  nov= "avg_Log10_NoV"
  # extract sample.size values from marginal of the likelihood variance
  # (it is always the last of the Gaussian observations, so we extract the position)
  
  par.index = length(which(substr(rownames(pred.lsoa$summary.hyperpar), 1,26)== "Precision for the Gaussian"))
  samples.var = 1/unlist(lapply(lapply(posterior.samples,"[[", "hyperpar" ),"[", par.index))
  # this is not exactly what we want, in theory we should sample from the transformed posterior marginal
  # rather than inverting the values sampled from the posterior marginal of the precision
  
  samples.fitted = cbind(samples.fitted, LSOA21CD=lsoa.cov[,paste0("LSOA21CD")])
  samples.fitted = cbind(samples.fitted, week=lsoa.cov[,paste0("week")])
  # Create the column names for columns 1 to 100
  new_names <- paste0("sample_", 1:100)
  
  # Assign the new names to columns 1 to 100
  colnames(samples.fitted)[1:100] <- new_names
  columns_to_convert <- 1:100
  
  # Step 3: Convert specified columns to numeric
  samples_df <- as.data.frame(samples.fitted)
  samples_df[, columns_to_convert] <- apply(samples_df[, columns_to_convert], 2, as.numeric)
  
  write.csv(samples_df, "outputs/Predictions/Option1/lsoa_pred_samples.csv"))
  ## Aggregate by time for plotting
  
  time_agg <- samples_df %>%
    group_by(week) %>%
    summarise(across(as.numeric(starts_with("sample_")), \(x) mean(x, na.rm = TRUE)))
  
  
  
  # Function to calculate mean and 95% confidence interval
  calculate_stats <- function(x) {
    mean_val <- mean(x)
    lower <- quantile(x, 0.025)
    upper <- quantile(x, 0.975)
    data.frame(mean = mean_val, lower = lower, upper = upper)
  }
  
  # Gather the data into long format, perform calculations, and spread it back
  time_long <- time_agg %>%
    pivot_longer(cols = starts_with("sample_"), names_to = "sample", values_to = "value") %>%
    group_by(week) %>%
    summarise(
      calculate_stats(value),
      .groups = 'drop'
    )
  
  # save outputs
  write.csv(time_long, "outputs/Predictions/Option1/lsoa_pred_agg.csv") # aggregated predictions
  
  
  space_agg<- samples_df %>%
    group_by(LSOA21CD) %>%
    summarise(across(as.numeric(starts_with("sample_")), \(x) mean(x, na.rm = TRUE)))
  # Gather the data into long format, perform calculations, and spread it back
  space_long <- space_agg %>%
    pivot_longer(cols = starts_with("sample_"), names_to = "sample", values_to = "value") %>%
    group_by(LSOA21CD) %>%
    summarise(
      calculate_stats(value),
      .groups = 'drop'
    )
  
  write.csv(space_long, "outputs/Predictions/Option1/lsoa_pred_agg_space.csv") # aggregated predictions
  
  save(pred.lsoa, file="outputs/Predictions/Option1/lsoa.pred.RData") # model 
  
  
  