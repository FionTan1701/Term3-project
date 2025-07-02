library(tidyverse)
library(mgcv)
library(sf)
library(sp)
library(raster)
library(blockCV)

set.seed(123)

setwd("~/Term3-project")

nov_df <- read.csv("Data/processed_final.csv")

#scale function-------------------------------------------------
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

nov <- nov_df
nov <- st_as_sf(nov_df, coords= c("Easting", "Northing"), crs= 27700)
nov <- st_transform(nov,  crs = "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +units=km +no_defs")

nov$one_week_date <- as.numeric(as.Date(nov$one_week_date, format="%d/%m/%Y"))

# create fold blocks------------------------------------------------------------
# unique sites

sites<- nov %>%
  dplyr::select(site_code) %>%
  unique()

# shapefile
england<- st_read("Data/shapefiles/england/england_crop.shp")
england<- st_transform(england, crs= st_crs(nov))

raster<- raster(england)

folds<- cv_spatial(
  x= sites,
  r= raster,
  k= 10,
  seed= 12,
  plot = FALSE
)

fold_blocks<- folds$blocks

fold_blocks<- st_transform(fold_blocks, crs= st_crs(nov))

# assign folds to data  
nov <- nov %>%
  st_intersection(fold_blocks)

#cross-validation---------------------------------------------------
fit <- list()
metrics<- data.frame()

for (k in 1:10) {
  
    # subset data
    train <- subset(nov, folds != k)  # All folds except k
    val <- subset(nov, folds == k)   # Only fold k


    train_coords <- st_coordinates(train)
    train$Easting <- train_coords[,1]
    train$Northing <- train_coords[,2]

    val_coords <- st_coordinates(val)
    val$Easting <- val_coords[,1]
    val$Northing <- val_coords[,2]

    
    fit[[k]] <- gam(nov_3week ~ lockdown_step3 + lockdown_step4 + lockdown_planB + lockdown_lifting + 
                      scale_school_density + scale_carehome_density + scale_mobility + scale_BAME + scale_imd_score + scale_prop_urb +
                      scale_rain_rolling_7day + scale_temp_rolling_7day + s(Easting, Northing, k=150, bs ="tp")+ s(date_index, k=20, bs ="tp") + ti(Easting, Northing, date_index, d=c(2,1), k=20, bs= c("tp","tp","tp")),
                    data = train, family =gaussian,method="REML")

    fit.fold<- fit[[k]]
  
    print(summary(fit.fold))
    print(gam.check(fit.fold))
    
  
    # Predictions
    val$predicted <- predict(fit.fold, newdata = val, type = "response")
    # Calculate mean squared error
    mse <- mean((val$nov_3week - val$predicted)^2, na.rm = TRUE)
    
    mae <- mean(abs(val$nov_3week - val$predicted), na.rm = TRUE)
    mape <- mean(abs((val$nov_3week - val$predicted) / val$nov_3week), na.rm = TRUE) * 100
    bias <- mean(val$predicted - val$nov_3week, na.rm = TRUE)
    pbias <- (bias / mean(val$nov_3week, na.rm = TRUE)) * 100
    corr <- cor(val$nov_3week, val$predicted, use="complete.obs", method="spearman")      

    metrics_fold <- data.frame(
    Fold = k,
    MSE = mse,
    MAE = mae,
    MAPE = mape,
    BIAS = bias,
    pBIAS = pbias,
    CORR = corr
  )
    metrics <- rbind(metrics, metrics_fold)

    print(paste(k, "folds done"))
   
}

summary_metrics <- metrics %>%
  summarise(
    Mean_MSE = mean(MSE, na.rm = TRUE),
    SE = sd(MSE, na.rm = TRUE) / sqrt(length(MSE)),
    CI_Lower = Mean_MSE - qt(0.975, df = length(MSE) - 1) * SE,
    CI_Upper = Mean_MSE + qt(0.975, df = length(MSE) - 1) * SE,
    Mean_MAE = mean(MAE, na.rm = TRUE),
    Mean_MAPE = mean(MAPE, na.rm = TRUE),
    Mean_BIAS = mean(BIAS, na.rm = TRUE),
    Mean_pBIAS = mean(pBIAS, na.rm = TRUE),
    Mean_CORR = mean(CORR, na.rm = TRUE)
  )
  

# Print the summary of metrics
print(summary_metrics)

write.csv(metrics, "outputs/ML_model/cv/ML_gam_model_metrics_full.csv")
