library(tidyverse)
library(ranger)
library(caret)
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
nov_df <- nov_df[!is.na(nov_df$nov_3week), ]

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

print(dim(nov))

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

    # DROP geometry before modeling
    train <- train %>% st_drop_geometry()
    val <- val %>% st_drop_geometry()
    

    formula <- as.formula("nov_3week ~ lockdown_step3 + lockdown_step4 + lockdown_planB + lockdown_lifting +
    scale_school_density + scale_carehome_density + scale_imd_score + scale_BAME +
    scale_mobility + scale_rain_rolling_7day + scale_temp_rolling_7day + scale_prop_urb +
    Easting + Northing + date_index")

    fit[[k]]  <- ranger(formula,
                        data=train, 
                        num.trees = 750, 
                        importance = "permutation",
                        mtry = 3, 
                        min.node.size=1,
                        write.forest = TRUE)

                         
    fit.fold<- fit[[k]]
    print(fit.fold)
    print(summary(fit.fold))

    
  
    # Predictions
    val$predicted <- predict(fit.fold,data = val)$predictions

    # Calculate mean squared error
    mse <- mean((val$nov_3week - val$predicted)^2, na.rm = TRUE)
    rmse <- sqrt(mse)
    mae <- mean(abs(val$nov_3week - val$predicted), na.rm = TRUE)
    mape <- mean(abs((val$nov_3week - val$predicted) / val$nov_3week), na.rm = TRUE) * 100
    bias <- mean(val$predicted - val$nov_3week, na.rm = TRUE)
    pbias <- (bias / mean(val$nov_3week, na.rm = TRUE)) * 100
    corr <- as.numeric(cor(val$nov_3week, val$predicted, use="complete.obs", method="spearman"))      

    metrics_fold <- data.frame(
    Fold = k,
    MSE = mse,
    RMSE = rmse,
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
    Mean_rmse = mean(RMSE, na.rm = TRUE),
    SE_rmse = sd(RMSE, na.rm = TRUE) / sqrt(length(RMSE)),
    CI_Lower_rmse = Mean_rmse - qt(0.975, df = length(RMSE) - 1) * SE_rmse,
    CI_Upper_rmse = Mean_rmse + qt(0.975, df = length(RMSE) - 1) * SE_rmse,
    Mean_MAE = mean(MAE, na.rm = TRUE),
    Mean_MAPE = mean(MAPE, na.rm = TRUE),
    Mean_BIAS = mean(BIAS, na.rm = TRUE),
    Mean_pBIAS = mean(pBIAS, na.rm = TRUE),
    Mean_CORR = mean(CORR, na.rm = TRUE)
  )
  

# Print the summary of metrics
print(summary_metrics)

write.csv(metrics, "outputs/ML_model/cv/ML_rf_model5_metrics_full.csv")
