library(tidyverse)
library(glmnet)
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

#cross-validation---------------------------------------------------
fit <- list()
metrics<- data.frame()
predictions <- data.frame()

lambda_grid <- 10^seq(-3, 3, length = 100)

#dataframe to store lambda values and their corresponding RMSE
lambda_results <- data.frame(lambda = lambda_grid, MSE = NA)

for (i in seq_along(lambda_grid)) {
  lambda_val <- lambda_grid[i]
  preds_all <- c()
  obs_all <- c()

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

    y_train <- train$nov_3week
    X_train <- model.matrix(nov_3week ~ -1 + lockdown_step3 + lockdown_step4 + lockdown_planB + lockdown_lifting +
                              scale_school_density + scale_carehome_density + scale_imd_score +scale_BAME+
                              scale_mobility+ scale_rain_rolling_7day+ scale_temp_rolling_7day+ scale_prop_urb
                              + date_index * Easting * Northing, data = train)
                              

    X_test <- model.matrix(nov_3week ~ -1 + lockdown_step3 + lockdown_step4 + lockdown_planB + lockdown_lifting +
                              scale_school_density + scale_carehome_density + scale_imd_score +scale_BAME+
                              scale_mobility+ scale_rain_rolling_7day+ scale_temp_rolling_7day+ scale_prop_urb+
                               date_index * Easting * Northing, data = val)                       

    y_test <- val$nov_3week   

    # Align test matrix columns to training matrix
    missing_cols <- setdiff(colnames(X_train), colnames(X_test))
    extra_cols <- setdiff(colnames(X_test), colnames(X_train))

    # Remove extra columns from test set if any
    if(length(extra_cols) > 0) {
      X_test <- X_test[, !(colnames(X_test) %in% extra_cols), drop = FALSE]
    }

    # Add missing columns as zeros
    if(length(missing_cols) > 0) {
      zero_mat <- matrix(0, nrow = nrow(X_test), ncol = length(missing_cols))
      colnames(zero_mat) <- missing_cols
      X_test <- cbind(X_test, zero_mat)
    }

    # Reorder columns to match training set
    X_test <- X_test[, colnames(X_train), drop = FALSE]

    # Fit model with specific lambda
    model <- glmnet(X_train, y_train, alpha = 1, lambda = lambda_val, family = "gaussian")

    # Predict
    pred <- predict(model, s = lambda_val, newx = X_test)
    
    # Collect predictions and actuals
    preds_all <- c(preds_all, pred)
    obs_all <- c(obs_all, y_test)
  }

  # Store performance
  lambda_results$MSE[i] <- mean((obs_all - preds_all)^2, na.rm = TRUE)
}

#Run model with best lambda
best_lambda <- lambda_grid[which.min(lambda_results$MSE)]
print(paste("Best lambda:", best_lambda))