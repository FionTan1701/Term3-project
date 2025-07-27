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
                              scale_mobility+ scale_rain_rolling_7day+ scale_temp_rolling_7day+ scale_prop_urb +
                              date_index + Easting +Northing +
                              date_index * Easting * Northing, data = train)
                              

    X_test <- model.matrix(nov_3week ~ -1 + lockdown_step3 + lockdown_step4 + lockdown_planB + lockdown_lifting +
                              scale_school_density + scale_carehome_density + scale_imd_score +scale_BAME+
                              scale_mobility+ scale_rain_rolling_7day+ scale_temp_rolling_7day+ scale_prop_urb+
                              date_index + Easting +Northing +
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
                              scale_mobility+ scale_rain_rolling_7day+ scale_temp_rolling_7day+ scale_prop_urb +
                              date_index + Easting + Northing +
                              date_index * Easting * Northing, data = train)
                              

    X_test <- model.matrix(nov_3week ~ -1 + lockdown_step3 + lockdown_step4 + lockdown_planB + lockdown_lifting +
                              scale_school_density + scale_carehome_density + scale_imd_score +scale_BAME+
                              scale_mobility+ scale_rain_rolling_7day+ scale_temp_rolling_7day+ scale_prop_urb+ 
                              date_index + Easting + Northing +
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

    fit[[k]] <- glmnet(X_train, y_train, alpha = 1, lambda = best_lambda, family = "gaussian")                    

    fit.fold<- fit[[k]]
  
    print(summary(fit.fold))

    
    # Predictions
    val$predicted <- predict(fit.fold, s= best_lambda, newx = X_test)

    # bootstrap predictions for uncertainty estimation
    n_boot <- 10000
    boot_preds <- matrix(NA, nrow = n_boot, ncol = nrow(val))

    for (i in 1:n_boot) {
      set.seed(123 + i)
      boot_idx <- sample(seq_len(nrow(train)), replace = TRUE)
      X_train_boot <- X_train[boot_idx, ]
      y_train_boot <- y_train[boot_idx]
      boot_model <- glmnet(X_train_boot, y_train_boot, alpha = 1, lambda = best_lambda, family = "gaussian")
      boot_preds[i, ] <- as.numeric(predict(boot_model, s = best_lambda, newx = X_test))
    }

    val$q0.025 <- apply(boot_preds, 2, quantile, probs = 0.025)
    val$q0.975 <- apply(boot_preds, 2, quantile, probs = 0.975)
        
    val <- val %>%
      dplyr::select(site_code, s_index, one_week_date, date_index, nov_3week, predicted, q0.025, q0.975) %>%
      st_drop_geometry()
      
    predictions <- rbind(predictions, val)

    # Calculate metrics
    mse <- mean((val$nov_3week - val$predicted)^2, na.rm = TRUE)
    rmse <- sqrt(mse)
    mae <- mean(abs(val$nov_3week - val$predicted), na.rm = TRUE)
    mape <- mean(abs((val$nov_3week - val$predicted) / val$nov_3week), na.rm = TRUE) * 100
    bias <- mean(val$predicted - val$nov_3week, na.rm = TRUE)
    pbias <- (bias / mean(val$nov_3week, na.rm = TRUE)) * 100
    corr <- as.numeric(cor(val$nov_3week, val$predicted, use="complete.obs", method="spearman"))  
    cov <- COV(val$nov_3week, lower = val$q0.025, upper = val$q0.975)    

    metrics_fold <- data.frame(
    Fold = k,
    MSE = mse,
    RMSE = rmse,
    MAE = mae,
    MAPE = mape,
    BIAS = bias,
    pBIAS = pbias,
    CORR = corr,
    COV = cov
  )
    metrics <- rbind(metrics, metrics_fold)

    print(paste(k, "folds done"))
   
}

# 95 % coverage probability
# Coverage
coverage_vector <- numeric(nrow(predictions))

for (i in 1:nrow(predictions)) {
  # Get the confidence interval bounds for the i-th observation
  lower_bound <- predictions$q0.025[i]
  upper_bound <- predictions$q0.975[i]
  
  # Get the observed value for the i-th observation
  observed <- predictions$nov_3week[i]
  
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
    Mean_CORR = mean(CORR, na.rm = TRUE),
    COV = coverage_probability
  )
  

# Print the summary of metrics
print(summary_metrics)

write.csv(metrics, "outputs/ML_model/cv/ML_lasso_model8b_metrics_full_final2.csv")
write.csv(predictions, "outputs/ML_model/cv/ML_lasso_model8b_predictions_full_final2.csv")
