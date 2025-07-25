# Load packages
library(data.table)
library(ggplot2)
library(sp)
library(sf)
library(terra)
library(mbg)
library(fmesher)
library(Matrix)
library(MatrixModels)
library(Metrics)
library(blockCV)
library(dplyr)
library(caret)
library(raster)

options(saveWorkspace = FALSE)
set.seed(123)
setwd("~/Term3-project")

# Load data
nov_df <- read.csv("Data/processed_final.csv")

# Define covariates to scale
covariates_to_scale <- c("school_density", "carehome_density", "imd_score",
                         "BAME", "mobility", "rain_rolling_7day",
                         "temp_rolling_7day", "prop_urb")

# Scale function
scale_covariates <- function(df, covariates_to_scale) {
  scale_func <- function(x) {
    (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
  }
  df <- df %>% mutate(across(all_of(covariates_to_scale), scale_func, .names = "scale_{.col}"))
  return(df)
}

# Scale covariates
nov_df <- scale_covariates(nov_df, covariates_to_scale)
nov_df <- as.data.frame(nov_df)

# Create s_index and add temporal features
nov_df <- nov_df %>%
  arrange(site_code, date_index) %>%
  mutate(
    site_code = as.factor(site_code),
    s_index = as.numeric(site_code),
    site_code = as.character(site_code)
  ) %>%
  # Remove rows with missing response
  filter(!is.na(nov_3week))

# Convert to sf
nov <- st_as_sf(nov_df, coords = c("Easting", "Northing"), crs = 27700, remove = FALSE)
nov <- st_transform(nov, crs = "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +units=km +no_defs")

# ============================================================================
# SPATIOTEMPORAL CROSS-VALIDATION SETUP
# ============================================================================

# Create spatiotemporal blocks for cross-validation
# Option 1: Spatial blocks with temporal consideration
sites <- nov %>%
  dplyr::select(site_code, geometry) %>%
  distinct()

# Load shapefile
england <- st_read("Data/shapefiles/england/england_crop.shp")
england <- st_transform(england, crs = st_crs(nov))

# Create spatial blocks
england_raster <- raster(england)
folds <- cv_spatial(
  x = sites,
  r = england_raster,
  k = 10,
  seed = 12,
  plot = FALSE
)

fold_blocks <- folds$blocks
fold_blocks <- st_transform(fold_blocks, crs = st_crs(nov))

# Assign spatial folds to data
nov <- nov %>%
  st_intersection(fold_blocks)


# ============================================================================
# COVARIATE PREPARATION FOR MBG
# ============================================================================

# Define covariate names including temporal features
covariate_names <- c(
  "lockdown_step3", "lockdown_step4", "lockdown_planB", "lockdown_lifting",
  "scale_school_density", "scale_carehome_density", "scale_imd_score", 
  "scale_BAME", "scale_mobility", "scale_rain_rolling_7day", 
  "scale_temp_rolling_7day", "scale_prop_urb"
)


# Create template raster
template_raster <- terra::rast(england, res = 1000)  # 1km resolution

# Create id raster
id_raster <- mbg::build_id_raster(england, template_raster)

# ============================================================================
# SPATIOTEMPORAL RASTERIZATION
# ============================================================================

# For spatiotemporal data, create separate rasters for each time period
# or use the full dataset with temporal features

# Option 1: Use all data points (recommended for MBG)
coordinates <- vect(nov)

# Rasterize covariates
covariates_raster <- list()
for (covariate in covariate_names) {
  print(paste("Processing covariate:", covariate))
  
  if (covariate %in% names(nov)) {
    # Use mean aggregation for multiple points in same cell
    cov_rast <- terra::rasterize(coordinates, template_raster, 
                                 field = covariate, fun = "mean")
    covariates_raster[[covariate]] <- cov_rast
  } else {
    warning(paste("Covariate", covariate, "not found in data"))
  }
}

print(paste("Number of covariates successfully rasterized:", length(covariates_raster)))

# ============================================================================
# MBG MACHINE LEARNING MODELS
# ============================================================================

# Enhanced cross-validation settings for spatiotemporal data
cross_validation_settings <- list(
  method = 'repeatedcv',
  number = 5,
  repeats = 3,
  # For spatiotemporal data, consider using custom CV
  index = NULL  # Could specify custom folds here
)

# Enhanced submodel settings
submodel_settings <- list(
  # Elastic Net
  enet = list(
    tuneLength = 10,
    trControl = trainControl(method = "cv", number = 5)
  ),
  
  # Gradient Boosting Machine
  gbm = list(
    verbose = FALSE,
    tuneLength = 10,
    trControl = trainControl(method = "cv", number = 5),
    # GBM parameters for spatiotemporal data
    n.trees = c(50, 100, 150),
    interaction.depth = c(2, 3, 4),
    shrinkage = c(0.01, 0.1),
    n.minobsinnode = c(5, 10)
  ),
  
  # Random Forest (alternative to treebag)
  rf = list(
    tuneLength = 5,
    trControl = trainControl(method = "cv", number = 5),
    ntree = 500,
    importance = TRUE
  ),
  
  # Support Vector Machine
  svm = list(
    tuneLength = 5,
    trControl = trainControl(method = "cv", number = 5)
  )
)

# ============================================================================
# CUSTOM SPATIOTEMPORAL CROSS-VALIDATION
# ============================================================================

# Create custom CV indices for spatiotemporal data
create_spatiotemporal_cv <- function(data, n_folds = 5) {
  # Get unique spatiotemporal fold combinations
  unique_folds <- unique(data$spatiotemporal_fold)
  fold_groups <- split(unique_folds, cut(seq_along(unique_folds), n_folds))
  
  cv_indices <- list()
  for (i in 1:n_folds) {
    train_folds <- unlist(fold_groups[-i])
    test_folds <- fold_groups[[i]]
    
    train_idx <- which(data$spatiotemporal_fold %in% train_folds)
    
    cv_indices[[i]] <- train_idx
  }
  
  return(cv_indices)
}

# Create custom CV indices

custom_cv_indices <- create_spatiotemporal_cv(nov, n_folds = 5)

# Update CV settings with custom indices
cross_validation_settings$index <- custom_cv_indices

# ============================================================================
# RUN MBG MODELS
# ============================================================================

print("Starting MBG spatiotemporal modeling...")
print(paste("Total observations:", nrow(nov)))
print(paste("Number of covariates:", length(covariate_names)))
print(paste("Unique sites:", length(unique(nov$site_code))))
print(paste("Time periods:", min(nov$date_index), "to", max(nov$date_index)))

# Run MBG submodels
submodels <- mbg::run_regression_submodels(
  input_data = nov,
  id_raster = id_raster,
  covariates = covariates_raster,
  cv_settings = cross_validation_settings,
  model_settings = submodel_settings,
  prediction_range = c(0, 1),
  # Additional parameters for spatiotemporal modeling
  indicator = "nov_3week",
  indicator_family = "gaussian"
)

print("MBG modeling completed successfully!")
print("Available models:")
print(names(submodels))

# ============================================================================
# MODEL EVALUATION AND COMPARISON
# ============================================================================

# Extract model performance metrics
model_performance <- list()

for (model_name in names(submodels)) {
  if (!is.null(submodels[[model_name]])) {
    # Extract CV results
    cv_results <- submodels[[model_name]]$cv_results
    
    # Calculate performance metrics
    rmse <- sqrt(mean((cv_results$obs - cv_results$pred)^2, na.rm = TRUE))
    mae <- mean(abs(cv_results$obs - cv_results$pred), na.rm = TRUE)
    r2 <- cor(cv_results$obs, cv_results$pred, use = "complete.obs")^2
    
    model_performance[[model_name]] <- list(
      RMSE = rmse,
      MAE = mae,
      R2 = r2
    )
  }
}

# Display results
print("\nModel Performance Comparison:")
performance_df <- do.call(rbind, lapply(names(model_performance), function(x) {
  data.frame(
    Model = x,
    RMSE = round(model_performance[[x]]$RMSE, 4),
    MAE = round(model_performance[[x]]$MAE, 4),
    R2 = round(model_performance[[x]]$R2, 4)
  )
}))

print(performance_df)

# ============================================================================
# VARIABLE IMPORTANCE ANALYSIS
# ============================================================================

# Extract variable importance for each model
variable_importance <- list()

for (model_name in names(submodels)) {
  if (!is.null(submodels[[model_name]]) && "importance" %in% names(submodels[[model_name]])) {
    variable_importance[[model_name]] <- submodels[[model_name]]$importance
  }
}

# ============================================================================
# PREDICTIONS AND MAPPING
# ============================================================================

# Generate predictions using the best model
best_model <- names(model_performance)[which.max(sapply(model_performance, function(x) x$R2))]
print(paste("Best performing model:", best_model))

# Make predictions
predictions <- mbg::predict_submodels(
  submodels = submodels,
  id_raster = id_raster,
  covariates = covariates_raster,
  model_name = best_model
)

# ============================================================================
# SAVE RESULTS
# ============================================================================

# Save all results
results_list <- list(
  submodels = submodels,
  model_performance = model_performance,
  variable_importance = variable_importance,
  predictions = predictions,
  covariate_names = covariate_names,
  cross_validation_settings = cross_validation_settings,
  data_summary = list(
    n_obs = nrow(nov),
    n_sites = length(unique(nov$site_code)),
    time_range = range(nov$date_index),
    n_covariates = length(covariate_names)
  )
)

saveRDS(results_list, "mbg_spatiotemporal_results.rds")

# Save performance table
write.csv(performance_df, "model_performance_comparison.csv", row.names = FALSE)

print("Analysis complete! Results saved to 'mbg_spatiotemporal_results.rds'")
print("Performance comparison saved to 'model_performance_comparison.csv'")