library(xgboost)
library(mlr)
library(dplyr)
library(blockCV)
library(sf)

set.seed(123)

setwd("~/Term3-project")

nov_df <- read.csv("Data/processed_final.csv")

# Scale function
covariates_to_scale <- c("school_density", "carehome_density", "imd_score", "BAME", 
                        "mobility", "rain_rolling_7day","temp_rolling_7day", "prop_urb")

scale_covariates <- function(df, covariates_to_scale) {
  scale <- function(x) {
    (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
  }
  
  df <- df %>% mutate(across(all_of(covariates_to_scale), scale, .names = "scale_{.col}"))
  
  return(df)
}

nov_df <- scale_covariates(nov_df, covariates_to_scale)

nov_df <- nov_df %>%
  dplyr::select(nov_3week, lockdown_step3, lockdown_step4, lockdown_planB, lockdown_lifting,
                scale_school_density, scale_carehome_density, scale_imd_score, scale_BAME,
                scale_mobility, scale_rain_rolling_7day, scale_temp_rolling_7day, scale_prop_urb,
                Easting, Northing, date_index)

nov_df <- as.data.frame(nov_df)

# Remove rows with NA in target variable
nov_df <- nov_df[!is.na(nov_df$nov_3week), ]

# Convert to spatial object for blockCV
nov_sf <- nov_df %>%
  st_as_sf(coords = c("Easting", "Northing"), crs = 27700)  # Adjust CRS as needed

# Create spatial folds
folds <- cv_spatial(
  x = nov_sf,
  k = 10,
  seed = 12,
  plot = FALSE
)

# Extract fold assignments and add to dataframe
nov_df$fold_id <- folds$folds_ids

ptm <- proc.time()

# Define task using ALL data
task <- makeRegrTask(id = "xgboost_spatial_cv",
                     data = nov_df %>% dplyr::select(-fold_id),
                     target = "nov_3week")

# Create XGBoost learner
learner <- makeLearner("regr.xgboost")

# Create spatial resampling strategy using the fold blocks from ALL data
spatial_folds <- nov_df$fold_id

# Create fold indices for mlr using ALL data
train_fold_indices <- list()
for(i in 1:length(unique(spatial_folds))) {
  train_idx <- which(spatial_folds != i)
  test_idx <- which(spatial_folds == i)
  train_fold_indices[[i]] <- list(train = train_idx, test = test_idx)
}

# Create fixed resampling instance
rin <- makeResampleInstance(desc = makeResampleDesc("CV", iters = length(unique(spatial_folds))),
                           size = nrow(nov_df))
rin$train.inds <- lapply(train_fold_indices, function(x) x$train)
rin$test.inds <- lapply(train_fold_indices, function(x) x$test)

# Define XGBoost parameter grid
ps <- makeParamSet(
  makeIntegerParam("nrounds", lower = 50, upper = 500),           # Number of boosting rounds
  makeNumericParam("eta", lower = 0.01, upper = 0.3),            # Learning rate
  makeIntegerParam("max_depth", lower = 3, upper = 10),          # Maximum tree depth
  makeNumericParam("subsample", lower = 0.5, upper = 1.0),       # Subsample ratio
  makeNumericParam("colsample_bytree", lower = 0.5, upper = 1.0), # Feature sampling ratio
  makeNumericParam("min_child_weight", lower = 1, upper = 10)     # Minimum child weight
)

# Alternative: Smaller grid for faster testing
ps_small <- makeParamSet(
  makeDiscreteParam("nrounds", values = c(100, 200, 300)),
  makeDiscreteParam("eta", values = c(0.05, 0.1, 0.2)),
  makeDiscreteParam("max_depth", values = c(4, 6, 8)),
  makeDiscreteParam("subsample", values = c(0.7, 0.8, 1.0)),
  makeDiscreteParam("colsample_bytree", values = c(0.7, 0.8, 0.9))
)

# Choose which parameter set to use
param_set <- ps  # Change to ps for full grid

# Perform grid search with spatial cross-validation
res <- tuneParams(learner, task, resampling = rin, par.set = param_set,
                 control = makeTuneControlGrid())

print("Best hyperparameters:")
print(res$x)
print("Best CV performance (MSE):")
print(res$y)

# Train final model with best hyperparameters on ALL data
lrn <- setHyperPars(makeLearner("regr.xgboost"), par.vals = res$x)
m <- train(lrn, task)

print(m)
print(proc.time() - ptm)

# Optional: Show CV performance details
print("Cross-validation results summary:")
print(paste("Mean CV RMSE:", round(sqrt(res$y), 4)))
print(paste("Number of spatial folds used:", length(unique(spatial_folds))))

# Optional: Get feature importance from final model
importance <- getFeatureImportance(m)
print("Feature Importance:")
print(importance$res)