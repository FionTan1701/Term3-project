library(ranger)
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

print(nov_df$fold_id)

ptm <- proc.time()

# Define task using ALL data
task <- makeRegrTask(id = "spatial_cv_data",
                     data = nov_df %>% dplyr::select(-fold_id),
                     target = "nov_3week")

# Create learner
learner <- makeLearner("regr.ranger")

# Create spatial resampling strategy using the fold blocks from ALL data
spatial_folds <- nov_df$fold_id

# Alternative approach: Use mlr's makeFixedHoldoutInstance if you prefer
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

# Define parameter grid
ps <- makeParamSet(
  makeIntegerParam("mtry", 3, 8),
  makeDiscreteParam("num.trees", c(500, 1000)),
  makeIntegerParam("min.node.size", lower = 1, upper = 10)
)

# Perform grid search with spatial cross-validation
res <- tuneParams(learner, task, resampling = rin, par.set = ps,
                 control = makeTuneControlGrid())

print(colnames(res))
print("Best hyperparameters:")
print(res$x)
print("Best CV performance:")
print(res$y)

# Train final model with best hyperparameters on ALL data
lrn <- setHyperPars(makeLearner("regr.ranger"), par.vals = res$x)
m <- train(lrn, task)

print(m)
print(proc.time() - ptm)

# Optional: Show CV performance details
print("Cross-validation results summary:")
print(paste("Mean CV RMSE:", round(sqrt(res$y), 4)))
#print(paste("Number of spatial folds used:", length(unique(spatial_folds))))