# Load required libraries
library(mlr3)
library(mlr3learners)
library(mlr3spatiotempcv)
library(mlr3tuning)
library(paradox)
library(blockCV)
library(sf)
library(data.table)
library(dplyr)

set.seed(123)
setwd("~/Term3-project")

# Load and scale data
nov_df <- read.csv("Data/processed_final.csv")

covariates_to_scale <- c("school_density", "carehome_density", "imd_score", "BAME", 
                         "mobility", "rain_rolling_7day", "temp_rolling_7day", "prop_urb")

scale_covariates <- function(df, covariates) {
  df %>% mutate(across(all_of(covariates), ~ (.-mean(., na.rm=TRUE)) / sd(., na.rm=TRUE), 
                       .names = "scale_{.col}"))
}

nov_df <- scale_covariates(nov_df, covariates_to_scale)

nov_df <- nov_df %>%
  dplyr::select(nov_3week, lockdown_step3, lockdown_step4, lockdown_planB, lockdown_lifting,
                scale_school_density, scale_carehome_density, scale_imd_score, scale_BAME,
                scale_mobility, scale_rain_rolling_7day, scale_temp_rolling_7day, scale_prop_urb,
                Easting, Northing, date_index) %>%
  na.omit()

# Convert to sf object
nov_sf <- nov_df %>% st_as_sf(coords = c("Easting", "Northing"), crs = 27700)

# Add spatial folds using blockCV (assigning fold IDs manually)
folds <- cv_spatial(x = nov_sf, k = 10, seed = 12, plot = FALSE)
nov_df$fold_id <- folds$folds_ids

# Create mlr3 Task
task <- TaskRegr$new(id = "spatial_task", backend = as.data.table(nov_df), target = "nov_3week")

# Manually create custom spatial resampling based on fold_id
resampling <- rsmp("custom")
resampling$instantiate(task,
  train_sets = lapply(1:10, function(i) which(nov_df$fold_id != i)),
  test_sets  = lapply(1:10, function(i) which(nov_df$fold_id == i))
)

# Create ranger learner
learner <- lrn("regr.ranger", predict_type = "response")

# Define hyperparameter space
param_set <- ps(
  mtry = p_int(lower = 3, upper = 8),
  num.trees = p_int(lower = 500, upper = 1000),
  min.node.size = p_int(lower = 1, upper = 10)
)

# Grid tuning
grid_search <- tnr("grid_search", resolution = 5)  # 5 levels per param

# AutoTuner for tuning and resampling
at <- AutoTuner$new(
  learner = learner,
  resampling = resampling,
  measure = msr("regr.rmse"),
  search_space = param_set,
  terminator = trm("none"),
  tuner = grid_search
)

# Train with tuning
ptm <- proc.time()
at$train(task)
print(proc.time() - ptm)

# Report best parameters and performance
print("Best hyperparameters:")
print(at$archive$best()$x_domain)
print("Best RMSE:")
print(at$archive$best()$regr.rmse)

# Final model
final_model <- at$learner
print(final_model)
