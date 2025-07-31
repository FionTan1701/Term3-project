library(ranger)
library(mlr)
library(ranger)
library(dplyr)

set.seed(123)

setwd("~/Term3-project")

nov_df <- read.csv("Data/processed_final.csv")

#scale function
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

nov_df <- nov_df %>% 
  dplyr::select(nov_3week, lockdown_step3, lockdown_step4, lockdown_planB, lockdown_lifting,
                scale_school_density, scale_carehome_density, scale_imd_score, scale_BAME,
                scale_mobility, scale_rain_rolling_7day, scale_temp_rolling_7day, scale_prop_urb,Easting, Northing, date_index)


nov_df<- as.data.frame(nov_df)

#remove rows with NA in target variable
nov_df <- nov_df[!is.na(nov_df$nov_3week), ]

#train/test split
train_index <- sample(1:nrow(nov_df), size = 0.8 * nrow(nov_df))
train_data <- nov_df[train_index, ]
test_data  <- nov_df[-train_index, ] 


ptm <- proc.time()
# Define task and learner
task <- makeRegrTask(id = "train_data",
                     data = train_data,
                     target = "nov_3week")

learner <- makeLearner("regr.ranger")

# Choose resampling strategy and define grid
rdesc <- makeResampleDesc("CV", iters = 5)
ps <- makeParamSet(makeIntegerParam("mtry", 3, 8),
                   makeDiscreteParam("num.trees", c(500,1000)),
                  makeIntegerParam("min.node.size",lower = 1, upper =10))
        
                   
# Tune
res = tuneParams(learner, task, rdesc, par.set = ps,
                 control = makeTuneControlGrid())

# Train on entire dataset (using best hyperparameters)
lrn = setHyperPars(makeLearner("regr.ranger"), par.vals = res$x)
m = train(lrn, task)

print(m)
print(proc.time() - ptm) # ~6 seconds