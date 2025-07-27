
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


y_train <- train_data$nov_3week
X_train <- train_data

y_test <- test_data$nov_3week
X_test <- test_data %>% select(-"nov_3week")

formula <- as.formula("nov_3week ~ lockdown_step3 + lockdown_step4 + lockdown_planB + lockdown_lifting +
  scale_school_density + scale_carehome_density + scale_imd_score + scale_BAME +
  scale_mobility + scale_rain_rolling_7day + scale_temp_rolling_7day + scale_prop_urb +
  Easting + Northing + date_index")

start_time <- Sys.time()

model  <- ranger(formula,
                    data=nov_df, 
                    num.trees = 750, 
                    importance = "permutation",
                    mtry = 3, 
                    min.node.size=1,
                    write.forest = TRUE,
                    quantreg = TRUE)

end_time <- Sys.time()
print(paste("Run time", end_time - start_time))
print(as.numeric(difftime(end_time, start_time, units = "secs")))


