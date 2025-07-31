library(xgboost)
library(caret)
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
                             scale_mobility, scale_rain_rolling_7day, scale_temp_rolling_7day, scale_prop_urb,date_index, Easting, Northing)


nov_df<- as.data.frame(nov_df)

#remove rows with NA in target variable
nov_df <- nov_df[!is.na(nov_df$nov_3week), ]

#train/test split
train_index <- sample(1:nrow(nov_df), size = 0.8 * nrow(nov_df))
train_data <- nov_df[train_index, ]
test_data  <- nov_df[-train_index, ]


y_train <- train_data$nov_3week
X_train <- model.matrix(nov_3week ~ . -1, data = train_data)

y_test <- test_data$nov_3week
X_test <- model.matrix(nov_3week ~ . -1, data = test_data)


custom_summary <- function(data, lev = NULL, model = NULL) {
  # data has: obs (true), pred (predicted)
  obs <- data$obs
  pred <- data$pred

  mae <- mean(abs(data$obs - data$pred), na.rm = TRUE)
  bias <- mean(data$pred - data$obs, na.rm = TRUE)
  rmse <- sqrt(mean((data$obs - data$pred)^2, na.rm = TRUE))

  # R-squared calculation
  tss <- sum((obs - mean(obs))^2)
  rss <- sum((pred - obs)^2)
  rsq <- 1 - (rss / tss)
  
  out <- c(MAE = mae, BIAS = bias, RMSE = rmse)
  return(out)
}

# Define parameter grid
grid <- expand.grid(
  nrounds = c(100, 200, 300),
  max_depth = c(5,6,7,8,9),
  eta = c(0.01, 0.1, 0.2),
  gamma = 0,
  colsample_bytree = c(0.7, 0.8, 0.9),
  min_child_weight = 1,
  subsample = c(0.7, 0.8, 0.9)
)

# Set up cross-validation
ctrl <- trainControl(
  method = "cv",
  number = 10,  
  verboseIter = TRUE,
  summaryFunction = custom_summary
)

# Train with grid search + k-fold CV
xgb_model <- train(
  x = X_train,
  y = y_train,
  method = "xgbTree",
  trControl = ctrl,
  tuneGrid = grid,
  metric = "RMSE"  
)

# View results
print(xgb_model$results)
print(xgb_model$bestTune)

#predict on test data
predictions <- predict(xgb_model, newdata = X_test)

# Calculate evaluation metrics
mse <- mean((y_test - predictions)^2, na.rm = TRUE)
rmse <- sqrt(mse)
mae <- mean(abs(y_test - predictions), na.rm = TRUE)
mape <- mean(abs((y_test - predictions) / y_test), na.rm = TRUE) * 100
bias <- mean(predictions - y_test, na.rm = TRUE)
pbias <- (bias / mean(y_test, na.rm = TRUE)) * 100
corr <- as.numeric(cor(y_test, predictions, use = "complete.obs", method = "spearman"))

print(paste("Mean Squared Error (MSE):", mse))
print(paste("Root Mean Squared Error (RMSE):", rmse))
print(paste("Mean Absolute Error (MAE):", mae))
print(paste("Mean Absolute Percentage Error (MAPE):", mape))
print(paste("Bias:", bias))
print(paste("Percent Bias (pBIAS):", pbias))
print(paste("Spearman Correlation:", corr))

#find TSS and RSS
tss <- sum((y_test - mean(y_test))^2)
rss <- sum((predictions - y_test)^2)

#find R-Squared
rsq <- 1 - rss/tss
print(paste("R-squared:",rsq))
