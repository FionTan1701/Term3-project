library(xgboost)
library(caret)
library(dplyr)
library(purrr)


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

#nov_df$one_week_date <- as.factor(nov_df$one_week_date)
nov_df$site_code <- as.factor(nov_df$site_code)

nov_df <- nov_df %>% 
  dplyr::select(nov_3week, lockdown_step3, lockdown_step4, lockdown_planB, lockdown_lifting,
                scale_school_density, scale_carehome_density, scale_imd_score, scale_BAME,
                scale_mobility, scale_rain_rolling_7day, scale_temp_rolling_7day, scale_prop_urb, date_index, Easting, Northing)


nov_df<- as.data.frame(nov_df)

#remove rows with NA in target variable
nov_df <- nov_df[!is.na(nov_df$nov_3week), ]

#train/test split
train_index <- sample(1:nrow(nov_df), size = 0.8 * nrow(nov_df))
train_data <- nov_df[train_index, ]
test_data  <- nov_df[-train_index, ]

y <- nov_df$nov_3week
nov_df <-model.matrix(nov_3week ~ . -1, data = nov_df)
xgb_nov <- xgb.DMatrix(data = nov_df, label = y)

start_time <- Sys.time()
final_model <- xgboost(
  data = xgb_nov,
  params = list(
    eta = 0.05,
    max_depth = 4,
    subsample = 0.7,
    colsample_bytree = 0.7,
    objective = "reg:squarederror",
    eval_metric = "rmse"
  ),
  nrounds = 300,
  verbose = 0
)
end_time <- Sys.time()
print(paste("Run Time:", end_time - start_time))

y_train <- train_data$nov_3week
X_train <- model.matrix(nov_3week ~ . -1, data = train_data)

y_test <- test_data$nov_3week
X_test <- model.matrix(nov_3week ~ . -1, data = test_data)

#define final training and testing sets
xgb_train <- xgb.DMatrix(data = X_train, label = y_train)
xgb_test <- xgb.DMatrix(data = X_test, label = y_test)

#defining a watchlist
watchlist <- list(train=xgb_train, test=xgb_test)

#fit XGBoost model and display training and testing data at each iteartion
fit <- xgb.train(data = xgb_train, max.depth = 3, watchlist=watchlist, nrounds = 200)

print(fit)


# hyperparameter grid
grid <- expand.grid(
  eta = c(0.01, 0.1, 0.3),
  max_depth = c(4, 6, 8),
  subsample = c(0.7, 0.8, 0.9),
  colsample_bytree = c(0.7, 0.8, 0.9)
)

# Apply xgb.cv to each row of the grid
results <- pmap(grid, function(eta, max_depth, subsample, colsample_bytree) {
  cv <- xgb.cv(
    params = list(
      eta = eta,
      max_depth = max_depth,
      subsample = subsample,
      colsample_bytree = colsample_bytree,
      objective = "reg:squarederror",
      eval_metric = "rmse"
    ),
    data = xgb_train,
    nrounds = 100,
    nfold = 5,
    early_stopping_rounds = 10,
    verbose = 0
  )
  list(cv = cv, score = min(cv$evaluation_log$test_rmse_mean))
})

# Extract RMSE scores
scores <- sapply(results, function(res) res$score)

# Find the index of the lowest RMSE
best_index <- which.min(scores)

# Get best hyperparameters
best_params <- grid[best_index, ]

# View best result
print(paste("best parameters:",best_params))
print(scores[best_index])


# Fit the XGBoost model with specified parameters
final_model <- xgboost(
  data = xgb_train,
  params = list(
    eta = best_params$eta,
    max_depth = best_params$max_depth,
    subsample = best_params$subsample,
    colsample_bytree = best_params$colsample_bytree,
    objective = "reg:squarederror",
    eval_metric = "rmse"
  ),
  nrounds = 100,
  verbose = 0
)

print(summary(final_model))

#predict on test data
predictions <- predict(final_model, newdata = xgb_test)

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
