library(glmnet)
library(tidyverse)
library(caret)



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
nov_df<- as.data.frame(nov_df)

#remove rows with NA in target variable
nov_df <- nov_df[!is.na(nov_df$nov_3week), ]

#train/test split
train_index <- sample(1:nrow(nov_df), size = 0.8 * nrow(nov_df))
train_data <- nov_df[train_index, ]
test_data  <- nov_df[-train_index, ]


X_train <- model.matrix(nov_3week ~  -1 +lockdown_step3 + lockdown_step4 + lockdown_planB + lockdown_lifting +
                              scale_school_density + scale_carehome_density + scale_imd_score +scale_BAME+
                              scale_mobility+ scale_rain_rolling_7day+ scale_temp_rolling_7day+ scale_prop_urb+
                              date_index * site_code, data = train_data)

y_train <- train_data$nov_3week

X_test <- model.matrix(nov_3week ~ -1 + lockdown_step3 + lockdown_step4 + lockdown_planB + lockdown_lifting +
                              scale_school_density + scale_carehome_density + scale_imd_score +scale_BAME+
                              scale_mobility+ scale_rain_rolling_7day+ scale_temp_rolling_7day+ scale_prop_urb+
                              date_index * site_code, data = test_data)                       

y_test <- test_data$nov_3week

# Fit the Lasso model

cv_model <- cv.glmnet(X_train, y_train, alpha = 1, family = "gaussian", nfolds = 10)

best_lambda <- cv_model$lambda.min
print(paste("Best lambda:", best_lambda))

#pdf("lasso_cv_plot.pdf")
#plot(cv_model) 
#dev.off()

# Fit the final model using the best lambda
start_time <- Sys.time()
lasso_model <- glmnet(X_train, y_train, alpha = 1, lambda = best_lambda)
end_time <- Sys.time()
print(paste("Run time:", end_time - start_time))
print(as.numeric(difftime(end_time, start_time, units = "secs")))

print(coef(lasso_model))   

#Predict on the test set
print("Predicting on the test set...")
y_predicted <- predict(lasso_model, s = best_lambda, newx = X_test)

mse <- mean((y_test - y_predicted)^2, na.rm = TRUE)
rmse <- sqrt(mse)
mae <- mean(abs(y_test - y_predicted), na.rm = TRUE)
mape <- mean(abs((y_test - y_predicted) /y_test), na.rm = TRUE) * 100
bias <- mean(y_predicted -y_test, na.rm = TRUE)
pbias <- (bias / mean(y_test, na.rm = TRUE)) * 100
corr <- cor(y_test, y_predicted, use="complete.obs", method="spearman")      

print(paste("Mean Squared Error (MSE):", mse))
print(paste("Root Mean Squared Error (RMSE):", rmse))
print(paste("Mean Absolute Error (MAE):", mae))
print(paste("Mean Absolute Percentage Error (MAPE):", mape))
print(paste("Bias:", bias))
print(paste("Percentage Bias (pBIAS):", pbias))
print(paste("Spearman Correlation (CORR):", corr))  


                       
#find SST and SSE
sst <- sum((y_test - mean(y_test))^2)
sse <- sum((y_predicted - y_test)^2)

#find R-Squared
rsq <- 1 - sse/sst
print(paste("R-squared:",rsq))