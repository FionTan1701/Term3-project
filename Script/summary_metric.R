library(tidyverse)

set.seed(123)
setwd("~/Term3-project")

metric <- read.csv("~/Term3-project/outputs/ML_model/cv/ML_xgb_model3_metrics_full.csv")

metric$RMSE <- sqrt(metric$MSE)
mse <- mean((val$nov_3week - val$predicted)^2, na.rm = TRUE)
rmse <- sqrt(mse)
mae <- mean(abs(val$nov_3week - val$predicted), na.rm = TRUE)
mape <- mean(abs((val$nov_3week - val$predicted) / val$nov_3week), na.rm = TRUE) * 100
bias <- mean(val$predicted - val$nov_3week, na.rm = TRUE)
pbias <- (bias / mean(val$nov_3week, na.rm = TRUE)) * 100
corr <- as.numeric(cor(val$nov_3week, val$predicted, use="complete.obs", method="spearman"))      



summary_metrics <- metric %>%
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
    Mean_CORR = mean(CORR, na.rm = TRUE)
  )
