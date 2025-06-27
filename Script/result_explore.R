library(tidyverse)

setwd("~/Term3-project")
cv_metric <- read.csv("outputs/cv/cv_model7_processed_metrics_full.csv")

metric <- data.frame()

mse_value <- cv_metric$MSE
mean_mse <- mean(mse_value)
mean_mse

se_mse <- sd(mse_value) / sqrt(length(mse_value))
se_mse

cat("Mean MSE:", mean_mse, "\n")
cat("Standard Error of MSE:", se_mse, "\n")