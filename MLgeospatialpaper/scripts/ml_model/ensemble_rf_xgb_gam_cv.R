# Load libraries
library(tidyverse)
library(data.table)
library(mgcv)
library(xgboost)
library(randomForest)
library(parallel)
library(ggplot2)
library(viridis)
library(scales)

options(mc.cores = parallel::detectCores())
dir.create("../../output/Figures/ensemble_cv/RF_XGB_GAM/", recursive = TRUE, showWarnings = FALSE)
dir.create("../../output/ensemble_cv/RF_XGB_GAM/", recursive = TRUE, showWarnings = FALSE)

# Load and prep data
data <- readr::read_csv("../../data/data/nov_df_reduced.csv") %>%
  dplyr::mutate(row_id = dplyr::row_number()) %>%
  dplyr::filter(!is.na(avg_Log10_NoV))


# ensure no missing coordinates
coords_master <- data %>%
  filter(!is.na(Easting) & !is.na(Northing)) %>%
  distinct(site_code, Easting, Northing)

data <- data %>%
  dplyr::select(-Easting, -Northing) %>%
  left_join(coords_master, by = "site_code")



covariates <- c("Easting", "Northing", "week",
                 "lockdown_step3", "lockdown_step4", "lockdown_planB", "scale_school_den", 
                 "scale_carehome_den", "scale_mob_7day","scale_bame","scale_imd", 
                 "scale_prop_agri" ,"scale_prop_urb", "temp_2", "temp_3", "temp_4",
                "temp_5", "rain_2","rain_3", "rain_4",  "rain_5" )

missing_vars <- setdiff(c("avg_Log10_NoV", covariates), names(data))
if (length(missing_vars) > 0) {
  stop("The following variables are missing from the data: ", paste(missing_vars, collapse = ", "))
}

data <- data %>% drop_na(all_of(covariates))


# Create folds
set.seed(123)
data <- data %>%
  dplyr::mutate(row_id = dplyr::row_number(),
                fold = sample(rep(1:10, length.out = n())))

# CV
run_fold <- function(k) {
  cat("Running Fold", k, "\n")
  train <- data %>% dplyr::filter(fold != k)
  test  <- data %>% dplyr::filter(fold == k)
  
  # GAM
  gam_fit <- mgcv::gam(
    avg_Log10_NoV ~ s(Easting,Northing) + s(week) +
     scale_school_den + scale_carehome_den + scale_mob_7day + scale_bame + scale_imd + scale_prop_agri + scale_prop_urb + 
    temp_2 + temp_3 + temp_4 + temp_5 + rain_2 + rain_3 + rain_4 + rain_5,
    data = train
  )
  gam_pred <- predict(gam_fit, newdata = test)
  
  # RF
  rf_formula <- as.formula(
    paste("avg_Log10_NoV ~", paste(covariates, collapse = " + "))
  )
  rf_fit <- randomForest(
    formula = rf_formula,
    data = train,
    ntree = 750,
    mtry = 5
  )
  rf_pred <- predict(rf_fit, newdata = test)
  
  # XGBoost
  x_train <- as.matrix(train[, covariates])
  y_train <- train$avg_Log10_NoV
  x_test  <- as.matrix(test[, covariates])
  
  # Optimised parameters via grid search
  xgb_fit <- xgboost(
    x = x_train,
    y = y_train,
    objective = "reg:squarederror",
    nrounds = 250,
    max_depth = 6,
    eta = 0.1,
    subsample = 0.8,
    colsample_bytree = 1,
    verbose = 0
  )
  
  xgb_pred <- predict(xgb_fit, newdata = x_test)
  
  # Meta-model (stack)
  meta_train <- train %>%
    dplyr::mutate(
      gam_pred = predict(gam_fit, newdata = train),
      rf_pred  = predict(rf_fit, newdata = train),
      xgb_pred = predict(xgb_fit, newdata = model.matrix(~ . - avg_Log10_NoV, data = train[, c("avg_Log10_NoV", covariates)]))
    )
  meta_fit <- lm(avg_Log10_NoV ~ gam_pred + rf_pred + xgb_pred, data = meta_train)
  
  # OOS predictions for held-out fold
  meta_test <- test %>%
    dplyr::select(row_id, fold, avg_Log10_NoV, week, dplyr::all_of(covariates)) %>%
    dplyr::mutate(
      gam_pred = gam_pred,
      rf_pred = rf_pred,
      xgb_pred = xgb_pred
    )
  meta_test$stacked_pred <- predict(meta_fit, newdata = meta_test)
  return(meta_test)
}

# Run folds in parallel ----
cv_results <- parallel::mclapply(1:10, run_fold, mc.cores = 10)

for (i in seq_along(cv_results)) {
  if (inherits(cv_results[[i]], "try-error")) {
    cat("\n❌ Fold", i, "ERROR MESSAGE:\n")
    cat(as.character(cv_results[[i]]), "\n")
  }
}

print(sapply(cv_results, class))


cv_results <- dplyr::bind_rows(cv_results)

# compute bias and other metrics 
cv_results <- cv_results %>%
  dplyr::mutate(Bias = avg_Log10_NoV - stacked_pred)

ensemble_metrics_by_fold <- cv_results %>%
  dplyr::group_by(fold) %>%
  dplyr::summarise(
    MAE = mean(abs(Bias)),
    RMSE = sqrt(mean(Bias^2)),
    Bias = mean(Bias),
    Correlation = cor(avg_Log10_NoV, stacked_pred)
  ) %>%
  dplyr::ungroup()

readr::write_csv(ensemble_metrics_by_fold, "../../output/ensemble_cv/RF_XGB_GAM/optimised_ensemble_metrics_by_fold.csv")

# Overall ensemble metrics 
overall_ensemble_metrics <- tibble::tibble(
  MAE = mean(abs(cv_results$Bias)),
  RMSE = sqrt(mean(cv_results$Bias^2)),
  Bias = mean(cv_results$Bias),
  Correlation = cor(cv_results$avg_Log10_NoV, cv_results$stacked_pred)
)

print("\nOverall ensemble metrics:")
print(overall_ensemble_metrics)
readr::write_csv(overall_ensemble_metrics, "../../output/ensemble_cv/RF_XGB_GAM/optimised_ensemble_metrics_overall.csv")

 # empirical CI
ts_summary <- cv_results %>%
  dplyr::group_by(week) %>%
  dplyr::summarise(
    actual_mean = mean(avg_Log10_NoV, na.rm=TRUE),
    pred_mean = mean(stacked_pred, na.rm=TRUE),
    ci_lower = quantile(stacked_pred, 0.025, na.rm=TRUE),
    ci_upper = quantile(stacked_pred, 0.975, na.rm=TRUE),
    n = dplyr::n()
  ) %>%
  dplyr::ungroup()

data_oos <- data %>%
  dplyr::mutate(week = as.numeric(week)) %>%
  dplyr::left_join(
    cv_results %>% dplyr::select(row_id, stacked_pred, fold, Bias, week),
    by = "row_id"
  )

# Clean up 
if ("week.y" %in% names(data_oos)) {
  data_oos$week <- data_oos$week.y
  data_oos$week.y <- NULL
}
if ("week.x" %in% names(data_oos)) {
  data_oos$week.x <- NULL
}

data_oos <- data_oos %>%
  dplyr::left_join(
    ts_summary %>% dplyr::select(week, ci_lower, ci_upper),
    by = "week"
  )

for (col in c("stacked_pred", "ci_lower", "ci_upper")) {
  if (col %in% names(data_oos)) {
    data_oos[[col]] <- as.numeric(data_oos[[col]])
    attributes(data_oos[[col]]) <- NULL
  }
}

# Save results to .csv
readr::write_csv(data_oos, "../../output/ensemble_cv/RF_XGB_GAM/data_oos_optimised.csv")

