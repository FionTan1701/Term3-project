library(tidyverse)
library(ranger)
library(caret)
library(sf)
library(sp)
library(raster)
library(blockCV)

set.seed(123)



nov_df <- readr::read_csv("../../data/data/nov_df_reduced.csv") %>%
  dplyr::mutate(row_id = dplyr::row_number()) %>%
  dplyr::filter(!is.na(avg_Log10_NoV))

# ensure no missing coordinates
coords_master <- nov_df %>%
  filter(!is.na(Easting) & !is.na(Northing)) %>%
  distinct(site_code, Easting, Northing)

nov_df <- nov_df %>%
  dplyr::select(-Easting, -Northing) %>%
  left_join(coords_master, by = "site_code")


#coverage probability function-------------------------------------------------
COV <- function(z, lower=NULL, upper=NULL, coverage=NULL) {
  if(!is.null(lower) && !is.null(upper)){
    z <- as.matrix(z)
    lower <- as.matrix(lower)
    upper <- as.matrix(upper)
    x <- z>=lower & z<=upper
    u <- x[!is.na(x)]
    round(sum(u)/length(u) * 100, 4)
  }else if(!is.null(coverage)){
    round(mean(coverage, na.rm = T),4)
  }
}

#scale function-------------------------------------------------
covariates_to_scale <-  c("school_den", "carehome_den", "imd", "bame", "mob_7day", "prop_agri", "prop_urb")
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
nov_df<- as.data.frame(nov_df)
nov <- nov_df
nov <- st_as_sf(nov_df, coords= c("Easting", "Northing"), crs= 27700)
nov <- st_transform(nov,  crs = "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +units=km +no_defs")

nov<- nov %>%
  arrange(site_code) %>%
  mutate(site_code= as.factor(site_code)) %>%
  mutate(s_index=as.numeric(site_code)) %>%
  mutate(site_code= as.character(site_code))

nov$Week_date<- as.Date(nov$Week_date)

nov<- nov %>%
  arrange(Week_date) %>%
  mutate(f_index= as.numeric(Week_date))

nov <- nov %>%
  mutate(avg_Log10_NoV = ifelse(GI_ND | GII_ND, NA, avg_Log10_NoV)) 

# create fold blocks------------------------------------------------------------
# unique sites

sites<- nov %>%
  dplyr::select(site_code) %>%
  unique()

# shapefile
england<- st_read("../../data/data/england_crop.shp")
england<- st_transform(england, crs= st_crs(nov))

raster<- raster(england)

folds<- cv_spatial(
  x= sites,
  r= raster,
  k= 10,
  seed= 12,
  plot = FALSE
)


fold_blocks<- folds$blocks

fold_blocks<- st_transform(fold_blocks, crs= st_crs(nov))

# assign folds to data  
nov <- nov %>%
  st_intersection(fold_blocks)



#cross-validation---------------------------------------------------
fit <- list()
metrics<- data.frame()
predictions <- data.frame()


run_fold <- function(k) {

  message("Running fold ", k)

  train <- data %>% filter(fold != k)
  test  <- data %>% filter(fold == k)

  rf_formula <- as.formula(avg_Log10_NoV ~  lockdown_step3 + lockdown_step4 + lockdown_planB +
    scale_school_den + scale_carehome_den + scale_mob_7day + scale_bame + scale_imd + scale_prop_agri + scale_prop_urb +
    temp_2 + temp_3 + temp_4 + temp_5 + rain_2 + rain_3 + rain_4 + rain_5 +Easting + Northing + week)

  # ---- Base RF models ----
  fit_rf <- function(seed) {
    set.seed(seed)
    randomForest(
      rf_formula,
      data = train,
      ntree = 750,
      mtry  = 5
    )
  }

  rf1 <- fit_rf(100 + k)
  rf2 <- fit_rf(200 + k)
  rf3 <- fit_rf(300 + k)

  # ---- Predictions ----
  train_preds <- tibble(
    rf1 = predict(rf1, train),
    rf2 = predict(rf2, train),
    rf3 = predict(rf3, train)
  )

  test_preds <- tibble(
    rf1 = predict(rf1, test),
    rf2 = predict(rf2, test),
    rf3 = predict(rf3, test)
  )

  # ---- Meta-model (stacking) ----
  meta_fit <- lm(
    train$avg_Log10_NoV ~ rf1 + rf2 + rf3,
    data = train_preds
  )

  test$stacked_pred <- predict(meta_fit, newdata = test_preds)

  metrics_fold <- data.frame(
    fold = k,
    MSE  = mean((test$avg_Log10_NoV - test$stacked_pred)^2, na.rm = TRUE),
    RMSE = sqrt(mean((test$avg_Log10_NoV - test$stacked_pred)^2, na.rm = TRUE)),
    MAE  = mean(abs(test$avg_Log10_NoV - test$stacked_pred), na.rm = TRUE),
    BIAS = mean(test$avg_Log10_NoV - test$stacked_pred, na.rm = TRUE),
    CORR = cor(test$avg_Log10_NoV, test$stacked_pred, method = "spearman")
  )

  metrics <- rbind(metrics, metrics_fold)
}

cv_results <- mclapply(1:10, run_fold, mc.cores = 10)


summary_metrics <- metrics %>%
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

write.csv(metrics, "../../output/ML_ensemble_rf_final_metrics_with_folds.csv")
write.csv(summary_metrics, "../../output/ML_ensemble_rf_final_summary_metrics.csv")