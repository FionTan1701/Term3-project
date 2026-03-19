# Load libraries
library(tidyverse)
library(data.table)
library(parallel)
library(scales)
library(glmnet)

options(mc.cores = parallel::detectCores())

# Load and prepare data
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

# Assign spatial folds by site 
set.seed(123)
if (!"fold" %in% names(data)) {
  if ("site_code" %in% names(data)) {
    unique_sites <- unique(data$`site_code`)
    site_folds <- tibble(
      `site_code` = sample(unique_sites),
      fold = rep(1:10, length.out = length(unique_sites))
    )
    data <- dplyr::left_join(data, site_folds, by = "site_code")
    message("Assigned folds using Site code.")
  } else {
    data <- data %>%
      dplyr::mutate(fold = sample(rep(1:10, length.out = n())))
    message("Assigned folds randomly (no Site code found).")
  }
}

if (!"row_id" %in% names(data)) stop("row_id is missing from data!")

# CV and bootstrapping
run_fold_lasso_bootstrap <- function(k, n_boot = 1000) {
  tryCatch({
    cat("Running Fold", k, "\n")
    train <- data %>% dplyr::filter(fold != k)
    test  <- data %>% dplyr::filter(fold == k)
    if (nrow(test) == 0 || nrow(train) < 5) stop("Fold has insufficient train/test rows")
    
    x_train <- as.matrix(train[, covariates])
    y_train <- train$avg_Log10_NoV
    x_test  <- as.matrix(test[, covariates])
    y_test  <- test$avg_Log10_NoV
    
    # Fit LASSO 
    lasso_fit <- cv.glmnet(x_train, y_train, alpha = 1)
    LASSO_pred <- as.numeric(predict(lasso_fit, s = "lambda.min", newx = x_test))
    
    # Bootstrapping
    preds_boot <- replicate(n_boot, {
      boot_idx <- sample(seq_len(nrow(train)), replace = TRUE)
      x_boot <- x_train[boot_idx, , drop = FALSE]
      y_boot <- y_train[boot_idx]
      fit_boot <- cv.glmnet(x_boot, y_boot, alpha = 1)
      as.numeric(predict(fit_boot, s = "lambda.min", newx = x_test))
    })
    preds_boot <- t(preds_boot)
    q0.025 <- apply(preds_boot, 2, quantile, probs = 0.025)
    q0.975 <- apply(preds_boot, 2, quantile, probs = 0.975)
    coverage <- as.numeric(y_test >= q0.025 & y_test <= q0.975)
    
    results <- tibble(
      row_id = test$row_id,
      fold = k,
      truth = y_test,
      LASSO_pred = LASSO_pred,
      q0.025 = q0.025,
      q0.975 = q0.975,
      coverage = coverage
    )
    
    eval_metrics <- function(obs, pred) {
      tibble(
        MAE = mean(abs(obs - pred),na.rm = TRUE),
        RMSE = sqrt(mean((obs - pred)^2),na.rm = TRUE),
        Bias = mean(obs - pred,na.rm = TRUE),
        Correlation = cor(obs, pred,use = "complete.obs")
      )
    }
    
    fold_metrics <- eval_metrics(results$truth, results$LASSO_pred) %>%
      mutate(
        Model = "LASSO",
        Fold = k,
        Coverage = mean(coverage, na.rm = TRUE)
      )
    list(results = results, metrics = fold_metrics)
  }, error = function(e) {
    cat(sprintf("Fold %d failed: [%s] %s\n", k, class(e)[1], conditionMessage(e)))
    return(NULL)
  })
}

# Parralelisation of CV folds 
n_folds <- length(unique(data$fold))
all_folds <- parallel::mclapply(1:n_folds, run_fold_lasso_bootstrap, n_boot = 1000, mc.cores = min(10, n_folds))
good_folds <- Filter(Negate(is.null), all_folds)
if (length(good_folds) == 0) stop("All folds failed! Check variable names and data splits.")

all_results <- purrr::map(good_folds, "results")
all_metrics <- purrr::map(good_folds, "metrics")
final_results <- dplyr::bind_rows(all_results)
final_metrics <- dplyr::bind_rows(all_metrics)

# Calculating coverage
overall_coverage <- mean(final_results$coverage, na.rm=TRUE)
cat(sprintf("\nOverall 95%% empirical coverage (LASSO): %.2f%%\n", 100 * overall_coverage))

# Merge predictions and fold back to data via row_id 
data_oos <- data %>%
  dplyr::left_join(
    final_results %>%
      dplyr::select(row_id, fold, LASSO_pred, q0.025, q0.975, coverage),
    by = "row_id"
  )

# Debugging
if ("time.y" %in% names(data_oos)) {
  data_oos$time <- data_oos$time.y
  data_oos$time.y <- NULL
}
if ("time.x" %in% names(data_oos)) {
  data_oos$time.x <- NULL
}

# More Debugging
badcols <- which(!sapply(data_oos, function(x) is.atomic(x) && is.vector(x)))
if (length(badcols) > 0) {
  print("Non-vector columns found in data_oos:")
  print(names(data_oos)[badcols])
  for (col in names(data_oos)[badcols]) {
    if (is.matrix(data_oos[[col]]) && ncol(data_oos[[col]]) == 1) {
      data_oos[[col]] <- as.numeric(data_oos[[col]][,1])
    } else if (is.matrix(data_oos[[col]])) {
      warning(paste("Column", col, "is a multi-column matrix! Only first column will be used."))
      data_oos[[col]] <- as.numeric(data_oos[[col]][,1])
    }
    if (is.list(data_oos[[col]])) {
      data_oos[[col]] <- unlist(data_oos[[col]])
    }
    attributes(data_oos[[col]]) <- NULL
  }
}

# Print output
cat("\n=== Final Metrics Across Folds ===\n")
print(final_metrics)

cat("\n=== Sample of Final Predictions ===\n")
print(head(data_oos))

cat("\nSummary Statistics:\n")
summary_stats <- final_metrics %>%
  dplyr::group_by(Model) %>%
  dplyr::summarise(
    MAE = mean(MAE),
    RMSE = mean(RMSE),
    Bias = mean(Bias),
    Correlation = mean(Correlation),
    Coverage = mean(Coverage, na.rm = TRUE),
    .groups = "drop"
  )
print(summary_stats)

# Write output to .csv
readr::write_csv(data_oos, "../../output/lasso_final_predictions_with_folds.csv")
readr::write_csv(final_metrics, "../../output/lasso_final_metrics_by_fold.csv")
