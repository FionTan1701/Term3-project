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

coords_km <- st_coordinates(nov)

nov$Easting  <- coords_km[, 1]
nov$Northing <- coords_km[, 2]


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

nov <- nov[!is.na(nov$avg_Log10_NoV), ]

fold_index <- lapply(1:10, function(k) {
  which(nov$folds != k)   # training indices
})
nov <- st_drop_geometry(nov)

predictors <- c("lockdown_step3", "lockdown_step4", "lockdown_planB",
                "scale_school_den", "scale_carehome_den", "scale_mob_7day",
                "scale_bame", "scale_imd", "scale_prop_agri", "scale_prop_urb",
                "temp_2", "temp_3", "temp_4", "temp_5",
                "rain_2", "rain_3", "rain_4", "rain_5", "Easting", "Northing","week")

setdiff(predictors, colnames(nov))

ctrl <- trainControl(
  method = "cv",
  number = 10,
  index = fold_index
)

param_grid <- expand.grid(
  mtry = c(3, 4,5, 6,7, 8),
  splitrule = "variance",
  min.node.size= c(1,2,3,4, 5,6,7,8,9,10)
)

trees_values <- c(500,1000)

results <- data.frame()

for (ntree in trees_values) {
  for (i in 1:nrow(param_grid)) {
    
    # Manual spatial CV loop over your folds
    rmse_fold <- numeric(10)
    
    for (k in 1:10) {
      train_idx <- fold_index[[k]]
      test_idx  <- setdiff(seq_len(nrow(nov)), train_idx)
      
      model <- ranger(
        dependent.variable.name = "avg_Log10_NoV",
        data = nov[train_idx, ],
        mtry = param_grid$mtry[i],
        min.node.size = param_grid$min.node.size[i],
        splitrule = param_grid$splitrule[i],
        num.trees = ntree
      )
      
      preds <- predict(model, nov[test_idx, ])$predictions
      rmse_fold[k] <- sqrt(mean((preds - nov$avg_Log10_NoV[test_idx])^2))
    }
    
    results <- rbind(results, 
                     cbind(param_grid[i,], num.trees = ntree, RMSE = mean(rmse_fold)))
  }
}

# Best combination
results[which.min(results$RMSE), ]
