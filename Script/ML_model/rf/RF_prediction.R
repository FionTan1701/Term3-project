library(tidyverse)
library(ranger)
library(caret)
library(sf)
library(sp)
library(raster)

set.seed(123)
setwd("~/Term3-project")

## read data -------------------------------------------------------------------
nov_df <- read.csv("Data/final_data/processed_final.csv")
#pred_grid <- read.csv("Data/prediction_data/lsoa_grid_prediction.csv")
pred_grid <- read.csv("Data/Covariates/lsoa_covariates/lsoa_cov.csv")
## scale------------------------------------------------------------------------

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


# convert df into sf and units m to km
nov <- st_as_sf(nov_df, coords= c("Easting", "Northing"), crs= 27700)
nov <- st_transform(nov,  crs = "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +units=km +no_defs")

#grid <-st_as_sf(pred_grid, coords= c("Easting", "Northing"), crs= 27700)
#grid <- st_transform(grid,  crs = "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +units=km +no_defs")

# rename columns in pred_grid -------------------------------------------------

grid <- pred_grid %>%
  rename(lockdown_step3 = lockdown_step3,
      lockdown_step4 = lockdown_step4,
      lockdown_lifting = lockdown_lifting,
      lockdown_planB = lockdown_planB,
      scale_school_density = scale_school_den,
      scale_carehome_density = scale_carehome_den,
      scale_mobility = scale_mob_7day,
      scale_BAME = scale_bame,
      scale_imd_score= scale_imd,
      scale_prop_urb= scale_prop_urb,
      scale_rain_rolling_7day = scale_rain_7day_avg,
      scale_temp_rolling_7day = scale_temp_7day_avg,
      date_index = week
    )

grid <- grid %>% 
  dplyr::select(lockdown_step3, lockdown_step4, lockdown_planB, lockdown_lifting,
                scale_school_density, scale_carehome_density, scale_imd_score, scale_BAME,
                scale_mobility, scale_rain_rolling_7day, scale_temp_rolling_7day, scale_prop_urb,Easting, Northing, date_index)

# formula ---------------------------------------------------------------
formula <- as.formula("nov_3week ~ lockdown_step3 + lockdown_step4 + lockdown_planB + lockdown_lifting +
  scale_school_density + scale_carehome_density + scale_imd_score + scale_BAME +
  scale_mobility + scale_rain_rolling_7day + scale_temp_rolling_7day + scale_prop_urb +
  Easting + Northing + date_index")

# train model----------------------------------------------------------------

model  <- ranger(formula,
                    data=nov_df, 
                    num.trees = 750, 
                    importance = "permutation",
                    mtry = 3, 
                    min.node.size=1,
                    write.forest = TRUE,
                    quantreg = TRUE)

# prediction on grid -------------------------------------------------

pred_mean <- predict(model, data = grid)$predictions
pred_quantiles <- predict(model, data = grid, type = "quantiles", quantiles = c(0.025, 0.5, 0.975))$predictions

grid$predicted <- pred_mean
grid$pred_025 <- pred_quantiles[, 1]
grid$pred_50 <- pred_quantiles[, 2]
grid$pred_975 <- pred_quantiles[, 3]

write.csv(grid, "outputs/prediction/rf_lsoa_prediction_new.csv", row.names = FALSE)

pdf("outputs/prediction/rf_lsoa_prediction_new.pdf", width = 10, height = 8)


subset_grid <- grid %>%
  filter( date_index ==1)

  ggplot(subset_grid, aes(x = Easting, y = Northing, color = predicted)) +
    geom_point(size = 0.2, alpha = 0.8) +
    scale_color_viridis_c(option = "plasma", direction = -1) +
    coord_equal() +
    theme_minimal() +
    labs(title = paste("Spatial Distribution of Predictions week",i),
        color = "Predicted Value")



dev.off()

