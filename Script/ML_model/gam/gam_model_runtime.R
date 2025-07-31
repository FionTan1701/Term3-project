library(tidyverse)
library(mgcv)
library(sf)
library(ggplot2)
library(raster)
library(viridis)

set.seed(123)

setwd("~/Term3-project")

nov_df <- read.csv("Data/processed_final.csv")
england <- st_read("Data/shapefiles/england/england_crop.shp")
england <- st_transform(england, crs = 27700)

nov_df <- nov_df %>%
  mutate(one_week_date = as.Date(one_week_date, format = "%d/%m/%Y")) %>%  # Adjust format as needed
  arrange(one_week_date) %>%
  mutate(f_index = as.numeric(one_week_date))


nov_df$one_week_date <- as.numeric(as.Date(nov_df$one_week_date, format="%d/%m/%Y"))

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
nov_df<- as.data.frame(nov_df)


start_time <- Sys.time()
gam_model4 <- gam(nov_3week ~ lockdown_step3 + lockdown_step4 + lockdown_planB + lockdown_lifting +
                    scale_school_density + scale_carehome_density + scale_mobility + scale_BAME + scale_imd_score + scale_prop_urb +
                    scale_rain_rolling_7day + scale_temp_rolling_7day + s(Easting, Northing, k=150, bs ="tp")+ s(date_index, k=20, bs ="tp") + ti(Easting, Northing, date_index, d=c(2,1), k=20, bs= c("tp","tp")),
                  data =nov_df, family =gaussian,method="REML")
end_time <- Sys.time()
print(paste("Runtime1:",as.numeric(difftime(end_time, start_time, units = "secs"))))
print(paste("Runtime2:",end_time - start_time))
