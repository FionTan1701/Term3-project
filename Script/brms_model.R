library(brms)
library(tidyverse)

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
nov_df<- as.data.frame(nov_df)

nov_df$location_id <- interaction(nov_df$Easting, nov_df$Northing)


model2 <- brm(
  nov_3week ~ s(Easting, Northing) + s(date_index) + t2(Easting, Northing, date_index),
  data = nov_df,
  family = gaussian(),
  chains = 4, cores = 4, iter = 4000,
  control = list(adapt_delta = 0.95)
)


print(summary(model2))
