library(gstat)
library(sf)


setwd("~/Term3-project")
nov_df <- read.csv("Data/final_df3.csv")
processed_df <- read.csv("Data/processed_final.csv")

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
nov_df<- as.data.frame(nov_df)

processed_df <- scale_covariates(processed_df, covariates_to_scale)
processed_df<- as.data.frame(processed_df)

## create s_index (numerical index for site code)-------------------------------

nov_df <- nov_df %>%
  arrange(site_code) %>%
  mutate(site_code= as.factor(site_code)) %>%
  mutate(s_index=as.numeric(site_code)) %>%
  mutate(site_code= as.character(site_code))

## create f_index (numerical index for week date)-------------------------------

nov <- nov_df
nov <- nov %>%
  mutate(one_week_date = as.Date(one_week_date, format = "%d/%m/%Y")) %>%  # Adjust format as needed
  arrange(one_week_date) %>%
  mutate(f_index = as.numeric(one_week_date))



## convert df to sf-------------------------------------------------------------

nov <- st_as_sf(nov_df, coords= c("Easting", "Northing"), crs= 27700)
nov <- st_transform(nov,  crs = "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +units=km +no_defs")

nov_clean <- nov[!is.na(nov$Log10_NoV_norm), ]

processed_df <- st_as_sf(processed_df, coords= c("Easting", "Northing"), crs= 27700)
processed_df <- st_transform(processed_df,  crs = "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +units=km +no_defs")

processed_df_clean <- processed_df[!is.na(processed_df$nov_3week), ]

# Fit variogram
v <- variogram(Log10_NoV_norm ~ 1, data = nov_clean)
v_processed <- variogram(nov_3week ~ 1, data = processed_df_clean)

# Bubble plot
nov_sp <- as(nov_clean, "Spatial")
nov_processed_sp <- as(processed_df_clean, "Spatial")

bubble(nov_sp, "Log10_NoV_norm", col=c("skyblue","skyblue"))
bubble(nov_processed_sp, "nov_3week", col=c("skyblue","skyblue"), maxsize = 1)

# Plot 
plot(v, main = "original_df")
plot(v_processed, main ="processed_df")

model.a<- fit.variogram(v, vgm(c("Sph", "Exp")))

model.1 <- fit.variogram(v_processed, vgm("Sph"))
model.2<- fit.variogram(v_processed, vgm(c("Sph", "Exp")))
model.3 <- fit.variogram(v_processed, vgm("Exp"))

model.2#sperical model is chosen as best fit

#fit model
modela.final <- fit.variogram(v, vgm(psill = 0.25, "Sph", range = 16, nugget = 0.35))
plot(v, model = modela.final, main ="orginal data")

model.final <- fit.variogram(v_processed, vgm(psill = 0.23, "Sph", range = 21, nugget = 0.297))
plot(v_processed, model = model.final, main ="processed data")
