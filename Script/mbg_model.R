# Load packages
library(data.table)
library(ggplot2)
library(sp)
library(sf)
library(terra)
library(mbg)
library(fmesher)
library(Matrix)
library(MatrixModels)
library(Metrics)
library(blockCV)
library(dplyr)

options(saveWorkspace = FALSE)

set.seed(123)

setwd("~/Term3-project")

nov_df <- read.csv("Data/processed_final.csv")


## scale function
covariates_to_scale <-  c("school_density", "carehome_density", "imd_score",
                          "BAME", "mobility", "rain_rolling_7day",
                          "temp_rolling_7day", "prop_urb")
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

# create s_index (numerical index for site code)

nov_df <- nov_df %>%
  arrange(site_code) %>%
  mutate(site_code= as.factor(site_code)) %>%
  mutate(s_index=as.numeric(site_code)) %>%
  mutate(site_code= as.character(site_code))

nov <- nov_df %>% 
  dplyr::filter(date_index == 2) %>%
  dplyr::select(s_index, nov_3week, Easting, Northing)
  
  
# convert df to sf
#convert units from m to km
nov<- st_as_sf(nov_df, coords= c("Easting", "Northing"), crs= 27700)
nov<- st_transform(nov,  crs = "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +units=km +no_defs")

# create fold blocks
# unique sites

sites<- nov %>%
  dplyr::select(site_code, geometry) %>%
  distinct()

# shapefile
england<- st_read("Data/shapefiles/england/england_crop.shp")
england<- st_transform(england, crs= st_crs(nov))

raster<- raster(england)


##create fold blocks
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

nov<- nov %>%
  st_intersection(fold_blocks)

covariate_names<- c("lockdown_step3", "lockdown_step4", "lockdown_planB", "lockdown_lifting",
                "scale_school_density",   
                "scale_carehome_density", 
                "scale_imd_score" ,       
                "scale_BAME",             
                "scale_mobility",         
                "scale_rain_rolling_7day",
                "scale_temp_rolling_7day",
                 "scale_prop_urb" )

coordinates <- vect(nov)

#create id raster
template_raster<- terra::rast(england, res = 1000)  # 1km resolution
id_raster <- mbg::build_id_raster(england, template_raster)

#rasterrise covariates
covariates_raster <- list()

for (covariate in covariate_names) {
  cov_rast <- terra::rasterize(coordinates, template_raster, field = covariate)
  covariates_raster[[covariate]] <- cov_rast
}



##Run ML models using input covariates
cross_validation_settings <- list(method = 'repeatedcv', number = 5, repeats = 5)
submodel_settings <- list(
  enet = NULL,
  gbm = list(verbose = FALSE),
  treebag = NULL
)

submodels <- mbg::run_regression_submodels(
  input_data = nov,
  id_raster = id_raster,
  covariates = covariates_raster,
  cv_settings = cross_validation_settings,
  model_settings = submodel_settings,
  prediction_range = c(0, 1)
)
