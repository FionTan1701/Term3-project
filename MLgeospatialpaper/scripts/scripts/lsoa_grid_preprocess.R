################################################################
## LSOA Predictions
################################################################

library(fmesher)
library(INLA)
library(Matrix)
library(Metrics)
library(sf)
library(scoringRules)
library(tibble)
library(dplyr)
library(sf)
library(sp)
library(fmesher)
library(lubridate)
library(terra)
library(readxl)
library(tidyr)
library(raster)
library(stringr)
library(inlabru)
library(zoo)
library(data.table)
library(fastDummies)
options(saveWorkspace = FALSE)

################################################################

setwd("~/Term3-project/MLgeospatialpaper")

##LSOA
lsoa <- st_read("data/LSOA/LSOA2021_boundaries/LSOA2021_boundaries.shp")
lsoa <- lsoa[grepl("E0", lsoa$LSOA21CD), ]

cov <- read.csv("data/data/lsoa_cov.csv")

rain <- read.csv("data/data/daily_rain_lsoa.csv")

start<- as.Date("2021-05-24")

cov <- cov %>%
  left_join(rain %>%
              mutate(date= as.Date(rain$date)) %>%
              mutate(week = floor(as.numeric(date - start) / 7) + 1) %>%
              dplyr::select(LSOA21CD, date,week, rain_7day_avg, rain_cat),
            by= c("LSOA21CD", "week")) %>%
            dummy_cols(
                select_columns = "rain_cat",
                remove_selected_columns = TRUE)


write.csv(cov, "data/lsoa_prediction_grid.csv", row.names = FALSE)