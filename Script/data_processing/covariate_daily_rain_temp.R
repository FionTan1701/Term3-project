library(lubridate)
library(dplyr)
library(tidyr)
library(zoo) #rollmean

setwd("~/Term3-project")
daily_rain_df <- read.csv("Data/Covariates/stw_daily_rain.csv")
daily_temp_df <- read.csv("Data/Covariates/stw_daily_temp.csv")

final_df1<- read.csv("Data/final_df1.csv")
one_week_date <- list(unique(as.Date(final_df1$one_week_date)))

daily_rain_df$date <- format(as.Date(daily_rain_df$date , format = "%d/%m/%Y"), "%Y-%m-%d")
daily_temp_df$date <-  format(as.Date(daily_temp_df$date , format = "%d/%m/%Y"), "%Y-%m-%d")


#7 day rolling average for every monday 
temp_df <- daily_temp_df %>%
  arrange(site_code, date) %>%
  group_by(site_code) %>%
  mutate(
    temp_rolling_7day = rollmean(dailytemp, 7, fill = NA, align = "center"),
    weekday = wday(date, label = TRUE) 
  ) %>%
  ungroup() %>%
  filter(weekday == "Mon")

rain_df <- daily_rain_df %>%
  arrange(site_code, date) %>%
  group_by(site_code) %>%
  mutate(
    rain_rolling_7day = rollmean(dailyrain, 7, fill = NA, align = "center"),
    weekday = wday(date, label = TRUE) 
  ) %>%
  ungroup() %>%
  filter(weekday == "Mon")

write.csv(temp_df, "Data/cleaned_covariates/stw_dailytemp.csv", row.names = FALSE)
write.csv(rain_df, "Data/cleaned_covariates/stw_dailyrain.csv", row.names = FALSE)
