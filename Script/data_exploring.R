library(tidyr)
library(dplyr)
library(lubridate)
library(tibble)
library(mvtsplot) #visualisation for missing data

setwd("~/Term3-project/Script")

nov_raw_data <- read_csv("../Data/Norovirus/nov_stw_raw.csv")
summary(nov_raw_data)


#extract date only
nov_raw_data$date_only <- ifelse(
  grepl(":", nov_raw_data$sample_collection_date_time),  # rows with time
  format(as.Date(nov_raw_data$sample_collection_date_time, format = "%d/%m/%Y %H:%M"), "%Y-%m-%d"),
  format(as.Date(nov_raw_data$sample_collection_date_time, format = "%d/%m/%Y"), "%Y-%m-%d")
)

nov_wide <- nov_raw_data %>%
  select(site_code, date_only, Log10_NoV_norm) %>%
  pivot_wider(names_from = date_only, values_from = Log10_NoV_norm) %>%
  arrange(site_code)

#set first column as row names
nov_wide <- nov_wide %>%
  column_to_rownames(var = colnames(nov_wide)[1])

nov_wide <- t(nov_wide)


# Ensure xtime is Date class and increasing
xtime <- as.Date(rownames(nov_wide))
sorted_index <- order(xtime)
nov_wide_sorted <- nov_wide[sorted_index, ]

# Then plot
mvtsplot(
  x = nov_wide_sorted,
  xtime = xtime[sorted_index],
  norm = "global",
  main = "Norovirus Trends by Site",
  gcol = 1
)


