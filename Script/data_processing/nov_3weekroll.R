### #3 week rolling average of nov virus concentration

library(tidyr)
library(dplyr)
library(lubridate)
library(tibble)
library(astsa)
library(zoo)
library(slider)


setwd("~/Term3-project")

nov_stw_raw <- read.csv("Data/Norovirus/nov_stw_raw.csv")
summary(nov_stw_raw)

#print rows where the column of interest is na
nov_stw_raw %>%
  filter(is.na(Log10_NoV_norm)) %>%
  print()

#remove rows where Log10_NoV_norm is NA
nov_stw_raw <- nov_stw_raw %>%
  filter(!is.na(Log10_NoV_norm))

## Extract weekly date

#extract date only
nov_stw_raw$date_only <- ifelse(
  grepl(":", nov_stw_raw$sample_collection_date_time),  # rows with time
  format(as.Date(nov_stw_raw$sample_collection_date_time, format = "%d/%m/%Y %H:%M"), "%Y-%m-%d"),
  format(as.Date(nov_stw_raw$sample_collection_date_time, format = "%d/%m/%Y"), "%Y-%m-%d")
)

#one-week bin date
nov_stw_raw <- nov_stw_raw %>%
  mutate(one_week_date = floor_date(as.Date(date_only), unit = "week", week_start = 1))


#sites at the same date with more than one outcome are taken as average
nov_df_week <- nov_stw_raw %>%
  select(site_code, one_week_date, Log10_NoV_norm) %>%
  group_by(site_code, one_week_date) %>%
  summarise(Log10_NoV_norm = mean(Log10_NoV_norm, na.rm = TRUE), .groups = "drop") %>%
  arrange(site_code, one_week_date)

nov <- nov_df_week %>%
  arrange(site_code, one_week_date) %>%
  group_by(site_code) %>%
  mutate(nov_3week = slide_dbl(Log10_NoV_norm,
                                .f = ~ mean(.x, na.rm = TRUE),
                                .before = 2,  # Include 2 previous
                                .complete = FALSE))  # Allow partial windows

##create full gird of all combinations of week and date
all_sites <- unique(nov_df_week$site_code)
all_weeks <- seq(min(nov_df_week$one_week_date),
                 max(nov_df_week$one_week_date),
                 by = "1 week")  # Adjust if needed

# Create full combination
full_grid <- expand.grid(site_code = all_sites, one_week_date = all_weeks)

nov_df_full <- full_grid %>%
  left_join(nov, by = c("site_code", "one_week_date"))

##merge with site info to get sit coords
site_info <- read.csv("Data/Norovirus/site_info.csv")

site_info <- site_info %>% 
  select(site_code, Easting, Northing)

nov_df_full <- nov_df_full %>%
  left_join(site_info, by = "site_code")

write.csv(nov_df_full,"Data/Norovirus/nov_df_full.csv", row.names = FALSE)


nov1 <- read.csv("Data/final_df3.csv")
nov1$one_week_date <- as.Date(nov1$one_week_date, format ="%d/%m/%Y")

nov1 <- nov1 %>%
  arrange(site_code, one_week_date) %>%
  group_by(site_code) %>%
  mutate(nov_3week = slide_dbl(Log10_NoV_norm,
                               .f = ~ mean(.x, na.rm = TRUE),
                               .before = 2,  # Include 2 previous
                               .complete = FALSE))  # Allow partial windows
nov2 <- nov1 %>%
  select(site_code, one_week_date, Log10_NoV_norm, nov_3week)
write.csv(nov2,"Data/Norovirus/nov_3weekroll.csv", row.names = FALSE)
