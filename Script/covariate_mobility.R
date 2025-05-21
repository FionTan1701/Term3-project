library(lubridate)



mobility_weekly <- mobility_df %>%
  mutate(week = floor_date(date, "week")) %>%
  group_by(LSOA21CD, week) %>%
  summarise(mobility_avg = mean(mobility, na.rm = TRUE), .groups = "drop")
