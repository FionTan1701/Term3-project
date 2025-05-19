library(tidyr)
library(dplyr)
library(lubridate)
library(tibble)
library(astsa)
library(mvtsplot) #visualisation for missing data

setwd("~/Term3-project/Script")

nov_stw_raw <- read_csv("../Data/Norovirus/nov_stw_raw.csv")
summary(nov_stw_raw)

#extract date only
nov_stw_raw$date_only <- ifelse(
  grepl(":", nov_stw_raw$sample_collection_date_time),  # rows with time
  format(as.Date(nov_stw_raw$sample_collection_date_time, format = "%d/%m/%Y %H:%M"), "%Y-%m-%d"),
  format(as.Date(nov_stw_raw$sample_collection_date_time, format = "%d/%m/%Y"), "%Y-%m-%d")
)

#one-week bin date
nov_stw_raw <- nov_stw_raw %>%
  mutate(one_week_date = floor_date(as.Date(date_only), unit = "week", week_start = 1))

#convert date stlye to dd/mm/yyyy
nov_stw_raw$one_week_date <- format(nov_stw_raw$one_week_date , "%d/%m/%Y")  


###plot for every sample date collection
nov_wide <- nov_stw_raw %>%
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

mvtsplot(
  x = nov_wide_sorted,
  xtime = xtime[sorted_index],
  norm = "global",
  main = "Norovirus Trends by Site",
  gcol = 1
)

###plot for one-week bin date
nov_wide_week <- nov_stw_raw %>%
  select(site_code, one_week_date, Log10_NoV_norm) %>%
  group_by(site_code, one_week_date) %>%
  summarise(Log10_NoV_norm = mean(Log10_NoV_norm, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = one_week_date, values_from = Log10_NoV_norm) %>%
  arrange(site_code)

nov_wide_week <- nov_wide_week %>%
  column_to_rownames(var = colnames(nov_wide_week)[1])

nov_wide_week <- t(nov_wide_week)
nov_wide_week <- nov_wide_week[order(as.Date(rownames(nov_wide_week))), ]

#plot with practical 10
mvtsplot(nov_wide_week, group = NULL, xtime = NULL, norm = c("global"),
         levels = 3, smooth.df = NULL, margin = TRUE, sort = NULL,
         main = "", palette = "PRGn", rowstat = "median", xlim,
         bottom.ylim = NULL, right.xlim = NULL, gcol = 3)

x11() 
site1 <- ts(nov_stw_raw[nov_stw_raw$site_code == "UKENSO_SW_TP000026",18])
tsplot(site1, col = 2, lwd=1.5, main="Site1")

outcome <- nov_stw_raw[,c("Log10_NoV_norm","site_code")]
outcome <- unstack(outcome)
max_len <- max(sapply(outcome, length))

# Step 3: Pad each vector with NA to match max length
outcome <- lapply(outcome, function(x) {
  length(x) <- max_len
  return(x)
})
outcome <- as.data.frame(outcome)
outcome <- as.matrix(outcome)
outcome <- ts(outcome)
dim(outcome)

tsplot(outcome[,1:3], col=2:4,lwd=1.5)
tsplot(outcome, spaghetti=TRUE, col=rgb(0, 0, 1, 0.2),
       ylab="log10_nov_norm", main="Time series for all sites")
lines(rowMeans(outcome, na.rm=TRUE), col=2, lwd=1.5)
