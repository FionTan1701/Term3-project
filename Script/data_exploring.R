library(tidyr)
library(dplyr)
library(lubridate)
library(tibble)
library(astsa)
library(mvtsplot) #visualisation for missing data
library(ggplot2)

setwd("~/Term3-project")

nov_stw_raw <- read.csv("Data/Norovirus/nov_stw_raw.csv")
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

write.csv(nov_wide_week, "nov_wide_week.csv")

#plot with practical 10
mvtsplot(nov_wide_week, group = NULL, xtime = NULL, norm = c("global"),
         levels = 3, smooth.df = NULL, margin = TRUE, sort = NULL,
         main = "", palette = "PRGn", rowstat = "median", xlim,
         bottom.ylim = NULL, right.xlim = NULL, gcol = 3)


#time series plot for one site--------------------------------------------------
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


#ggplot
#long format df
#sites at the same date with more than one outcome are taken as average
nov_df_week <- nov_stw_raw %>%
  select(site_code, one_week_date, Log10_NoV_norm) %>%
  group_by(site_code, one_week_date) %>%
  summarise(Log10_NoV_norm = mean(Log10_NoV_norm, na.rm = TRUE), .groups = "drop") %>%
  arrange(site_code, one_week_date)

write.csv(nov_df_week, "Data/Norovirus/nov_long_week.csv")

ggplot(nov_df_week, aes(x = one_week_date, y = site_code, fill = Log10_NoV_norm)) +
  geom_tile() +
  scale_fill_distiller(palette = "RdBu", direction = 1) +
  labs(
    title = "Multivariate Time Series Plot",
    x = "weekly date",
    y = "STW site",
    fill = "NoV concentration"
  ) +
  theme(
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    panel.grid.major = element_line(color = "white"),
    panel.grid.minor = element_blank()
  ) +
  scale_x_date(
    date_labels = "%Y-%m-%d",   # Or "%b %d" for "Jul 01", etc.
    date_breaks = "2 weeks"     # Adjust break interval
  ) +
  theme(
    axis.line = element_line(color = "black"),
    axis.text.y = element_text(size = 4),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

summary(nov_df_week)

#histogram of log10 norovirus concentration
nov_df_week %>%
  ggplot +
  geom_histogram(aes(Log10_NoV_norm), col = "blue") +
  labs(title = "Histogram of NoV Concentration")


#count how many dates are under each site
count <- nov_df_week %>%
  group_by(site_code) %>%
  summarise(n_dates = n()) %>%
  arrange(desc(n_dates))  # Optional: to sort by count

# site with less than 11 dates with data
#UKENNE_NU_TP000048,UKENNE_NU_TP000039,UKENNW_UU_TP000017,UKENNW_UU_TP000023,
#UKENNW_UU_TP000115,UKENNE_YW_TP000054,UKENNE_YW_TP000086

#create full grid with all combination of site x week date
# Get all unique sites and weeks
all_sites <- unique(nov_df_week$site_code)
all_weeks <- seq(min(nov_df_week$one_week_date),
                 max(nov_df_week$one_week_date),
                 by = "1 week")  # Adjust if needed

# Create full combination
full_grid <- expand.grid(site_code = all_sites, one_week_date = all_weeks)

# Join with your original data
nov_df_full <- full_grid %>%
  left_join(nov_df_week, by = c("site_code", "one_week_date"))

write.csv(nov_df_full,"Data/Norovirus/nov_df_full.csv")

count_full <-nov_df_full %>%
  group_by(site_code) %>%
  summarise(n_dates = n()) %>%
  arrange(desc(n_dates))  # Optional: sort by most records

#merge to get coordinates of site-----------------------------------------------

site_info <- read.csv("Data/Norovirus/site_info.csv")

site_info <- site_info %>% 
  select(site_code, Easting, Northing)

nov_df <- nov_df_full %>%
  left_join(site_info, by = "site_code")

write.csv(nov_df,"Data/Norovirus/nov_df_full.csv")

#######read covariates csv
folder_path <- "Data/cleaned_covariates"

# List all CSV files
csv_files <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)

# Create a name for each data frame from the file name (without extension)
df_names <- tools::file_path_sans_ext(basename(csv_files))

# Read each file and assign to a variable in the global environment
for (i in seq_along(csv_files)) {
  assign(df_names[i], read.csv(csv_files[i]))
}
########

##bind covariates df with nov_df

nov_df <- nov_df %>%
  left_join(stw_carehome_density %>%
              select(site_code, weighted_carehome_density), by = "site_code") %>%
  left_join(stw_school_density %>%
              select(site_code, weighted_school_density), by = "site_code")  %>%
  left_join(stw_ethnicity %>%
              select(site_code, weighted_prop_non_white), by = "site_code")  %>%
  left_join(stw_prop_urb %>%
              select(site_code, prop_urb), by = "site_code") %>%
  left_join(nov_df_lockdown_stage %>%
              select(one_week_date, lockdown_lifting,
                     lockdown_step3,lockdown_step4,lockdown_planA,lockdown_planB,
                     lockdown_NA), by = "one_week_date") %>%
  left_join(stw_mobility_weekly, by = "site_code", "one_week_date")

