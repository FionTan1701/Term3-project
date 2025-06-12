library(tidyr)
library(dplyr)
library(lubridate)
library(tibble)
library(astsa)
library(mvtsplot) #visualisation for missing data
library(ggplot2)
library(viridis)

# read all covariates csv-------------------------------------------------------
folder_path <- "Data/cleaned_covariates"

# List all CSV files
csv_files <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)

# Create a name for each data frame from the file name (without extension)
df_names <- tools::file_path_sans_ext(basename(csv_files))

# Read each file and assign to a variable in the global environment
for (i in seq_along(csv_files)) {
  assign(df_names[i], read.csv(csv_files[i]))
}


##bind covariates df with nov_df------------------------------------------------
nov_df <- read.csv("Data/Norovirus/nov_df_full.csv")

nov_df <- nov_df %>% select(site_code, one_week_date, Log10_NoV_norm,Easting, Northing)

# rename all date columns to same colname
lockdown_stage <- lockdown_stage %>% 
  rename(one_week_date = week_date)
stw_mobility_weekly <- stw_mobility_weekly %>% 
  rename(one_week_date = date)
stw_dailyrain <- stw_dailyrain  %>% 
  rename(one_week_date = date)
stw_dailytemp <- stw_dailytemp %>% 
  rename(one_week_date = date)

#merge all covariates to nov concentration
nov_df <- nov_df %>%
  left_join(stw_carehome_density %>%
              select(site_code, weighted_carehome_density), by = "site_code") %>%
  left_join(stw_school_density %>%
              select(site_code, weighted_school_density), by = "site_code")  %>%
  left_join(stw_ethnicity %>%
              select(site_code, weighted_prop_non_white), by = "site_code")  %>%
  left_join(stw_prop_urb %>%
              select(site_code, prop_urb), by = "site_code") %>%
  left_join(lockdown_stage %>%
              select(one_week_date, lockdown_lifting,
                     lockdown_step3,lockdown_step4,lockdown_planB), by = "one_week_date") %>%
  left_join(stw_mobility_weekly, by = c("site_code", "one_week_date")) %>%
  left_join(stw_imd, by = "site_code") %>%
  left_join(stw_dailyrain %>% 
              select(site_code, one_week_date, rain_rolling_7day), by = c("site_code", "one_week_date")) %>%
  left_join(stw_dailytemp %>% 
              select(site_code, one_week_date, temp_rolling_7day), by = c("site_code", "one_week_date"))

write.csv(final_df2, "~/Term3-project/Data/final_df3.csv", row.names = FALSE) 

# Create an ordered index
final_df2$one_week_date <- as.Date(final_df2$one_week_date, format = "%d/%m/%Y")

final_df2 <- final_df2 %>%
  arrange(one_week_date) %>%                # Sort by date
  mutate(date_index = dense_rank(one_week_date))  # Assign increasing index
