library(tidyverse)
library(mgcv)

set.seed(123)

setwd("~/Term3-project")

nov_df <- read.csv("Data/processed_final.csv")

nov_df <- nov_df %>%
  mutate(one_week_date = as.Date(one_week_date, format = "%d/%m/%Y")) %>%  # Adjust format as needed
  arrange(one_week_date) %>%
  mutate(f_index = as.numeric(one_week_date))


nov_df$one_week_date <- as.numeric(as.Date(nov_df$one_week_date, format="%d/%m/%Y"))

#scale function
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



#regression formula
formula <- as.formula("nov_3week ~ lockdown_step3 + lockdown_step4 + lockdown_planB + lockdown_lifting +
                        scale_school_density + scale_carehome_density + scale_mobility + scale_BAME + scale_imd_score + scale_prop_urb +
                        scale_rain_rolling_7day + scale_temp_rolling_7day")

nov_df$site_date <- interaction(nov_df$site_code, nov_df$one_week_date, drop = TRUE)
formula2 <- as.formula("nov_3week ~ lockdown_step3 + lockdown_step4 + lockdown_planB + lockdown_lifting +
                        scale_school_density + scale_carehome_density + scale_mobility + scale_BAME + scale_imd_score + scale_prop_urb +
                        scale_rain_rolling_7day + scale_temp_rolling_7day + site_date")

reg_model <- glm(formula, data = nov_df, family = gaussian(link = "identity"))
reg_model2<-glm(formula2, data = nov_df, family = gaussian(link = "identity"))


gam_model <- gam(nov_3week ~ lockdown_step3 + lockdown_step4 + lockdown_planB + lockdown_lifting +
                        scale_school_density + scale_carehome_density + scale_mobility + scale_BAME + scale_imd_score + scale_prop_urb +
                        scale_rain_rolling_7day + scale_temp_rolling_7day + s(Easting, Northing)+ s(one_week_date) + ti(Easting, Northing, one_week_date, d=c(2,1)),
                        data =nov_df, family =gaussian)

gam_model1 <- gam(nov_3week ~ lockdown_step3 + lockdown_step4 + lockdown_planB + lockdown_lifting +
                   scale_school_density + scale_carehome_density + scale_mobility + scale_BAME + scale_imd_score + scale_prop_urb +
                   scale_rain_rolling_7day + scale_temp_rolling_7day + s(Easting, Northing)+ s(date_index) + ti(Easting, Northing, date_index, d=c(2,1)),
                 data =nov_df, family =gaussian)


par(mfrow=c(2,2))
plot(gam_model, select =1)
summary(gam_model1)
print(gam.check(gam_model))
anova(gam_model, gam_model1)
