library(tidyverse)
library(mgcv)

set.seed(123)

setwd("~/Term3-project")

nov_df <- read.csv("Data/processed_final.csv")

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
                        scale_rain_rolling_7day + scale_temp_rolling_7day + site_code*one_week_date")

nov_df$site_date <- interaction(nov_df$site_code, nov_df$one_week_date, drop = TRUE)
formula2 <- as.formula("nov_3week ~ lockdown_step3 + lockdown_step4 + lockdown_planB + lockdown_lifting +
                        scale_school_density + scale_carehome_density + scale_mobility + scale_BAME + scale_imd_score + scale_prop_urb +
                        scale_rain_rolling_7day + scale_temp_rolling_7day + site_date")

reg_model <- glm(formula, data = nov_df, family = gaussian(link = "identity"))
reg_model2<-glm(formula2, data = nov_df, family = gaussian(link = "identity"))


gam_linear <- gam(formula2, data =nov_df, family =gaussian)
gam_smooth <- gam(nov_3week ~ lockdown_step3 + lockdown_step4 + lockdown_planB + lockdown_lifting +
                   s(scale_school_density) + s(scale_carehome_density) + s(scale_mobility) + s(scale_BAME) +
                   s(scale_imd_score) + s(scale_prop_urb) + s(scale_rain_rolling_7day) + 
                   s(scale_temp_rolling_7day) + site_date,
                 data = nov_df, family = gaussian)

print(fam_linear)
print(gam_smooth)

print(anova(gam_linear, gam_smooth, test = "F"))