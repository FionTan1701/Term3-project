library(corrplot)
library(tidyverse)
library(ggplot2)

setwd("~/Term3-project")
nov_df<- read.csv("Data/final_df2.csv")

nov_df <- nov_df %>% 
  rename(school_den = weighted_school_density,
         carehome_den = weighted_carehome_density,
         bame = weighted_prop_non_white,
         mobility = weighted_mobility,
         imd = weighted_imd_score,
         dailytemp = temp_rolling_7day,
         dailyrain = rain_rolling_7day)
  
# select variables for correlation
cor_vars<- nov_df %>%
  select(mobility, dailytemp,dailyrain, school_den, carehome_den, prop_urb, imd, bame,
         lockdown_lifting, lockdown_step3, lockdown_step4, lockdown_planB)
cor_vars$mobility<- as.numeric(cor_vars$mobility)
#=================================================== 
### Correlation matrix
#===================================================
# Format column names
colnames(cor_vars)<- c("Population mobility", "Temperature (K)","Rain", "School density", "Carehome density", "Proportion urban", 
                       "IMD", "BAME","Lifting of measures", "Lockdown step 3", "Lockdown step 4", "Lockdown plan B")

M= cor(cor_vars, method="pearson", use= "complete.obs")
M
# plot
x11()
corrplot<- corrplot(M, method = 'ellipse', type= 'upper')


################################################################################
#scatter plot to explore relationship between nov concentration with each covariates

scatter_vars<- nov_df %>%
  dplyr::select(Log10_NoV_norm, mobility, dailytemp,dailyrain, school_den, carehome_den, prop_urb, imd, bame)

# Pivot to long format
scatter_long <- scatter_vars %>%
  pivot_longer(-Log10_NoV_norm, names_to = "variable", values_to = "value")

scatter_long_clean <- scatter_long %>%
  filter(is.finite(value), is.finite(Log10_NoV_norm))

# Create scatter plots using facet_wrap
x11()
ggplot(scatter_long, aes(x = value, y = Log10_NoV_norm)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  facet_wrap(~ variable, scales = "free_x") +
  geom_smooth(method = "lm", color = "black", se = FALSE) +
  theme_minimal() +
  labs(x = "Covariate value", y = "Log10 NoV concentration", 
       title = "Nov concentration vs Covariates")
