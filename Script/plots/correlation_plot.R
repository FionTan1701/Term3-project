library(corrplot)
library(tidyverse)
library(ggplot2)
library(metan)
library(broom)

setwd("~/Term3-project")
nov_df<- read.csv("Data/final_df3.csv")

nov_df <- nov_df %>% 
  mutate(lockdown_phase = case_when(
    lockdown_step3 == 1 ~ "step3",
    lockdown_step4 == 1 ~ "step4",
    lockdown_planB == 1 ~ "planB",
    lockdown_lifting == 1 ~ "lifting"))


nov_df <- nov_df %>% 
  rename(school_den = school_density,
         carehome_den = carehome_density,
         bame = BAME,
         mobility = mobility,
         imd = imd_score,
         dailytemp = temp_rolling_7day,
         dailyrain = rain_rolling_7day)
  
# select variables for correlation
cor_vars<- nov_df %>%
  select(mobility, dailytemp,dailyrain, school_den, carehome_den, prop_urb, imd, bame,
         lockdown_phase)
cor_vars$mobility<- as.numeric(cor_vars$mobility)

cor_vars$lockdown_stage_numeric <- as.numeric(factor(cor_vars$lockdown_phase))

cor_vars <- cor_vars %>%
  select(mobility, dailytemp,dailyrain, school_den, carehome_den, prop_urb, imd, bame,
           lockdown_stage_numeric)
#=================================================== 
### Correlation matrix
#===================================================
# Format column names
colnames(cor_vars)<- c("Population mobility", "Temperature (K)","Rain", "School density", "Carehome density", "Proportion urban", 
                       "IMD", "BAME","Lockdown Stage")

M= cor(cor_vars, method="pearson", use= "complete.obs")
M
# plot
x11()
corrplot <- corrplot(M, method = 'ellipse', type= 'upper')

corrplot2 <-corr_coef(cor_vars)
corrplot2
plot(corrplot2)
################################################################################
#scatter plot to explore relationship between nov concentration with each covariates

scatter_vars<- nov_df %>%
  dplyr::select(Log10_NoV_norm, mobility, dailytemp,dailyrain, school_den, carehome_den, prop_urb, imd, bame) %>%
  mutate(across(-Log10_NoV_norm, scale)) 

# Pivot to long format
scatter_long <- scatter_vars %>%
  pivot_longer(-Log10_NoV_norm, names_to = "variable", values_to = "value")

scatter_long_clean <- scatter_long %>%
  filter(is.finite(value), is.finite(Log10_NoV_norm))

# Calculate p-values using linear models
p_values <- scatter_long %>%
  group_by(variable) %>%
  do(tidy(lm(Log10_NoV_norm ~ value, data = .))) %>%
  filter(term == "value") %>%
  select(variable, p.value)

p_values <- p_values %>%
  mutate(p_label = paste0("p = ", formatC(p.value, format = "e", digits = 2)))

scatter_long_annotated <- left_join(scatter_long, p_values, by = "variable")

# Create scatter plots using facet_wrap
x11()
ggplot(scatter_long_annotated, aes(x = value, y = Log10_NoV_norm)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  facet_wrap(~ variable, scales = "free_x") +
  geom_smooth(method = "lm", color = "black", se = FALSE) +
  geom_text(data = distinct(scatter_long_annotated, variable, p_label),
            aes(label = p_label), x = -Inf, y = Inf, hjust = -0.1, vjust = 1.2,
            inherit.aes = FALSE, size = 3.5) +
  theme_minimal() +
  labs(x = "Covariate value", y = "Log10 NoV concentration", 
       title = "Nov concentration vs Covariates")
