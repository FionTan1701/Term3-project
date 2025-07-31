library(brms)
library(tidyverse)
library(bayesplot)


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

nov_df$location_id <- interaction(nov_df$Easting, nov_df$Northing)

priors <- c(
  prior(normal(0, 100), class = "b"),
  prior(normal(0, 100), class = "Intercept"),
  prior(exponential(0.3), class = "sds"),
  prior(exponential(0.3), class = "sigma")
)

formula <- as.formula("nov_3week ~  lockdown_step3 + lockdown_step4 + lockdown_planB + lockdown_lifting +
  scale_school_density + scale_carehome_density + scale_imd_score + scale_BAME +
  scale_mobility + scale_rain_rolling_7day + scale_temp_rolling_7day + scale_prop_urb +
  ( 1 | site_code ) + (1|date_index) + (1 | site_code :date_index)")

start_time <- Sys.time()
model1 <- brm(nov_3week ~ lockdown_step3 + lockdown_step4 + lockdown_planB + lockdown_lifting + 
                      scale_school_density + scale_carehome_density + scale_mobility + scale_BAME + scale_imd_score + scale_prop_urb +
                      scale_rain_rolling_7day + scale_temp_rolling_7day + s(Easting, Northing, k=130, bs ="tp")+ s(date_index, k=20, bs ="tp") + t2(Easting, Northing, date_index, d=c(2,1), k=20, bs= c("tp","tp")),
                      data = nov_df,
                      family = gaussian(),
                      chains = 4, cores = 4, iter = 4000,
                      control = list(adapt_delta = 0.95))
end_time <- Sys.time()
print(as.numeric(difftime(end_time, start_time, units = "secs")))



#print(summary(model1))
#print(summary(model1)$rhat)
#print(summary(model1)$neff_ratio)

#print(paste("check prior distributions:"))
#print(get_prior(model))

#pdf("brms_model_summary.pdf")
# Page 1: Check smooths
#print(check_smooths(model1))
# Page 2: Posterior predictive checks
#print(pp_check(model1))
# Page 3: Conditional smooths
#plot(conditional_smooths(model1)) 

#mcmc_rhat(rhat(model1))
#mcmc_neff(neff_ratio(model1))
#dev.off()

#pdf("brms_traceplot.pdf")
#plot(model)
#dev.off()

waic1 <- waic(model1)
#waic2 <- waic(model2)
print(waic1)
#print(waic2)  

loo1 <- loo(model1)
#loo2 <- loo(model2)

print("LOO for model1:")
print(loo1)
#print("LOO for model2:")
#print(loo2)


#print("LOO comparison:")
#comp<-loo_compare(loo1, loo2)
#print(comp)

#saveRDS(model1, file = "outputs/ML_model/brms_model1.rds")
