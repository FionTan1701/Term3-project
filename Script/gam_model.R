library(tidyverse)
library(mgcv)
library(sf)
library(ggplot2)
library(raster)
library(viridis)

set.seed(123)

setwd("~/Term3-project")

nov_df <- read.csv("Data/processed_final.csv")
england <- st_read("Data/shapefiles/england/england_crop.shp")
england <- st_transform(england, crs = 27700)

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
                 data =nov_df, family =gaussian,method="REML")

gam_model2 <- gam(nov_3week ~ lockdown_step3 + lockdown_step4 + lockdown_planB + lockdown_lifting +
                    scale_school_density + scale_carehome_density + scale_mobility + scale_BAME + scale_imd_score + scale_prop_urb +
                    scale_rain_rolling_7day + scale_temp_rolling_7day + s(Easting, Northing)+ s(date_index) + ti(Easting, Northing, date_index, d=c(2,1)),
                  data =nov_df, family =gaussian,method="ML")

gam_model3 <- gam(nov_3week ~ lockdown_step3 + lockdown_step4 + lockdown_planB + lockdown_lifting +
                    scale_school_density + scale_carehome_density + scale_mobility + scale_BAME + scale_imd_score + scale_prop_urb +
                    scale_rain_rolling_7day + scale_temp_rolling_7day + s(Easting, Northing, k=150)+ s(date_index, k=20) + ti(Easting, Northing, date_index, d=c(2,1), k=20),
                  data =nov_df, family =gaussian,method="ML")
gam_model4 <- gam(nov_3week ~ lockdown_step3 + lockdown_step4 + lockdown_planB + lockdown_lifting +
                    scale_school_density + scale_carehome_density + scale_mobility + scale_BAME + scale_imd_score + scale_prop_urb +
                    scale_rain_rolling_7day + scale_temp_rolling_7day + s(Easting, Northing, k=150, bs ="tp")+ s(date_index, k=20, bs ="tp") + ti(Easting, Northing, date_index, d=c(2,1), k=20, bs= c("tp","tp","tp")),
                  data =nov_df, family =gaussian,method="REML")

gam_model5 <- gam(nov_3week ~ lockdown_step3 + lockdown_step4 + lockdown_planB + lockdown_lifting +
                    scale_school_density + scale_carehome_density + scale_mobility + scale_BAME + scale_imd_score + scale_prop_urb +
                    scale_rain_rolling_7day + scale_temp_rolling_7day + s(Easting, Northing, k=150, bs ="re")+ s(date_index, k=20, bs ="re") + ti(Easting, Northing, date_index, d=c(2,1), k=20, bs= c("re","re","re")),
                  data =nov_df, family =gaussian,method="REML")


#results interpretation
par(mfrow=c(1,1))
plot(gam_model4, select =1)
summary(gam_model4)
gam.check(gam_model4)
anova(gam_model3, gam_model4)


layout(matrix(1:3,nrow=1))

plot.gam(gam_model4, scheme =2)
plot(gam_model4,pages=1,scheme=1,unconditional=TRUE) 
AIC(gam_model1, gam_model3)


##Visual plots with ggplot
# Get coordinate range from your data
e_range <- range(nov_df$Easting, na.rm = TRUE)
n_range <- range(nov_df$Northing, na.rm = TRUE)

# Create a grid over the study region
grid <- expand.grid(
  Easting = seq(e_range[1], e_range[2], length.out = 150),
  Northing = seq(n_range[1], n_range[2], length.out = 150)
)

# Create prediction dataframe with all necessary covariates
pred_df <- grid %>%
  mutate(
    lockdown_step3 = 0,
    lockdown_step4 = 0,
    lockdown_planB = 0,
    lockdown_lifting = 0,
    scale_school_density = 0,
    scale_carehome_density = 0,
    scale_mobility = 0,
    scale_BAME = 0,
    scale_imd_score = 0,
    scale_prop_urb = 0,
    scale_rain_rolling_7day = 0,
    scale_temp_rolling_7day = 0,
    date_index = median(nov_df$date_index, na.rm = TRUE)  # hold time constant
  )

# Predict only the spatial smooth: s(Easting, Northing)
terms_pred <- predict(gam_model4, newdata = pred_df, type = "terms", se.fit = TRUE)

# Extract the spatial smooth estimate and standard error
pred_df$fit <- terms_pred$fit[, "s(Easting,Northing)"]
pred_df$se <- terms_pred$se.fit[, "s(Easting,Northing)"]

pred_sf <- st_as_sf(pred_df, coords = c("Easting", "Northing"), crs = 27700)

ggplot() +
  geom_sf(data = england, fill = "grey90", color = NA) +
  geom_sf(data = pred_sf, aes(fill = fit) )+
  scale_fill_viridis(option = "C", name = "Spatial smooth") +
  coord_sf() +
  labs(title = "Spatial smooth from GAM: s(Easting, Northing)",
       x = "Easting", y = "Northing") +
  theme_minimal()


#prediction
predict_nov = expand.grid(
  Easting= seq(min(nov_df$Easting), 
                max(nov_df$Easting),
                length=50),
  Northing = seq(min(nov_df$Northing),
                  max(nov_df$Northing),
                  length=50),
  Year = seq(1,45, by = 2)
)

dp <- as.matrix(predict_nov)

p <- st_as_sf(data.frame(x = dp[, 1], y = dp[, 2]),coords = c("x", "y"))
st_crs(p) <- st_crs(27700)
ind <- st_intersects(england, p)
predict_nov<- dp[ind[[1]], ]

predict_nov$model_fit = predict(gam_model4,predict_nov,type = "response")
##need all covariates used in fitted gam model
ggplot(aes(Easting, Northing, fill= model_fit),
       data=predict_nov)+
  geom_tile()+
  facet_wrap(~Year,nrow=2)+
  scale_fill_viridis("Nov concentration")+
  theme_bw(10)