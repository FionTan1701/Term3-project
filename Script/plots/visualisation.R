library(tidyverse)
library(INLA)
library(ggplot2)
library(RColorBrewer)

setwd("~/Term3-project")

data<- read.csv("Data/processed_final.csv")
model1<- readRDS("outputs/prediction/model1.rds")
model1_metric <- read.csv("")

data_pred <- data
summary(model1)

output.field <- inla.spde2.result(inla = model1,
                                  name = "spatial.field",
                                  spde = spde,
                                  do.transf = TRUE)
out.range = exp(output.field$summary.log.range.nominal)
out.var = exp(output.field$summary.log.variance.nominal)

par(mgp=c(2.2,0.45,0), tcl=-0.4, mar=c(3.3,4,2,2))
par(mfrow=c(2,2))

# AR1 parameter    
#plot(model1$marginals.hyperpar$`GroupRho for spatial.field`,type = 'l',xlab = expression(rho),ylab = "density", xlim=c(0.6,1))

# range
plot(output.field$marginals.variance.nominal[[1]],type = 'l',xlab = expression(sigma^2),ylab = "density")

# variance
plot(output.field$marginals.range.nominal[[1]],type = 'l',xlab = "spatial range",ylab = "density")

####################################

p <- ggplot(site_data, aes(x = week)) +
  geom_line(aes_string(y = observed_column, color = shQuote(observed_label)), size = 1, linetype = "dashed") +
  geom_point(aes_string(y = observed_column, color = shQuote(observed_label)), size = 2) +
  geom_line(aes_string(y = mean_col, color = shQuote("Predicted")), size = 1) +
  geom_point(aes_string(y = mean_col, color = shQuote("Predicted")), size = 2) +
  geom_ribbon(aes_string(ymin = q025_col, ymax = q975_col), fill = "red", alpha = 0.3) +
  labs(
    title = if (wales) "Region: Wales" else if (region) paste("Region:", region_name) else "All Regions",
    subtitle = paste("Site:", i),
    x = "Week",
    y = "Log10 NoV"
  ) +
  scale_color_manual(values = c("Observed" = "blue", "Predicted" = "red")) +
  scale_y_continuous(limits = c(2, 7.5)) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.box = "horizontal"
  )

print(p)
################################

# shapefile
england<- st_read("Data/shapefiles/england/england_crop.shp")
england<- st_transform(england, crs= st_crs(nov))

raster<- raster(england)

folds<- cv_spatial(
  x= sites,
  r= raster,
  k= 10,
  seed= 12,
  plot = FALSE
)

fold_blocks<- folds$blocks

fold_blocks<- st_transform(fold_blocks, crs= st_crs(nov))

# assign folds to data

nov<- nov %>%
  st_intersection(fold_blocks)

#number of unique sites in each fold
nov %>%
  group_by(folds) %>%
  summarise(unique_sites = n_distinct(s_index)) %>%
  arrange(folds)

nov %>%
  group_by(folds) %>%
  summarise(sites = list(unique(site_code))) %>%
  arrange(folds)

fold_sites <- nov %>%
  group_by(folds) %>%
  summarise(sites = list(unique(s_index)))  # or s_index, depending on what you want

# View all
for (i in seq_len(nrow(fold_sites))) {
  cat("\nFold", fold_sites$folds[i], "has", length(fold_sites$sites[[i]]), "sites:\n")
  print(fold_sites$sites[[i]])
}

#get sites in fold 6
fold6 <- nov %>%
  filter(folds == "6") 

fold6 <- st_as_sf(fold6, coords = c("Easting", "Northing"), crs = 27700)
nov <- st_as_sf(nov, coords = c("Easting", "Northing"), crs = 27700)

#plot all sites section by folds
ggplot() +
  geom_sf(data = england, fill = "white", color = "black") +
  geom_sf(data = nov, aes(color = as.factor(folds)), size = 2, alpha = 0.8) +
  #scale_color_viridis_d(name = "Fold") +
  scale_color_brewer(palette = "Paired", name = "Fold") +
  theme_minimal() +
  labs(title = "Site Locations by Fold",
       subtitle = "Each color represents a different spatial CV fold",
       x = "Easting", y = "Northing")

#maps with fold 6 sites
ggplot() +
  geom_sf(data = england, fill = "white", color = "black") +
  geom_sf(data = fold6, color = "red", size = 2) +
  theme_minimal() +
  labs(
    title = "Site Locations in Fold 6",
    x = "Easting",
    y = "Northing"
  )

# get regions of sites
fold6 <- fold6 %>%
  mutate(region = case_when(
    grepl("^UKENAN", site_code) ~ "East of England",
    grepl("^UKENMI", site_code) ~ "Midlands",
    grepl("^UKENNE", site_code) ~ "North East",
    grepl("^UKENNW", site_code) ~ "North West",
    grepl("^UKENSO", site_code) ~ "South East",
    grepl("^UKENSW", site_code) ~ "South West",
    grepl("^UKENTH", site_code) ~ "London"))

fold6 %>%
  group_by(region) %>%
  summarise(n_sites = n_distinct(site_code)) %>%
  arrange(desc(n_sites))

#maps with sits in fold6 section by region
ggplot() +
  geom_sf(data = england, fill = "white", color = "grey") +
  geom_sf(data = fold6, aes(color = as.factor(region)), size = 2, alpha = 0.8) +
  #scale_color_viridis_d(name = "Fold") +
  scale_color_brewer(palette = "Set1", name = "Region") +
  theme_minimal() +
  labs(title = "Site Locations by Region",
       subtitle = "Each color represents a different region",
       x = "Easting", y = "Northing")


# density plot of covariates in each fold
#fold 6 in red
nov_col <- c("Log10_NoV_norm","lockdown_lifting","lockdown_step3","lockdown_step4","lockdown_planB" ,"school_density", "carehome_density", "imd_score", "BAME", "mobility", "rain_rolling_7day","temp_rolling_7day", "prop_urb")
covariates_to_scale <-  c("school_density", "carehome_density", "imd_score", "BAME", "mobility", "rain_rolling_7day","temp_rolling_7day", "prop_urb")

pdf("density_plots_by_fold.pdf", width = 8, height = 6)  # Adjust size as needed
p<-NULL

for (i in 1:length(nov_col)) {
  p[[i]] <- nov %>%
    ggplot(aes(x = !!sym(nov_col[i]), group = as.factor(folds))) +
    geom_density(aes(color = as.factor(folds)), fill = NA, size = 0.8) +
    scale_color_manual(
      values = setNames(
        rep("lightgrey", length(unique(nov$folds))),
        unique(nov$folds)
      ) %>% replace(names(.) == "6", "#800000")
    ) +
    theme_minimal() +
    theme(legend.position = "none") +
    labs(
      title = paste(nov_col[i]),
      x = nov_col[i],
      y = "Density",
      color = "Fold"
    )
  
}

multiplot(p, layout=c(4, 4))

dev.off()

#function to scale covariates
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

############

nov_df<- scale_covariates(nov, covariates_to_scale)

#function to plot multiple plots in one frame
multiplot <- function(plotlist=NULL, layout=NULL) {
  
  #Get the number of plots
  numPlots=length(plotlist)
  
  if (is.null(layout)) {
    
    layout=NULL
    layout[1]=layout[2]=ceiling(sqrt(numPlots))
    nrow=ncol=ceiling(sqrt(numPlots))
    
  } 
  return(grid.arrange(arrangeGrob(grobs=plotlist, nrow=layout[1], ncol=layout[2])))
}
#######

plots_list=NULL

scaled_covariates <-c("scale_school_density","scale_carehome_density" , "scale_imd_score","scale_BAME","scale_mobility","scale_rain_rolling_7day" ,"scale_temp_rolling_7day","scale_prop_urb")

#plot histogram of covariates
for (k in 1:length(scaled_covariates)) {
  plots_list[[k]]=ggplot(nov_df) + geom_histogram(aes(x=!!sym(scaled_covariates[k])),
                                                           fill="#800045", color="black", bins=30) + ggtitle(scaled_covariates[k])
}

multiplot(plots_list, layout=c(3, 3))
 