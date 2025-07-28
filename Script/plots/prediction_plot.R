library(ggplot2)
library(ggforce)
library(dplyr)

setwd("~/Term3-project")
predictions <- read_csv("Data/prediction_data/inla_pred.csv")

# Convert date
predictions$one_week_date <- as.Date(predictions$one_week_date, format = "%d/%m/%Y")

# Plot
pdf("outputs/prediction/timeseries_by_site2.pdf", width = 14, height = 10)

n_pages <- ceiling(length(unique(predictions$site_code)) / 6)

for (i in 1:n_pages) {
  p <- ggplot(predictions, aes(x = one_week_date)) +
    geom_point(aes(y = mean), color = "blue", size = 2) +
    geom_line(aes(y = mean), color = "blue") +
    geom_ribbon(aes(ymin = q0.025, ymax = q0.975), fill = "grey80", alpha = 0.3) +
    geom_point(aes(y = nov_3week), color = "red") +
    geom_line(aes(y = nov_3week), color = "red", size = 1, linetype = "dashed") +
    facet_wrap_paginate(~ site_code, scales = "free_y", ncol = 2, nrow = 3, page = i) +
    labs(x = "Date", y = "Value") +
    scale_color_manual(values = c("mean" = "blue", "nov_3week" = "red"), labels = c("Predicted", "Observed")) +
    scale_y_continuous(limits = c(2, 7.5)) +
    theme(
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.box = "horizontal")+
    theme_bw()
  
  print(p)
}

dev.off()

pred<- data.frame()
pred_df<-data.frame()


#make site index
predictions <- predictions %>% 
  arrange(site_code) %>%
  mutate(site_code = as.factor(site_code)) %>%
  mutate( s_index = as.numeric(site_code)) %>%
  mutate( site_code = as.character(site_code))
  
date_number<- length(unique(predictions$date_index))
#plot time series average over all sites
for (i in 1:date_number) {
  pred_date<- predictions[predictions$date_index == i, ]
  avg_nov <- mean(pred_date$mean,na.rm =TRUE )
  pred <- data.frame(
    date_index = i,
    avg_nov = avg_nov
  )
  pred_df <- rbind(pred_df,pred)
}
plot(pred_df$date_index,pred_df$avg_nov,
     type ="l",
     xlab = "date index",
     ylab = "average nov concentration",
     main = "time series plot ",
     col = "tomato3",
     lwd = 2)

#alternative
pred_df <- predictions %>%
  group_by(date_index) %>%
  summarise(avg_nov = mean(mean, na.rm = TRUE))


#plot predicted NoV for each site
site_number<- length(unique(predictions$s_index))
for (i in 1:site_number) {
  pred_date<- predictions[predictions$site_number == i, ]
  ggplot(
    
  )
}
site<- predictions[predictions$s_index == 1, ]

ggplot(site, aes(x = date_index, y = predicted)) +
  geom_line(colour = "tomato3")+
  geom_ribbon(aes(ymin = q0.025, ymax = q0.975), alpha =0.2, fill = "pink") +
  labs( x = "date index",
        y = "NoV concentration",
        title = "Nov prediction over time (Site 1)") +
          theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 
  


        