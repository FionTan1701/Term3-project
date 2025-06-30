library(ggplot2)
library(ggforce)

setwd("~/Term3-project")
predictions <- read_csv("outputs/prediction/m7processed_prediction.csv")


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
