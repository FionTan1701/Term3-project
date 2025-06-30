library(ggplot2)
library(ggforce)

setwd("~/Term3-project")
predictions <- read.csv("/rds/general/user/ft824/home/Term3-project/outputs/prediction/m7processed_prediction.csv", check.names = TRUE)


# Convert date
predictions$one_week_date <- as.Date(predictions$one_week_date, format = "%d/%m/%Y")

# Plot
pdf("outputs/prediction/timeseries_by_site.pdf", width = 14, height = 10)

ggplot(predictions, aes(x = one_week_date)) +
  geom_ribbon(aes(ymin = q0.025, ymax = q0.975), fill = "grey80") +
  geom_line(aes(y = mean), color = "blue") +
  geom_point(aes(y = nov_3week), color = "red") +
  facet_wrap(~ site_code, scales = "free_y") +
  labs(x = "Date", y = "Value") +
  theme_bw()

dev.off()