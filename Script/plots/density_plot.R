library(tidyr)
library(ggplot2)

setwd("~/Term3-project")

nov_df <- read.csv("Data/final_data/processed_final.csv")

str(nov_df$nov_3week)
length(unique(nov_df$nov_3week))

ggplot(data = nov_df, aes(x = nov_3week)) +
  #geom_histogram(aes(y = ..density..),fill = NA, color = "black") +
  geom_density(fill = "lightblue",color = "lightblue", size = 0.8, alpha = 0.5) +
  theme_bw() +
  labs( x = "Norovirus Concentration",
        y = "",
        title = "Density Plot of Norovirus Concetration")+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )
  

# Test for normality
ks.test(na.omit(nov_df$nov_3week), "pnorm", mean = mean(nov_df$nov_3week, na.rm = TRUE), sd = sd(nov_df$nov_3week, na.rm = TRUE))
install.packages("nortest")
library(nortest)
ad.test(nov_df$nov_3week)

qqnorm(nov_df$nov_3week)
qqline(nov_df$nov_3week, col = "red")

