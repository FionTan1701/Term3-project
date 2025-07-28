library(ggplot2)
library(ggforce)
library(dplyr)
library(readr)

setwd("~/Term3-project")

read_model_predictions <- function(file_path, model_name) {
  read_csv(file_path) %>%
    mutate(model = model_name) %>%
    select(site_code, s_index,date_index, one_week_date, nov_3week, predicted, q0.025, q0.975, model)
}

model_INLA <- read_model_predictions("Data/prediction_data/INLA_pred.csv", "INLA")
model_GAM <- read_model_predictions("Data/prediction_data/gam_pred.csv", "GAM")
model_BRMS <- read_model_predictions("Data/prediction_data/brms_pred.csv", "BRMS")
model_Lasso <- read_model_predictions("Data/prediction_data/lasso_pred.csv","Lasso")
model_xgb <- read_model_predictions("Data/prediction_data/xgb_pred.csv", "XGB")
model_RF <- read_model_predictions("Data/prediction_data/rf_pred.csv", "RF")

all_preds <- bind_rows(model_INLA,model_GAM,model_BRMS,model_Lasso,model_xgb,model_RF)

save

avg_pred <- all_preds %>% 
  group_by(model, date_index) %>%
  summarise(mean_nov = mean(predicted, na.rm = TRUE))


# Plot
ggplot(avg_pred, aes(x = date_index, y = mean_nov, color = model)) +
  geom_line() +
  labs(title = "Predicted NoV concentration by Model", y = "Mean predicted nov concentration" , x = "Date Index") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))


#plot with approximate credible intervals (taking the average of quantiles)
avg_preds <- all_preds %>%
  group_by(model, date_index) %>%
  summarise(
    mean = mean(predicted),
    lower = mean(q0.025),
    upper = mean(q0.975),
    .groups = "drop"
  )

ggplot(avg_preds, aes(x = date_index, y = mean, color = model, fill = model)) +
  geom_line(size = 0.5) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1, colour = NA) +
  labs(
    title = "Predicted Mean NoV Concentration Across Models",
    x = "Date Index",
    y = "Mean NoV concentration",
    color = "Model",
    fill = "Model"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

