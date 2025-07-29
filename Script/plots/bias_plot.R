library(dplyr)
library(readr)
library(ggplot2)

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

bias_df <- all_preds %>%
  mutate(bias = predicted - nov_3week)

bias_df$model <- factor(bias_df$model, levels = c("INLA", "BRMS", "GAM", "Lasso", "XGB", "RF"))


ggplot(bias_df, aes(x = model, y = bias, fill = model)) +
  geom_boxplot() +
  scale_fill_manual(values = c(
    "INLA" = "slategray1",
    "GAM" = "thistle2",
    "BRMS" = "#7570b3",
    "Lasso" = "lemonchiffon1",
    "XGB" = "lightsalmon",
    "RF" = "plum1"
  )) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red")+
  theme_bw() +
  labs(title = "Comparing Bias across Models",
       x = "Model",
       y = "Bias (Predicted - Observed)") +
  theme(legend.position = "none")+
  theme(plot.title = element_text(hjust = 0.5))
