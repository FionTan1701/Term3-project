library(GGally)
library(dplyr)

setwd("~/Term3-project")

df <- read.csv("Data/processed_final.csv")

#reverse one-hot encoded lockdowns stage
# Step 1: Identify lockdown stage columns
stage_cols <- grep("^lockdown_", names(df), value = TRUE)

# Step 2: Extract stage numbers from column names
stage_labels <- gsub("lockdown_", "", stage_cols)

# Step 3: Apply to get the original category
df$lockdown_stage <- apply(df[stage_cols], 1, function(row) {
  stage_labels[which.max(row)]
})

# Step 4: Optionally convert to factor
df$lockdown_stage <- factor(df$lockdown_stage, levels = stage_labels)

# Step 5: Drop the one-hot columns (optional)
df <- df[ , !names(df) %in% stage_cols]

write.csv(df,"Data/final_processed2.csv", row.names = FALSE)

cor_vars <- df %>%
  select(mobility, rain_rolling_7day,temp_rolling_7day, school_density,
         carehome_density, prop_urb, imd_score, BAME, lockdown_stage)

colnames(cor_vars)<- c("Population mobility","Rain", "Temperature (K)", 
                       "School Density", "Carehome Density", "Proportion Urban", 
                       "IMD", "BAME", "Lockdown Stage")

pdf("Figures/ggpair_corrplot3.pdf", width =15 , height = 12)

corr_plot <- ggpairs(cor_vars, lower = "blank")
corr_plot_bw <- corr_plot 
print(corr_plot_bw)

dev.off()
