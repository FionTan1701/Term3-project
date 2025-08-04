# exploring sites with more that 50% weeks missing data

library(readr)
setwd("~/Term3-project/")
site_rm <- c("UKENNE_NU_TP000039","UKENNE_NU_TP000048","UKENNE_YW_TP000054"
             ,"UKENNE_YW_TP000086","UKENNW_UU_TP000017","UKENNW_UU_TP000023"
             ,"UKENNW_UU_TP000115","UKENSW_SWS_TP000016")

new <- processed_final %>% 
  filter(!site_code %in% site_rm)
new_check<- new%>% group_by(site_code) %>%count(!is.na(Log10_NoV_norm))
new_check <- new %>% filter(is.na(nov_3week))
length(unique(processed_final$site_code))


write.csv(new, "Data/new_processed_final(rm_site).csv", row.names = FALSE)

nov_df <- read_csv("Data/final_data/processed_final.csv")