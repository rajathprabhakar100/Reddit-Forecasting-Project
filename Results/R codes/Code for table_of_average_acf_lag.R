library(tidyverse)
library(data.table)
combined <- read_csv("Results/CSV Files/combined_data.csv")

combined_negative <- combined %>% filter(Lag <= 0) %>%
  group_by(Variable) %>% 
  summarize(Average_ACF_negative = mean(Max_ACF),
            Average_Lag_negative = mean(Lag))

combined_positive <- combined %>% filter(Lag > 0) %>% 
  group_by(Variable) %>% 
  summarize(Average_ACF_positive = mean(Max_ACF),
            Average_Lag_positive = mean(Lag))

table1 <- left_join(combined_positive, combined_negative, by = "Variable") %>% 
  mutate(Type = c("Summary", "Social", "Tone", "Drives", "Cognition", "Cognition",
                  "Cognition", "Cognitive", "Social", "Social", "Physical",
                  "Cognition", "Cognition", "Affect", "Affect", "Affect", "Affect", "Affect", "Affect",
                  "Social", "States", "Social", "Social", "Physical", "Linguistic", "Physical", "Cognition", 
                  "Linguistic", "States", "Social", "Physical", "Social", "States", "Affect", "Linguistic", "Linguistic",
                  "Social", "Motives", "Motives", "Linguistic", "Social", "Social", "Cognition", "Linguistic", "Affect",
                  "Affect", "States", "Linguistic", "Physical", "Linguistic"))
fwrite(table1, paste("Results/CSV Files/table_of_average_acf_lag.csv"))
