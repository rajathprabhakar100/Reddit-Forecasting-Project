combined <- read_csv("combined_data.csv")

combined_negative <- combined %>% filter(Lag <= 0) %>%
  group_by(Variable) %>% 
  summarize(Average_ACF_negative = mean(Max_ACF),
            Average_Lag_negative = mean(Lag))

combined_positive <- combined %>% filter(Lag > 0) %>% 
  group_by(Variable) %>% 
  summarize(Average_ACF_positive = mean(Max_ACF),
            Average_Lag_positive = mean(Lag))

table1 <- left_join(combined_positive, combined_negative, by = "Variable")
fwrite(table1, paste("table_of_average_acf_lag.csv"))
