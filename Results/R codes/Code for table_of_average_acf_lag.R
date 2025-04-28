library(tidyverse)
library(data.table)
combined <- read_csv("Results/CSV Files/combined_data.csv")

combined_negative <- combined %>% filter(Lag <= 0 & Lag >= -6) %>%
  group_by(Variable) %>% 
  summarize(Average_ACF_negative = mean(Max_ACF),
            Average_Lag_negative = mean(Lag))

combined_positive <- combined %>% filter(Lag > 0 & Lag <= 6) %>% 
  group_by(Variable) %>% 
  summarize(Average_ACF_positive = mean(Max_ACF),
            Average_Lag_positive = mean(Lag))

table1 <- left_join(combined_positive, combined_negative, by = "Variable")
fwrite(table1, paste("Results/CSV Files/table_of_average_acf_lag.csv"))


combined_data_year <- read_csv("Results/CSV Files/combined_data_year.csv")
negative <- combined_data_year %>% 
  filter(Lag <= 0 & Lag >= -6) %>%
  group_by(year, Variable) %>% 
  summarize(Average_ACF_negative = mean(Max_ACF),
            Average_Lag_negative = mean(Lag))
positive <- combined_data_year %>% 
  filter(Lag > 0 & Lag <= 6) %>% 
  group_by(year, Variable) %>% 
  summarize(Average_ACF_positive = mean(Max_ACF),
            Average_Lag_positive = mean(Lag))
table2 <- left_join(negative, positive, by = c("year", "Variable"))
fwrite(table2, "Results/CSV Files/average_acf_lag_year.csv")




#Deaths

combo_death <- read_csv("Results/CSV Files/combined_data_deaths.csv")

combined_negative_deaths <- combo_death %>% filter(Lag <= 0 & Lag >= -6) %>%
  group_by(Variable) %>% 
  summarize(Average_ACF_negative = mean(Max_ACF),
            Average_Lag_negative = mean(Lag))

combined_positive_deaths <- combo_death %>% filter(Lag > 0 & Lag <= 6) %>% 
  group_by(Variable) %>% 
  summarize(Average_ACF_positive = mean(Max_ACF),
            Average_Lag_positive = mean(Lag))

table1_death <- left_join(combined_positive_deaths, combined_negative_deaths, by = "Variable")
fwrite(table1_death, paste("Results/CSV Files/table_of_average_acf_lag_deaths.csv"))


combined_data_deaths_year <- read_csv("Results/CSV Files/combined_data_deaths_year.csv")
negative_deaths <- combined_data_deaths_year %>% 
  filter(Lag <= 0 & Lag >= -6) %>%
  group_by(year, Variable) %>% 
  summarize(Average_ACF_negative = mean(Max_ACF),
            Average_Lag_negative = mean(Lag))
positive_deaths <- combined_data_deaths_year %>% 
  filter(Lag > 0 & Lag <= 6) %>% 
  group_by(year, Variable) %>% 
  summarize(Average_ACF_positive = mean(Max_ACF),
            Average_Lag_positive = mean(Lag))
table2_death <- left_join(negative_deaths, positive_deaths, by = c("year", "Variable"))
fwrite(table2_death, "Results/CSV Files/average_acf_lag_year_death.csv")

