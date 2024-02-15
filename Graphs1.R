rank_avg <- read_csv("Results/CSV Files/Average_City_Rank.csv")
table1 <- read_csv("Results/CSV Files/table_of_average_acf_lag.csv")
combined <- read_csv("Results/CSV Files/combined_data.csv")


top12variables <- c("mean_illness", "mean_health", "mean_family", "mean_Tone",
                    "mean_fatigue", "mean_Analytic", "mean_allnone", "mean_emo_pos",
                    "mean_mental", "mean_emo_sad", "mean_tone_pos", "mean_emotion")

top_12 <- combined %>% 
  filter(Lag > 0 & Variable %in% top12variables)

ggplot(top_12, aes(x = Lag, y = Max_ACF))+
  geom_point()+
  labs(x = "Lag",
       y = "Max Correlation", 
       title = "Scatter Plot of Top 12 Reddit Indicators by Max Correlation")+
  facet_wrap(~ Variable)

ggplot(reddit_and_cases1, aes(x = Date))+
  geom_line(aes(y = Scaled_DailyCases7, color = "Cases"))+
  geom_line(aes(y = Scaled_mean_health, color = "mean_health"))+
  facet_wrap(~ City, nrow = 4)+
  scale_color_manual(values = c("Cases" = "blue", "mean_health" = "red"))+
  labs(x = "Date",
       y = "Z Score",
       title = "Time Series of mean_Health vs Cases")

ggplot(reddit_and_cases1, aes(x = Date))+
  geom_line(aes(y = Scaled_DailyCases7, color = "Cases"))+
  geom_line(aes(y = Scaled_mean_illness, color = "mean_illness"))+
  facet_wrap(~ City, nrow = 4)+
  scale_color_manual(values = c("Cases" = "blue", "mean_illness" = "orange"))+
  labs(x = "Date",
       y = "Z Score",
       title = "Time Series of mean_illness vs Cases")

#Graphing Projections
final_data_atlanta <- read_csv("Results/Projections/atlanta_projected_cases.csv") %>% 
  mutate(Absolute_Error7 = abs(fcast7_mean - case7),
         Absolute_Error14 = abs(fcast14_mean - case14),
         Absolute_Error21 = abs(fcast21_mean - case21))

MAE <- final_data_atlanta %>% 
  summarize(MAE7 = mean(Absolute_Error7, na.rm = T),
            MAE14 = mean(Absolute_Error14, na.rm = T),
            MAE21 = mean(Absolute_Error21, na.rm = T))

library(tidyverse)
library(gridExtra)
atlanta7 <- ggplot(final_data_atlanta, aes(x = Forecast_Date))+
  geom_line(y = final_data_atlanta$fcast7_mean, color = "red")+
  geom_ribbon(aes(ymin = fcast7_lwr, ymax = fcast7_upr), fill = "yellow", alpha = 0.3)+
  geom_point(y = final_data_atlanta$case7)+
  labs(x = "Date", y = "Cases", title = "7-day Forecast Results")
  
atlanta14 <- ggplot(final_data_atlanta, aes(x = Forecast_Date))+
  geom_line(y = final_data_atlanta$fcast14_mean, color = "red")+
  geom_ribbon(aes(ymin = fcast14_lwr, ymax = fcast14_upr), fill = "yellow", alpha = 0.3)+
  geom_point(y = final_data_atlanta$case14)+
  labs(x = "Date", y = "Cases", title = "14-day Forecast Results")

atlanta21 <- ggplot(final_data_atlanta, aes(x = Forecast_Date))+
  geom_line(y = final_data_atlanta1$fcast21_mean, color = "red")+
  geom_ribbon(aes(ymin = fcast21_lwr, ymax = fcast21_upr), fill = "yellow", alpha = 0.3)+
  geom_point(y = final_data_atlanta1$case21)+
  labs(x = "Date", y = "Cases", title = "21-day Forecast Results")

final_data_atlanta2 <- read_csv("Results/Projections/atlanta_projected_cases_8weeks.csv") %>% 
  mutate(Absolute_Error7 = abs(fcast7_mean - case7),
         Absolute_Error14 = abs(fcast14_mean - case14),
         Absolute_Error21 = abs(fcast21_mean - case21)) %>% 
  summarize(MAE7 = mean(Absolute_Error7, na.rm = T),
            MAE14 = mean(Absolute_Error14, na.rm = T),
            MAE21 = mean(Absolute_Error21, na.rm = T))

atlanta7_56days <- ggplot(final_data_atlanta1, aes(x = Forecast_Date))+
  geom_line(y = final_data_atlanta1$fcast7_mean, color = "red")+
  geom_ribbon(aes(ymin = fcast7_lwr, ymax = fcast7_upr), fill = "yellow", alpha = 0.3)+
  geom_point(y = final_data_atlanta1$case7)+
  labs(x = "Date", y = "Cases", title = "7-day Forecast Results", subtitle = "8 weeks")

atlanta14_56days <- ggplot(final_data_atlanta1, aes(x = Forecast_Date))+
  geom_line(y = final_data_atlanta1$fcast14_mean, color = "red")+
  geom_ribbon(aes(ymin = fcast14_lwr, ymax = fcast14_upr), fill = "yellow", alpha = 0.3)+
  geom_point(y = final_data_atlanta1$case14)+
  labs(x = "Date", y = "Cases", title = "14-day Forecast Results", subtitle = "8 weeks")

atlanta21_56days <- ggplot(final_data_atlanta1, aes(x = Forecast_Date))+
  geom_line(y = final_data_atlanta1$fcast21_mean, color = "red")+
  geom_ribbon(aes(ymin = fcast21_lwr, ymax = fcast21_upr), fill = "yellow", alpha = 0.3)+
  geom_point(y = final_data_atlanta1$case21)+
  labs(x = "Date", y = "Cases", title = "21-day Forecast Results", subtitle = "8 weeks")

atlanta7
atlanta14
atlanta21

atlanta7_56days
atlanta14_56days
atlanta21_56days

grid.arrange(atlanta7, atlanta7_56days)
grid.arrange(atlanta14, atlanta14_56days)
grid.arrange(atlanta21, atlanta21_56days)

rbind(MAE, final_data_atlanta2)
