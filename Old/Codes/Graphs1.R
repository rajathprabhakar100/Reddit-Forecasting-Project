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


atlanta14_daily_56days <- ggplot(final_data_atlanta_daily_8, aes(x = Forecast_Date))+
  geom_line(y = final_data_atlanta_daily_8$fcast14_mean, color = "red")+
  geom_ribbon(aes(ymin = fcast14_lwr, ymax = fcast14_upr), fill = "yellow", alpha = 0.3)+
  geom_point(y = final_data_atlanta_daily_8$case14)+
  labs(x = "Date", y = "Cases", title = "14-day Forecast Results (Daily)", subtitle = "8 weeks")

atlanta21_daily_56days <- ggplot(final_data_atlanta_daily_8, aes(x = Forecast_Date))+
  geom_line(y = final_data_atlanta_daily_8$fcast21_mean, color = "red")+
  geom_ribbon(aes(ymin = fcast21_lwr, ymax = fcast21_upr), fill = "yellow", alpha = 0.3)+
  geom_point(y = final_data_atlanta_daily_8$case21)+
  labs(x = "Date", y = "Cases", title = "21-day Forecast Results (Daily)", subtitle = "8 weeks")


atlanta7_daily_56days
atlanta14_daily_56days
atlanta21_daily_56days

library(cowplot)
ggsave("Atlanta_Daily_7days_8_week.png", atlanta7_daily_56days)
ggsave("Atlanta_Daily_14days_8_week.png", atlanta14_daily_56days)
ggsave("Atlanta_Daily_21days_8_week.png", atlanta21_daily_56days)

atlanta7_daily_28days <- ggplot(final_data_atlanta_daily_4, aes(x = Forecast_Date))+
  geom_line(y = final_data_atlanta_daily_4$fcast7_mean, color = "red")+
  geom_ribbon(aes(ymin = fcast7_lwr, ymax = fcast7_upr), fill = "yellow", alpha = 0.3)+
  geom_point(y = final_data_atlanta_daily_4$case7)+
  labs(x = "Date", y = "Cases", title = "7-day Forecast Results (Daily)", subtitle = "4 weeks")

atlanta14_daily_28days <- ggplot(final_data_atlanta_daily_4, aes(x = Forecast_Date))+
  geom_line(y = final_data_atlanta_daily_4$fcast14_mean, color = "red")+
  geom_ribbon(aes(ymin = fcast14_lwr, ymax = fcast14_upr), fill = "yellow", alpha = 0.3)+
  geom_point(y = final_data_atlanta_daily_4$case14)+
  labs(x = "Date", y = "Cases", title = "14-day Forecast Results (Daily)", subtitle = "4 weeks")

atlanta21_daily_28days <- ggplot(final_data_atlanta_daily_4, aes(x = Forecast_Date))+
  geom_line(y = final_data_atlanta_daily_4$fcast21_mean, color = "red")+
  geom_ribbon(aes(ymin = fcast21_lwr, ymax = fcast21_upr), fill = "yellow", alpha = 0.3)+
  geom_point(y = final_data_atlanta_daily_4$case21)+
  labs(x = "Date", y = "Cases", title = "21-day Forecast Results (Daily)", subtitle = "4 weeks")

atlanta7_daily_28days
atlanta14_daily_28days
atlanta21_daily_28days

ggsave("Atlanta_Daily_7days_4_week.png", atlanta7_daily_28days)
ggsave("Atlanta_Daily_14days_4_week.png", atlanta14_daily_28days)
ggsave("Atlanta_Daily_21days_4_week.png", atlanta21_daily_28days)

grid.arrange(atlanta7_daily_28days, atlanta7_daily_56days)
grid.arrange(atlanta14_daily_28days, atlanta14_daily_56days)
grid.arrange(atlanta21_daily_28days, atlanta21_daily_56days)


#nowcast - 4 wk, 8 wk, cumulative
atlanta0_28days <- ggplot(final_data_atlanta_4, aes(x = Forecast_Date))+
  geom_line(y = final_data_atlanta_4$fcast0_mean, color = "red")+
  geom_ribbon(aes(ymin = fcast0_lwr, ymax = fcast0_upr), fill = "yellow", alpha = 0.3)+
  geom_point(y = final_data_atlanta_4$case0)+
  labs(x = "Date", y = "Cases", title = "Nowcast Results (Weekly)", subtitle = "4 weeks")

atlanta0_56days <- ggplot(final_data_atlanta_8, aes(x = Forecast_Date))+
  geom_line(y = final_data_atlanta_8$fcast0_mean, color = "red")+
  geom_ribbon(aes(ymin = fcast0_lwr, ymax = fcast0_upr), fill = "yellow", alpha = 0.3)+
  geom_point(y = final_data_atlanta_8$case0)+
  labs(x = "Date", y = "Cases", title = "Nowcast Forecast Results (Weekly)", subtitle = "8 weeks")

atlanta0_cumulative <- ggplot(final_data_atlanta_cumulative, aes(x = Forecast_Date))+
  geom_line(y = final_data_atlanta_cumulative$fcast0_mean, color = "red")+
  geom_ribbon(aes(ymin = fcast0_lwr, ymax = fcast0_upr), fill = "yellow", alpha = 0.3)+
  geom_point(y = final_data_atlanta_cumulative$case0)+
  labs(x = "Date", y = "Cases", title = "Nowcast Forecast Results (Weekly)", subtitle = "cumulative")
grid.arrange(atlanta0_28days, atlanta0_56days, atlanta0_cumulative)

#Seven Day Forecasts - 4 wk, 8 wk, cumulative
atlanta7_28days <- ggplot(final_data_atlanta_4, aes(x = Forecast_Date))+
  geom_line(y = final_data_atlanta_4$fcast7_mean, color = "red")+
  geom_ribbon(aes(ymin = fcast7_lwr, ymax = fcast7_upr), fill = "yellow", alpha = 0.3)+
  geom_point(y = final_data_atlanta_4$case7)+
  #scale_y_continuous(limits = c(0, max(final_data_atlanta_4$fcast7_upr)),
  #                   breaks = seq(0, max(final_data_atlanta_4$fcast7_upr, final_data_atlanta_4$case7), by = 1000))+
  labs(x = "Date", y = "Cases", title = "7-day Forecast Results (Weekly)", subtitle = "4 weeks")

atlanta7_56days <- ggplot(final_data_atlanta_8, aes(x = Forecast_Date))+
  geom_line(y = final_data_atlanta_8$fcast7_mean, color = "red")+
  geom_ribbon(aes(ymin = fcast7_lwr, ymax = fcast7_upr), fill = "yellow", alpha = 0.3)+
  geom_point(y = final_data_atlanta_8$case7)+
  labs(x = "Date", y = "Cases", title = "7-day Forecast Results (Weekly)", subtitle = "8 weeks")

atlanta7_cumulative <- ggplot(final_data_atlanta_cumulative, aes(x = Forecast_Date))+
  geom_line(y = final_data_atlanta_cumulative$fcast7_mean, color = "red")+
  geom_ribbon(aes(ymin = fcast7_lwr, ymax = fcast7_upr), fill = "yellow", alpha = 0.3)+
  geom_point(y = final_data_atlanta_cumulative$case7)+
  labs(x = "Date", y = "Cases", title = "7-day Forecast Results (Weekly)", subtitle = "cumulative")
grid.arrange(atlanta7_28days,atlanta7_cumulative)

#Fourteen Day Forecasts - 4 wk, 8 wk, cumulative
atlanta14_28days <- ggplot(final_data_atlanta_4, aes(x = Forecast_Date))+
  geom_line(y = final_data_atlanta_4$fcast14_mean, color = "red")+
  geom_ribbon(aes(ymin = fcast14_lwr, ymax = fcast14_upr), fill = "yellow", alpha = 0.3)+
  geom_point(y = final_data_atlanta_4$case14)+
  labs(x = "Date", y = "Cases", title = "14-day Forecast Results (Weekly)", subtitle = "4 weeks")

atlanta14_56days <- ggplot(final_data_atlanta_8, aes(x = Forecast_Date))+
  geom_line(y = final_data_atlanta_8$fcast14_mean, color = "red")+
  geom_ribbon(aes(ymin = fcast14_lwr, ymax = fcast14_upr), fill = "yellow", alpha = 0.3)+
  geom_point(y = final_data_atlanta_8$case14)+
  labs(x = "Date", y = "Cases", title = "14-day Forecast Results (Weekly)", subtitle = "8 weeks")

atlanta14_cumulative <- ggplot(final_data_atlanta_cumulative, aes(x = Forecast_Date))+
  geom_line(y = final_data_atlanta_cumulative$fcast14_mean, color = "red")+
  geom_ribbon(aes(ymin = fcast14_lwr, ymax = fcast14_upr), fill = "yellow", alpha = 0.3)+
  geom_point(y = final_data_atlanta_cumulative$case14)+
  labs(x = "Date", y = "Cases", title = "14-day Forecast Results (Weekly)", subtitle = "cumulative")
grid.arrange(atlanta14_28days, atlanta14_56days, atlanta14_cumulative)

#21-day - 4 wk, 8 wk, cumulative
atlanta21_28days <- ggplot(final_data_atlanta_4, aes(x = Forecast_Date))+
  geom_line(y = final_data_atlanta_4$fcast21_mean, color = "red")+
  geom_ribbon(aes(ymin = fcast21_lwr, ymax = fcast21_upr), fill = "yellow", alpha = 0.3)+
  geom_point(y = final_data_atlanta_4$case21)+
  labs(x = "Date", y = "Cases", title = "21-day Forecast Results (Weekly)", subtitle = "4 weeks")

atlanta21_56days <- ggplot(final_data_atlanta_8, aes(x = Forecast_Date))+
  geom_line(y = final_data_atlanta_8$fcast21_mean, color = "red")+
  geom_ribbon(aes(ymin = fcast21_lwr, ymax = fcast21_upr), fill = "yellow", alpha = 0.3)+
  geom_point(y = final_data_atlanta_8$case21)+
  labs(x = "Date", y = "Cases", title = "21-day Forecast Results (Weekly)", subtitle = "8 weeks")

atlanta21_cumulative <- ggplot(final_data_atlanta_cumulative, aes(x = Forecast_Date))+
  geom_line(y = final_data_atlanta_cumulative$fcast21_mean, color = "red")+
  geom_ribbon(aes(ymin = fcast21_lwr, ymax = fcast21_upr), fill = "yellow", alpha = 0.3)+
  geom_point(y = final_data_atlanta_cumulative$case21)+
  labs(x = "Date", y = "Cases", title = "21-day Forecast Results (Weekly)", subtitle = "cumulative")
grid.arrange(atlanta21_28days, atlanta21_56days, atlanta21_cumulative)

atlanta7_cumulative
atlanta0_cumulative
atlanta14_cumulative
atlanta21_cumulative
grid.arrange(atlanta7_28days, atlanta7_cumulative, ncol = 1)

atlanta0_28days
atlanta0_56days
atlanta7_28days
atlanta7_56days
ggplot(final_data_atlanta_4, aes(x = Forecast_Date))+
  geom_line(y = final_data_atlanta_cumulative$fcast7_mean, color = "Forecast")+
  geom_line(y = final_data_atlanta_4)
