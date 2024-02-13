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