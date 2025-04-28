library(tvReg)
library(tidyverse)
library(forecast)
reddit_and_cases <- read_csv("Results/CSV Files/reddit_and_cases_deaths.csv")

city_data <- reddit_and_cases %>%
  filter(City == "Atlanta") %>% 
  select(-mean_function) %>% 
  rename(mean_function = mean_function.)
date <- "2020-04-05"
cutoff_date <- as.Date(date)

data_for_fitting <- city_data %>%
  filter(week < cutoff_date) %>%
  filter(!is.na(Weekly_Cases7)) %>%
  select(week:Weekly_Cases, Weekly_Cases7, everything())
data_for_predicting <- city_data %>%
  filter(week == cutoff_date) %>%
  select(week:Weekly_Cases, Weekly_Cases7, everything())
#tvAR
tvAR_COVID_model <- tvAR(y = as.ts(data_for_fitting$Weekly_Cases),
                         p = 1,
                         exogen = as.ts(data_for_fitting$mean_COVID_Related))
tvAR_Baseline_model <- tvAR(y = as.ts(data_for_fitting$Weekly_Cases),
                            p = 1)



# Then forecast just the single new point
fcast_tvAR_Baseline <- forecast(tvAR_Baseline_model, 
                                newdata = data_for_predicting$Weekly_Cases7) %>%
  as.data.frame() %>%
  rename(tvAR_Baseline_prediction = value)


fcast_tvAR <- forecast(tvAR_COVID_model,
                       newdata = data_for_predicting) %>%
  as.data.frame() %>%
  rename(
    tvAR_COVID_prediction = value) %>%
  bind_cols(fcast_tvAR_Baseline) %>% 
  bind_cols(data_for_predicting) %>%
  select(week:Weekly_Cases7, tvAR_COVID_prediction, tvAR_Baseline_prediction, everything())

###########################################################

nowcast_list <- list()
for (date in city_data %>% filter(week >= "2020-04-01" & week < "2023-03-05") %>% pull(week)) {
#  print(date)
  message(paste("Forecasting", as.character(as.Date(date))))

  cutoff_date <- as.Date(date)

  data_for_fitting <- city_data %>%
    filter(week < cutoff_date) %>%
    select(week:Weekly_Cases, everything())
  data_for_predicting <- city_data %>%
    filter(week == cutoff_date) %>%
    select(week:Weekly_Cases, everything())
  #tvAR
  tvAR_COVID_model <- tvARX(data = data_for_fitting,
                           response = "Weekly_Cases",
                           order = 1,
                           predictors = "mean_COVID_Related",
                           time = "week")
  tvAR_Baseline_model <- tvARX(data = data_for_fitting,
                              response = "Weekly_Cases",
                              order = 1,
                              time = "week")
  
  fcast_tvAR_Baseline <- forecast_tvar(tvAR_Baseline_model, data_for_predicting, time_var = "week") %>%
    as.data.frame() %>% 
    rename(Baseline_prediction = forecast,
           Baseline_lower = lower,
           Baseline_upper = upper) %>% 
    select(Baseline_lower, Baseline_prediction, Baseline_upper)
  
  fcast_tvAR <- forecast_tvar(tvAR_COVID_model, data_for_predicting, time_var = "week") %>%
    as.data.frame() %>% 
    rename(COVID_prediction = forecast,
           COVID_lower = lower,
           COVID_upper = upper) %>%
    select(COVID_lower, COVID_prediction, COVID_upper) %>% 
    bind_cols(fcast_tvAR_Baseline) %>% 
    bind_cols(data_for_predicting) %>%
    mutate(COVID_prediction = ifelse(COVID_prediction < 0, 0, COVID_prediction),
           COVID_lower = ifelse(COVID_lower < 0, 0, COVID_lower),
           COVID_upper = ifelse(COVID_upper < 0, 0, COVID_upper),
           Baseline_prediction = ifelse(Baseline_prediction < 0, 0, Baseline_prediction),
           Baseline_lower = ifelse(Baseline_lower < 0, 0, Baseline_lower),
           Baseline_upper = ifelse(Baseline_upper < 0, 0, Baseline_upper)) %>% 
    select(week:Weekly_Cases, COVID_lower:COVID_upper, Baseline_lower:Baseline_upper, everything())
  

  nowcast_list[[date]] <- fcast_tvAR
}

tv_nowcast <- bind_rows(nowcast_list)

nowcast_stats <- tv_nowcast %>% 
  #group_by(week) %>% 
  summarize(MAE_Baseline = mean(abs(Baseline_prediction - Weekly_Cases)),
            MAE_COVID = mean(abs(COVID_prediction - Weekly_Cases)),
            MAPE_Baseline = mean(abs((Baseline_prediction - Weekly_Cases)/Weekly_Cases)),
            MAPE_COVID = mean(abs((COVID_prediction - Weekly_Cases)/Weekly_Cases)))

nowcast_COVID <- ggplot(tv_nowcast, aes(x = week))+
  geom_point(aes(y = Weekly_Cases))+
  geom_line(aes(y = COVID_prediction), color = "red")+
  #geom_ribbon(aes(ymin = COVID_lower, ymax = COVID_upper), fill = "yellow", alpha = 0.3)+
  labs(x = "Week", 
       y = "Weekly Cases", 
       title = "Nowcast for Atlanta",
       subtitle = "Time Varying Model Using mean_COVID_Related as exogen")

nowcast_Baseline <- ggplot(tv_nowcast, aes(x = week))+
  geom_point(aes(y = Weekly_Cases))+
  geom_line(aes(y = Baseline_prediction), color = "red")+
  #geom_ribbon(aes(ymin = Baseline_lower, ymax = Baseline_upper), fill = "yellow", alpha = 0.3)+
  labs(x = "Week", 
       y = "Weekly Cases", 
       title = "Nowcast for Atlanta",
       subtitle = "Time Varying Baseline Model")
library(cowplot)
plot_grid(nowcast_COVID, nowcast_Baseline)
ggplot(tv_nowcast, aes(x = week))+
  geom_line(aes(y = mean_COVID_Related), color = "blue")+
  geom_line(aes(y = Weekly_Cases), color = "red")

#####################################################
#####################################################

#####################################################
ggplot(tv_nowcast, aes(x = week)) +
  geom_point(aes(y = Weekly_Cases), shape = 16, color = "black") +  # Use numeric shape code
  geom_line(aes(y = tvLM_prediction, color = "TVLM Forecast")) +
  geom_line(aes(y = tvAR_prediction, color = "TVAR Forecast")) +
  geom_ribbon(aes(ymin = lb_lm, ymax = ub_lm, fill = "CI LM"), alpha = 0.3) +
  geom_ribbon(aes(ymin = lb_ar, ymax = ub_ar, fill = "CI AR"), alpha = 0.5) +
  scale_fill_manual(values = c("CI LM" = "yellow", "CI AR" = "blue"),
                    labels = c("TVAR Confidence Interval", "TVLM Confidence Interval")) +  # Optional labels
  scale_color_manual(values = c("TVLM Forecast" = "red", "TVAR Forecast" = "yellow"),
                     labels = c("TVAR Forecast", "TVLM Forecast")) +  # Optional labels
  labs(x = "Week",
       y = "Weekly Cases",
       fill = "Confidence Intervals",
       color = "Forecasts",
       shape = "True Cases",
       title = "Nowcast for Atlanta") +
  theme_minimal()+
  coord_cartesian(ylim = c(0,125000))

ggplot(tv_nowcast, aes(x = week))+
  geom_point(aes(y = Weekly_Cases),color = "black")+
  geom_line(aes(tvAR_prediction, color = "TVAR Forecast"))+
  geom_ribbon(aes(ymin = lb_ar, ymax = ub_ar, fill = "CI AR"), alpha = 0.3)+
  scale_fill_manual(values = c("CI LM" = "yellow"),
                    labels = c("TVAR Confidence Interval"))+
  scale_color_manual(values = c("TVAR Forecast" = "red"),
                     labels = c("TVAR Forecast"))+
  labs(x = "Week",
       y = "Weekly Cases",
       fill = "Confidence Intervals",
       color = "Forecasts",
       shape = "True Cases",
       title = "Nowcast for Atlanta")+
  theme_minimal()

ggplot(tv_nowcast, aes(x = week))+
  geom_point(aes(y = Weekly_Cases), shape = 16, color = "black")+
  geom_line(aes(tvAR_prediction, color = "red"))+
  geom_ribbon(aes(ymin = lb_ar, ymax = ub_ar, fill = "yellow"), alpha = 0.3)


































