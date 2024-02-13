
source("Functions/05 - forecast_reddit().R")
library(zoo)
forecast_reddit("2021-06-30", city = "Atlanta")$projection
eg <- forecast_reddit("2020-06-22", city = "Atlanta")$projection 
eg1 <- eg %>% 
  slice(c(1,8,15)) %>%
  summarize(Forecast_Date = Date,
            R2_7 = R2[1],
            R2_14 = R2[2],
            R2_21 = R2[3],
            fcast7_lwr = Forecast_Lwr[1],
            fcast7_mean = Forecast[1],
            fcast7_upr = Forecast_Upr[1],
            fcast14_lwr = Forecast_Lwr[2],
            fcast14_mean = Forecast[2],
            fcast14_upr = Forecast_Upr[2],
            fcast21_lwr = Forecast_Lwr[3],
            fcast21_mean = Forecast[3],
            fcast21_upr = Forecast_Upr[3],
            target_date7 = Date[1],
            case7 = Daily_Cases[1],
            target_date14 = Date[2],
            case14 = Daily_Cases[2],
            target_date21 = Date[3],
            case21 = Daily_Cases[3])

date <- "2021-07-30"
eg2 <- reddit_and_cases %>% 
  filter(between(Date, as.Date(date), as.Date(date) + 27) & City == city) %>%
  mutate(illness7 = lag(rollmean(mean_illness, k = 7, align = "left", fill = NA, na.pad = T), n = 7),
         illness14 = lag(rollmean(mean_illness, k = 7, align = "left", fill = NA, na.pad = T), n = 14),
         illness21 = lag(rollmean(mean_illness, k = 7, align = "left", fill = NA, na.pad = T), n = 21)) %>% 
  select(Date, City, MSA_Code, Daily_Cases7, mean_illness, illness7, illness14, illness21)

