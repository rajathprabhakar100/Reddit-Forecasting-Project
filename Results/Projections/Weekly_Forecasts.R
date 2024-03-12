source("Functions/05 - forecast_reddit().R")
library(zoo)
library(data.table)
#forecast_reddit("2021-06-30", city = "Atlanta")$projection
#eg <- forecast_reddit("2020-06-26", city = "Atlanta")$projection 

#eg1 <- eg %>% 
#  slice(c(1,8,15,22)) #%>%
#  summarize(Forecast_Date = Date[1],
#            R2_0 = R2[1],
#            R2_7 = R2[2],
#            R2_14 = R2[3],
#            R2_21 = R2[4],
#            fcast0_lwr = Forecast_Lwr[1],
#            fcast0_mean = Forecast[1],
#            fcast0_upr = Forecast_Upr[1],
#            fcast7_lwr = Forecast_Lwr[2],
#            fcast7_mean = Forecast[2],
#            fcast7_upr = Forecast_Upr[2],
#            fcast14_lwr = Forecast_Lwr[3],
#            fcast14_mean = Forecast[3],
#            fcast14_upr = Forecast_Upr[3],
#            fcast21_lwr = Forecast_Lwr[4],
#            fcast21_mean = Forecast[4],
#            fcast21_upr = Forecast_Upr[4],
#            case0 = Daily_Cases[1],
#            target_date7 = Date[2],
#            case7 = Daily_Cases[2],
#            target_date14 = Date[3],
#            case14 = Daily_Cases[3],
#            target_date21 = Date[4],
#            case21 = Daily_Cases[4])

#date <- "2021-07-30"
#eg2 <- reddit_and_cases %>% 
#  filter(between(Date, as.Date(date), as.Date(date) + 27) & City == city) %>%
#  mutate(illness7 = lag(rollmean(mean_illness, k = 7, align = "left", fill = NA, na.pad = T), n = 7),
#         illness14 = lag(rollmean(mean_illness, k = 7, align = "left", fill = NA, na.pad = T), n = 14),
#         illness21 = lag(rollmean(mean_illness, k = 7, align = "left", fill = NA, na.pad = T), n = 21)) %>% 
#  select(Date, City, MSA_Code, Daily_Cases7, mean_illness, illness7, illness14, illness21)

#fwrite(final_data_atlanta1, "Results/Projections/atlanta_projected_cases_8weeks.csv")


data_list_4_weeks <- list()
for (date in seq(as.Date("2020-05-01"), as.Date("2022-02-18"), by = "7 days")) {
  projection <- forecast_reddit(date = date, city = "Atlanta")$projection
  projection1 <- projection %>% 
    slice(c(1,8,15,22)) %>%
    summarize(Forecast_Date = Date[1],
              R2_0 = R2[1],
              R2_7 = R2[2],
              R2_14 = R2[3],
              R2_21 = R2[4],
              fcast0_lwr = Forecast_Lwr[1],
              fcast0_mean = Forecast[1],
              fcast0_upr = Forecast_Upr[1],
              fcast7_lwr = Forecast_Lwr[2],
              fcast7_mean = Forecast[2],
              fcast7_upr = Forecast_Upr[2],
              fcast14_lwr = Forecast_Lwr[3],
              fcast14_mean = Forecast[3],
              fcast14_upr = Forecast_Upr[3],
              fcast21_lwr = Forecast_Lwr[4],
              fcast21_mean = Forecast[4],
              fcast21_upr = Forecast_Upr[4],
              case0 = Daily_Cases[1],
              target_date7 = Date[2],
              case7 = Daily_Cases[2],
              target_date14 = Date[3],
              case14 = Daily_Cases[3],
              target_date21 = Date[4],
              case21 = Daily_Cases[4])
  data_list_4_weeks[[length(data_list_4_weeks) + 1]] <- projection1
}
final_data_atlanta_4 <- do.call(rbind, data_list_4_weeks)

data_list_8_weeks <- list()
for (date in seq(as.Date("2020-05-01"), as.Date("2022-02-18"), by = "7 days")) {
  projection <- forecast_reddit1(date = date, city = "Atlanta")$projection
  projection1 <- projection %>% 
    slice(c(1,8,15,22)) %>%
    summarize(Forecast_Date = Date[1],
              R2_0 = R2[1],
              R2_7 = R2[2],
              R2_14 = R2[3],
              R2_21 = R2[4],
              fcast0_lwr = Forecast_Lwr[1],
              fcast0_mean = Forecast[1],
              fcast0_upr = Forecast_Upr[1],
              fcast7_lwr = Forecast_Lwr[2],
              fcast7_mean = Forecast[2],
              fcast7_upr = Forecast_Upr[2],
              fcast14_lwr = Forecast_Lwr[3],
              fcast14_mean = Forecast[3],
              fcast14_upr = Forecast_Upr[3],
              fcast21_lwr = Forecast_Lwr[4],
              fcast21_mean = Forecast[4],
              fcast21_upr = Forecast_Upr[4],
              case0 = Daily_Cases[1],
              target_date7 = Date[2],
              case7 = Daily_Cases[2],
              target_date14 = Date[3],
              case14 = Daily_Cases[3],
              target_date21 = Date[4],
              case21 = Daily_Cases[4])
  
  data_list_8_weeks[[length(data_list_8_weeks) + 1]] <- projection1
}
final_data_atlanta_8 <- do.call(rbind, data_list_8_weeks)

data_list_cumulative <- list()
for (date in seq(as.Date("2020-05-01"), as.Date("2022-02-18"), by = "7 days")) {
  projection <- forecast_reddit2(date = date, city = "Atlanta")$projection
  projection1 <- projection %>% 
    slice(c(1,8,15,22)) %>%
    summarize(Forecast_Date = Date[1],
              R2_0 = R2[1],
              R2_7 = R2[2],
              R2_14 = R2[3],
              R2_21 = R2[4],
              fcast0_lwr = Forecast_Lwr[1],
              fcast0_mean = Forecast[1],
              fcast0_upr = Forecast_Upr[1],
              fcast7_lwr = Forecast_Lwr[2],
              fcast7_mean = Forecast[2],
              fcast7_upr = Forecast_Upr[2],
              fcast14_lwr = Forecast_Lwr[3],
              fcast14_mean = Forecast[3],
              fcast14_upr = Forecast_Upr[3],
              fcast21_lwr = Forecast_Lwr[4],
              fcast21_mean = Forecast[4],
              fcast21_upr = Forecast_Upr[4],
              case0 = Daily_Cases[1],
              target_date7 = Date[2],
              case7 = Daily_Cases[2],
              target_date14 = Date[3],
              case14 = Daily_Cases[3],
              target_date21 = Date[4],
              case21 = Daily_Cases[4])
  data_list_cumulative[[length(data_list_cumulative) + 1]] <- projection1
}
final_data_atlanta_cumulative <- do.call(rbind, data_list_cumulative)
#fwrite(final_data_atlanta_cumulative, "debugging1.csv")
