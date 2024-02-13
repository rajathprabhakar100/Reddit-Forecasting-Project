
source("Functions/05 - forecast_reddit().R")
library(zoo)
data_list <- list()
for (date in seq(as.Date("2020-05-01"), as.Date("2022-02-18"), by = "7 days")) {
  projection <- forecast_reddit(date = date, city = "Atlanta")$projection
  projection1 <- projection %>% 
    slice(c(1,8,15, 22)) %>%
    summarize(Forecast_Date = Date[1],
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
              target_date7 = Date[2],
              case7 = Daily_Cases[1],
              target_date14 = Date[3],
              case14 = Daily_Cases[2],
              target_date21 = Date[4],
              case21 = Daily_Cases[3])
  data_list[[length(data_list) + 1]] <- projection1
}
final_data_atlanta <- do.call(rbind, data_list)
library(data.table)
fwrite(final_data_atlanta, "Results/Projections/atlanta_projected_cases.csv")
