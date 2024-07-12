library(here)
library(cowplot)
source(here("Functions/06 - forecast_reddit().R"))

graph_reddit_forecasts <- function(city, start.date, end.date) {
  #with autoregressive term
  
  city_poisson_8_weeks <- list()
  for (date in seq(as.Date(start.date), as.Date(end.date), by = "7 days")) {
    projection <- forecast_reddit_poisson_8_ar(date = date, city = city)
    projection1 <- projection %>% 
      summarize(Forecast_Date = week[1],
                fcast0_lwr = Forecast_Lwr[1],
                fcast0_mean = Forecast[1],
                fcast0_upr = Forecast_Upr[1],
                fcast7_lwr = Forecast_Lwr[2],
                fcast7_mean = Forecast[2],
                fcast7_upr = Forecast_Upr[2],
                case0 = Weekly_Cases[1],
                target_date7 = week[2],
                case7 = Weekly_Cases[2])
    
    city_poisson_8_weeks[[length(city_poisson_8_weeks) + 1]] <- projection1
  }
    
  poisson_data_city_8 <- do.call(rbind, city_poisson_8_weeks)

  max_value <- max(c(max(poisson_data_city_8$case0),
                      max(poisson_data_city_8$case7)))
  round_max_value <- round(1.1 * (max_value/1000)) * 1000
  

  ##Graphs
  city0_28days <- ggplot(poisson_data_city_8, aes(x = Forecast_Date))+
    geom_line(y = poisson_data_city_8$fcast0_mean, color = "red")+
    geom_ribbon(aes(ymin = fcast0_lwr, ymax = fcast0_upr), fill = "yellow", alpha = 0.3)+
    geom_point(y = poisson_data_city_8$case0)+
    labs(x = "Date", y = "Cases", title = paste0(city, " ", "Nowcast Results (Weekly)"), subtitle = "4 weeks")+
    scale_y_continuous(breaks = seq(0, round_max_value, length.out = 25),
                       limits = c(0, 1.1 * round_max_value),
                       labels = scales::comma)+
    scale_x_date(date_breaks = "2 months")+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  city7_28days <- ggplot(poisson_data_city_8, aes(x = Forecast_Date))+
    geom_line(y = poisson_data_city_8$fcast7_mean, color = "red")+
    geom_ribbon(aes(ymin = fcast7_lwr, ymax = fcast7_upr), fill = "yellow", alpha = 0.3)+
    geom_point(y = poisson_data_city_8$case7)+
    labs(x = "Date", y = "Cases", title = paste0(city, " ", "7-day Forecast Results (Weekly)"),
         subtitle = "4 weeks")+
    scale_y_continuous(breaks = seq(0, round_max_value, length.out = 25),
                       limits = c(0, 1.1 * round_max_value),
                       labels = scales::comma)+
    scale_x_date(date_breaks = "2 months")+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  #Without Autoregressive term
  city_poisson_8_weeks_no_ar <- list()
  for (date in seq(as.Date(start.date), as.Date(end.date), by = "7 days")) {
    projection <- forecast_reddit_poisson_8(date = date, city = city)
    projection1 <- projection %>% 
      summarize(Forecast_Date = week[1],
                fcast0_lwr = Forecast_Lwr[1],
                fcast0_mean = Forecast[1],
                fcast0_upr = Forecast_Upr[1],
                fcast7_lwr = Forecast_Lwr[2],
                fcast7_mean = Forecast[2],
                fcast7_upr = Forecast_Upr[2],
                case0 = Weekly_Cases[1],
                target_date7 = week[2],
                case7 = Weekly_Cases[2])
    
    city_poisson_8_weeks_no_ar[[length(city_poisson_8_weeks_no_ar) + 1]] <- projection1
  }
  
  poisson_data_city_8_no_ar <- do.call(rbind, city_poisson_8_weeks_no_ar)
  
  max_value1 <- max(c(max(poisson_data_city_8_no_ar$case0),
                     max(poisson_data_city_8_no_ar$case7)))
  round_max_value1 <- round(1.1 * (max_value1/1000)) * 1000
  
  
  ##Graphs
  city0_56days_no_ar <- ggplot(poisson_data_city_8_no_ar, aes(x = Forecast_Date))+
    geom_line(y = poisson_data_city_8_no_ar$fcast0_mean, color = "red")+
    geom_ribbon(aes(ymin = fcast0_lwr, ymax = fcast0_upr), fill = "yellow", alpha = 0.3)+
    geom_point(y = poisson_data_city_8_no_ar$case0)+
    labs(x = "Date", y = "Cases", title = paste0(city, " ", "Nowcast Results (Weekly)"), subtitle = "8 weeks",
         caption = "without autoregressive term")+
    scale_y_continuous(breaks = seq(0, round_max_value1, length.out = 25),
                       limits = c(0, 1.1 * round_max_value1),
                       labels = scales::comma)+
    scale_x_date(date_breaks = "2 months")+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  city7_56days_no_ar <- ggplot(poisson_data_city_8_no_ar, aes(x = Forecast_Date))+
    geom_line(y = poisson_data_city_8_no_ar$fcast7_mean, color = "red")+
    geom_ribbon(aes(ymin = fcast7_lwr, ymax = fcast7_upr), fill = "yellow", alpha = 0.3)+
    geom_point(y = poisson_data_city_8_no_ar$case7)+
    labs(x = "Date", y = "Cases", title = paste0(city, " ", "7-day Forecast Results (Weekly)"),
         subtitle = "8 weeks", caption = "without autoregressive term")+
    scale_y_continuous(breaks = seq(0, round_max_value1, length.out = 25),
                       limits = c(0, 1.1 * round_max_value1),
                       labels = scales::comma)+
    scale_x_date(date_breaks = "2 months")+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  results <- list(poisson_8_ar = poisson_data_city_8,
                  nowcast_8_ar = city0_28days,
                  forecast_8_ar = city7_28days, 
                  poisson8 = poisson_data_city_8_no_ar,
                  nowcast_8 = city0_56days_no_ar,
                  forecast8 = city7_56days_no_ar)
  return(results)
  paste0("Finished graphing ", city)
}


