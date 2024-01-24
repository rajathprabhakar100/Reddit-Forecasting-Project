library(tidyverse)
library(here)
library(zoo)
library(data.table)
library(conflicted)
reddit_and_cases <- read_csv("C:/Users/14049/Desktop/Reddit-Forecasting-Project/Results/CSV Files/reddit_and_cases_deaths.csv")

forecast_reddit <- function(date = NULL, city, weeks = "3", csv=F) {
  conflict_prefer("select", "dplyr")
  conflict_prefer("filter", "dplyr")
  conflict_prefer("lag", "dplyr")
  conflict_prefer("between", "dplyr")
  city_training_data <- reddit_and_cases %>%
    mutate(illness7 = lag(rollmeanr(mean_illness, k = 7, fill = NA), n = 7),
           illness14 = lag(rollmeanr(mean_illness, k = 7, fill = NA), n = 14),
           illness21 = lag(rollmeanr(mean_illness, k = 7, fill = NA), n = 21)) %>% 
    filter(between(Date, as.Date(date) - 27, as.Date(date)) & City == city) %>%
    select(Date, City, MSA_Code, Daily_Cases7, mean_illness, illness7, illness14, illness21) %>% 
    na.omit()
  #print(head(city_training_data))
  city7_model <- lm(Daily_Cases7 ~ illness7 + illness14 + illness21, data = city_training_data)
  city14_model <- lm(Daily_Cases7 ~ illness14 + illness21, data = city_training_data)
  city21_model <- lm(Daily_Cases7 ~ illness21, data = city_training_data)
  
  city7_model_summary <- summary(city7_model)
  city14_model_summary <- summary(city14_model)
  city21_model_summary <- summary(city21_model)
  #print(city7_model_summary)
  #print(city14_model_summary)
  #print(city21_model_summary)
  
  city_projection_data <- reddit_and_cases %>% 
    filter(City == city) %>% 
    mutate(illness7 = lag(rollmeanr(mean_illness, k = 7, fill = NA), n = 7),
           illness14 = lag(rollmeanr(mean_illness, k = 7, fill = NA), n = 14),
           illness21 = lag(rollmeanr(mean_illness, k = 7, fill = NA), n = 21)) %>%
    na.omit()
  
  if (weeks == 1) {
    city_projection_data <- city_projection_data %>% 
      filter(between(Date, as.Date(date), as.Date(date) + 6)) %>% 
      mutate(R2 = city7_model_summary$adj.r.squared,
             Forecast = predict(city7_model, newdata =., interval = "prediction", level = 0.95)[, "fit"],
             Forecast_Lwr = predict(city7_model, newdata = ., interval = "prediction", level = 0.95)[, "lwr"],
             Forecast_Upr = predict(city7_model, newdata = ., interval = "prediction", level = 0.95)[, "upr"]) %>% 
      select(Date, MSA_Title, City, Daily_Cases, mean_illness, illness7, illness14, illness21, R2, Forecast_Lwr,
             Forecast, Forecast_Upr)
    if (csv) {
      formatted_date <- gsub("-", "_", date)
      filename <- paste("Results/Projections/forecasting_data_", gsub(" ", "", city), "_", formatted_date, ".csv",
                        sep = "")
      #print(filename)
      fwrite(city_projection_data, filename)
    }
    
    else {
      output <- list(projection = city_projection_data, 
                     summary_7days = city7_model_summary <- summary(city7_model))
      return(output)
    }
  }
  
  else if (weeks == 2) {
    city_projection_data <- city_projection_data %>% 
      filter(between(Date, as.Date(date), as.Date(date) + 13)) %>%
      mutate(R2 = case_when(
        row_number() <= 7 ~ city7_model_summary$adj.r.squared,
        row_number() > 7 ~ city14_model_summary$adj.r.squared
      )) %>% 
      mutate(
        Forecast = case_when(
          row_number() <= 7 ~ predict(city7_model, newdata = ., interval = "prediction", level = 0.95)[, "fit"],
          row_number() > 7  ~ predict(city14_model, newdata = .,interval = "prediction", level = 0.95)[, "fit"]),
        Forecast_Lwr = case_when(
          row_number() <= 7 ~ predict(city7_model, newdata = ., interval = "prediction", level = 0.95)[, "lwr"],
          row_number() > 7 ~ predict(city14_model, newdata = ., interval = "prediction", level = 0.95)[, "lwr"]),
        Forecast_Upr = case_when(
          row_number() <= 7 ~ predict(city7_model, newdata = ., interval = "prediction", level = 0.95)[, "upr"],
          row_number() > 7 ~ predict(city14_model, newdata = ., interval = "prediction", level = 0.95)[, "upr"])) %>% 
      select(Date, MSA_Title, City, Daily_Cases, mean_illness, illness7, illness14, illness21, R2, Forecast_Lwr,
             Forecast, Forecast_Upr)
    if (csv) {
      formatted_date <- gsub("-", "_", date)
      filename <- paste("Results/Projections/forecasting_data_", gsub(" ", "", city), "_", formatted_date, ".csv",
                        sep = "")
      #print(filename)
      fwrite(city_projection_data, filename)
    }
    
    else {
      output <- list(projection = city_projection_data, 
                     summary_7days = city7_model_summary,
                     summary_14days = city14_model_summary)
      return(output)
    }
  }
  
  else if (weeks == 3) {
    city_projection_data <- city_projection_data %>% 
      filter(between(Date, as.Date(date), as.Date(date) + 20)) %>% 
      mutate(R2 = case_when(
        row_number() <= 7 ~ city7_model_summary$adj.r.squared,
        row_number() > 7 & row_number() <= 14 ~ city14_model_summary$adj.r.squared,
        row_number() > 14 ~ city21_model_summary$adj.r.squared
      )) %>% 
      mutate(
        Forecast = case_when(
          row_number() <= 7 ~ predict(city7_model, newdata = ., interval = "prediction", level = 0.95)[, "fit"],
          row_number() > 7 & row_number() <= 14 ~ predict(city14_model, 
                                                          newdata = .,
                                                          interval = "prediction", 
                                                          level = 0.95)[, "fit"],
          row_number() > 14 ~ predict(city21_model, newdata = ., interval = "prediction", level = 0.95)[, "fit"],
          TRUE ~ NA_real_),
        Forecast_Lwr = case_when(
          row_number() <= 7 ~ predict(city7_model, newdata = ., interval = "prediction", level = 0.95)[, "lwr"],
          row_number() > 7 & row_number() <= 14 ~ predict(city14_model, newdata = ., interval = "prediction", level = 0.95)[, "lwr"],
          row_number() > 14 ~ predict(city21_model, newdata = ., interval = "prediction", level = 0.95)[, "lwr"],
          TRUE ~ NA_real_
        ),
        Forecast_Upr = case_when(
          row_number() <= 7 ~ predict(city7_model, newdata = ., interval = "prediction", level = 0.95)[, "upr"],
          row_number() > 7 & row_number() <= 14 ~ predict(city14_model, newdata = ., interval = "prediction", level = 0.95)[, "upr"],
          row_number() > 14 ~ predict(city21_model, newdata = ., interval = "prediction", level = 0.95)[, "upr"],
          TRUE ~ NA_real_
        )) %>% 
      select(Date, MSA_Title, City, Daily_Cases, mean_illness, illness7, illness14, illness21, R2, Forecast_Lwr,
             Forecast, Forecast_Upr)
    #print(city_projection_data)
    if (csv) {
      formatted_date <- gsub("-", "_", date)
      filename <- paste("Results/Projections/", gsub(" ", "", city), "/forecasting_data_", gsub(" ", "", city), "_", formatted_date, ".csv",
                        sep = "")
      #print(filename)
      fwrite(city_projection_data, filename)
    }
    else {
      output <- list(projection = city_projection_data, 
                     summary_7days = city7_model_summary,
                     summary_14days = city14_model_summary,
                     summary_21days = city21_model_summary)
      return(output)
    }
  }
  
  else {
    print("Must have value 1, 2, or 3 for argument 'weeks'.")
  }
  
  
  
}

forecast_reddit(date = "2021-05-01", city = "Atlanta", csv = TRUE)
test_atlanta <- reddit_and_cases %>% 
  filter(MSA_Code == "C1206" & Date >= "2020-05-01" & Date <= "2022-05-01")

for (i in test_atlanta$Date) {
  formatted_date <- as.character(as.Date(i))
  forecast_reddit(date = formatted_date, city = "Atlanta", csv = TRUE)
}


May_2_2021_Atlanta <- forecast_reddit(date = "2021-05-04", city = "Atlanta")
May_2_2021_Atlanta <- May_2_2021_Atlanta$projection
May_2_2021_Atlanta %>% 
  slice(c(8,15,22)) #%>% 
  mutate(Forecast_Date = )

folder_path <- "Results/Projections/Atlanta"
files <- list.files(folder_path)

pattern_index <- seq(1, length(files), by = 7)

data_list <- list()

for (i in pattern_index) {
  
}
