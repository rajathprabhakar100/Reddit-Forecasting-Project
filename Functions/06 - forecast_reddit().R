library(tidyverse)
library(here)
library(zoo)
library(data.table)
library(conflicted)
library(ciTools)
library(forecast)
reddit_and_cases <- read_csv(here("Results/CSV Files/reddit_and_cases_deaths.csv"))

conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")
conflict_prefer("between", "dplyr")


forecast_reddit <- function(date = NULL, city, weeks = "3", csv=F) {
  city_training_data <- reddit_and_cases %>%
    filter(City == city) %>%
    mutate(illness7 = lag(rollmean(mean_illness, k = 7, align = "right", fill = NA, na.pad = T), n = 7),
           illness14 = lag(rollmean(mean_illness, k = 7, align = "right", fill = NA, na.pad = T), n = 14),
           illness21 = lag(rollmean(mean_illness, k = 7, align = "right", fill = NA, na.pad = T), n = 21)) %>%
    filter(between(Date, as.Date(date) - 27, as.Date(date))) %>% 
    select(Date, City, MSA_Code, Daily_Cases7, mean_illness, illness7, illness14, illness21)
  #print(city_training_data)
  
  city0_model <- lm(Daily_Cases7 ~ mean_illness + illness7 + illness14 + illness21, data = city_training_data)
  city7_model <- lm(Daily_Cases7 ~ illness7 + illness14 + illness21, data = city_training_data)
  city14_model <- lm(Daily_Cases7 ~ illness14 + illness21, data = city_training_data)
  city21_model <- lm(Daily_Cases7 ~ illness21, data = city_training_data)
  
  city0_model_summary <- summary(city0_model)
  city7_model_summary <- summary(city7_model)
  city14_model_summary <- summary(city14_model)
  city21_model_summary <- summary(city21_model)
  #print(city7_model_summary)
  #print(city14_model_summary)
  #print(city21_model_summary)
  
  city_projection_data <- reddit_and_cases %>% 
    filter(City == city) %>% 
    mutate(illness7 = lag(rollmean(mean_illness, k = 7, align = "right", fill = NA, na.pad = T), n = 7),
           illness14 = lag(rollmean(mean_illness, k = 7, align = "right", fill = NA, na.pad = T), n = 14),
           illness21 = lag(rollmean(mean_illness, k = 7, align = "right", fill = NA, na.pad = T), n = 21)) #%>%
    #na.omit()

  if (weeks == 3) {
    city_projection_data <- city_projection_data %>% 
      filter(between(Date, as.Date(date), as.Date(date) + 21)) %>% 
      mutate(R2 = case_when(
        row_number() == 1 ~ city0_model_summary$adj.r.squared,
        row_number() > 1 & row_number() <= 8 ~ city7_model_summary$adj.r.squared,
        row_number() > 8 & row_number() <= 15 ~ city14_model_summary$adj.r.squared,
        row_number() > 15 ~ city21_model_summary$adj.r.squared
      )) %>% 
      mutate(
        Forecast = case_when(
          row_number() == 1 ~ predict(city0_model, newdata = .,
                                      interval = "prediction", level = 0.95)[, "fit"],
          row_number() > 1 & row_number() <= 8 ~ predict(city7_model, newdata = .,
                                                         interval = "prediction", level = 0.95)[, "fit"],
          row_number() > 8 & row_number() <= 15 ~ predict(city14_model,newdata = .,
                                                          interval = "prediction", 
                                                          level = 0.95)[, "fit"],
          row_number() > 15 ~ predict(city21_model, newdata = .,
                                      interval = "prediction", level = 0.95)[, "fit"],
          TRUE ~ NA_real_),
        Forecast_Lwr = case_when(
          row_number() == 1 ~ predict(city0_model, newdata = ., interval = "prediction", level = 0.95)[, "lwr"],
          row_number() > 1 & row_number() <= 8 ~ predict(city7_model, newdata = ., interval = "prediction", level = 0.95)[, "lwr"],
          row_number() > 8 & row_number() <= 15 ~ predict(city14_model, newdata = ., interval = "prediction", level = 0.95)[, "lwr"],
          row_number() > 15 ~ predict(city21_model, newdata = ., interval = "prediction", level = 0.95)[, "lwr"],
          TRUE ~ NA_real_
        ),
        Forecast_Upr = case_when(
          row_number() == 1 ~ predict(city0_model, newdata = ., interval = "prediction", level = 0.95)[, "upr"],
          row_number() > 1 & row_number() <= 8 ~ predict(city7_model, newdata = ., interval = "prediction", level = 0.95)[, "upr"],
          row_number() > 8 & row_number() <= 15 ~ predict(city14_model, newdata = ., interval = "prediction", level = 0.95)[, "upr"],
          row_number() > 15 ~ predict(city21_model, newdata = ., interval = "prediction", level = 0.95)[, "upr"],
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
                     summary_0days = city0_model_summary,
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

#forecast_reddit1 - for 8 weeks of training data 
forecast_reddit1 <- function(date = NULL, city, weeks = "3", csv=F) {
  #conflict_prefer("select", "dplyr")
  #conflict_prefer("filter", "dplyr")
  #conflict_prefer("lag", "dplyr")
  #conflict_prefer("between", "dplyr")
  city_training_data <- reddit_and_cases %>%
    filter(City == city) %>%
    mutate(illness7 = lag(rollmean(mean_illness, k = 7, align = "right", fill = NA, na.pad = T), n = 7),
           illness14 = lag(rollmean(mean_illness, k = 7, align = "right", fill = NA, na.pad = T), n = 14),
           illness21 = lag(rollmean(mean_illness, k = 7, align = "right", fill = NA, na.pad = T), n = 21)) %>%
    filter(between(Date, as.Date(date) - 55, as.Date(date))) %>% 
    select(Date, City, MSA_Code, Daily_Cases7, mean_illness, illness7, illness14, illness21)
  #print(city_training_data)
  
  city0_model <- lm(Daily_Cases7 ~ mean_illness + illness7 + illness14 + illness21, data = city_training_data)
  city7_model <- lm(Daily_Cases7 ~ illness7 + illness14 + illness21, data = city_training_data)
  city14_model <- lm(Daily_Cases7 ~ illness14 + illness21, data = city_training_data)
  city21_model <- lm(Daily_Cases7 ~ illness21, data = city_training_data)
  
  city_0_model_summary <- summary(city0_model)
  city7_model_summary <- summary(city7_model)
  city14_model_summary <- summary(city14_model)
  city21_model_summary <- summary(city21_model)
  #print(city7_model_summary)
  #print(city14_model_summary)
  #print(city21_model_summary)
  
  city_projection_data <- reddit_and_cases %>% 
    filter(City == city) %>% 
    mutate(illness7 = lag(rollmean(mean_illness, k = 7, align = "right", fill = NA, na.pad = T), n = 7),
           illness14 = lag(rollmean(mean_illness, k = 7, align = "right", fill = NA, na.pad = T), n = 14),
           illness21 = lag(rollmean(mean_illness, k = 7, align = "right", fill = NA, na.pad = T), n = 21)) #%>%
  #na.omit()
  
  if (weeks == 3) {
    city_projection_data <- city_projection_data %>% 
      filter(between(Date, as.Date(date), as.Date(date) + 21)) %>% 
      mutate(R2 = case_when(
        row_number() == 1 ~ city_0_model_summary$adj.r.squared,
        row_number() > 1 & row_number() <= 8 ~ city7_model_summary$adj.r.squared,
        row_number() > 8 & row_number() <= 15 ~ city14_model_summary$adj.r.squared,
        row_number() > 15 ~ city21_model_summary$adj.r.squared
      )) %>% 
      mutate(
        Forecast = case_when(
          row_number() == 1 ~ predict(city0_model, newdata = ., interval = "prediction", level = 0.95)[, "fit"],
          row_number() > 1 & row_number() <= 8 ~ predict(city7_model, newdata = ., interval = "prediction", level = 0.95)[, "fit"],
          row_number() > 8 & row_number() <= 15 ~ predict(city14_model, 
                                                          newdata = .,
                                                          interval = "prediction", 
                                                          level = 0.95)[, "fit"],
          row_number() > 15 ~ predict(city21_model, newdata = ., interval = "prediction", level = 0.95)[, "fit"],
          TRUE ~ NA_real_),
        Forecast_Lwr = case_when(
          row_number() == 1 ~ predict(city0_model, newdata = ., interval = "prediction", level = 0.95)[, "lwr"],
          row_number() > 1 & row_number() <= 8 ~ predict(city7_model, newdata = ., interval = "prediction", level = 0.95)[, "lwr"],
          row_number() > 8 & row_number() <= 15 ~ predict(city14_model, newdata = ., interval = "prediction", level = 0.95)[, "lwr"],
          row_number() > 15 ~ predict(city21_model, newdata = ., interval = "prediction", level = 0.95)[, "lwr"],
          TRUE ~ NA_real_
        ),
        Forecast_Upr = case_when(
          row_number() == 1 ~ predict(city0_model, newdata = ., interval = "prediction", level = 0.95)[, "upr"],
          row_number() > 1 & row_number() <= 8 ~ predict(city7_model, newdata = ., interval = "prediction", level = 0.95)[, "upr"],
          row_number() > 8 & row_number() <= 15 ~ predict(city14_model, newdata = ., interval = "prediction", level = 0.95)[, "upr"],
          row_number() > 15 ~ predict(city21_model, newdata = ., interval = "prediction", level = 0.95)[, "upr"],
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

#forecast_reddit2 - for cumulative training data
forecast_reddit2 <- function(date = NULL, city, weeks = "3", csv=F) {
  #conflict_prefer("select", "dplyr")
  #conflict_prefer("filter", "dplyr")
  #conflict_prefer("lag", "dplyr")
  #conflict_prefer("between", "dplyr")
  city_training_data <- reddit_and_cases %>%
    filter(City == city) %>%
    mutate(illness7 = lag(rollmean(mean_illness, k = 7, align = "right", fill = NA, na.pad = T), n = 7),
           illness14 = lag(rollmean(mean_illness, k = 7, align = "right", fill = NA, na.pad = T), n = 14),
           illness21 = lag(rollmean(mean_illness, k = 7, align = "right", fill = NA, na.pad = T), n = 21)) %>%
    filter(between(Date, as.Date("2020-05-01"), as.Date(date))) %>% 
    select(Date, City, MSA_Code, Daily_Cases7, mean_illness, illness7, illness14, illness21)
  #print(city_training_data)
  
  city0_model <- lm(Daily_Cases7 ~ mean_illness + illness7 + illness14 + illness21, data = city_training_data)
  city7_model <- lm(Daily_Cases7 ~ illness7 + illness14 + illness21, data = city_training_data)
  city14_model <- lm(Daily_Cases7 ~ illness14 + illness21, data = city_training_data)
  city21_model <- lm(Daily_Cases7 ~ illness21, data = city_training_data)
  
  city0_model_summary <- summary(city0_model)
  city7_model_summary <- summary(city7_model)
  city14_model_summary <- summary(city14_model)
  city21_model_summary <- summary(city21_model)
  #print(city7_model_summary)
  #print(city14_model_summary)
  #print(city21_model_summary)
  
  city_projection_data <- reddit_and_cases %>% 
    filter(City == city) %>% 
    mutate(illness7 = lag(rollmean(mean_illness, k = 7, align = "right", fill = NA, na.pad = T), n = 7),
           illness14 = lag(rollmean(mean_illness, k = 7, align = "right", fill = NA, na.pad = T), n = 14),
           illness21 = lag(rollmean(mean_illness, k = 7, align = "right", fill = NA, na.pad = T), n = 21)) #%>%
  #na.omit()
  
  if (weeks == 3) {
    city_projection_data <- city_projection_data %>% 
      filter(between(Date, as.Date(date), as.Date(date) + 21)) %>% 
      mutate(R2 = case_when(
        row_number() == 1 ~ city0_model_summary$adj.r.squared,
        row_number() > 1 & row_number() <= 8 ~ city7_model_summary$adj.r.squared,
        row_number() > 8 & row_number() <= 15 ~ city14_model_summary$adj.r.squared,
        row_number() > 15 ~ city21_model_summary$adj.r.squared
      )) %>% 
      mutate(
        Forecast = case_when(
          row_number() == 1 ~ predict(city0_model, newdata = ., interval = "prediction", level = 0.95)[, "fit"],
          row_number() > 1 & row_number() <= 8 ~ predict(city7_model, newdata = ., interval = "prediction", level = 0.95)[, "fit"],
          row_number() > 8 & row_number() <= 15 ~ predict(city14_model, 
                                                          newdata = .,
                                                          interval = "prediction", 
                                                          level = 0.95)[, "fit"],
          row_number() > 15 ~ predict(city21_model, newdata = ., interval = "prediction", level = 0.95)[, "fit"],
          TRUE ~ NA_real_),
        Forecast_Lwr = case_when(
          row_number() == 1 ~ predict(city0_model, newdata = ., interval = "prediction", level = 0.95)[, "lwr"],
          row_number() > 1 & row_number() <= 8 ~ predict(city7_model, newdata = ., interval = "prediction", level = 0.95)[, "lwr"],
          row_number() > 8 & row_number() <= 15 ~ predict(city14_model, newdata = ., interval = "prediction", level = 0.95)[, "lwr"],
          row_number() > 15 ~ predict(city21_model, newdata = ., interval = "prediction", level = 0.95)[, "lwr"],
          TRUE ~ NA_real_
        ),
        Forecast_Upr = case_when(
          row_number() == 1 ~ predict(city0_model, newdata = ., interval = "prediction", level = 0.95)[, "upr"],
          row_number() > 1 & row_number() <= 8 ~ predict(city7_model, newdata = ., interval = "prediction", level = 0.95)[, "upr"],
          row_number() > 8 & row_number() <= 15 ~ predict(city14_model, newdata = ., interval = "prediction", level = 0.95)[, "upr"],
          row_number() > 15 ~ predict(city21_model, newdata = ., interval = "prediction", level = 0.95)[, "upr"],
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

#Poisson - 4 weeks 
forecast_reddit_poisson_4 <- function(date = NULL, city, weeks = "3", csv=F) {
  city_training_data <- reddit_and_cases %>%
    mutate(Daily_Cases7 = round(Daily_Cases7,0)) %>%
    filter(City == city) %>%
    mutate(illness7 = lag(rollmean(mean_illness, k = 7, align = "right", fill = NA, na.pad = T), n = 7),
           illness14 = lag(rollmean(mean_illness, k = 7, align = "right", fill = NA, na.pad = T), n = 14),
           illness21 = lag(rollmean(mean_illness, k = 7, align = "right", fill = NA, na.pad = T), n = 21),
           cases_lag7 = lag(Daily_Cases, n = 7)) %>%
    filter(between(Date, as.Date(date) - 27, as.Date(date))) %>% 
    select(Date, City, MSA_Code, Daily_Cases, cases_lag7, mean_illness, illness7, illness14, illness21)
  #print(city_training_data)
  suppressWarnings({
    city0_model <- glm(Daily_Cases ~ mean_illness + illness7 + illness14 + illness21 + cases_lag7,
                       data = city_training_data, family = poisson)
    city7_model <- glm(Daily_Cases ~ illness7 + illness14 + illness21 + cases_lag7,
                       data = city_training_data, family = poisson)
  })
  
  city0_model_summary <- summary(city0_model)
  city7_model_summary <- summary(city7_model)
  #print(city7_model_summary)
  #print(city14_model_summary)
  #print(city21_model_summary)
  
  city_projection_data <- reddit_and_cases %>% 
    filter(City == city) %>% 
    mutate(illness7 = lag(rollmean(mean_illness, k = 7, align = "right", fill = NA, na.pad = T), n = 7),
           illness14 = lag(rollmean(mean_illness, k = 7, align = "right", fill = NA, na.pad = T), n = 14),
           illness21 = lag(rollmean(mean_illness, k = 7, align = "right", fill = NA, na.pad = T), n = 21),
           cases_lag7 = lag(Daily_Cases, n = 7)) %>% 
    filter(between(Date, as.Date(date) + 1 , as.Date(date) + 8))
  city_projection_data1 <- city_projection_data %>% 
    slice(1) %>% 
    add_pi(df = ., fit = city0_model, names = c("Forecast_Lwr", "Forecast_Upr"))
  city_projection_data2 <- city_projection_data %>% 
    slice(-1) %>% 
    add_pi(df = ., fit = city7_model, names = c("Forecast_Lwr", "Forecast_Upr"))
  city_projection_data <- bind_rows(city_projection_data1, city_projection_data2) %>% 
    rename(Forecast = pred)
    
  return(city_projection_data)
  

}




forecast_reddit_poisson_8_ar <- function(date = NULL, city, weeks = "3", csv=F) {
  city_training_data <- reddit_and_cases %>%
    filter(City == city) %>%
    mutate(illness7 = lag(mean_illness, n = 1),
           illness14 = lag(mean_illness, n = 2),
           illness21 = lag(mean_illness, n = 3),
           cases_lag7 = lag(Weekly_Cases, n = 1)) %>%
    filter(between(week, as.Date(date) - 56, as.Date(date) - 1)) %>% 
    select(week, City, MSA_Code, Weekly_Cases, mean_illness, illness7, illness14, illness21,
           cases_lag7)
  #print(city_training_data)
  suppressWarnings({
    city0_model <- glm(Weekly_Cases ~ mean_illness + illness7 + illness14 + illness21 + cases_lag7,
                       data = city_training_data, family = poisson)
    city7_model <- glm(Weekly_Cases ~ illness7 + illness14 + illness21 + cases_lag7,
                       data = city_training_data, family = poisson)
    })
    
  city0_model_summary <- summary(city0_model)
  city7_model_summary <- summary(city7_model)

  #print(city7_model_summary)
  #print(city14_model_summary)
  #print(city21_model_summary)
  city_projection_data <- reddit_and_cases %>% 
    filter(City == city) %>% 
    mutate(illness7 = lag(mean_illness, n = 1),
           illness14 = lag(mean_illness, n = 2),
           illness21 = lag(mean_illness, n = 3),
           cases_lag7 = lag(Weekly_Cases, n = 1)) %>% 
    filter(between(week, as.Date(date), as.Date(date) + 7))
  suppressWarnings({
    city_projection_data1 <- city_projection_data %>% 
      slice(1) %>% 
      add_pi(df = ., fit = city0_model, names = c("Forecast_Lwr", "Forecast_Upr"))
    city_projection_data2 <- city_projection_data %>% 
      slice(-1) %>% 
      add_pi(df = ., fit = city7_model, names = c("Forecast_Lwr", "Forecast_Upr"))
  })
  city_projection_data <- bind_rows(city_projection_data1, city_projection_data2) %>% 
    rename(Forecast = pred) %>% 
    select(week, MSA_Code, MSA_Title, City, Forecast_Lwr, Forecast, Forecast_Upr, everything())
  return(city_projection_data)
  
  
}

forecast_reddit_poisson_8 <- function(date = NULL, city, weeks = "3", csv=F) {
  city_training_data <- reddit_and_cases %>%
    filter(City == city) %>%
    mutate(illness7 = lag(mean_illness, n = 1),
           illness14 = lag(mean_illness, n = 2),
           illness21 = lag(mean_illness, n = 3)) %>%
    filter(between(week, as.Date(date) - 56, as.Date(date) - 1)) %>% 
    select(week, City, MSA_Code, Weekly_Cases, mean_illness, illness7, illness14, illness21)
  #print(city_training_data)
  suppressWarnings({
    city0_model <- glm(Weekly_Cases ~ mean_illness + illness7 + illness14 + illness21,
                       data = city_training_data, family = poisson)
    city7_model <- glm(Weekly_Cases ~ illness7 + illness14 + illness21,
                       data = city_training_data, family = poisson)
  })
  
  city0_model_summary <- summary(city0_model)
  city7_model_summary <- summary(city7_model)
  
  #print(city7_model_summary)
  #print(city14_model_summary)
  #print(city21_model_summary)
  city_projection_data <- reddit_and_cases %>% 
    filter(City == city) %>% 
    mutate(illness7 = lag(mean_illness, n = 1),
           illness14 = lag(mean_illness, n = 2),
           illness21 = lag(mean_illness, n = 3)) %>% 
    filter(between(week, as.Date(date), as.Date(date) + 7))
  suppressWarnings({
    city_projection_data1 <- city_projection_data %>% 
      slice(1) %>% 
      add_pi(df = ., fit = city0_model, names = c("Forecast_Lwr", "Forecast_Upr"))
    
    city_projection_data2 <- city_projection_data %>% 
      slice(-1) %>% 
      add_pi(df = ., fit = city7_model, names = c("Forecast_Lwr", "Forecast_Upr"))
  })
  city_projection_data <- bind_rows(city_projection_data1, city_projection_data2) %>% 
    rename(Forecast = pred) %>% 
    select(week, MSA_Code, MSA_Title, City, Forecast_Lwr, Forecast, Forecast_Upr, everything())
  return(city_projection_data)
  
  
}

get_0week_forecasts <- function(cutoff_date, city, explanatory = NULL, case_lag = 0, reddit_lag = 0) {
  city_data <- reddit_and_cases %>% 
    filter(City == city)
  cutoff_date <- as.Date(cutoff_date)
  
  if (!is.null(explanatory)) {
    explanatory <- as.character(explanatory)
    invalid_vars <- setdiff(explanatory, colnames(reddit_and_cases))

    if (length(invalid_vars) > 0) {
      stop(paste("The following variables are not in the data frame:", paste(invalid_vars, collapse = ", ")))
    }
    
  }
  ##Check Case Lag##
  if (case_lag > 0) {
    city_data <- city_data %>% 
      mutate(!!!set_names(map(1:case_lag, ~ lag(city_data$Weekly_Cases, .x)), paste0("Weekly_Cases", 1:case_lag)))
    }
    
  else if (case_lag < 0) {
    stop("Invalid case lag")
  }
  
  ##Check Reddit Lag##
  if (reddit_lag > 0) {
    for (var in explanatory) {
      city_data <- city_data %>% 
        mutate(!!!set_names(map(1:reddit_lag, ~lag(city_data[[var]], .x)), paste0(var, 1:reddit_lag)))
    }
  }
  
  else if (reddit_lag < 0) {
    stop("Invalid reddit lag")
  }

  data_for_fitting <- city_data %>%
    filter(week < cutoff_date)

  data_for_predicting <- city_data %>% 
    filter(week == cutoff_date)

  if (all(data_for_fitting$Weekly_Cases == 0)) {
    # Directly return zero forecast if all cases are zero
    fcast <- data_for_predicting %>%
      mutate(`Point Forecast` = 0, `Lo 95` = 0, `Lo 80` = 0, `Hi 80` = 0, `Hi 95` = 0)
  } 
  
  else {
    
    if (!is.null(explanatory)) {
      full_fit <- suppressWarnings(auto.arima(data_for_fitting$Weekly_Cases,
                                              xreg = data_for_fitting %>% select(starts_with(explanatory),
                                                                                 matches("^Weekly_Cases\\d+$")) %>% 
                                                as.matrix()))
    }
    
    else {
      full_fit <- suppressWarnings(auto.arima(data_for_fitting$Weekly_Cases))
    }
    
    model_coeffs <- coef(full_fit)
    
    # Initialize a row with the cutoff date and xreg (external regressor) coefficient
    row <- data.frame(date = format(cutoff_date, "%Y-%m-%d"))
    
    # Loop through coefficients and dynamically add AR and MA terms to the row
    for (name in names(model_coeffs)) {
      row[[name]] <- model_coeffs[name]
    }
    
    row <- row %>% select(date, starts_with("ar"), starts_with("ma"), everything())
    
    if (!is.null(explanatory)) {
      xreg_predict <- data_for_predicting %>% select(starts_with(explanatory),
                                                     matches("^Weekly_Cases\\d+$")) %>%
        as.matrix()
    }
    
    else {
      xreg_predict <- NULL
    }
    
    
    fcast <- suppressWarnings(forecast(full_fit,
                                          h = nrow(data_for_predicting),
                                          xreg = xreg_predict)) %>%
      as_tibble() %>%
      bind_cols(data_for_predicting) %>% 
      mutate_at(vars(`Point Forecast`:`Hi 95`), function(x) {ifelse(x < 0, 0, x)}) %>% 
      select(week, MSA_Code, MSA_Title, `Lo 95`, `Lo 80`,
             `Point Forecast`, `Hi 80`, `Hi 95`, Weekly_Cases, everything())
  }
  
  results <- list(forecast = fcast,
                  model = row)

  return(results)
}

get_1week_forecasts <- function(cutoff_date, city, explanatory = NULL, case_lag = 0, reddit_lag = 0,
                                training_period = NULL) {
  
  city_data <- reddit_and_cases %>% 
    filter(City == city)
  
  cutoff_date <- as.Date(cutoff_date)
  
  if (!is.null(explanatory)) {
    explanatory <- as.character(explanatory)
    
    if (!(explanatory %in% colnames(reddit_and_cases))) {
      stop(paste(explanatory, "not in data frame."))
    }
    
    ##Check Case Lag##
    if (case_lag > 0) {
      city_data <- city_data %>% 
        mutate(!!!set_names(map(1:case_lag, ~ lag(city_data$Weekly_Cases, .x)),
                            paste0("Weekly_Cases", 1:case_lag)))
    }
    else if (case_lag < 0) {
      stop("Invalid case lag")
    }
    
    #Check Reddit Lag##
    if (reddit_lag > 0) {
      city_data <- city_data %>% 
        mutate(!!!set_names(map(1:reddit_lag, ~ lag(city_data[[explanatory]], .x)),
                            paste0(explanatory, 1:reddit_lag)))
    }
    
    else if (reddit_lag < 0) {
      stop("Invalid reddit lag")
    }
  }
  data_for_fitting <- city_data %>%
    filter(week < cutoff_date)
  
  data_for_predicting <- city_data %>% 
    filter(week == cutoff_date + weeks(1))
  

  if (all(data_for_fitting$Weekly_Cases == 0)) {
    # Directly return zero forecast if all cases are zero
    fcast <- data_for_predicting %>%
      mutate(`Point Forecast` = 0, `Lo 95` = 0, `Lo 80` = 0, `Hi 80` = 0, `Hi 95` = 0)
  } 
  
  else {
    if (!is.null(explanatory)) {
      xreg <- data_for_fitting %>% 
        select(matches("^Weekly_Cases\\d+$"), starts_with(explanatory)) %>% 
        as.matrix()
      
      full_fit <- suppressWarnings(auto.arima(data_for_fitting$Weekly_Cases, xreg = xreg))
    }
    
    else {
      full_fit <- suppressWarnings(auto.arima(data_for_fitting$Weekly_Cases))
    }
    
    
    if (!is.null(explanatory)) {
      xreg_predict <- data_for_predicting %>% 
        select(matches("^Weekly_Cases\\d+$"), starts_with(explanatory)) %>% 
        as.matrix()
    } 
    
    else {
      xreg_predict <- NULL
    }
    
    fcast <- suppressWarnings(forecast(full_fit, h = 1, xreg = xreg_predict)) %>%
      as_tibble() %>%
      bind_cols(data_for_predicting) %>% 
      mutate(across(`Point Forecast`:`Hi 95`, ~ ifelse(. < 0, 0, .))) %>% 
      select(week, MSA_Code, MSA_Title, `Lo 95`, `Lo 80`, `Point Forecast`, `Hi 80`, `Hi 95`, Weekly_Cases, everything())
  }
  
  # Collect model coefficients
  model_coeffs <- coef(full_fit)
  row <- data.frame(date = format(cutoff_date, "%Y-%m-%d"))
  for (name in names(model_coeffs)) {
    row[[name]] <- model_coeffs[name]
  }
  row <- row %>% select(date, starts_with("ar"), starts_with("ma"), everything())
  
  results <- list(forecast = fcast, model = row)
  return(results)
}
