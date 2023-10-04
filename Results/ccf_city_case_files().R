library(tidyverse)
library(data.table)
library(readr)
library(zoo)

covid_cases <- read.csv("Covid Data/time_series_covid19_confirmed_US.csv")
covid_cases <- covid_cases %>% 
  mutate(across(starts_with("X"), ~ as.numeric(.)))
colnames(covid_cases) <- gsub("^X", "", colnames(covid_cases))

covid_deaths <- read.csv("Covid Data/time_series_covid19_deaths_US.csv")
#head(covid_deaths)
covid_deaths <- covid_deaths %>% 
  mutate(across(starts_with("X"), ~ as.numeric(.)))
colnames(covid_deaths) <- gsub("^X", "", colnames(covid_deaths))

crosswalk <- read.csv("modified_crosswalk.csv")
new_colnames <- gsub("\\.", "_", colnames(crosswalk))
colnames(crosswalk) <- new_colnames
new_crosswalk <- crosswalk %>% select(c(FIPS, MSA_Code, MSA_Title))

cases <- left_join(covid_cases, new_crosswalk, by = "FIPS") %>%
  mutate(across(starts_with("new_crosswalk"), ~ ifelse(is.na(FIPS), "NA", .))) %>% 
  select(UID, iso2, iso3, code3, FIPS, MSA_Code, MSA_Title, everything())
cases <- cases %>% 
  gather(key = "Date", value = "Value", "1.22.20":ncol(cases)) %>% 
  select(Date, everything()) 
cases$Date <- as.Date(cases$Date, format = "%m.%d.%y")
cases <- cases %>% rename(Cases = Value)

deaths <- left_join(covid_deaths, new_crosswalk, by = "FIPS") %>% 
  mutate(across(starts_with("new_crosswalk"), ~ ifelse(is.na(FIPS), "NA", .))) %>% 
  select(UID, iso2, iso3, code3, FIPS, MSA_Code, MSA_Title, everything())
deaths <- deaths %>% 
  gather(key = "Date", value = "Value", "1.22.20":ncol(deaths)) %>% 
  select(Date, everything()) 
deaths$Date <- as.Date(deaths$Date, format = "%m.%d.%y")
deaths <- deaths %>% rename(Deaths = Value)  

ccf_city_case_files <- function(folder_path, filename, explanatory = NULL, code = NULL, city = NULL,
                                state = NULL, plots = TRUE, lags = "both") {
  #folder path - give name of folder or path to folder
  #filename - give name of file within folder_path
  #explanatory - if not null, function only runs for one variable. If null, runs for all variables
  #code - give MSA Code starting with C
  #plots - If TRUE, graph plots. If false, don't graph CCF plots. 
  #lags - can be "negative", "positive", or "both"
  
  file_path <- file.path(folder_path, filename)
  #print(file_path)
  data <- fread(file_path)
  data$Date <- as.Date(data$Date)
  
  if (!is.null(code)) {
    city_msa_cases <- cases %>% 
      filter(MSA_Code == code) %>% 
      group_by(Date) %>% 
      summarize(Cases = sum(Cases))
    #set up for ccf
    city_msa_deaths <- deaths %>% 
      filter(MSA_Code == code) %>% 
      group_by(Date) %>% 
      summarize(Deaths = sum(Deaths))

    city_combined <- left_join(city_msa_cases, city_msa_deaths, by = "Date") %>% 
      mutate(Daily_Cases = Cases - lag(Cases, default = 0),
             Daily_Deaths = Deaths - lag(Deaths, default = 0),
             Daily_Cases7 = rollmean(Daily_Cases, k = 7, fill = NA),
             Daily_Deaths7 = rollmean(Daily_Deaths, k = 7, fill = NA))
    city_combined$Daily_Cases7 <- round(city_combined$Daily_Cases7, 2)
    city_combined$Daily_Deaths7 <- round(city_combined$Daily_Deaths7, 2)
    city_combined <- left_join(city_combined, data, by = "Date") %>% 
      na.omit()
    #return(city_combined)
    
    if (!is.null(explanatory)) {
      #run ccf for one variable
      if (plots) {
        #show plots
        ccf_result <- ccf(city_combined$Daily_Cases7, city_combined[[explanatory]], plot = FALSE, lag.max = 40)
        acf_table <- data.frame(Lag = ccf_result$lag, ACF = ccf_result$acf)
        acf_table <- acf_table %>%
          mutate(Sign = case_when(
            Lag <= 0 ~ "Lag < 0 ",
            TRUE ~ "Lag > 0"
          )) %>%
          group_by(Sign) %>% 
          summarise(Max_ACF = max(ACF), 
                    Lag = Lag[which.max(ACF)]) %>% 
          mutate(Variable = explanatory,
                 City = city,
                 State = state) %>% 
          select(Variable, City, State, Max_ACF, Lag)
        
        actual_column_name <- colnames(city_combined)[which(colnames(city_combined) == explanatory)]
        
        plot(ccf_result, main = paste("Daily Cases &", actual_column_name))
        return(acf_table)
        
        
      }
      else {
        #don't show plots
        ccf_result <- ccf(city_combined$Daily_Cases7, city_combined[[explanatory]], plot = FALSE, lag.max = 40)
        acf_table <- data.frame(Lag = ccf_result$lag, ACF = ccf_result$acf)
        acf_table <- acf_table %>%
          mutate(Sign = case_when(
            Lag <= 0 ~ "Lag < 0 ",
            TRUE ~ "Lag > 0"
          )) %>%
          group_by(Sign) %>% 
          summarise(Max_ACF = max(ACF), 
                    Lag = Lag[which.max(ACF)]) %>% 
          mutate(Variable = explanatory, 
                 City = city,
                 State = state) %>% 
          select(Variable, City, State, Max_ACF, Lag)
        
        actual_column_name <- colnames(city_combined)[which(colnames(city_combined) == explanatory)]
        #if (lags == "negative") {
        #  final_acf_table <- acf_table %>% 
        #    filter(Sign == "Lag < 0")
        #  print(final_acf_table)
        #}
        
        
        return(acf_table)
      }
    }
    
    else {
      #run ccf for multiple variables
      if (plots) {
        #show plots
        
        # Select columns to perform ccf on (excluding the first 7 columns)
        ccf_columns <- names(city_combined)[8:ncol(city_combined)]
        
        acf_tables_list <- list()
        
        for (column in ccf_columns) {
          ccf_result <- ccf(city_combined$Daily_Cases7, city_combined[[column]], plot = FALSE, lag.max = 40)
          
          acf_table <- data.frame(Lag = ccf_result$lag, ACF = ccf_result$acf)
          acf_table <- acf_table %>%
            mutate(Sign = case_when(
              Lag <= 0 ~ "Lag < 0",
              TRUE ~ "Lag > 0")) %>% 
            group_by(Sign) %>% 
            summarise(Max_ACF = max(ACF), 
                      Lag = Lag[which.max(ACF)]) %>% 
            mutate(Variable = column) %>% 
            select(Variable, City, State, Max_ACF, Lag)
          
          acf_tables_list[[column]] <- acf_table
          
          
          actual_column_name <- colnames(city_combined)[which(colnames(city_combined) == column)]
          
          plot(ccf_result, main = paste("Daily Cases &", actual_column_name))
        }
        combined_acf_table <- do.call(rbind, acf_tables_list)
        summary_table_positive <- combined_acf_table %>% 
          filter(Sign == "Lag > 0") %>%
          mutate(City = city,
                 State = state) %>% 
          select(-c(1)) %>% 
          arrange(desc(abs(Max_ACF)))
        summary_table_negative <- combined_acf_table %>% 
          filter(Sign == "Lag < 0") %>% 
          mutate(City = city, 
                 State = state) %>% 
          select(-c(1)) %>% 
          arrange(desc(abs(Max_ACF)))
        if (lags == "negative") {
          return(summary_table_negative)
        }
        else if (lags == "positive") {
          return(summary_table_positive)
        }
        else {
          return(combined_acf_table %>% select(Variable, City, State, Max_ACF, Lag) %>%
                   mutate(City = city, State = state))
        }
        
        
        
      }
      
      else {
        #don't show plots
        # Select columns to perform ccf on (excluding the first 7 columns)
        ccf_columns <- names(city_combined)[8:ncol(city_combined)]
        
        acf_tables_list <- list()
        
        for (column in ccf_columns) {
          ccf_result <- ccf(city_combined$Daily_Cases7, city_combined[[column]], plot = FALSE,
                            lag.max = 40)
          
          acf_table <- data.frame(Lag = ccf_result$lag, ACF = ccf_result$acf)
          acf_table <- acf_table %>%
            mutate(Sign = case_when(
              Lag <= 0 ~ "Lag < 0",
              TRUE ~ "Lag > 0")) %>% 
            group_by(Sign) %>% 
            summarise(Max_ACF = max(ACF), 
                      Lag = Lag[which.max(ACF)]) %>% 
            mutate(Variable = column)
          
          acf_tables_list[[column]] <- acf_table
          
        }
        combined_acf_table <- do.call(rbind, acf_tables_list)
        summary_table_positive <- combined_acf_table %>% 
          filter(Sign == "Lag > 0") %>%
          mutate(City = city,
                 State = state) %>% 
          select(-c(1)) %>% 
          arrange(desc(abs(Max_ACF)))
        summary_table_negative <- combined_acf_table %>% 
          filter(Sign == "Lag < 0") %>% 
          mutate(City = city, 
                 State = state) %>% 
          select(-c(1)) %>% 
          arrange(desc(abs(Max_ACF)))
        if (lags == "negative") {
          return(summary_table_negative)
        }
        else if (lags == "positive") {
          return(summary_table_positive)
        }
        else {
          return(combined_acf_table %>% 
                   select(-c(1)) %>% mutate(City = city, State = state))
        }
      }
    }
  }
  else {
    stop("Missing either state or city input")
  }
}


