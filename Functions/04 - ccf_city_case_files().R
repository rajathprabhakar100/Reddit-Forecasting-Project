library(tidyverse)
library(data.table)
library(zoo)
library(here)
library(readxl)
covid_cases <- read_csv(here("Source Data/Covid Data/time_series_covid19_confirmed_US.csv")) #%>%
  #mutate(across(starts_with("X"), ~ as.numeric(.)))
#colnames(covid_cases) <- gsub("^X", "", colnames(covid_cases))

covid_deaths <- read_csv(here("Source Data/Covid Data/time_series_covid19_deaths_US.csv")) %>% 
#head(covid_deaths)
  mutate(across(starts_with("X"), ~ as.numeric(.)))
colnames(covid_deaths) <- gsub("^X", "", colnames(covid_deaths))

census_data <- read_excel("Source Data/co-est2022-pop.xlsx", skip = 4)
census_data <- census_data %>% 
  rename(County_Title = `United States`,
         estpop2020 = `331449520`,
         pop2020 = `331511512`,
         pop2021 = `332031554`,
         pop2022 = `333287557`)
census_data$County_Title <- sub("^\\.", "", census_data$County_Title)

crosswalk <- read.csv(here("Source Data/modified_crosswalk.csv"))
new_colnames <- gsub("\\.", "_", colnames(crosswalk))
colnames(crosswalk) <- new_colnames
new_crosswalk <- crosswalk %>% select(FIPS, County_Title, MSA_Code, MSA_Title) %>% 
  left_join(census_data, by = "County_Title")

cases <- left_join(covid_cases, new_crosswalk, by = "FIPS") %>%
  mutate(across(starts_with("new_crosswalk"), ~ ifelse(is.na(FIPS), "NA", .))) %>% 
  select(UID, iso2, iso3, code3, FIPS, MSA_Code, MSA_Title, Admin2, estpop2020, pop2020, everything()) %>% 
  select(-c(pop2021, pop2022))
cases <- cases %>% 
  gather(key = "Date", value = "Value", "1/22/20":ncol(cases)) %>% 
  select(Date, everything()) 

cases$Date <- as.Date(cases$Date, format = "%m/%d/%y")
cases <- cases %>% rename(Cases = Value)
cases$Cases <- as.numeric(cases$Cases)

cases <- cases %>% 
  mutate(week = floor_date(Date, "week")) %>% 
  select(-Date) %>% 
  select(week, everything())

deaths <- left_join(covid_deaths, new_crosswalk, by = "FIPS") %>% 
  mutate(across(starts_with("new_crosswalk"), ~ ifelse(is.na(FIPS), "NA", .))) %>% 
  select(UID, iso2, iso3, code3, FIPS, MSA_Code, MSA_Title, Admin2, estpop2020, pop2020, everything()) %>% 
  select(-c(pop2021, pop2022))
deaths <- deaths %>% 
  gather(key = "Date", value = "Value", "1/22/20":ncol(deaths)) %>% 
  select(Date, everything()) 
deaths$Date <- as.Date(deaths$Date, format = "%m/%d/%y")
deaths <- deaths %>% rename(Deaths = Value)
deaths$Deaths <- as.numeric(deaths$Deaths)

deaths <- deaths %>% 
  mutate(week = floor_date(Date, "week")) %>%
  select(-Date) %>% 
  select(week, everything())

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
  data <- fread("Source Data/Weekly Data/atlanta_weekly.csv") %>% 
    mutate(week = as_date(week))
  
  if (!is.null(code)) {
    city_msa_cases <- cases %>% 
      filter(MSA_Code == code) %>% 
      group_by(week, MSA_Code, MSA_Title, FIPS, Admin2) %>% 
      summarize(Cumulative_Cases = Cases,
                Est_Population = unique(estpop2020),
                Population = unique(pop2020)) %>% 
      ungroup() %>% 
      group_by(FIPS, Admin2,MSA_Code, MSA_Title, week) %>% 
      slice_tail(n = 1) %>% 
      na.omit() %>% 
      ungroup() %>% 
      group_by(week, MSA_Code, MSA_Title) %>%
      summarize(Cumulative_Cases = sum(Cumulative_Cases),
                Est_Population = sum(Est_Population),
                Population = sum(Population)) %>% 
      mutate(week = as_date(week))
    
    #set up for ccf
    city_msa_deaths <- deaths %>% 
      filter(MSA_Code == code) %>% 
      group_by(week, MSA_Code, MSA_Title, FIPS, Admin2) %>% 
      summarize(Cumulative_Deaths = Deaths,
                Est_Population = unique(estpop2020),
                Population = unique(pop2020)) %>% 
      ungroup() %>% 
      group_by(FIPS, Admin2, MSA_Code, MSA_Title, week) %>% 
      slice_tail(n = 1) %>% 
      na.omit() %>% 
      ungroup() %>% 
      group_by(week, MSA_Code, MSA_Title) %>%
      summarize(Cumulative_Deaths = sum(Cumulative_Deaths),
                Est_Population = sum(Est_Population),
                Population = sum(Population)) %>% 
      mutate(week = as_date(week))
    
    city_combined <- left_join(city_msa_cases, city_msa_deaths, by = "week") %>%
      ungroup() %>% 
      rename(MSA_Code = MSA_Code.x,
             MSA_Title = MSA_Title.x,
             Est_Population = Est_Population.x,
             Population = Population.x) %>% 
      select(-c(Est_Population.y, Population.y, MSA_Code.y, MSA_Title.y)) %>% 
      arrange(week) %>% 
      mutate(Weekly_Cases = Cumulative_Cases - lag(Cumulative_Cases, n = 1, default = 0),
             Weekly_Deaths = Cumulative_Deaths - lag(Cumulative_Deaths, n = 1, default = 0))
    city_combined <- left_join(city_combined, data, by = "week") %>% 
      na.omit()
    
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
        
        plot(ccf_result, main = paste("Daily Cases &", actual_column_name, "for", city, ",", state))
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
        ccf_plots <- list()
        
        for (column in ccf_columns) {
          ccf_result <- ccf(city_combined$Weekly_Cases, city_combined[[column]], plot = FALSE, lag.max = 40)
          
          acf_table <- data.frame(Lag = ccf_result$lag, ACF = ccf_result$acf)
          acf_table <- acf_table %>%
            mutate(Sign = case_when(
              Lag <= 0 ~ "Lag < 0",
              TRUE ~ "Lag > 0")) %>% 
            group_by(Sign) %>% 
            summarise(Max_ACF = max(ACF), 
                      Lag = Lag[which.max(ACF)]) %>% 
            mutate(Variable = column,
                   City = city,
                   State = state) %>% 
            select(Variable, City, State, Max_ACF, Lag, Sign)
          
          acf_tables_list[[column]] <- acf_table
          
          
          actual_column_name <- colnames(city_combined)[which(colnames(city_combined) == column)]
          
          plot <- plot(ccf_result, main = paste("Weekly Cases &", actual_column_name, "for", city,",", state))
          ccf_plots[[column]] <- plot
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
        return(ccf_plots)
        
        
      }
      
      else {
        #don't show plots
        # Select columns to perform ccf on (excluding the first 7 columns)
        ccf_columns <- names(city_combined)[8:ncol(city_combined)]
        
        acf_tables_list <- list()
        
        for (column in ccf_columns) {
          ccf_result <- ccf(city_combined$Weekly_Cases, city_combined[[column]], plot = FALSE,
                            lag.max = 40)
          
          acf_table <- data.frame(Lag = ccf_result$lag, ACF = ccf_result$acf)
          acf_table <- acf_table %>%
            mutate(Sign = case_when(
              Lag <= 0 ~ "Lag < 0",
              TRUE ~ "Lag > 0")) %>% 
            group_by(Sign) %>% 
            summarise(Max_ACF = max(ACF), 
                      Lag = Lag[which.max(ACF)]) %>% 
            mutate(Variable = column,
                   City = city,
                   State = state,
                   Est_Population = mean(city_combined$Est_Population)) %>% 
            select(Variable, City, State, Max_ACF, Lag, Sign)
          #return(acf_table)
          
          acf_tables_list[[column]] <- acf_table
          
        }
        combined_acf_table <- do.call(rbind, acf_tables_list)
        summary_table_positive <- combined_acf_table %>% 
          filter(Sign == "Lag > 0") %>%
          mutate(City = city,
                 State = state) %>% 
          arrange(desc(abs(Max_ACF)))
        summary_table_negative <- combined_acf_table %>% 
          filter(Sign == "Lag < 0") %>% 
          mutate(City = city, 
                 State = state) %>% 
          arrange(desc(abs(Max_ACF)))
        if (lags == "negative") {
          return(summary_table_negative)
        }
        else if (lags == "positive") {
          return(summary_table_positive)
        }
        else {
          return(combined_acf_table %>%
                   mutate(City = city, 
                          State = state,
                          Est_Population = mean(city_combined$Est_Population),
                          Population = mean(city_combined$Population)))
        }
      }
    }
  }
  else {
    stop("Missing either state or city input")
  }
}

ccf_tables <- function(folder_path, filename, explanatory = NULL, code = NULL, city = NULL,
                       state = NULL) {
  file_path <- file.path(folder_path, filename)
  #print(file_path)
  data <- fread(here(file_path))
  data$week <- as.Date(data$week)
  
  city_msa_cases <- cases %>% 
    filter(MSA_Code == code) %>% 
    group_by(week) %>% 
    summarize(Cases = sum(Cases),
              Est_Population = sum(estpop2020),
              Population = sum(pop2020))
  
  #set up for ccf
  city_msa_deaths <- deaths %>% 
    filter(MSA_Code == code) %>% 
    group_by(week) %>% 
    summarize(Deaths = sum(Deaths),
              Est_Population = sum(estpop2020),
              Population = sum(pop2020))
  
  city_combined <- left_join(city_msa_cases, city_msa_deaths, by = "week") %>% 
    mutate(Weekly_Cases = Cases - lag(Cases, default = 0),
           Weekly_Deaths = Deaths - lag(Deaths, default = 0))
  city_combined <- left_join(city_combined, data, by = "week") %>% 
    na.omit() %>% 
    rename(Est_Population = Est_Population.x,
           Population = Population.x) %>% 
    select(-c(Est_Population.y, Population.y))
  ccf_columns <- names(city_combined)[8:ncol(city_combined)]
  
  acf_tables_list <- list()
  
  for (column in ccf_columns) {
    ccf_result <- ccf(city_combined$Weekly_Cases, city_combined[[column]], plot = FALSE,
                      lag.max = 40)
    
    acf_table <- data.frame(Lag = ccf_result$lag, ACF = ccf_result$acf)
    acf_table <- acf_table %>%
      mutate(Sign = case_when(
        Lag <= 0 ~ "Lag < 0",
        TRUE ~ "Lag > 0")) %>% 
      group_by(Sign) %>% 
      summarise(Max_ACF = max(ACF), 
                Lag = Lag[which.max(ACF)]) %>% 
      mutate(Variable = column,
             City = city,
             State = state,
             Est_Population = mean(city_combined$Est_Population)) %>% 
      select(Variable, City, State, Max_ACF, Lag, Sign)
    #return(acf_table)
    
    acf_tables_list[[column]] <- acf_table
    
  }
  combined_acf_table <- do.call(rbind, acf_tables_list)
  summary_table_positive <- combined_acf_table %>% 
    filter(Sign == "Lag > 0") %>%
    mutate(City = city,
           State = state) %>% 
    arrange(desc(abs(Max_ACF)))
  summary_table_negative <- combined_acf_table %>% 
    filter(Sign == "Lag < 0") %>% 
    mutate(City = city, 
           State = state) %>% 
    arrange(desc(abs(Max_ACF)))
  return(combined_acf_table %>%
           mutate(City = city, 
                  State = state,
                  Est_Population = mean(city_combined$Est_Population),
                  Population = mean(city_combined$Population)))
}

plot_ccf <- function(folder_path, filename, code = NULL, city = NULL, state = NULL) {
  file_path <- file.path(folder_path, filename)
  data <- fread(here(file_path))
  data$week <- as.Date(data$week)
  
  city_msa_cases <- cases %>% 
    filter(MSA_Code == code) %>% 
    group_by(week) %>% 
    summarize(Cases = sum(Cases),
              Est_Population = sum(estpop2020),
              Population = sum(pop2020))
  
  city_msa_deaths <- deaths %>% 
    filter(MSA_Code == code) %>% 
    group_by(week) %>% 
    summarize(Deaths = sum(Deaths),
              Est_Population = sum(estpop2020),
              Population = sum(pop2020))
  city_combined <- left_join(city_msa_cases, city_msa_deaths, by = "week") %>% 
    mutate(Weekly_Cases = Cases - lag(Cases, default = 0),
           Weekly_Deaths = Deaths - lag(Deaths, default = 0))
  city_combined <- left_join(city_combined, data, by = "week") %>% 
    na.omit() %>% 
    rename(Est_Population = Est_Population.x,
           Population = Population.x) %>% 
    select(-c(Est_Population.y, Population.y))
  ccf_columns <- names(city_combined)[8:ncol(city_combined)]
  
  output_dir <- paste0("Results/Graphs/CCF Plots/", city)
  
  if (!dir.exists(output_dir)) {
    dir.create(output_dir)
  }
  
  for (column in ccf_columns) {
    # Generate the plot filename
    plot_filename <- file.path(output_dir, paste0(city, "_", state, "_", column, "_ccf.png"))
    
    
    # Generate the CCF plot
    ccf_result <- ccf(city_combined$Weekly_Cases, city_combined[[column]], plot = FALSE, lag.max = 40)
    
    # Open a PNG device
    png(plot_filename, width = 800, height = 600)
    
    plot(ccf_result, main = paste("Weekly Cases &", column, "for", city, ",", state))
    
    # Close the PNG device
    dev.off()
    
    # Store the plot filename in the list
    #ccf_plots[[column]] <- plot_filename
  }
  
  #return(ccf_plots)
  
}


ccf_by_year <- function(folder_path, filename, explanatory = NULL, code = NULL, city = NULL,
                        state = NULL) {
  file_path <- file.path(folder_path, filename)
  #print(file_path)
  data <- fread(here(file_path)) %>% 
    separate(week, into = c("year", "month", "day"), sep = "-")
  
  city_msa_cases <- cases %>%  
    filter(MSA_Code == code) %>% 
    separate(week, into = c("year", "month", "day"), sep = "-") %>%
    group_by(year, month, day, MSA_Code, MSA_Title, FIPS, Admin2) %>% 
    summarize(Cumulative_Cases = Cases,
              Est_Population = unique(estpop2020),
              Population = unique(pop2020)) %>% 
    ungroup() %>% 
    group_by(FIPS, Admin2,MSA_Code, MSA_Title, year, month, day) %>% 
    slice_tail(n = 1) %>% 
    na.omit() %>% 
    ungroup() %>% 
    group_by(year, month, day, MSA_Code, MSA_Title) %>%
    summarize(Cumulative_Cases = sum(Cumulative_Cases),
              Est_Population = sum(Est_Population),
              Population = sum(Population)) %>% 
    ungroup()
  city_msa_deaths <- deaths %>% 
    filter(MSA_Code == code) %>%
    separate(week, into = c("year", "month", "day"), sep = "-") %>% 
    group_by(year, month, day, MSA_Code, MSA_Title, FIPS, Admin2) %>% 
    summarize(Cumulative_Deaths = Deaths,
              Est_Population = unique(estpop2020),
              Population = unique(pop2020)) %>% 
    ungroup() %>% 
    group_by(FIPS, Admin2, MSA_Code, MSA_Title, year, month, day) %>% 
    slice_tail(n = 1) %>% 
    na.omit() %>% 
    ungroup() %>% 
    group_by(year, month, day, MSA_Code, MSA_Title) %>%
    summarize(Cumulative_Deaths = sum(Cumulative_Deaths),
              Est_Population = sum(Est_Population),
              Population = sum(Population)) %>% 
    ungroup()
  city_combined <- left_join(city_msa_cases, city_msa_deaths) %>%
    ungroup() %>% 
    arrange(year, month, day) %>% 
    mutate(Weekly_Cases = Cumulative_Cases - lag(Cumulative_Cases, n = 1, default = 0),
           Weekly_Deaths = Cumulative_Deaths - lag(Cumulative_Deaths, n = 1, default = 0)) %>% 
    left_join(data) %>% 
    na.omit()
  
  ccf_columns <- city_combined %>% select(starts_with("mean_")) %>% names()
  years <- as.vector(unique(city_combined$year))
  
  full_acf_table <- data.frame()
  for (y in years) {
    city_combined1 <- city_combined %>% filter(year == y)
    for (column in ccf_columns) {
      
      ccf_result <- ccf(city_combined1$Weekly_Cases, city_combined1[[column]], plot = FALSE,
                        lag.max = 40)
      #print(str(ccf_result))
      
      acf_table <- data.frame(Lag = ccf_result$lag, ACF = ccf_result$acf, year = y) %>%
        mutate(Sign = case_when(
          Lag <= 0 ~ "Lag < 0",
          TRUE ~ "Lag > 0")) %>%
        group_by(Sign) %>%
        summarise(Max_ACF = max(ACF),
                  Lag = Lag[which.max(ACF)]) %>%
        ungroup() %>%
        mutate(Variable = column,
               City = city,
               State = state,
               Est_Population = mean(city_combined1$Est_Population),
               year = y) %>%
        select(year, Variable, City, State, Max_ACF, Lag, Sign)
      #acf_table_year <- rbind(acf_table_year, acf_table)
      full_acf_table <- rbind(full_acf_table, acf_table)
    }
  }
  return(full_acf_table)
}

