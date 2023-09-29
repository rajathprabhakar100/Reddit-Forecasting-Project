#Run lines 2-13 first, then run combine_reddit()
library(tidyverse)
library(data.table)
library(readr)
library(zoo)
#library(readxl)

setwd("C:/Users/14049/Desktop/The Fox Lab")
#allcities
allcities <- fread("Old Data/liwc22_allcities_fromJan2019toMay2020.csv") 
allcities <- allcities %>%
  mutate(Date = as_datetime(created_utc, tz = "America/Chicago") %>% 
           as_date()) %>%
  select(-created_utc) %>% 
  relocate(Date, .after = 4)

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

cases <- left_join(covid_cases, new_crosswalk)

cases <- left_join(covid_cases, new_crosswalk, by = "FIPS") %>%
  #mutate(across(starts_with("new_crosswalk"), ~ ifelse(is.na(FIPS), "NA", .))) %>% 
  select(UID, iso2, iso3, code3, FIPS, MSA_Code, MSA_Title, everything()) 
cases <- cases %>% 
  gather(key = "Date", value = "Value", "1.22.20":ncol(cases)) %>% 
  select(Date, everything()) %>% 
  #group by Date, MSA, summarise cumulative cases
deaths <- left_join(covid_deaths, new_crosswalk, by = "FIPS") %>%
  mutate(across(starts_with("new_crosswalk"), ~ ifelse(is.na(FIPS), "NA", .))) %>% 
  select(UID, iso2, iso3, code3, FIPS, MSA_Code, MSA_Title, everything())




#combine city files with allcities filtered data to produce city_clean file
combine_reddit <- function(folder_path, allcities, filename) {
  
  # Prepares path of the specific liwc22_ file
  liwc_file_path <- file.path(folder_path, filename)
  
  # Read the specific liwc22_ file into a data frame
  liwc_df <- fread(liwc_file_path)
  liwc_df <- liwc_df %>% 
    mutate(Date = as_datetime(created_utc, tz = "America/Chicago") %>% 
             as_date()) %>%
    select(-created_utc) %>% 
    relocate(Date, .after = 4)
  
  # Extract the city name from the specified file name (e.g., liwc22_Atlanta.csv -> "Atlanta")
  city_name <- gsub("^liwc22_(.+)\\.csv$", "\\1", filename)
  
  # Filter the data from allcities for the corresponding city based on the subreddit column
  city_data <- allcities %>%
    filter(subreddit == city_name)
  message(paste("Finished filtering", city_name))
  
  # Check if city_data is empty
  if (nrow(city_data) == 0) {
    message(paste("No matching data for city:", city_name))
    return(NULL)
  }
  
  # Combine the data from allcities with the liwc22_ data
  combined_df <- bind_rows(city_data, liwc_df)

  # Create a new file with the combined data (e.g., "Atlanta_clean.csv")
  fwrite(combined_df, paste0(city_name, "_clean.csv"))
  
  message(paste(city_name, "exported to local folder"))
}
#combine_reddit(folder_path = "Reddit Data", allcities = allcities, filename = "liwc22_nyc.csv") - 9:37
#combine_reddit(folder_path = "Reddit Data", allcities = allcities, filename = "liwc22_orlando.csv") - 1:31
#combine_reddit(folder_path = "Reddit Data", allcities = allcities, filename = "liwc22_philadelphia.csv") - 2:26
#combine_reddit(folder_path = "Reddit Data", allcities = allcities, filename = "liwc22_phoenix.csv") - 2:27
#combine_reddit(folder_path = "Reddit Data", allcities = allcities, filename = "liwc22_Portland.csv") - 6:18
#combine_reddit(folder_path = "Reddit Data", allcities = allcities, filename = "liwc22_raleigh.csv") - 1:01
#combine_reddit(folder_path = "Reddit Data", allcities = allcities, filename = "liwc22_sandiego.csv") - 2:23
combine_reddit(folder_path = "Reddit Data", allcities = allcities, filename = "liwc22_sanfrancisco.csv")
combine_reddit(folder_path = "Reddit Data", allcities = allcities, filename = "liwc22_Seattle.csv")
combine_reddit(folder_path = "Reddit Data", allcities = allcities, filename = "liwc22_StLouis.csv")
combine_reddit(folder_path = "Reddit Data", allcities = allcities, filename = "liwc22_washingtondc.csv")
#Note: the cases of the subreddit and the liwc22_* must match. To this end, I changed some of the names of the city files. The ones that were capitalized are the Atlanta, Austin, Los Angeles, Portland, Seattle, and St Louis Reddit files. 


#condense city_clean into city_daily
process_reddit_files <- function(folder_path, filename) {
  file_path <- file.path(folder_path, filename)
  data <- fread(file_path)
  data <- data %>%
    mutate(author = replace(author, author %in% c("[deleted]", "AutoModerator"), NA),
           body = replace(body, body == "[removed]", NA)) %>%
    na.omit()
  
  new_data <- data %>% select(-c(1:4, 6:10))
  means <- data.frame(Date = unique(new_data$Date))
  
  for (col in colnames(new_data)[-1]) {
    col_means <- tapply(new_data[[col]], new_data$Date, mean)
    means[[paste0("mean_", col)]] <- col_means[match(means$Date, names(col_means))]
  }
  
  means_significant <- means %>%
    select(Date, starts_with("mean_")) %>%
    mutate_at(vars(starts_with("mean_")),
              list(~rollmean(., k = 7, fill = NA, align = "right"))) %>% 
    arrange(Date)
  
  # Extract city name from the filename
  city_name <- gsub("_clean.csv$", "", basename(filename))
  
  # Export the data frame with the modified city name in the filename
  output_filename <- paste0(city_name, "_daily.csv")
  
  fwrite(means_significant, output_filename)
  
  message(paste(output_filename, "exported to local folder"))
  #return(means_significant)
}


ts_city_files <- function(folder_path, filename, explanatory = NULL) {
  #for daily city data, defaults to plotting time series of all variables for a city's daily data. Can also plot ccf
  file_path <- file.path(folder_path, filename)
  data <- fread(file_path)
  data$Date <- as.Date(data$Date)
  
  data_long <- data %>%
    gather(key = "variable", value = "value", -Date) %>% 
    group_by(variable) %>% 
    mutate(rolling_mean = rollmean(value, k = 7, fill = NA)) %>%
    ungroup()

  if (!is.null(explanatory)) {
    plot <- ggplot(data, aes(x = Date, y = !!sym(explanatory))) + 
      geom_line() + 
      xlab("Date") + 
      scale_x_date(date_breaks = "4 month", date_labels = "%m %Y") +
      ylab(paste("7-Day Rolling Average of", explanatory)) +
      ggtitle(paste("Time Series Plot of", explanatory, "with 7-Day Rolling Average"))
    return(plot)
  }
  else {
    plots_list <- lapply(unique(data_long$variable), function(var) {
      ggplot(data_long %>% filter(variable == var), aes(Date, rolling_mean)) +
        geom_line() +
        xlab("Date") +
        scale_x_date(date_breaks = "4 month", date_labels = "%m %Y")+
        ylab(paste("7-Day Rolling Average of", var)) +
        ggtitle(paste("Time Series Plot of", var, "with 7-Day Rolling Average"))
    })
    invisible(lapply(plots_list, print))
    
  }
}


ccf_city_case_files <- function(folder_path, filename, explanatory = NULL, city = NULL, state = NULL, 
                           plots = TRUE, lags = "both") {
  #folder path - give name of folder or path to folder
  #filename - give name of file within folder_path
  #explanatory - if not null, function only runs for one variable. If null, runs for all variables
  #city - give name of city, like "City", not "city"
  #state = give name of state, like "State", not "state"
  #plots - If TRUE, graph plots. If false, don't graph CCF plots. 
  #lags - can be "negative", "positive", or "both"
  file_path <- file.path(folder_path, filename)
  #print(file_path)
  data <- fread(file_path)
  data$Date <- as.Date(data$Date)
  
if (!is.null(city) & !is.null(state)) {
    #set up for ccf
    city_msa_cases <- cases %>%
      filter(Province_State == state) %>% 
      filter(grepl(city, MSA_Title, ignore.case = TRUE)) %>%
      select(-c(1:13))
    #return(city_msa_cases)
    column_dates <- as.Date(colnames(city_msa_cases), format = "%m.%d.%y")
    city_cases <- data.frame(Date = column_dates, Cases = colSums(city_msa_cases))
    #return(city_cases)
    
    city_msa_deaths <- deaths %>% 
      filter(Province_State == state) %>% 
      filter(grepl(city, MSA_Title, ignore.case = TRUE)) %>% 
      select(-c(1:14))
    #return(city_msa_deaths)
    column_dates_deaths <- as.Date(colnames(city_msa_deaths), format = "%m.%d.%y")
    city_deaths <- data.frame(Date = column_dates_deaths, Deaths = colSums(city_msa_deaths))
    #return(city_deaths)
    
    city_combined <- left_join(city_cases, city_deaths, by = "Date") %>% 
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
                 State = state)

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
                 State = state)
        
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
            mutate(Variable = column)
          
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
          return(combined_acf_table %>% select(-c(1)) %>% mutate(City = city, State = state))
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
          return(combined_acf_table %>% select(-c(1)) %>% mutate(City = city, State = state))
        }
        #print(summary_table_negative)
        #print(summary_table_positive)
      }
    }
  }
  else {
    stop("Missing either state or city input")
  }
}

#ccf_city_files("DailyData", "Atlanta_daily.csv", city = "Atlanta", state = "Georgia")
#atlanta <- ccf_city_files("DailyData", "Atlanta_daily.csv", city = "Atlanta", state = "Georgia", plots = FALSE)
#austin <- ccf_city_files("DailyData", "Austin_daily.csv", city = "Austin", state = "Texas", plots = FALSE)
#boston <- ccf_city_files("DailyData", "boston_daily.csv", city = "Boston", state = "Massachusetts", plots = FALSE)
#chicago <- ccf_city_files("DailyData", "chicago_daily.csv", city = "Chicago", state = "Illinois", plots = FALSE)
#houston <- ccf_city_files("DailyData", "houston_daily.csv", city = "Houston", state = "Texas", plots = FALSE)
#LA <- ccf_city_files("DailyData", "LosAngeles_daily.csv", city = "Los Angeles", state = "California", plots = FALSE)
#nashville <- ccf_city_files("DailyData", "nashville_daily.csv", city = "Nashville", state = "Tennessee", plots = FALSE)
#nyc  <- ccf_city_files("DailyData", "nyc_daily.csv", city = "New York", state = "New York", plots = FALSE)
#orlando <- ccf_city_files("DailyData", "orlando_daily.csv", city = "Orlando", state = "Florida", plots = FALSE)
#philadelphia <- ccf_city_files("DailyData", "philadelphia_daily.csv", city = "Philadelphia", state = "Pennsylvania", plots = FALSE)
#phoenix <- ccf_city_files("DailyData", "phoenix_daily.csv", city = "Phoenix", state = "Arizona", plots = FALSE)
#portland <- ccf_city_files("DailyData", "Portland_daily.csv", city = "Portland", state = "Oregon", plots = FALSE)
#raleigh <- ccf_city_files("DailyData", "raleigh_daily.csv", city = "Raleigh", state = "North Carolina", plots = FALSE)
#san_diego <- ccf_city_files("DailyData", "sandiego_daily.csv", city = "San Diego", state = "California", plots = FALSE)
#san_francisco <- ccf_city_files("DailyData", "sanfrancisco_daily.csv", city = "San Francisco", state = "California", plots = FALSE)
#seattle <- ccf_city_files("DailyData", "Seattle_daily.csv", city = "Seattle", state = "Washington", plots = FALSE)
#stl <- ccf_city_files("DailyData", "StLouis_daily.csv", city = "St. Louis", state = "Missouri", plots = FALSE)
#washingtondc <- ccf_city_files("DailyData", "washingtondc_daily.csv", city = "Washington", state = "Maryland", plots = FALSE)
#combo <- bind_rows(atlanta, austin, boston, chicago, houston, LA, nashville, nyc, orlando, philadelphia, phoenix, portland, phoenix, portland, raleigh, san_diego, san_francisco, seattle, stl, washingtondc) %>% 
#  select(City, State, Variable, Max_ACF, Lag)
#combo
#fwrite(combo, paste("combined_data.csv"))




rank <- read_csv("Average_City_Rank.csv")
rank$Variable <- str_replace_all(rank$Variable, "_", " ") %>% 
  str_to_title()
combined <- read_csv("combined_data.csv")

avg_acf_lag <- read_csv("table_of_average_acf_lag.csv")
avg_acf_lag$Variable <- str_replace_all(avg_acf_lag$Variable, "_", " ") %>% 
  str_to_title()
#Bar Graph for Average Rank
ggplot(rank, aes(x = reorder(Variable, -avg_rank_pos), y = avg_rank_pos)) +
  geom_bar(stat = "identity") +
  labs(title = "Variables by Average Rank (Positive Lags)",
       x = "Variable",
       y = "Average Rank")+
  coord_flip()+
  theme(axis.text.y = element_text(angle = 0, size = 10))+
  guides(fill = FALSE)

ggplot(rank, aes(x = reorder(Variable, -avg_rank_neg), y = avg_rank_neg)) +
  geom_bar(stat = "identity") +
  labs(title = "Variables by Average Rank (Negative Lags)",
       x = "Variable",
       y = "Average Rank")+
  coord_flip()+
  theme(axis.text.y = element_text(angle = 0, size = 10))+
  guides(fill = FALSE)


#Bar Graph for Average Correlations
ggplot(avg_acf_lag, aes(x = reorder(Variable, -abs(Average_ACF_positive)), 
                        y = Average_ACF_positive))+
  geom_bar(stat = "identity") + 
  labs(title = "Variables by Average Correlations (Positive Lags)",
       x = "Variable",
       y = "Average Lag")+
  coord_flip()+
  guides(fill = FALSE)

ggplot(avg_acf_lag, aes(x = reorder(Variable, -abs(Average_ACF_negative)), 
                        y = Average_ACF_negative))+
  geom_bar(stat = "identity") + 
  labs(title = "Variables by Average Correlations (Negative Lags)",
       x = "Variable",
       y = "Average ACF")+
  coord_flip()+
  guides(fill = FALSE)
#Bar Graph for Average Lags
ggplot(avg_acf_lag, aes(x = reorder(Variable, -abs(Average_Lag_positive)), 
                        y = Average_Lag_positive))+
  geom_bar(stat = "identity") + 
  labs(title = "Variables by Average Lag (Positive Lags)",
       x = "Variable",
       y = "Average Lag")+
  coord_flip()+
  guides(fill = FALSE)

ggplot(avg_acf_lag, aes(x = reorder(Variable, -abs(Average_Lag_negative)), 
                        y = Average_Lag_negative))+
  geom_bar(stat = "identity") + 
  labs(title = "Variables by Average Lag (Negative Lags)",
       x = "Variable",
       y = "Average Lag")+
  coord_flip()+
  guides(fill = FALSE)

head(avg_acf_lag %>% arrange(desc(abs(Average_ACF_positive))), 12)

variable_names <- c("mean_illness", 
                    "mean_health",
                    "mean_family",
                    "mean_Tone",
                    "mean_Analytic",
                    "mean_fatigue",
                    "mean_emo_pos",
                    "mean_allnone",
                    "mean_tone_pos",
                    "mean_emo_sad",
                    "mean_mental",
                    "mean_emotion")

top_12_variables <- combined %>%
  filter(Lag > 0 & Variable %in% variable_names)
head(top_12_variables)

ggplot(top_12_variables, aes(x = Lag, y = Max_ACF))+
  geom_point()+
  labs(x = "Lag", 
       y = "Max Correlation", 
       title = "Scatter Plot of Top 12 Reddit Indicators by Max Correlation")+
  facet_wrap(~Variable)

#9/28/23
cases <- left_join(covid_cases, new_crosswalk, by = "FIPS") %>%
  mutate(across(starts_with("new_crosswalk"), ~ ifelse(is.na(FIPS), "NA", .))) %>% 
  select(UID, iso2, iso3, code3, FIPS, MSA_Code, MSA_Title, everything())
cases <- cases %>% 
  gather(key = "Date", value = "Value", "1.22.20":ncol(cases)) %>% 
  select(Date, everything()) 
cases$Date <- as.Date(cases$Date, format = "%m.%d.%y")
cases <- cases %>% rename(Cases = Value)

atlanta <- read_csv("DailyData/atlanta_daily.csv") %>% 
  mutate(MSA_Code = "C1206", 
         City = "Atlanta")
austin <- read_csv("DailyData/austin_daily.csv") %>% 
  mutate(MSA_Code = "C1242",
         City = "Austin")
#MSA Code = C1242
boston <- read_csv("DailyData/boston_daily.csv") %>% 
  mutate(MSA_Code = "C1446",
         City = "Boston")
chicago <- read_csv("DailyData/chicago_daily.csv") %>% 
  mutate(MSA_Code = "C1698",
         City = "Chicago")
houston <- read_csv("DailyData/houston_daily.csv") %>% 
  mutate(MSA_Code = "C2642",
         City = "Houston")
losangeles <- read_csv("DailyData/losangeles_daily.csv") %>% 
  mutate(MSA_Code = "C3108",
         City = "Los Angeles")
nashville <- read_csv("DailyData/nashville_daily.csv") %>% 
  mutate(MSA_Code = "C3498",
         City = "Nashville")
nyc <- read_csv("DailyData/nyc_daily.csv") %>% 
  mutate(MSA_Code = "C3562",
         City = "New York City")
orlando <- read_csv("DailyData/orlando_daily.csv") %>% 
  mutate(MSA_Code = "C3674",
         City = "Orlando")
philadelphia <- read_csv("DailyData/philadelphia_daily.csv") %>% 
  mutate(MSA_Code = "C3798",
         City = "Philadelphia")
phoenix <- read_csv("DailyData/phoenix_daily.csv") %>% 
  mutate(MSA_Code = "C3806",
         City = "Phoenix")
portland <- read_csv("DailyData/portland_daily.csv") %>% 
  mutate(MSA_Code = "C3890", 
         City = "Portland")
raleigh <- read_csv("DailyData/raleigh_daily.csv") %>% 
  mutate(MSA_Code = "C3958",
         City = "Raleigh")
sandiego <- read_csv("DailyData/sandiego_daily.csv") %>% 
  mutate(MSA_Code = "C4174", 
         City = "San Diego")
sanfrancisco <- read_csv("DailyData/sanfrancisco_daily.csv") %>% 
  mutate(MSA_Code = "C4186", 
         City = "San Francisco")
seattle <- read_csv("DailyData/seattle_daily.csv") %>% 
  mutate(MSA_Code = "C4266",
         City = "Seattle")
stl <- read_csv("DailyData/stlouis_daily.csv") %>% 
  mutate(MSA_Code = "C4118",
         City = "St. Louis")
washingtondc <- read_csv("DailyData/washingtondc_daily.csv") %>% 
  mutate(MSA_Code = "C4790",
         City = "Washington DC")

#daily_combined <- rbind(atlanta, austin, boston,
##                        chicago, houston, losangeles,
#                        nashville, nyc, orlando, philadelphia,
#                        phoenix, portland, raleigh, sandiego,
#                        sanfrancisco, seattle, stl, washingtondc)
fwrite(daily_combined, paste("combined_daily.csv"))

daily_combined <- read_csv("combined_daily.csv")

combined_cases <- left_join(cases,daily_combined, by = c("Date", "MSA_Code")) %>% 
  group_by(City) %>%
  arrange_by(Date) %>% 
  mutate(Daily_Cases = Cases - lag(Cases, default = 0),
         Daily_Cases7 = round(rollmean(Daily_Cases, k = 7, fill = NA), 2)) %>% 
  select(Date, Daily_Cases7, City, everything()) %>% 
  select(-Cases, Daily_Cases) 

fwrite(combined_cases, paste("combined_cases.csv"))

combined_cases <- read_csv("combined_cases.csv")
combined_cases1 <- combined_cases %>% 
  group_by(City, Date) %>% 
  summarize(Cases = round(mean(Daily_Cases7), 2),
            mean_health = mean(mean_health),
            mean_illness = mean(mean_illness)) %>% 
  na.omit()

ggplot(data = combined_cases1 %>% filter(City == "Austin"), aes(x = Date))+
  geom_line(aes(y = Cases), color = "blue")+
  geom_line(aes(y = mean_health), color = "red")


#Revised Case counts
#Take negative 

