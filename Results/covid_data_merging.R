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

deaths1 <- deaths %>% 
  select(Date, FIPS, MSA_Code, Deaths)

cases_and_deaths <- left_join(cases, deaths1)

rank_avg <- read_csv("Results/CSV Files/Average_City_Rank.csv")
table1 <- read_csv("Results/CSV Files/table_of_average_acf_lag.csv")
combined <- read_csv("Results/CSV Files/combined_data.csv")


top12variables <- c("mean_illness", "mean_health", "mean_family", "mean_Tone",
                    "mean_fatigue", "mean_Analytic", "mean_allnone", "mean_emo_pos",
                    "mean_mental", "mean_emo_sad", "mean_tone_pos", "mean_emotion")

top_12 <- combined %>% 
  filter(Lag > 0 & Variable %in% top12variables)

ggplot(top_12, aes(x = Lag, y = Max_ACF))+
  geom_point()+
  labs(x = "Lag",
       y = "Max Correlation", 
       title = "Scatter Plot of Top 12 Reddit Indicators by Max Correlation")+
  facet_wrap(~ Variable)



MSA_Codes <- c("C1206", "C1242", "C1446", "C1698", "C2642", "C3108", "C3498", "C3562", "C3674", 
               "C3798", "C3806", "C3890", "C3958", "C4174", "C4186", "C4266", "C4118", "C4790")


library(readr)
library(purrr)

# Define the function that reads and processes a single file
process_daily_file <- function(file_path, MSA_Code) {
  data <- read_csv(file_path) %>%
    select(Date, mean_health, mean_illness) %>%
    mutate(MSA_Code = MSA_Code)
  return(data)
}

# Get the list of file names in the "Daily Data" folder
file_names <- list.files("Daily Data", pattern = "*_daily.csv", full.names = TRUE)

# Use map_dfr to apply the function to all files based on the MSA Codes
data_list <- file_names %>%
  map2_dfr(MSA_Codes, ~ process_daily_file(.x, .y))

# Print or use data_list as needed
combined_reddit_df <- bind_rows(data_list)

#For cases_and_deaths, filter by MSA_Code, then group_by Date and summarise mean cases and deaths
cases_and_deaths1 <- cases_and_deaths %>% 
  filter(MSA_Code %in% MSA_Codes) %>%
  group_by(MSA_Code, MSA_Title, Date) %>% 
  summarise(Cases = round(sum(Cases), 0),
            Deaths = round(sum(Deaths), 0)) %>%
  mutate(Daily_Cases = Cases - lag(Cases, default = 0),
         Daily_Deaths = Deaths - lag(Deaths, default = 0),
         Daily_Cases7 = round(rollmean(Daily_Cases, k = 7, fill = NA),2),
         Daily_Deaths7 = round(rollmean(Daily_Deaths, k = 7, fill = NA), 2)) %>% 
  select(Date, everything())

reddit_and_cases <- left_join(cases_and_deaths1, combined_reddit_df) %>% na.omit()
#fwrite(reddit_and_cases, paste("Results/CSV Files/reddit_and_cases_deaths.csv"))

reddit_and_cases <- read_csv("Results/CSV Files/reddit_and_cases_deaths.csv")
reddit_and_cases1 <- reddit_and_cases %>%
  group_by(MSA_Code) %>%
  mutate(Scaled_DailyCases7 = round(scale(Daily_Cases7), 2),
         Scaled_DailyDeaths7 = round(scale(Daily_Deaths7), 2),
         Scaled_mean_health = round(scale(mean_health), 2),
         Scaled_mean_illness = round(scale(mean_illness), 2))

ggplot(reddit_and_cases1, aes(x = Date))+
  geom_line(aes(y = Scaled_DailyCases7, color = "Cases"))+
  geom_line(aes(y = Scaled_mean_health, color = "mean_health"))+
  facet_wrap(~ MSA_Title, ncol = 3)+
  scale_color_manual(values = c("Cases" = "blue", "mean_health" = "red"))+
  labs(x = "Date",
       y = "Z Score",
       title = "Time Series of mean_Health vs Cases")

ggplot(reddit_and_cases1, aes(x = Date))+
  geom_line(aes(y = Scaled_DailyCases7, color = "Cases"))+
  geom_line(aes(y = Scaled_mean_illness, color = "mean_illness"))+
  facet_wrap(~ MSA_Title, ncol = 3)+
  scale_color_manual(values = c("Cases" = "blue", "mean_illness" = "orange"))+
  labs(x = "Date",
       y = "Z Score",
       title = "Time Series of mean_illness vs Cases")


mean(reddit_and_cases$Daily_Cases7)

