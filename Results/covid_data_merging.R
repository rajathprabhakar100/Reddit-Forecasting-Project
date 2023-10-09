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

#Average Correlation

#Positive
ggplot(table1, aes(x = reorder(Variable,
                               abs(Average_ACF_positive)),
                   y = Average_ACF_positive))+
  geom_bar(stat = "identity")+
  coord_flip()+
  labs(x = "Average ACF",
       y = "Variable",
       title = "Variables by Average Correlations (Positive Lags)")

#Negative
ggplot(table1, aes(x = reorder(Variable, -abs(Average_ACF_negative)),
                   y = Average_ACF_negative))+
  geom_bar(stat = "identity")+
  coord_flip()+
  labs(x = "Average ACF",
       y = "Variable",
       title = "Variables by Average Correlations (Negative Lags)")


#Average Rank

#Positive
ggplot(rank_avg, aes(x = reorder(Variable, -abs(avg_rank_pos)),
                     y = avg_rank_pos))+
  geom_bar(stat = "identity")+
  coord_flip()+
  labs(x = "Average Rank",
       y = "Variable",
       title = "Variables by Average Rank (Positive Lags)")

#Negative
ggplot(rank_avg, aes(x = reorder(Variable, -abs(avg_rank_neg)),
                     y = avg_rank_neg))+
  geom_bar(stat = "identity")+
  coord_flip()+
  labs(x = "Average Rank",
       y = "Variable",
       title = "Variables by Average Rank (Negative Lags)")

#Average Lag

#Positive
ggplot(table1, aes(x = reorder(Variable, Average_Lag_positive), 
                   y = Average_Lag_positive))+
  geom_bar(stat = "identity")+
  coord_flip()+
  labs(x = "Average Lag",
       y = "Variable",
       title = "Variables by Average Lag (Positive Lags)")

#Negative
ggplot(table1, aes(x = reorder(Variable, Average_Lag_negative), 
                   y = Average_Lag_negative))+
  geom_bar(stat = "identity")+
  coord_flip()+
  labs(x = "Average Lag",
       y = "Variable",
       title = "Variables by Average Lag (Negative Lags)")

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



health_illness <- top_12 %>% 
  filter(Variable == "mean_health" | Variable == "mean_illness")

atlanta <- read_csv("Daily Data/atlanta_daily.csv") %>% 
  select(Date, mean_health, mean_illness) %>% 
  mutate(MSA_Code = "C1206")

MSA_Codes <- c("C1206", "C1242", "C1446", "C1698", "C2642", "C3108", "C3498", "C3562", "C3674", 
               "C3798", "C3806", "C3890", "C3958", "C4174", "C4186", "C4266", "C4118", "C4790")


#For all files in "Daily Data", do the above. Then join to cases_and_Deaths


#For cases_and_deaths, filter by MSA_Code, then group_by Date and summarise mean cases and deaths
cases_and_deaths1 <- cases_and_deaths %>% 
  filter(MSA_Code %in% MSA_Codes) %>%
  group_by(MSA_Code, MSA_Title, Date) %>% 
  summarise(Cases = round(mean(Cases), 2),
            Deaths = round(mean(Deaths), 2)) %>% 
  select(Date, everything())

