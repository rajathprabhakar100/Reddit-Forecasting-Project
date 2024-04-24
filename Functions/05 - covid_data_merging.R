source("Functions/04 - ccf_city_case_files().R")
deaths1 <- deaths %>% 
  select(Date, FIPS, MSA_Code, Deaths)

cases_and_deaths <- left_join(cases, deaths1)


MSA_Codes <- c("C1206", "C1242", "C1446", "C1698", "C2642", "C3108", "C3498", "C3562", "C3674", 
               "C3798", "C3806", "C3890", "C3958", "C4174", "C4186", "C4266", "C4118", "C4790")
Cities <- c("Atlanta", "Austin", "Boston", "Chicago", "Houston", "Los Angeles",
            "Nashville", "New York City", "Orlando", "Philadelphia", "Phoenix", "Portland", 
            "Raleigh", "San Diego", "San Francisco", "Seattle", "St. Louis", "Washington DC")
table2 <- data.frame(cbind(MSA_Codes, Cities))
table2 <- table2 %>% 
  rename(MSA_Code = MSA_Codes)


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
file_names <- list.files("Source Data/03 - Daily Data", pattern = "*_daily.csv", full.names = TRUE)
#file_names <- list.files("Source Data/Daily Data - New1", pattern = "*_daily.csv", full.names = TRUE)
# Use map_dfr to apply the function to all files based on the MSA Codes
data_list <- file_names %>%
  map2_dfr(MSA_Codes, ~ process_daily_file(.x, .y))

# Print or use data_list as needed
combined_reddit_df <- bind_rows(data_list)
combined_reddit_df <- left_join(combined_reddit_df, table2)

#For cases_and_deaths, filter by MSA_Code, then group_by Date and summarise mean cases and deaths
cases_and_deaths1 <- cases_and_deaths %>% 
  filter(MSA_Code %in% MSA_Codes) %>%
  group_by(MSA_Code, MSA_Title, Date) %>% 
  summarise(Cumulative_Cases = round(sum(Cases), 0),
            Cumulative_Deaths = round(sum(Deaths), 0)) %>%
  mutate(Daily_Cases = Cumulative_Cases - lag(Cumulative_Cases, default = 0),
         Daily_Deaths = Cumulative_Deaths - lag(Cumulative_Deaths, default = 0)) %>% 
  mutate(Daily_Cases = ifelse(Daily_Cases < 0, 0, Daily_Cases),
         Daily_Deaths = ifelse(Daily_Deaths < 0, 0, Daily_Deaths)) %>% 
  mutate(Daily_Cases7 = round(rollmeanr(Daily_Cases, k = 7, fill = NA),2),
         Daily_Deaths7 = round(rollmeanr(Daily_Deaths, k = 7, fill = NA), 2)) %>% 
  select(Date, everything()) 
fwrite(cases_and_deaths1, "Results/CSV Files/df of Seven-Day Average of Cases and Deaths by City.csv")



reddit_and_cases <- left_join(cases_and_deaths1, combined_reddit_df, by = c("MSA_Code", "Date")) %>%
  rename(City = Cities) #%>% 
  #na.omit()
reddit_and_cases$Date <- as.Date.default(reddit_and_cases$Date, format = "%y-%m-%d")
fwrite(reddit_and_cases, paste("Results/CSV Files/reddit_and_cases_deaths.csv"))

#reddit_and_cases <- read_csv("Results/CSV Files/reddit_and_cases_deaths.csv")

sf <- reddit_and_cases %>% 
  filter(City == "San Francisco") %>% 
  filter(Date >= "2021-06-01")
sea <- reddit_and_cases %>% 
  filter(City == "Seattle") %>% 
  filter()