source("Functions/04 - ccf_city_case_files().R")
deaths1 <- deaths %>% 
  select(week, FIPS, MSA_Code, Deaths)

cases_and_deaths <- left_join(cases, deaths1)

MSA_Codes <- c("C1206", "C1242","C1258", "C1446","C1674", "C1698","C1814",
               "C1910","C1974","C1982","C2642","C2814", "C3108","C3346",
               "C3498","C3538", "C3562", "C3674", "C3798", "C3806", "C3890",
               "C3958","C4162", "C4174", "C4186", "C4266", "C4118","C4530","C2982", "C4790")
Cities <- c("Atlanta", "Austin","Baltimore", "Boston", "Charlotte", "Chicago", "Columbus","Dallas","Denver",
            "Detroit","Houston","Kansas City","Los Angeles","Minneapolis","Nashville","New Orleans",
            "New York City", "Orlando", "Philadelphia", "Phoenix", "Portland", "Raleigh","Salt Lake City",
            "San Diego", "San Francisco", "Seattle", "St. Louis","Tampa Bay","Las Vegas", "Washington DC")
table2 <- data.frame(cbind(MSA_Codes, Cities))
table2 <- table2 %>% 
  rename(MSA_Code = MSA_Codes)


library(readr)
library(purrr)

# Define the function that reads and processes a single file
process_daily_file <- function(file_path, MSA_Code) {
  data <- read_csv(file_path) %>%
    select(week, everything()) %>%
    mutate(MSA_Code = MSA_Code)
  return(data)
}

# Get the list of file names in the "Daily Data" folder
file_names <- list.files("Source Data/Weekly Data", pattern = "*_weekly.csv", full.names = TRUE)
#file_names <- list.files("Source Data/Daily Data - New1", pattern = "*_daily.csv", full.names = TRUE)
# Use map_dfr to apply the function to all files based on the MSA Codes
data_list <- file_names %>%
  map2_dfr(MSA_Codes, ~ process_daily_file(.x, .y))

# Print or use data_list as needed
combined_reddit_df <- bind_rows(data_list) %>% left_join(table2)

#For cases_and_deaths, filter by MSA_Code, then group_by Date and summarise mean cases and deaths
cases_and_deaths1 <- cases_and_deaths %>% 
  filter(MSA_Code %in% MSA_Codes) %>%
  group_by(MSA_Code, MSA_Title, week) %>% 
  summarise(Cumulative_Cases = round(sum(Cases), 0),
            Cumulative_Deaths = round(sum(Deaths), 0)) %>%
  mutate(Weekly_Cases = Cumulative_Cases - lag(Cumulative_Cases, default = 0),
         Weekly_Deaths = Cumulative_Deaths - lag(Cumulative_Deaths, default = 0)) %>% 
  mutate(Weekly_Cases = ifelse(Weekly_Cases < 0, 0, Weekly_Cases),
         Weekly_Deaths = ifelse(Weekly_Deaths < 0, 0, Weekly_Deaths)) %>%  
  select(week, everything()) 
fwrite(cases_and_deaths1, "Results/CSV Files/df of Seven-Day Average of Cases and Deaths by City.csv")

atlanta = cases_and_deaths1 %>% filter(MSA_Code == "C1206")
ggplot(data = cases_and_deaths1 %>% filter(MSA_Code == "C1206"), aes(x = week))+
  geom_line(aes(y = Weekly_Cases))

reddit_and_cases <- left_join(cases_and_deaths1, combined_reddit_df, by = c("MSA_Code", "week")) %>%
  rename(City = Cities) #%>% 
  #na.omit()


reddit_and_cases1 <- reddit_and_cases %>% 
  group_by(City) %>% 
  mutate(week = floor_date(Date, "week")) %>% 
  select(Date, week, everything()) %>%
  select(-c(Cumulative_Cases, Cumulative_Deaths, Daily_Cases, Daily_Deaths, Daily_Cases7, Daily_Deaths7)) %>%
  group_by(week, MSA_Code, MSA_Title, City) %>% 
  summarize(across(starts_with("mean_"), mean, na.rm = T))

reddit_and_cases2 <- reddit_and_cases %>% 
  group_by(City) %>% 
  mutate(week = floor_date(Date, "week")) %>%
  group_by(week, MSA_Code) %>% 
  summarize(weekly_cases = sum(Daily_Cases),
            weekly_deaths = sum(Daily_Deaths)) %>% 
  left_join(reddit_and_cases1, by = c("week", "MSA_Code")) %>% 
  select(week, MSA_Code, MSA_Title, City, everything())

reddit_and_cases2$Date <- as.Date.default(reddit_and_cases2$Date, format = "%y-%m-%d")


fwrite(reddit_and_cases2, paste("Results/CSV Files/reddit_and_cases_deaths.csv"))

reddit_and_cases <- read_csv("Results/CSV Files/reddit_and_cases_deaths.csv")

#sf <- reddit_and_cases %>% 
##  filter(City == "San Francisco") %>% 
#  filter(Date >= "2021-06-01")
#sea <- reddit_and_cases %>% 
#  filter(City == "Seattle") %>% 
#  filter()
#L <- combined_reddit_df %>% 
#  filter(MSA_Code == "C1206")
#Atlanta <- Atlanta %>%
#  group_by(Date) %>%
#  summarize(num_comments = n()) %>%
#  mutate(date_diff = c(NA, diff(Date))) %>% 
#  filter(date_diff != 1)
