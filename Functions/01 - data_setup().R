library(data.table)
library(tidyverse)
allcities <- fread("Source Data/01 - Raw/liwc22_allcities_fromJan2019toMay2020.csv") 
allcities <- allcities %>%
  mutate(Date = as_datetime(created_utc, tz = "America/Chicago") %>% 
           as_date()) %>%
  select(-created_utc) %>% 
  relocate(Date, .after = 4)
