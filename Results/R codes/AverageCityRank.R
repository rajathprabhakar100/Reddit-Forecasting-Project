library(tidyverse)
library(readr)
library(data.table)
combined <- read_csv("Results/CSV Files/combined_data.csv")
city_list <- split(combined, combined$City)

city_list1 <- list()
for (city in city_list) {
  city_rank_negative <- city %>% 
    filter(Lag <= 0) %>% 
    mutate(Negative_Lag_Rank = rank(-abs(Max_ACF)))
  city_rank_positive <- city %>% 
    filter(Lag > 0) %>% 
    mutate(Positive_Lag_Rank = rank(-abs(Max_ACF)))
  city_rank <- left_join(city_rank_positive, city_rank_negative, by = "Variable") %>% 
    select(-c(City.y, State.y)) %>% 
    rename(City = City.x,
           State = State.x,
           Max_ACF_Positive = Max_ACF.x,
           Lag_Positive = Lag.x,
           Max_ACF_Negative = Max_ACF.y,
           Lag_Negative = Lag.y) %>% 
    select(Variable, City, State, Max_ACF_Positive, Lag_Positive, Positive_Lag_Rank, 
           Max_ACF_Negative, Lag_Negative, Negative_Lag_Rank)
 city_list1[[length(city_list1) + 1]] <- city_rank 
}
average_city_rank <- do.call(rbind, city_list1)  

rank_avg <- average_city_rank %>% 
  group_by(Variable) %>% 
  summarize(avg_rank_pos = mean(Positive_Lag_Rank),
            avg_rank_neg = mean(Negative_Lag_Rank))
fwrite(rank_avg, paste("Results/CSV Files/Average_City_Rank.csv"))


