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
#fwrite(rank_avg, paste("Results/CSV Files/Average_City_Rank.csv"))

######################################################################################################

combined_data_year <- read_csv("Results/CSV Files/combined_data_year.csv")
year_list <- as.vector(unique(combined_data_year$year))
city_list <- as.vector(unique(combined_data_year$City))

city_rank_list <- list()
for (c in city_list) {
  #for each city, have a df containing all rank avgs for each year
  rank_year <- list()
  for (y in year_list) {
    rank_negative <- combined_data_year %>%
      filter(City == c) %>% 
      filter(year == y) %>% 
      filter(Sign == "Lag < 0") %>% 
      filter(abs(Lag) <= 42) %>% 
      mutate(Negative_Lag_Rank = rank(-abs(Max_ACF)))
    
    rank_positive <- combined_data_year %>% 
      filter(City == c) %>% 
      filter(year == y) %>% 
      filter(Sign == "Lag > 0") %>% 
      filter(abs(Lag) <= 42) %>% 
      mutate(Positive_Lag_Rank = rank(-abs(Max_ACF)))
    
    city_rank1 <- left_join(rank_positive, rank_negative,
                            by = c("year", "Variable", "City", "State")) %>% 
      rename(Max_ACF_Positive = Max_ACF.x,
             Lag_Positive = Lag.x,
             Max_ACF_Negative = Max_ACF.y,
             Lag_Negative = Lag.y) %>% 
      select(year, Variable, City, State, Max_ACF_Positive, Lag_Positive, Positive_Lag_Rank, 
             Max_ACF_Negative, Lag_Negative, Negative_Lag_Rank)
    rank_year[[length(rank_year) + 1]] <- city_rank1
  }
  average_city_rank_year <- do.call(rbind, rank_year)
  city_rank_list[[length(city_rank_list) + 1]] <- average_city_rank_year
}
rank_avg_year <- do.call(rbind, city_rank_list)
#fwrite(rank_avg_year, "Results/CSV Files/AverageCityRankYear.csv")
# city_ranks_v1 <- do.call(rbind, city_rank_list) %>% 
#   group_by(Variable, year) %>% 
#   summarize(avg_rank_pos = mean(Positive_Lag_Rank),
#             avg_rank_neg = mean(Negative_Lag_Rank)) %>% 
#   ungroup() %>% 
#   group_by(year) %>% 
#   summarize(highest_rank_pos = min(avg_rank_pos),
#             Variable_pos = Variable[which.min(avg_rank_pos)],
#             highest_rank_neg = min(avg_rank_neg),
#             Variable_neg = Variable[which.min(avg_rank_neg)])



######################Deaths#############################################



