library(tidyverse)
library(readr)
combined <- read_csv("Results/CSV Files/combined_data.csv")
city_list <- split(combined, combined$City)
for (city_name in names(city_list)) {
  assign(city_name, city_list[[city_name]])
}
assign("Los_Angeles", `Los Angeles`)
assign("San_Francisco", `San Francisco`)
assign("San_Diego", `San Diego`)
assign("stl", `St. Louis`)
assign("NYC", `New York`)

atlanta_rank_negative <- Atlanta %>% 
  filter(Lag <= 0) %>% 
  mutate(Negative_Lag_Rank = rank(-abs(Max_ACF)))  
atlanta_rank_positive <- Atlanta %>% 
  filter(Lag > 0) %>% 
  mutate(Positive_Lag_Rank = rank(-abs(Max_ACF))) 
atlanta_rank <- left_join(atlanta_rank_positive, atlanta_rank_negative, by = "Variable")
atlanta_rank <- atlanta_rank %>% 
  select(-c(City.y, State.y)) %>% 
  rename(City = City.x,
         State = State.x,
         Max_ACF_Positive = Max_ACF.x,
         Lag_Positive = Lag.x,
         Max_ACF_Negative = Max_ACF.y,
         Lag_Negative = Lag.y)

austin_rank_negative <- Austin %>% 
  filter(Lag <= 0) %>% 
  mutate(Negative_Lag_Rank = rank(-abs(Max_ACF)))   
austin_rank_positive <- Austin %>% 
  filter(Lag > 0) %>% 
  mutate(Positive_Lag_Rank = rank(-abs(Max_ACF))) 
austin_rank <- left_join(austin_rank_positive, austin_rank_negative, by = "Variable")
austin_rank <- austin_rank %>% 
  select(-c(City.y, State.y)) %>% 
  rename(City = City.x,
         State = State.x,
         Max_ACF_Positive = Max_ACF.x,
         Lag_Positive = Lag.x,
         Max_ACF_Negative = Max_ACF.y,
         Lag_Negative = Lag.y)

boston_rank_negative <- Boston %>% 
  filter(Lag <= 0) %>% 
  mutate(Negative_Lag_Rank = rank(-abs(Max_ACF)))   
boston_rank_positive <- Boston %>% 
  filter(Lag > 0) %>% 
  mutate(Positive_Lag_Rank = rank(-abs(Max_ACF))) 
boston_rank <- left_join(boston_rank_positive, boston_rank_negative, by = "Variable")
boston_rank <- boston_rank %>% 
  select(-c(City.y, State.y)) %>% 
  rename(City = City.x,
         State = State.x,
         Max_ACF_Positive = Max_ACF.x,
         Lag_Positive = Lag.x,
         Max_ACF_Negative = Max_ACF.y,
         Lag_Negative = Lag.y)

chicago_rank_negative <- Chicago %>% 
  filter(Lag <= 0) %>% 
  mutate(Negative_Lag_Rank = rank(-abs(Max_ACF))) 
chicago_rank_positive <- Chicago %>% 
  filter(Lag > 0) %>% 
  mutate(Positive_Lag_Rank = rank(-abs(Max_ACF))) 
chicago_rank <- left_join(chicago_rank_positive, chicago_rank_negative, by = "Variable")
chicago_rank <- chicago_rank %>% 
  select(-c(City.y, State.y)) %>% 
  rename(City = City.x,
         State = State.x,
         Max_ACF_Positive = Max_ACF.x,
         Lag_Positive = Lag.x,
         Max_ACF_Negative = Max_ACF.y,
         Lag_Negative = Lag.y)
houston_rank_negative <- Houston %>% 
  filter(Lag <= 0) %>% 
  mutate(Negative_Lag_Rank = rank(-abs(Max_ACF))) 
houston_rank_positive <- Houston %>% 
  filter(Lag > 0) %>% 
  mutate(Positive_Lag_Rank = rank(-abs(Max_ACF))) 
houston_rank <- left_join(houston_rank_positive, houston_rank_negative, by = "Variable")
houston_rank <- houston_rank %>% 
  select(-c(City.y, State.y)) %>% 
  rename(City = City.x,
         State = State.x,
         Max_ACF_Positive = Max_ACF.x,
         Lag_Positive = Lag.x,
         Max_ACF_Negative = Max_ACF.y,
         Lag_Negative = Lag.y)
LA_rank_negative <- Los_Angeles %>% 
  filter(Lag <= 0) %>% 
  mutate(Negative_Lag_Rank = rank(-abs(Max_ACF))) 
LA_rank_positive <- Los_Angeles %>% 
  filter(Lag > 0) %>% 
  mutate(Positive_Lag_Rank = rank(-abs(Max_ACF))) 
LA_rank <- left_join(LA_rank_positive, LA_rank_negative, by = "Variable")
LA_rank <- LA_rank %>% 
  select(-c(City.y, State.y)) %>% 
  rename(City = City.x,
         State = State.x,
         Max_ACF_Positive = Max_ACF.x,
         Lag_Positive = Lag.x,
         Max_ACF_Negative = Max_ACF.y,
         Lag_Negative = Lag.y)
nashville_rank_negative <- Nashville %>% 
  filter(Lag <= 0) %>% 
  mutate(Negative_Lag_Rank = rank(-abs(Max_ACF)))   
nashville_rank_positive <- Nashville %>% 
  filter(Lag > 0) %>% 
  mutate(Positive_Lag_Rank = rank(-abs(Max_ACF))) 
nashville_rank <- left_join(nashville_rank_positive, nashville_rank_negative, by = "Variable")
nashville_rank <- nashville_rank %>% 
  select(-c(City.y, State.y)) %>% 
  rename(City = City.x,
         State = State.x,
         Max_ACF_Positive = Max_ACF.x,
         Lag_Positive = Lag.x,
         Max_ACF_Negative = Max_ACF.y,
         Lag_Negative = Lag.y)
nyc_rank_negative <- NYC %>% 
  filter(Lag <= 0) %>% 
  mutate(Negative_Lag_Rank = rank(-abs(Max_ACF)))   
nyc_rank_positive <- NYC %>% 
  filter(Lag > 0) %>% 
  mutate(Positive_Lag_Rank = rank(-abs(Max_ACF))) 
nyc_rank <- left_join(nyc_rank_positive, nyc_rank_negative, by = "Variable")
nyc_rank <- nyc_rank %>% 
  select(-c(City.y, State.y)) %>% 
  rename(City = City.x,
         State = State.x,
         Max_ACF_Positive = Max_ACF.x,
         Lag_Positive = Lag.x,
         Max_ACF_Negative = Max_ACF.y,
         Lag_Negative = Lag.y)
orlando_rank_negative <- Orlando %>% 
  filter(Lag <= 0) %>% 
  mutate(Negative_Lag_Rank = rank(-abs(Max_ACF))) 
orlando_rank_positive <- Orlando %>% 
  filter(Lag > 0) %>% 
  mutate(Positive_Lag_Rank = rank(-abs(Max_ACF))) 
orlando_rank <- left_join(orlando_rank_positive, orlando_rank_negative, by = "Variable")
orlando_rank <- orlando_rank %>% 
  select(-c(City.y, State.y)) %>% 
  rename(City = City.x,
         State = State.x,
         Max_ACF_Positive = Max_ACF.x,
         Lag_Positive = Lag.x,
         Max_ACF_Negative = Max_ACF.y,
         Lag_Negative = Lag.y)
PHI_rank_negative <- Philadelphia %>% 
  filter(Lag <= 0) %>% 
  mutate(Negative_Lag_Rank = rank(-abs(Max_ACF))) 
PHI_rank_positive <- Philadelphia %>% 
  filter(Lag > 0) %>% 
  mutate(Positive_Lag_Rank = rank(-abs(Max_ACF))) 
PHI_rank <- left_join(PHI_rank_positive, PHI_rank_negative, by = "Variable")
PHI_rank <- PHI_rank %>% 
  select(-c(City.y, State.y)) %>% 
  rename(City = City.x,
         State = State.x,
         Max_ACF_Positive = Max_ACF.x,
         Lag_Positive = Lag.x,
         Max_ACF_Negative = Max_ACF.y,
         Lag_Negative = Lag.y)
Phoenix <- Phoenix %>% 
  slice(1:100)
PHX_rank_negative <- Phoenix %>% 
  filter(Lag <= 0) %>% 
  mutate(Negative_Lag_Rank = rank(-abs(Max_ACF))) 
PHX_rank_positive <- Phoenix %>% 
  filter(Lag > 0) %>% 
  mutate(Positive_Lag_Rank = rank(-abs(Max_ACF))) 
PHX_rank <- left_join(PHX_rank_positive, PHX_rank_negative, by = "Variable")
PHX_rank <- PHX_rank %>% 
  select(-c(City.y, State.y)) %>% 
  rename(City = City.x,
         State = State.x,
         Max_ACF_Positive = Max_ACF.x,
         Lag_Positive = Lag.x,
         Max_ACF_Negative = Max_ACF.y,
         Lag_Negative = Lag.y)

Portland <- Portland %>% 
  slice(1:100)
portland_rank_negative <- Portland %>% 
  filter(Lag <= 0) %>% 
  mutate(Negative_Lag_Rank = rank(-abs(Max_ACF))) 
portland_rank_positive <- Portland %>% 
  filter(Lag > 0) %>% 
  mutate(Positive_Lag_Rank = rank(-abs(Max_ACF))) 
portland_rank <- left_join(portland_rank_positive, portland_rank_negative, by = "Variable")
portland_rank <- portland_rank %>% 
  select(-c(City.y, State.y)) %>% 
  rename(City = City.x,
         State = State.x,
         Max_ACF_Positive = Max_ACF.x,
         Lag_Positive = Lag.x,
         Max_ACF_Negative = Max_ACF.y,
         Lag_Negative = Lag.y)

raleigh_rank_negative <- Raleigh %>% 
  filter(Lag <= 0) %>% 
  mutate(Negative_Lag_Rank = rank(-abs(Max_ACF))) 
raleigh_rank_positive <- Raleigh %>% 
  filter(Lag > 0) %>% 
  mutate(Positive_Lag_Rank = rank(-abs(Max_ACF))) 
raleigh_rank <- left_join(raleigh_rank_positive, raleigh_rank_negative, by = "Variable")
raleigh_rank <- raleigh_rank %>% 
  select(-c(City.y, State.y)) %>% 
  rename(City = City.x,
         State = State.x,
         Max_ACF_Positive = Max_ACF.x,
         Lag_Positive = Lag.x,
         Max_ACF_Negative = Max_ACF.y,
         Lag_Negative = Lag.y)

sd_rank_negative <- San_Diego %>% 
  filter(Lag <= 0) %>% 
  mutate(Negative_Lag_Rank = rank(-abs(Max_ACF))) 
sd_rank_positive <- San_Diego %>% 
  filter(Lag > 0) %>% 
  mutate(Positive_Lag_Rank = rank(-abs(Max_ACF))) 
sd_rank <- left_join(sd_rank_positive, sd_rank_negative, by = "Variable")
sd_rank <- sd_rank %>% 
  select(-c(City.y, State.y)) %>% 
  rename(City = City.x,
         State = State.x,
         Max_ACF_Positive = Max_ACF.x,
         Lag_Positive = Lag.x,
         Max_ACF_Negative = Max_ACF.y,
         Lag_Negative = Lag.y)
sf_rank_negative <- San_Francisco %>% 
  filter(Lag <= 0) %>% 
  mutate(Negative_Lag_Rank = rank(-abs(Max_ACF))) 
sf_rank_positive <- San_Francisco %>% 
  filter(Lag > 0) %>% 
  mutate(Positive_Lag_Rank = rank(-abs(Max_ACF))) 
sf_rank <- left_join(sf_rank_positive, sf_rank_negative, by = "Variable")
sf_rank <- sf_rank %>% 
  select(-c(City.y, State.y)) %>% 
  rename(City = City.x,
         State = State.x,
         Max_ACF_Positive = Max_ACF.x,
         Lag_Positive = Lag.x,
         Max_ACF_Negative = Max_ACF.y,
         Lag_Negative = Lag.y)
seattle_rank_negative <- Seattle %>% 
  filter(Lag <= 0) %>% 
  mutate(Negative_Lag_Rank = rank(-abs(Max_ACF)))  
seattle_rank_positive <- Seattle %>% 
  filter(Lag > 0) %>% 
  mutate(Positive_Lag_Rank = rank(-abs(Max_ACF))) 
seattle_rank <- left_join(seattle_rank_positive, seattle_rank_negative, by = "Variable")
seattle_rank <- seattle_rank %>% 
  select(-c(City.y, State.y)) %>% 
  rename(City = City.x,
         State = State.x,
         Max_ACF_Positive = Max_ACF.x,
         Lag_Positive = Lag.x,
         Max_ACF_Negative = Max_ACF.y,
         Lag_Negative = Lag.y)
stl_rank_negative <- stl %>% 
  filter(Lag <= 0) %>% 
  mutate(Negative_Lag_Rank = rank(-abs(Max_ACF)))  
stl_rank_positive <- stl %>% 
  filter(Lag > 0) %>% 
  mutate(Positive_Lag_Rank = rank(-abs(Max_ACF))) 
stl_rank <- left_join(stl_rank_positive, stl_rank_negative, by = "Variable")
stl_rank <- stl_rank %>% 
  select(-c(City.y, State.y)) %>% 
  rename(City = City.x,
         State = State.x,
         Max_ACF_Positive = Max_ACF.x,
         Lag_Positive = Lag.x,
         Max_ACF_Negative = Max_ACF.y,
         Lag_Negative = Lag.y)
washington_rank_negative <- Washington %>% 
  filter(Lag <= 0) %>% 
  mutate(Negative_Lag_Rank = rank(-abs(Max_ACF))) 
washington_rank_positive <- Washington %>% 
  filter(Lag > 0) %>% 
  mutate(Positive_Lag_Rank = rank(-abs(Max_ACF))) 
washington_rank <- left_join(washington_rank_positive, washington_rank_negative, by = "Variable")
washington_rank <- washington_rank %>% 
  select(-c(City.y, State.y)) %>% 
  rename(City = City.x,
         State = State.x,
         Max_ACF_Positive = Max_ACF.x,
         Lag_Positive = Lag.x,
         Max_ACF_Negative = Max_ACF.y,
         Lag_Negative = Lag.y)

combined_ranks <- bind_rows(atlanta_rank, austin_rank, boston_rank, 
                            chicago_rank, houston_rank, LA_rank, nashville_rank, 
                            nyc_rank, orlando_rank, PHI_rank, PHX_rank, portland_rank,
                            raleigh_rank,sd_rank, sf_rank, seattle_rank, stl_rank,
                            washington_rank)
rank_avg <- combined_ranks %>% 
  group_by(Variable) %>% 
  summarize(avg_rank_pos = mean(Positive_Lag_Rank),
            avg_rank_neg = mean(Negative_Lag_Rank))
fwrite(rank_avg, paste("Results/CSV Files/Average_City_Rank.csv"))

