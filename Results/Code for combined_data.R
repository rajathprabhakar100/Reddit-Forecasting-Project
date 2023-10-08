atlanta <- ccf_city_case_files("Daily Data", "atlanta_daily.csv", code = "C1206", city = "Atlanta", state = "Georgia", plots = FALSE, lags = "both")
austin <- ccf_city_case_files("Daily Data", "austin_daily.csv", code = "C1242", city = "Austin", state = "Texas", plots = FALSE, lags = "both")
boston <- ccf_city_case_files("Daily Data", "boston_daily.csv", code = "C1446", city = "Boston", state = "Massachusetts", plots = FALSE, lags = "both")
chicago <- ccf_city_case_files("Daily Data", "chicago_daily.csv", code = "C1698", city = "Chicago", state = "Illinois", plots = FALSE, lags = "both")
houston <- ccf_city_case_files("Daily Data", "houston_daily.csv", code = "C2642", city = "Houston", state = "Texas", plots = FALSE, lags = "both")
LA <- ccf_city_case_files("Daily Data", "losangeles_daily.csv", code = "C3108", city = "Los Angeles", state = "California", plots = FALSE, lags = "both")
nashville <- ccf_city_case_files("Daily Data", "nashville_daily.csv", code = "C3498", city = "Nashville", state = "Tennessee", plots = FALSE, lags = "both")
nyc <- ccf_city_case_files("Daily Data", "nyc_daily.csv", code = "C3562", city = "New York", state = "New York", plots = FALSE, lags = "both")
orlando <- ccf_city_case_files("Daily Data", "orlando_daily.csv", code = "C3674", city = "Orlando", state = "Florida", plots = FALSE, lags = "both")
philadelphia <- ccf_city_case_files("Daily Data", "philadelphia_daily.csv", code = "C3798", city = "Philadelphia", state = "Pennsylvania", plots = FALSE, lags = "both")
phoenix <- ccf_city_case_files("Daily Data", "phoenix_daily.csv", code = "C3806", city = "Phoenix", state = "Arizona", plots = FALSE, lags = "both")
portland <- ccf_city_case_files("Daily Data", "portland_daily.csv", code = "C3890", city = "Portland", state = "Oregon", plots = FALSE, lags = "both")
raleigh <- ccf_city_case_files("Daily Data", "raleigh_daily.csv", code = "C3958", city = "Raleigh", state = "North Carolina", plots = FALSE, lags = "both")
san_diego <- ccf_city_case_files("Daily Data", "sandiego_daily.csv", code = "C4174", city = "San Diego", state = "California", plots = FALSE, lags = "both")
san_francisco <- ccf_city_case_files("Daily Data", "sanfrancisco_daily.csv", code = "C4186", city = "San Francisco", state = "California", plots = FALSE, lags = "both")
seattle <- ccf_city_case_files("Daily Data", "seattle_daily.csv", code = "C4266", city = "Seattle", state = "Washington", plots = FALSE, lags = "both")
stl <- ccf_city_case_files("Daily Data", "stlouis_daily.csv", code = "C4118", city = "St. Louis", state = "Missouri", plots = FALSE, lags = "both")
washington <- ccf_city_case_files("Daily Data", "washingtondc_daily.csv", code = "C4790", city = "Washington", state = "Maryland", plots = FALSE, lags = "both")



combo <- bind_rows(atlanta, austin, boston, chicago, houston, LA, nashville, nyc, orlando, philadelphia, phoenix, portland, raleigh, san_diego, san_francisco, seattle, stl, washington) %>% 
  select(City, State, Variable, Max_ACF, Lag)
fwrite(combo, paste("Results/CSV Files/combined_data.csv"))