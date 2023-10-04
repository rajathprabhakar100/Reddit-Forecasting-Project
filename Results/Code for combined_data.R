atlanta <- ccf_city_case_files("Daily Data", "atlanta_daily.csv", code = "C1206", city = "Atlanta", state = "Georgia", plots = FALSE, lags = "both")
austin
boston
chicago
houston
LA
nashville
nyc
orlando
philadelphia
phoenix
portland
raleigh
san_diego
san_francisco
seattle
stl
washington
combo <- bind_rows(atlanta, austin, boston, chicago, houston, LA, nashville, nyc, orlando, philadelphia, phoenix, portland, raleigh, san_diego, san_francisco, seattle, stl, washingtondc) %>% 
  select(City, State, Variable, Max_ACF, Lag)
fwrite(combo, paste("combined_data.csv"))
