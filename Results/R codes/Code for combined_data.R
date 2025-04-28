library(here)
source(here("Functions/04 - ccf_city_case_files().R"))

files <- list.files("Source Data/Weekly Data") %>% 
  as.vector()
MSA_Codes <- c("C1206", "C1242","C1258", "C1446","C1674", "C1698","C1814",
               "C1910","C1974","C1982","C2642","C2814", "C3108","C3346",
               "C3498","C3538", "C3562", "C3674", "C3798", "C3806", "C3890",
               "C3958","C4162", "C4174", "C4186", "C4266", "C4118","C4530","C2982", "C4790")
Cities <- c("Atlanta", "Austin","Baltimore", "Boston", "Charlotte", "Chicago", "Columbus","Dallas","Denver",
            "Detroit","Houston","Kansas City","Los Angeles","Minneapolis","Nashville","New Orleans",
            "New York City", "Orlando", "Philadelphia", "Phoenix", "Portland", "Raleigh","Salt Lake City",
            "San Diego", "San Francisco", "Seattle", "St. Louis","Tampa Bay","Las Vegas", "Washington DC")
States <- c("Georgia", "Texas", "Maryland", "Massachussetts", "North Carolina", "Illinois", "Ohio", "Texas",
            "Colorado", "Michigan", "Texas", "Missouri", "California", "Minnesota", "Tennessee", "Louisiana",
            "New York", "Florida", "Pennsylvania", "Arizona", "Oregon", "North Carolina", "Utah",
            "California", "California", "Washington", "Missouri", "Florida", "Nevada", "Maryland")
info <- bind_cols(Cities, MSA_Codes) %>% 
  rename(city = `...1`,
         msa_code = `...2`) %>% 
  bind_cols(filename = files, 
            state = States) %>%
  select(city, state, msa_code, filename) %>% 
  rename(City = city, 
         State = state) %>% 
  slice(-16)

result_list <- list()
for (i in 1:nrow(info)) {
  row <- info[i, ]
  City <- row$City
  State <- row$State
  msa_code <- row$msa_code
  filename <- row$filename
  
  # Call your function for each row
  acf_result <- ccf_city_case_files("Source Data/Weekly Data", filename = as.character(filename),
                                    code = msa_code, city = City, state = State, 
                                    plots = FALSE, explanatory = NULL, lags = "both")
  
  # Store the result in the list
  result_list[[i]] <- acf_result
}
result_table <- do.call(rbind, result_list) %>% 
  filter(Variable != "Weekly_Cases" & Variable != "Weekly_Deaths")
result_table[result_table$Variable == "mean_function.", "Variable"] <- "mean_function"
#fwrite(result_table, paste("Results/CSV Files/combined_data.csv"))

combo <- fread("Results/CSV Files/combined_data.csv")

result_list1 <- list()
for (i in 1:nrow(info)) {

  row <- info[i, ]
  City <- row$City
  State <- row$State
  msa_code <- row$msa_code
  filename <- row$filename

  message(paste("Processing", City))
  # Call your function for each row
  acf_result <- ccf_by_year("Source Data/Weekly Data", filename = as.character(filename),
                                    code = msa_code, city = City, state = State)
  print(class(acf_result))
  # Store the result in the list
  result_list1[[i]] <- acf_result

  message(paste("Finished processing", City))
}
result_table1 <- do.call(rbind, result_list1)


fwrite(result_table1, "Results/CSV Files/combined_data_year.csv")
# 
# 
# City <- "Atlanta"
# State <- "Georgia"
# msa_code <- "C1206"
# filename <- "atlanta_weekly.csv"
# acf_result <- ccf_by_year("Source Data/Weekly Data", filename = as.character(filename),
#                           code = msa_code, city = City, state = State)
# 



#Deaths Analysis

result_list_deaths <- list()
for (i in 1:nrow(info)) {
  row <- info[i, ]
  City <- row$City
  State <- row$State
  msa_code <- row$msa_code
  filename <- row$filename
  
  # Call your function for each row
  acf_result <- ccf_city_death_files("Source Data/Weekly Data", filename = as.character(filename),
                                    code = msa_code, city = City, state = State, 
                                    plots = FALSE, explanatory = NULL, lags = "both")
  
  # Store the result in the list
  result_list_deaths[[i]] <- acf_result
}
result_table_deaths <- do.call(rbind, result_list_deaths) %>% 
  filter(Variable != "Weekly_Cases" & Variable != "Weekly_Deaths")
result_table_deaths[result_table_deaths$Variable == "mean_function.", "Variable"] <- "mean_function"
fwrite(result_table_deaths, paste("Results/CSV Files/combined_data_deaths.csv"))

combo_death <- fread("Results/CSV Files/combined_data_deaths.csv")

#Deaths by Year

result_list2 <- list()
for (i in 1:nrow(info)) {
  
  row <- info[i, ]
  City <- row$City
  State <- row$State
  msa_code <- row$msa_code
  filename <- row$filename
  
  message(paste("Processing", City))
  # Call your function for each row
  acf_result <- ccf_deaths_by_year("Source Data/Weekly Data", filename = as.character(filename),
                            code = msa_code, city = City, state = State)
  print(class(acf_result))
  # Store the result in the list
  result_list2[[i]] <- acf_result
  
  message(paste("Finished processing", City))
}
result_table2 <- do.call(rbind, result_list2)


fwrite(result_table2, "Results/CSV Files/combined_data_deaths_year.csv")
combo_death_year <- fread("Results/CSV Files/combined_data_deaths_year.csv")


result_list3 <- list()
for (i in 1:nrow(info)) {
  
  row <- info[i, ]
  City <- row$City
  State <- row$State
  msa_code <- row$msa_code
  filename <- row$filename
  
  message(paste("Processing", City))
  # Call your function for each row
  acf_result <- ccf_tables("Source Data/Weekly Data", filename = as.character(filename),
                           code = msa_code, city = City, state = State, explanatory = NULL)
  acf_table <- acf_result$complete_table %>%
    as_tibble() %>%
    filter(Lag >= -6 & Lag <= 6) %>% 
    mutate(Lag = as.factor(Lag)) %>% 
    group_by(Lag, City) %>% 
    slice_max(order_by = abs(ACF), n = 1)

  # Store the result in the list
  result_list3[[i]] <- acf_table
  
  message(paste("Finished processing", City))
}
result_table3 <- do.call(rbind, result_list3)
