#combine city files with allcities filtered data to produce city_clean file
allcities <- fread("Old Data/liwc22_allcities_fromJan2019toMay2020.csv") 
allcities <- allcities %>%
  mutate(Date = as_datetime(created_utc, tz = "America/Chicago") %>% 
           as_date()) %>%
  select(-created_utc) %>% 
  relocate(Date, .after = 4)

combine_reddit <- function(folder_path, allcities, filename) {
  
  # Prepares path of the specific liwc22_ file
  liwc_file_path <- file.path(folder_path, filename)
  
  # Read the specific liwc22_ file into a data frame
  liwc_df <- fread(liwc_file_path)
  liwc_df <- liwc_df %>% 
    mutate(Date = as_datetime(created_utc, tz = "America/Chicago") %>% 
             as_date()) %>%
    select(-created_utc) %>% 
    relocate(Date, .after = 4)
  
  # Extract the city name from the specified file name (e.g., liwc22_Atlanta.csv -> "Atlanta")
  city_name <- gsub("^liwc22_(.+)\\.csv$", "\\1", filename)
  
  # Filter the data from allcities for the corresponding city based on the subreddit column
  city_data <- allcities %>%
    filter(subreddit == city_name)
  message(paste("Finished filtering", city_name))
  
  # Check if city_data is empty
  if (nrow(city_data) == 0) {
    message(paste("No matching data for city:", city_name))
    return(NULL)
  }
  
  # Combine the data from allcities with the liwc22_ data
  combined_df <- bind_rows(city_data, liwc_df)
  
  # Create a new file with the combined data (e.g., "Atlanta_clean.csv")
  fwrite(combined_df, paste0(city_name, "_clean.csv"))
  
  message(paste(city_name, "exported to local folder"))
}
