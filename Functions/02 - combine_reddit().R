#memory.limit(size = 16000)
source("Functions/01 - data_setup().R")
#source("Functions/03 - process_reddit_files().R")
combine_reddit <- function(folder_path, allcities = allcities, filename) {
  
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
  fwrite(combined_df, paste0("Source Data/02 - Clean Data/", city_name, "_clean.csv"))
  
  message(paste(city_name, "exported to local folder"))
}
#combine_reddit("Source Data/01 - Raw", allcities, filename = "liwc22_Atlanta.csv")
#combine_reddit("Source Data/01 - Raw", allcities, filename = "liwc22_Austin.csv")
#combine_reddit("Source Data/01 - Raw", allcities, filename = "liwc22_LosAngeles.csv")
#combine_reddit("Source Data/01 - Raw", allcities, filename = "liwc22_nashville.csv")
#combine_reddit("Source Data/01 - Raw", allcities, filename = "liwc22_nyc.csv")
#combine_reddit("Source Data/01 - Raw", allcities, filename = "liwc22_orlando.csv")
#combine_reddit("Source Data/01 - Raw", allcities, filename = "liwc22_philadelphia.csv")
#combine_reddit("Source Data/01 - Raw", allcities, filename = "liwc22_Portland.csv")
#combine_reddit("Source Data/01 - Raw", allcities, filename = "liwc22_phoenix.csv")
#combine_reddit("Source Data/01 - Raw", allcities, filename = "liwc22_raleigh.csv")
#combine_reddit("Source Data/01 - Raw", allcities, filename = "liwc22_sandiego.csv")
#combine_reddit("Source Data/01 - Raw", allcities, filename = "liwc22_sanfrancisco.csv")
#combine_reddit("Source Data/01 - Raw", allcities, filename = "liwc22_Seattle.csv")
#combine_reddit("Source Data/01 - Raw", allcities, filename = "liwc22_StLouis.csv")
#combine_reddit("Source Data/01 - Raw", allcities, filename = "liwc22_washingtondc.csv")
