library(data.table)
library(tidyverse)
library(zoo)
process_reddit_files <- function(folder_path, filename) {
  file_path <- file.path(folder_path, filename)
  data <- fread(file_path)
  data <- data %>%
    mutate(author = replace(author, author %in% c("[deleted]", "AutoModerator"), NA),
           body = replace(body, body == "[removed]", NA)) %>%
    select(-c(1:4, 6:10))
  means <- data.frame(Date = unique(data$Date))
  
  for (col in colnames(data)[-1]) {
    col_means <- tapply(data[[col]], data$Date, mean, na.rm = T)   
    means[[paste0("mean_", col)]] <- col_means[match(means$Date, names(col_means))]
  }
  
  means_significant <- means %>%
    select(Date, starts_with("mean_")) %>%
    mutate_at(vars(starts_with("mean_")),
              list(~rollmean(., k = 7, fill = NA, align = "right"))) %>% 
    arrange(Date)
  
  # Extract city name from the filename
  city_name <- gsub("_clean.csv$", "", basename(filename))
  
  # Export the data frame with the modified city name in the filename
  output_filename <- paste0("Source Data/03 - Daily Data/", city_name, "_daily.csv")
  
  fwrite(means_significant, output_filename)
  
  message(paste(output_filename, "exported to local folder"))
  #return(means_significant)
}

process_reddit_files(folder_path = "Source Data/02 - Clean Data/", filename = "Atlanta_clean.csv")
process_reddit_files(folder_path = "Source Data/02 - Clean Data", filename = "Austin_clean.csv")
process_reddit_files(folder_path = "Source Data/02 - Clean Data/", filename = "boston_clean.csv")
process_reddit_files(folder_path = "Source Data/02 - Clean Data/", filename = "chicago_clean.csv")
process_reddit_files(folder_path = "Source Data/02 - Clean Data/", filename = "houston_clean.csv")
process_reddit_files(folder_path = "Source Data/02 - Clean Data/", filename = "LosAngeles_clean.csv")
process_reddit_files(folder_path = "Source Data/02 - Clean Data/", filename = "nashville_clean.csv")
process_reddit_files(folder_path = "Source Data/02 - Clean Data/", filename = "nyc_clean.csv")
process_reddit_files(folder_path = "Source Data/02 - Clean Data/", filename = "orlando_clean.csv")
process_reddit_files(folder_path = "Source Data/02 - Clean Data/", filename = "philadelphia_clean.csv")
process_reddit_files(folder_path = "Source Data/02 - Clean Data/", filename = "Portland_clean.csv")
process_reddit_files(folder_path = "Source Data/02 - Clean Data/", filename = "phoenix_clean.csv")
process_reddit_files(folder_path = "Source Data/02 - Clean Data/", filename = "raleigh_clean.csv")
process_reddit_files(folder_path = "Source Data/02 - Clean Data/", filename = "sandiego_clean.csv")
process_reddit_files(folder_path = "Source Data/02 - Clean Data/", filename = "sanfrancisco_clean.csv")
process_reddit_files(folder_path = "Source Data/02 - Clean Data/", filename = "Seattle_clean.csv")
process_reddit_files(folder_path = "Source Data/02 - Clean Data/", filename = "StLouis_clean.csv")
process_reddit_files(folder_path = "Source Data/02 - Clean Data/", filename = "washingtondc_clean.csv")
