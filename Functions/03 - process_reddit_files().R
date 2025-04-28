library(data.table)
library(tidyverse)
library(zoo)

process_reddit_files <- function(folder_path, filename) {
  file_path <- file.path(folder_path, filename)
  data <- fread(file_path) %>%
    mutate(Date = as_datetime(created_utc, tz = "America/Chicago") %>%
             as_date()) %>%
    select(-created_utc) %>%
    select(author, Date, everything()) %>%
    mutate(author = replace(author, author %in% c("[deleted]", "AutoModerator"), NA)) %>%
    select(-c(author, subreddit, parent_id, score, id, Segment)) %>%
    group_by(Date) %>%
    summarize_all(mean, na.rm = T) %>%
    rename_with(~paste0("mean_", .), -Date) %>%
    mutate_at(vars(starts_with("mean_")),
              list(~rollmean(., k = 7, fill = NA, align = "right"))) %>%
    arrange(Date) %>%
    filter(Date >= "2019-01-01")



  # Extract city name from the filename
  city_name <- gsub("^merged_(.*?).csv$", "\\1", basename(filename))

  # Export the data frame with the modified city name in the filename
  output_filename <- paste0("Source Data/Daily Data/", city_name, "_daily.csv")
  #output_filename <- paste0("Source Data/Daily Data - New/", city_name, "_daily.csv")
  #output_filename <- paste0("Source Data/Daily Data - New1/", city_name, "_daily.csv")
  fwrite(data, output_filename)

  message(paste(output_filename, "exported to local folder"))
  #return(means_significant)
}

#process_reddit_files(folder_path = "Source Data/02 - Clean Data/", filename = "Atlanta_clean.csv")
#process_reddit_files(folder_path = "Source Data/02 - Clean Data", filename = "Austin_clean.csv")
#process_reddit_files(folder_path = "Source Data/02 - Clean Data/", filename = "boston_clean.csv")
#process_reddit_files(folder_path = "Source Data/02 - Clean Data/", filename = "chicago_clean.csv")
#process_reddit_files(folder_path = "Source Data/02 - Clean Data/", filename = "houston_clean.csv")
#process_reddit_files(folder_path = "Source Data/02 - Clean Data/", filename = "LosAngeles_clean.csv")
#process_reddit_files(folder_path = "Source Data/02 - Clean Data/", filename = "nashville_clean.csv")
process_reddit_files(folder_path = "Source Data/01 - Raw/", filename = "merged_newyorkcity.csv")
#process_reddit_files(folder_path = "Source Data/02 - Clean Data/", filename = "orlando_clean.csv")
#process_reddit_files(folder_path = "Source Data/02 - Clean Data/", filename = "philadelphia_clean.csv")
#process_reddit_files(folder_path = "Source Data/02 - Clean Data/", filename = "Portland_clean.csv")
#process_reddit_files(folder_path = "Source Data/02 - Clean Data/", filename = "phoenix_clean.csv")
process_reddit_files(folder_path = "Source Data/02 - Clean Data/", filename = "raleigh_clean.csv")
process_reddit_files(folder_path = "Source Data/02 - Clean Data/", filename = "sandiego_clean.csv")
process_reddit_files(folder_path = "Source Data/02 - Clean Data/", filename = "sanfrancisco_clean.csv")
process_reddit_files(folder_path = "Source Data/02 - Clean Data/", filename = "Seattle_clean.csv")
process_reddit_files(folder_path = "Source Data/02 - Clean Data/", filename = "StLouis_clean.csv")
process_reddit_files(folder_path = "Source Data/02 - Clean Data/", filename = "washingtondc_clean.csv")
