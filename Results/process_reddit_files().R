process_reddit_files <- function(folder_path, filename) {
  file_path <- file.path(folder_path, filename)
  data <- fread(file_path)
  data <- data %>%
    mutate(author = replace(author, author %in% c("[deleted]", "AutoModerator"), NA),
           body = replace(body, body == "[removed]", NA)) %>%
    na.omit()
  
  new_data <- data %>% select(-c(1:4, 6:10))
  means <- data.frame(Date = unique(new_data$Date))
  
  for (col in colnames(new_data)[-1]) {
    col_means <- tapply(new_data[[col]], new_data$Date, mean)
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
  output_filename <- paste0(city_name, "_daily.csv")
  
  fwrite(means_significant, output_filename)
  
  message(paste(output_filename, "exported to local folder"))
  #return(means_significant)
}
