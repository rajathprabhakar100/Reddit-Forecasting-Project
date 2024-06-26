ts_city_files <- function(folder_path, filename, explanatory = NULL) {
  #for daily city data, defaults to plotting time series of all variables for a city's daily data. Can also plot ccf
  file_path <- file.path(folder_path, filename)
  data <- fread(file_path)
  data$Date <- as.Date(data$Date)
  
  data_long <- data %>%
    gather(key = "variable", value = "value", -Date) %>% 
    group_by(variable) %>% 
    mutate(rolling_mean = rollmean(value, k = 7, fill = NA)) %>%
    ungroup()
  
  if (!is.null(explanatory)) {
    plot <- ggplot(data, aes(x = Date, y = !!sym(explanatory))) + 
      geom_line() + 
      xlab("Date") + 
      scale_x_date(date_breaks = "4 month", date_labels = "%m %Y") +
      ylab(paste("7-Day Rolling Average of", explanatory)) +
      ggtitle(paste("Time Series Plot of", explanatory, "with 7-Day Rolling Average"))
    return(plot)
  }
  else {
    plots_list <- lapply(unique(data_long$variable), function(var) {
      ggplot(data_long %>% filter(variable == var), aes(Date, rolling_mean)) +
        geom_line() +
        xlab("Date") +
        scale_x_date(date_breaks = "4 month", date_labels = "%m %Y")+
        ylab(paste("7-Day Rolling Average of", var)) +
        ggtitle(paste("Time Series Plot of", var, "with 7-Day Rolling Average"))
    })
    invisible(lapply(plots_list, print))
    
  }
}
