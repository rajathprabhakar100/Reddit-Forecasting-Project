library(tidyverse)
library(ciTools)
library(cowplot)
reddit_and_cases <- read_csv("Results/CSV Files/reddit_and_cases_deaths.csv") %>% 
  select(where(~ !all(is.na(.)))) %>%
  na.omit() %>% 
  rename(mean_function = mean_function.)

model_selection_algorithm <- function(city) {
  filter_city <- reddit_and_cases %>% 
    filter(City == city) %>% 
    filter(week >= "2020-04-05")
  
  date_vector <- as_date(as.vector(filter_city$week))
  date_vector <- date_vector[date_vector >= "2021-02-28"]
  
  results_by_date <- list()
  
  for (date in date_vector) {
    message(paste("Calculating", as.character(date), "for", city))
    city_correlation <- filter_city %>%
      filter(week <= date - 84) %>% 
      select(where(~ !all(is.na(.)))) %>% 
      summarize(across(starts_with("mean_"), ~cor(., Weekly_Cases, use = "complete.obs"), .names = "{col}")) %>% 
      pivot_longer(cols = everything(), names_to = "Variable", values_to = "Correlation") %>% 
      arrange(desc(abs(Correlation))) %>% 
      slice(1:40)
    
    variables <- as.vector(city_correlation$Variable)
    results <- list()
    
    for (variable in 1:length(variables)) {
      formula_str <- paste("Weekly_Cases ~", paste(variables[1:variable], collapse = "+"))
      #print(formula_str)
      
      city_training_data <- reddit_and_cases %>%
        filter(City == city) %>%
        filter(week <= date - 84)
      
      # Use tryCatch to handle errors
      result <- tryCatch({
        model <- glm(as.formula(formula_str), data = city_training_data, family = "poisson")
        
        city_prediction_data <- reddit_and_cases %>%
          filter(City == city) %>%
          filter(week > date - 84 & week <= date) %>%
          add_pi(df = ., fit = model, names = c("Forecast_Lwr", "Forecast_Upr")) %>%
          select(week, Weekly_Cases, Forecast_Lwr, pred, Forecast_Upr, everything()) %>%
          summarize(num_pred = variable,
                    MAE = mean(abs(Weekly_Cases - pred)),
                    MAPE = (100/nrow(.)) * sum(abs((Weekly_Cases - pred)/Weekly_Cases)))
        
        results[[variable]] <- city_prediction_data
        TRUE  # Indicate success
      }, error = function(e) {
        message("Error in model fitting: ", e$message)
        FALSE  # Indicate failure
      })
      
      # If an error occurred, break the inner loop
      if (!result) {
        break
      }
    }
    
    result_df <- bind_rows(results)
    results_by_date[[as.character(as_date(date))]] <- result_df
    
  }
  
  
  return(results_by_date)
}



model_selection_pca <- function(city) {
  filter_city <- reddit_and_cases %>% 
    filter(City == city) %>% 
    filter(week >= "2020-04-05") %>% 
    select(-c(Cumulative_Cases, Cumulative_Deaths, Weekly_Deaths)) %>% 
    filter(complete.cases(.))
  
  date_vector <- as_date(as.vector(filter_city$week))
  date_vector <- date_vector[date_vector >= "2020-10-04"]
  
  results_by_date <- list()
  
  for (date in date_vector) {
    message(paste("Calculating", as.character(as_date(date)), "for", city))
    filter_city1 <- filter_city %>% 
      select(-c(MSA_Code, MSA_Title, Est_Population, Population, City)) %>% 
      filter(week <= date - 84)
    
    prcomp <- prcomp(filter_city1 %>% select(starts_with("mean_")), scale = TRUE)
    prcomp$rotation <- -1 * prcomp$rotation
    rotation <- data.frame(prcomp$rotation)
    rotation <- rownames_to_column(rotation, var = "Variable")
    r2 <- data.frame(R2 = (prcomp$sdev)^2 / sum((prcomp$sdev)^2)) %>% 
      mutate(Cumulative_R2 = cumsum(R2),
             PC = seq(1, nrow(.), by = 1)) %>% 
      select(PC, R2, Cumulative_R2)
    
    rotation1 <- rotation %>%
      summarize(across(starts_with("PC"),
                       list(variable = ~ Variable[which.max(.)]), .names = "{.col}_{.fn}")) %>%
      t() %>% 
      as.data.frame() %>% 
      rename(Variable = V1) %>% 
      bind_cols(r2) %>% 
      rownames_to_column(., var = "rowname") %>% 
      select(-rowname) %>% 
      select(PC, everything()) %>% 
      mutate(R2 = round(R2, 3), 
             Cumulative_R2 = round(Cumulative_R2, 3)) %>% 
      filter(Cumulative_R2 <= 0.998)
    
    variables <- as.vector(rotation1$Variable)
    additional_column <- c("Weekly_Cases", "week")
    variables <- c(variables, additional_column)
    
    results <- list()
    for (variable in 1:length(variables)) {
      formula_str <- paste("Weekly_Cases ~ Weekly_Cases7 + ", paste(variables[1:variable], collapse = "+"))
      city_training_data <- reddit_and_cases %>%
        filter(City == city) %>%
        mutate(Weekly_Cases7 = lag(Weekly_Cases, n = 1)) %>% 
        filter(week <= date - 84)
      
      result <- tryCatch({
        model <- glm(as.formula(formula_str), data = city_training_data, family = "poisson")
        
        insample <- reddit_and_cases %>% 
          filter(City == city) %>% 
          mutate(Weekly_Cases7 = lag(Weekly_Cases, n = 1)) %>% 
          filter(week <= date - 84) %>% 
          add_pi(df = ., fit = model, names = c("Forecast_Lwr", "Forecast_Upr")) %>% 
          select(week, Weekly_Cases, Forecast_Lwr, pred, Forecast_Upr, everything())
        outsample <- reddit_and_cases %>%
          filter(City == city) %>%
          mutate(Weekly_Cases7 = lag(Weekly_Cases, n = 1)) %>% 
          filter(week > date - 84 & week <= date) %>%
          add_pi(df = ., fit = model, names = c("Forecast_Lwr", "Forecast_Upr")) %>%
          select(week, Weekly_Cases, Forecast_Lwr, pred, Forecast_Upr, everything())
        city_prediction_data_summary <- outsample %>%
          summarize(num_pred = variable,
                    MAE = mean(abs(Weekly_Cases - pred)),
                    MAPE = (100/nrow(.)) * sum(abs((Weekly_Cases - pred)/Weekly_Cases)))
        
        results[[paste0("predictors", variable)]] <- list(
          training = city_training_data,
          insample_data = insample,
          outsample_data = outsample,
          summary1 = city_prediction_data_summary
        )
        TRUE  # Indicate success
      },
      error = function(e) {
        message("Error in model fitting: ", e$message)
        FALSE  # Indicate failure
      })
      
      if (!result) {
        break
      }
    }
    
    results_by_date[[as.character(as_date(date))]] <- results
  }
  return(results_by_date)
}

atlanta1 <- model_selection_pca("Atlanta")


all_summaries <- list()  
for (date in names(atlanta1)) {
  date_summary <- list()
  for (predictor in names(atlanta1[[date]])) {
    summary_for_predictor <- atlanta1[[date]][[predictor]]$summary1
    
    # Add a new column with the current date
    summary_for_predictor$week <- date
    
    # Append the modified summary data frame to the list
    date_summary[[predictor]] <- summary_for_predictor
  }
  summaries_for_date <- bind_rows(date_summary)
  all_summaries[[date]] <- summaries_for_date

}



ggplot(atlanta4, aes(x = week))+
  geom_point(aes(y = Weekly_Cases))+
  geom_line(aes(y = pred))+
  geom_ribbon(aes(ymin = Forecast_Lwr, ymax = Forecast_Upr), fill = "yellow", alpha = 0.2)+
  labs(x = "Week",
       y = "Weekly Cases",
       title = "Expected vs Actual Cases Using 84 Predictors",
       subtitle = "2022-08-21 In-sample fit")

ggplot(atlanta5, aes(x = week))+
  geom_point(aes(y = Weekly_Cases))+
  geom_line(aes(y = pred))+
  geom_ribbon(aes(ymin = Forecast_Lwr, ymax = Forecast_Upr), fill = "yellow", alpha = 0.2)+
  labs(x = "Week",
       y = "Weekly Cases",
       title = "Expected vs Actual Cases Using 1 Predictor",
       subtitle = "2022-08-21 out-sample fit")

x <- "2021-07-04"
w <- "2022-08-21"
ggplot(atlanta1 %>% filter(week == w), aes(x = num_pred, y = MAE))+
  geom_point()+
  geom_line()+
  labs(x = "Number of Predictors",
       y = "MAE", title = paste("# Principal Components vs. MAE for", x),
       subtitle = "PCA")

ggplot(atlanta1 %>% filter(week == w), aes(x = num_pred, y = MAPE))+
  geom_point()+
  geom_line()+
  labs(x = "Number of Predictors",
       y = "Mean Absolute Percentage Error (MAPE)", title = paste("Number of Predictors vs. MAPE for", w),
       subtitle = "1 predictor")

for (date in as.vector(unique(atlanta1$week))) {
  modified_date <- gsub("-", "_", date)
  #print(as.character(date)) 
  p <- ggplot(atlanta1 %>% filter(week == date), aes(x = num_pred, y = MAE))+
    geom_point()+
    geom_line()+
    labs(x = "Principal Components",
         y = "MAE", title = paste("Principal Components vs. MAE for", date))
  #print(p)
  save_plot(paste0("Results/Graphs/2024_08_12/MAE/", modified_date, ".png"), p, base_width = 8, base_height = 8)
  
}

for (date in as.vector(unique(atlanta1$week))) {
  modified_date <- gsub("-", "_", date)
  p <- ggplot(atlanta1 %>% filter(week == date), aes(x = num_pred, y = MAPE))+
    geom_point()+
    geom_line()+
    labs(x = "Principal Components",
         y = "MAPE", title = paste("Principal Components vs. MAPE for", date))
  
  save_plot(paste0("Results/Graphs/2024_08_12/MAPE/", modified_date, ".png"), p, base_width = 8, base_height = 8)
}

































