library(tidyverse)
library(covidcast)
library(cowplot)
library(here)
library(data.table)
library(readxl)
stl <- fread("Source Data/01 - Raw/merged_stlouis.csv")

stl1 <- head(stl, 10000)
nyc_reddit <- read_csv("Mask Wearing Analysis/nycCOVID.txt") %>% 
  mutate(Date = as.Date(date_of_interest, format = "%m/%d/%Y")) %>%
  mutate(week = floor_date(Date, unit = "week", week_start = 7)) %>%
  select(-c(date_of_interest, Date)) %>% 
  select(week, everything()) %>%
  rename(cases = CASE_COUNT,
         deaths = DEATH_COUNT,
         hospitalizations = HOSPITALIZED_COUNT) %>%
  group_by(week) %>% 
  summarize(cases = sum(cases),
            deaths = sum(deaths),
            hospitalizations = sum(hospitalizations)) %>% 
  ungroup() %>% 
  right_join(read_csv("Source Data/Weekly Data/nyc_weekly.csv"), by = "week") %>% 
  arrange(week) 


mask_nyc <- read_csv("Mask Wearing Analysis/data_download_file_reference_2020.csv") %>% 
  bind_rows(read_csv("Mask Wearing Analysis/data_download_file_reference_2021.csv")) %>% 
  bind_rows(read_csv("Mask Wearing Analysis/data_download_file_reference_2022.csv")) %>% 
  filter(location_name == "New York") %>% 
  select(location_id:location_name, mask_use_mean, mask_use_obs)
signals <- c("completely_home_prop", "full_time_work_prop", "part_time_work_prop", "median_home_dwell_time",
             "bars_visit_prop", "restaurants_visit_num")

mobility_list <- list()

for (s in signals) {
  df <- covidcast_signal(data_source = "safegraph", signal = s, 
                         start_day = "2020-01-01", end_day = "2020-07-01",
                         geo_type = "state", issues = "2020-11-30", geo_values = "ny")
  mobility_list[[s]] <- df
}

mobility <- bind_rows(mobility_list) %>% 
  mutate(signal = as.factor(signal)) %>% 
  rename(date = time_value)

nyc_master <- left_join(mobility, mask_nyc, by = "date") %>% 
  drop_na(signal) %>% 
  pivot_wider(names_from = "signal", values_from = "value") %>% 
  mutate(week = floor_date(date, unit = "week", week_start = 7)) %>% 
  select(location_id, week, everything()) %>%
  select(-date) %>%
  group_by(week) %>% 
  reframe(location_id = first(location_id),
          location_name = first(location_name),
          mask_use_mean = mean(mask_use_mean),
          mask_use_obs = sum(mask_use_obs),
          bars_visit_prop = mean(bars_visit_prop),
          restaurants_visit_num = mean(restaurants_visit_num),
          across(starts_with("mean_"), ~mean(.x, na.rm = T))) %>% 
  ungroup() %>% 
  inner_join(nyc_reddit, by = "week") %>% 
  group_by(week) %>% 
  reframe(location_id = first(location_id),
          location_name = first(location_name),
          mask_use_mean = mean(mask_use_mean),
          mask_use_obs = sum(mask_use_obs),
          cases = first(cases),
          deaths = first(deaths),
          hospitalizations = first(hospitalizations),
          bars_visit_prop = mean(bars_visit_prop),
          restaurants_visit_num = mean(restaurants_visit_num),
          across(starts_with("mean_"), ~mean(.x, na.rm = T))) %>% 
  select(week:mask_use_mean, cases:hospitalizations, bars_visit_prop, restaurants_visit_num,
         mean_Analytic, mean_cogproc, mean_emo_anx, mean_emo_anger, mean_emo_sad, mean_emo_pos,
         mean_COVID_Related, mean_family, mean_friend, mean_mental, mean_conflict) %>% 
  select(-c(location_id, location_name)) %>%
  filter(week >= "2020-01-01" & week <= "2020-06-30") %>%
  mutate(across(-week, ~ scale(.))) %>%
  select(week, cases, hospitalizations, deaths, everything())

graph_reddit <- function(data, x, y, type = NULL, lag_value = 0, variable_to_lag = c("x", "y")) {
  #Check x is valid
  if (!x %in% names(data)) {
    stop("x not in data frame")
  }
  
  #Check y is valid
  if (!y %in% names(data)) {
    stop("y not in data frame")
  }
  
  #Check type is valid
  available_geoms <- ls(pattern = "^geom_", envir = asNamespace("ggplot2"))
  allowed_geoms <- gsub("^geom_", "", available_geoms)
  if (!is.null(type)) {
    if (!type %in% allowed_geoms) {
      stop("Invalid type. Available geoms: ", paste(allowed_geoms, collapse = ", "))
    }
  } 
  
  else {
    ggplot2::ggplot(data, ggplot2::aes(!!sym(x), !!sym(y)))
  }
  
  #Check lag and variable_to_lag is specified
  if (lag_value < 0) {
    stop("lag_value cannot be less than 0")
  }

  else if (lag_value > 0) {
    if (!all(variable_to_lag %in% c("x", "y"))) {
      stop("variable_to_lag not specified")
    }
    else {
      data1 <- data %>%
        mutate(
          !!x := if ("x" %in% variable_to_lag) lag(.data[[x]], lag_value) else .data[[x]],
          !!y := if ("y" %in% variable_to_lag) lag(.data[[y]], lag_value) else .data[[y]]
        )
    }

  }
  
  else {
    data1 <- data
  }


  #Plot
  

  geom_func <- get(paste0("geom_", type), envir = asNamespace("ggplot2"))
  
  p <- ggplot(data1, aes(!!sym(x), !!sym(y))) + 
    geom_func()
  p <- p + 
    labs(x = x,
         y = y,
         title = paste(x, "vs.", y),
         subtitle = paste(x, "lag =", as.character(lag_value)))
  return(p)
}

lags <- c(0, 1, 2)
reddit_variables = c("mean_cogproc", "mean_emo_anx", "mean_Analytic")
variables_list <- list()
for (v in reddit_variables) {
  lag_list <- list()
  for (i in lags) {
    label_name <- paste0("lag", i)
    x <- graph_reddit(nyc_master, x = v, y = "mask_use_mean", type = "point", lag_value = i, variable_to_lag = "x")
    lag_list[[label_name]] <- x
  }
  p <- plot_grid(lag_list$lag0, lag_list$lag1, lag_list$lag2, nrow = 1, align = "h") 
  variables_list[[v]] <- p
}
variables_list$mean_cogproc
variables_list$mean_emo_anx
variables_list$mean_Analytic
nyc_master_long <- nyc_master %>% pivot_longer(cols = -week, names_to = "variable", values_to = "value")


reddit <- ggplot(nyc_master %>% filter(variable %in% c("mean_Analytic", "mean_cogproc", "mean_emo_anger", "mean_emo_anx")),
                 aes(x = week, y = value, color = variable))+
  geom_line()+
  scale_x_date(date_breaks = "month")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))
behavior <- ggplot(nyc_master %>% filter(variable %in% c("bars_visit_prop", "restaurants_visit_num")), 
                   aes(x = week, y = value, color = variable))+
  geom_line()+
  scale_x_date(date_breaks = "month")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))
epi <- ggplot(nyc_master %>% filter(variable %in% c("cases", "deaths", "hospitalizations")), 
              aes(x = week, y = value, colour = variable))+
  geom_line()+
  scale_x_date(date_breaks = "month")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))
reddit
behavior  
epi
nyc_grid <- plot_grid(reddit, behavior, epi, ncol = 1, align = "v")
nyc_grid
save_plot("nyc_behavioral_plot.png", nyc_grid, base_width = 8, base_height = 8)

ggplot(nyc_master1, aes(x = week))+
  geom_line(aes(y = mean_emo_anx, color = "Anxiety"))+
  geom_line(aes(y = bars_visit_prop, color = "Visits to Bars"))+
  geom_line(aes(y = cases, color = "Cases"))+
  scale_color_manual(name = "Legend",
                     values = c("Anxiety" = "red",
                     "Visits to Bars" = "blue",
                     "Cases" = "green"))+
  labs(x = "Week",
       y = "Scaled Value",
       title = "Trends in Indicators")



nyc <- read_csv("Source Data/Daily Data/newyorkcity_daily.csv")
#Daily Data
nyc_reddit <- read.csv("Mask Wearing Analysis/nycCOVID.txt") %>% 
  mutate(Date = as.Date(date_of_interest, format = "%m/%d/%Y")) %>%
  select(-date_of_interest) %>% 
  select(Date, everything()) %>%
  rename(cases = CASE_COUNT,
         deaths = DEATH_COUNT,
         hospitalizations = HOSPITALIZED_COUNT) %>%
  group_by(Date) %>% 
  summarize(cases = sum(cases),
            deaths = sum(deaths),
            hospitalizations = sum(hospitalizations)) %>% 
  ungroup() %>% 
  right_join(read_csv("Source Data/Daily Data/newyorkcity_daily.csv"), by = "Date") %>% 
  arrange(Date) 


mask_nyc <- read_csv("Mask Wearing Analysis/data_download_file_reference_2020.csv") %>% 
  bind_rows(read_csv("Mask Wearing Analysis/data_download_file_reference_2021.csv")) %>% 
  bind_rows(read_csv("Mask Wearing Analysis/data_download_file_reference_2022.csv")) %>% 
  filter(location_name == "New York") %>% 
  select(location_id:location_name, mask_use_mean, mask_use_obs)
signals <- c("completely_home_prop", "full_time_work_prop", "part_time_work_prop", "median_home_dwell_time",
             "bars_visit_prop", "restaurants_visit_num")

mobility_list <- list()
for (s in signals) {
  df <- covidcast_signal(data_source = "safegraph", signal = s, 
                         start_day = "2020-01-01", end_day = "2020-07-01",
                         geo_type = "state", issues = "2020-11-30", geo_values = "ny")
  mobility_list[[s]] <- df
}

mobility <- bind_rows(mobility_list) %>% 
  mutate(signal = as.factor(signal)) %>% 
  rename(date = time_value)

nyc_master <- left_join(mobility, mask_nyc, by = "date") %>% 
  drop_na(signal) %>% 
  pivot_wider(names_from = "signal", values_from = "value") %>% 
  rename(Date = date) %>% 
  select(location_id, Date, everything()) %>%
  group_by(Date) %>% 
  reframe(location_id = first(location_id),
          location_name = first(location_name),
          mask_use_mean = mean(mask_use_mean),
          mask_use_obs = sum(mask_use_obs),
          bars_visit_prop = mean(bars_visit_prop),
          restaurants_visit_num = mean(restaurants_visit_num),
          across(starts_with("mean_"), ~mean(.x, na.rm = T))) %>% 
  ungroup() %>% 
  inner_join(nyc_reddit, by = "Date") %>% 
  group_by(Date) %>% 
  reframe(location_id = first(location_id),
          location_name = first(location_name),
          mask_use_mean = mean(mask_use_mean),
          mask_use_obs = sum(mask_use_obs),
          cases = first(cases),
          deaths = first(deaths),
          hospitalizations = first(hospitalizations),
          bars_visit_prop = mean(bars_visit_prop),
          restaurants_visit_num = mean(restaurants_visit_num),
          across(starts_with("mean_"), ~mean(.x, na.rm = T))) %>% 
  select(Date:mask_use_mean, cases:hospitalizations, bars_visit_prop, restaurants_visit_num,
         mean_Analytic, mean_cogproc, mean_emo_anx, mean_emo_anger, mean_emo_sad, mean_emo_pos,
         mean_COVID_Related, mean_family, mean_friend, mean_mental, mean_conflict) %>% 
  select(-c(location_id, location_name)) %>%
  filter(Date >= "2020-01-01" & Date <= "2020-06-30") %>%
  mutate(across(-Date, ~ scale(.))) %>%
  pivot_longer(cols = -Date, names_to = "variable", values_to = "value")

reddit <- ggplot(nyc_master %>% filter(variable %in% c("mean_Analytic", "mean_cogproc", "mean_emo_anger", "mean_emo_anx")),
                 aes(x = Date, y = value, color = variable))+
  geom_line()+
  scale_x_date(date_breaks = "month")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))
behavior <- ggplot(nyc_master %>% filter(variable %in% c("bars_visit_prop", "restaurants_visit_num")), 
                   aes(x = Date, y = value, color = variable))+
  geom_line()+
  scale_x_date(date_breaks = "month")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))
epi <- ggplot(nyc_master %>% filter(variable %in% c("cases", "deaths", "hospitalizations")), 
              aes(x = Date, y = value, colour = variable))+
  geom_line()+
  scale_x_date(date_breaks = "month")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))
reddit
behavior  
epi
nyc_grid <- plot_grid(reddit, behavior, epi, ncol = 1, align = "v")
nyc_grid
save_plot("nyc_behavioral_plot.png", nyc_grid, base_width = 8, base_height = 8)





