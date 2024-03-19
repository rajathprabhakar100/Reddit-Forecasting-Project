# Reddit-Forecasting-Project


## Data Provided on Request
## Cleaning and Tidying Data
1. Download files off of Box
2. combine_reddit().R

First, the csv file containing Reddit data from all studied cities is loaded into R. Then, this data is cleaned by converting the created_utc column to a date format, removing the created_utc column, and relocating the Date column.

Next, a function called combine_reddit() is defined. It takes a folder path, the aformentioned allcities data frame, and a file name as arguments. Inside the function, for the individual city Reddit data:
  - constructs a file path for the specified file name
  - Reads the file into a data frame called liwc_df
  - Cleans the liwc_df data in an identical manner as the allcities data
  - Extracts the city name from the filename
  - Filters the allcities data for the corresponding city based on the subreddit column
  - Combines the filtered allcities data with the liwc_df data using bind_rows
  - Writes the combined data to a new CSV file with the city name and _clean.csv suffix
3. process_reddit_files().R

This function takes the cleaned Reddit files (seen in the combine_reddit() function), calculates the daily mean values for each numeric column, applies a rolling mean (7 days), and exports the resulting data frame to a new CSV file called *city*_clean.csv. 

4. ccf_city_case_files().R

This function performs cross-correlation function (CCF) analysis between daily COVID-19 cases and various explanatory variables for a specified city or metropolitan statistical area (MSA)
  - reads COVID-19 cases and deaths data from CSV files and census data from an Excel file.
  - cleans and preprocesses the data, including joining the COVID-19 data with census data and crosswalk data to obtain MSA information.
  - arguments:
    - folder_path: The path to the folder containing the data files.
    - filename: The name of the file within the folder_path.
    - explanatory (optional): If provided, the function will run the CCF only for the specified variable. If NULL, it will run for all variables.
    - code: The MSA code starting with "C" to filter the data for a specific MSA.
    - city (optional): The name of the city
    - state (optional): The name of the state
    - plots (default = TRUE): If TRUE, the function will generate CCF plots. If FALSE, it will not generate plots.
    - lags (default "both"): Specifies whether to consider negative lags ("negative"), positive lags ("positive"), or both ("both").
  - Inside the function:
    - constructs the file path for the specified filename and reads the data
    - If an MSA code is provided, it filters the COVID-19 cases and deaths data for that MSA and joins them with the data from the specified file.
    - calculates daily cases, daily deaths, and rolling averages for cases and deaths
    - If an explanatory variable is provided, it runs the CCF analysis for that variable and returns a table with the maximum cross-correlation value and corresponding lag.
    - If no explanatory variable is provided, it runs the CCF analysis for all variables (except the first 9 columns) and returns a table with the maximum cross-correlation values and corresponding lags for each variable
    - generate CCF plots if plots = TRUE
    - returned table includes the variable name, city, state, maximum cross-correlation value, and corresponding lag 
  


5. forecast_reddit().R

## Cities Studied
- Atlanta
- Austin
- Boston
- Chicago
- Houston
- LA
- Nashville
- NYC
- Orlando
- Philadelphia
- Phoenix
- Portland
- Raleigh
- San Diego
- San Francisco
- Seattle
- St. Louis
- Washington DC

## To Do
* for the health and illness variables, plot the covid-19 case count time series alongside the time-series of the variable on the same plot. 18 time-series plots for each one
** 
## MSA Codes
### Atlanta = C1206
### Austin = C1242
### Boston = C1446
### Chicago = C1698
### Houston = C2642
### LA = C3108
### Nashville = C3498
### NYC = C3562
### Orlando = C3674
### Philadelphia = C3798
### Phoenix = C3806
### Portland = C3890
### Raleigh = C3958
### San Diego = C4174
### San Francisco = C4186
### Seattle = C4266
### St. Louis = C4118
### Washington DC = C4790
