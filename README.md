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
- Constructs the file path for the specified filename.
- Reads the file into a data table called data
- Cleans the data by replacing [deleted] and AutoModerator values in the author column with NA, and replacing [removed] values in the body column with NA
- For each numerical column in the original data, it calculates the mean value for each date and adds a new column to the means data frame with the column name prefixed with "mean_"
- Applies a rolling mean with a window size of 7 days to each mean_ column in the means_significant data frame, filling missing values with NA and aligning the window to the right.
- Extracts the city name from the input filename by removing the _clean.csv suffix
- Writes the means_significant data frame to a CSV file with the constructed output filename

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
   

4. covid_data_merging.R

  - starts by selecting specific columns from the deaths dataset and creating a new dataset called deaths1
  - performs a left join between the cases dataset and deaths1 to create a new dataset called cases_and_deaths
  - defines two vectors: MSA_Codes and Cities, which contain the MSA codes and names of cities, respectively
  - creates a data frame table2 by combining the MSA_Codes and Cities vectors
  - defines a function process_daily_file that reads a CSV file, selects specific columns, and adds the corresponding MSA code to the data
  - gets a list of file names in the "Daily Data" folder and applies the process_daily_file function to each file, using the corresponding MSA code. The resulting data frames are combined into data_list
  - data_list is then combined into a single data frame called combined_reddit_df using bind_rows, and joined with table2 to add the city names
  - for the cases_and_deaths dataset, it filters by the MSA_Codes, groups by MSA_Code, MSA_Title, and Date, and calculates various statistics like daily cases, daily deaths, and 7-day rolling averages
  - resulting dataset cases_and_deaths1 is then joined with combined_reddit_df to create reddit_and_cases

5. forecast_reddit().R

- file consisting of three functions, with each corresponding to the number of weeks of training data (4 weeks, 8 weeks, cumulative)
- loads a dataset called reddit_and_cases from a CSV file
- filters the dataset to include only the specified city and calculates rolling averages for the mean_illness column over different time windows (7, 14, and 21 days)
- creates four linear regression models for the city, using different combinations of the mean_illness and rolling averages as predictors for the Daily_Cases7 (7-day average of daily cases) target variable
- creates a new dataset called city_projection_data that includes the future dates for which the forecast will be made, along with the rolling averages of mean_illness
- calculates the forecast for Daily_Cases7 using the appropriate linear regression model and adds the forecasted values, confidence intervals, and R-squared values to the city_projection_data dataset
- If the csv option is set to TRUE, it saves the city_projection_data dataset as a CSV file in the "Results/Projections/<city_name>" directory with a filename that includes the date
- If the csv option is set to FALSE, it returns a list containing the city_projection_data dataset and the summaries of the four linear regression models

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
### Baltimore = C1258
### Boston = C1446
### Charlotte = C1674
### Chicago = C1698
### Columbus = C1814
### Dallas = C1910
### Denver = C1974
### Detroit = C1982
### Houston = C2642
### Kansas City = C2814
### LA = C3108
### Minneapolis = C3346
### Nashville = C3498
### New Orleans = C3538
### NYC = C3562
### Orlando = C3674
### Philadelphia = C3798
### Phoenix = C3806
### Portland = C3890
### Raleigh = C3958
### Salt Lake City = C4162
### San Diego = C4174
### San Francisco = C4186
### Seattle = C4266
### St. Louis = C4118
### Tampa Bay = C4530
### Las Vegas = C2982
### Washington DC = C4790
