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
4. ccf_city_case_files().R

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
