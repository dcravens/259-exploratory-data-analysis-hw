# 259 Homework - exploratory data analysis + integrating skills
# For full credit, answer at least 8/10 questions
# List students working with below: NA

library(tidyverse)
library(lubridate)
library(DataExplorer)

#> These data are drawn from the fivethirtyeight article:
#> http://fivethirtyeight.com/features/what-12-months-of-record-setting-temperatures-looks-like-across-the-u-s/
#> The directory us-weather-history contains a data file for each of 10 cities, labelled by their station name
#> Each data file contains:
#> `date` | The date of the weather record, formatted YYYY-M-D
#> `actual_mean_temp` | The measured average temperature for that day
#> `actual_min_temp` | The measured minimum temperature for that day
#> `actual_max_temp` | The measured maximum temperature for that day
#> `average_min_temp` | The average minimum temperature on that day since 1880
#> `average_max_temp` | The average maximum temperature on that day since 1880
#> `record_min_temp` | The lowest ever temperature on that day since 1880
#> `record_max_temp` | The highest ever temperature on that day since 1880
#> `record_min_temp_year` | The year that the lowest ever temperature occurred
#> `record_max_temp_year` | The year that the highest ever temperature occurred
#> `actual_precipitation` | The measured amount of rain or snow for that day
#> `average_precipitation` | The average amount of rain or snow on that day since 1880
#> `record_precipitation` | The highest amount of rain or snow on that day since 1880

stations <- c("KCLT", "KCQT", "KHOU", "KIND", "KJAX", "KMDW", "KNYC", "KPHL", "KPHX", "KSEA")
cities <- c("Charlotte", "Los Angeles", "Houston", "Indianapolis", "Jacksonville", 
            "Chicago", "New York City", "Philadelphia", "Phoenix", "Seattle")

# QUESTION 1
#> The data files are in the directory 'us-weather-history'
#> Write a function that takes each station abbreviation and reads
#> the data file and adds the station name in a column
#> Make sure the date column is a date
#> The function should return a tibble
#> Call the function "read_weather" 
#> Check by reading/glimpsing a single station's file

read_weather <- function(station) {
  # Define the file path
  file_path <- file.path("us-weather-history", paste0(station, ".csv"))
  
  # Read the CSV file into a tibble
  weather_data <- read_csv(file_path, show_col_types = FALSE) %>%
    mutate(date = as.Date(date),  # Ensure the date column is in Date format
           station = station)     # Add station name column
  
  return(weather_data)
}

# Test with a single station
weather_sample <- read_weather("KCLT")
glimpse(weather_sample)

# QUESTION 2
#> Use map() and your new function to read in all 10 stations
#> Note that because map_dfr() has been superseded, and map() does not automatically bind rows, you will need to do so in the code.
#> Save the resulting dataset to "ds"

# Station abbreviations and corresponding city names
stations <- c("KCLT", "KCQT", "KHOU", "KIND", "KJAX", "KMDW", "KNYC", "KPHL", "KPHX", "KSEA")

# Read and combine all weather data
ds <- stations %>%
  map(read_weather) %>%  # Apply read_weather to each station
  bind_rows()            # Combine all data into one tibble

# Check the structure of the combined dataset
glimpse(ds)

# QUESTION 3
#> Make a factor called "city" based on the station variable
#> (station should be the level and city should be the label)
#> Use fct_count to check that there are 365 days of data for each city 

# Define station-to-city mapping
cities <- c("Charlotte", "Los Angeles", "Houston", "Indianapolis", "Jacksonville", 
            "Chicago", "New York City", "Philadelphia", "Phoenix", "Seattle")

# Create a named vector for mapping
station_to_city <- set_names(cities, stations)

# Add the "city" factor column
ds <- ds %>%
  mutate(city = factor(station, levels = stations, labels = cities))

# Check the count of observations per city
fct_count(ds$city)

# QUESTION 4
#> Since we're scientists, let's convert all the temperatures to C
#> Write a function to convert F to C, and then use mutate across to 
#> convert all of the temperatures, rounded to a tenth of a degree

# Function to convert Fahrenheit to Celsius
f_to_c <- function(f) {
  round((f - 32) * 5/9, 1)  # Convert and round to one decimal place
}

# Apply conversion to all temperature-related columns
ds <- ds %>%
  mutate(across(contains("temp"), f_to_c))

# Check the first few rows to confirm conversion
glimpse(ds)

### CHECK YOUR WORK
#> At this point, your data should look like the "compiled_data.csv" file
#> in data-clean. If it isn't, read in that file to use for the remaining
#> questions so that you have the right data to work with.

# QUESTION 5
#> Write a function that counts the number of extreme temperature days,
#> where the actual min or max was equal to the (i.e., set the) record min/max
#> A piped function starting with '.' is a good strategy here.
#> Group the dataset by city to see how many extreme days each city experienced,
#> and sort in descending order to show which city had the most:
#> (Seattle, 20, Charlotte 12, Phoenix 12, etc...)
#> Don't save this summary over the original dataset!

# Function to count extreme temperature days
count_extreme_days <- function(df) {
  df %>%
    mutate(extreme_day = (actual_min_temp == record_min_temp) | (actual_max_temp == record_max_temp)) %>%
    group_by(city) %>%
    summarize(extreme_days = sum(extreme_day), .groups = "drop") %>%
    arrange(desc(extreme_days))
}

# Apply function and display the summary
extreme_days_summary <- count_extreme_days(ds)
print(extreme_days_summary)
      
# QUESTION 6
#> Pull out the month from the date and make "month" a factor
#> Split the tibble by month into a list of tibbles 

# Add a "month" factor column
ds <- ds %>%
  mutate(month = factor(lubridate::month(date, label = TRUE, abbr = FALSE)))

# Split the tibble by month into a list of tibbles
ds_by_month <- ds %>%
  group_split(month)

# Check the structure
str(ds_by_month)

# QUESTION 7
#> For each month, determine the correlation between the actual_precipitation
#> and the average_precipitation (across all cities), and between the actual and average mins/maxes
#> Use a for loop, and print the month along with the resulting correlation
#> Look at the documentation for the ?cor function if you've never used it before

# Initialize an empty list to store results
correlations <- list()

# Loop through each month
for (m in levels(ds$month)) {
  # Filter data for the current month
  df_month <- ds %>% filter(month == m)
  
  # Compute correlations
  cor_precip <- cor(df_month$actual_precipitation, df_month$average_precipitation, use = "complete.obs")
  cor_min_temp <- cor(df_month$actual_min_temp, df_month$average_min_temp, use = "complete.obs")
  cor_max_temp <- cor(df_month$actual_max_temp, df_month$average_max_temp, use = "complete.obs")
  
  # Store results in a list
  correlations[[m]] <- tibble(
    month = m,
    cor_precipitation = cor_precip,
    cor_min_temp = cor_min_temp,
    cor_max_temp = cor_max_temp
  )
  
  # Print results for each month
  cat("\nMonth:", m, 
      "\n  Correlation (Actual vs. Avg Precipitation):", round(cor_precip, 3),
      "\n  Correlation (Actual vs. Avg Min Temp):", round(cor_min_temp, 3),
      "\n  Correlation (Actual vs. Avg Max Temp):", round(cor_max_temp, 3), "\n")
}

# Combine all results into a tibble 
correlation_summary <- bind_rows(correlations)

# QUESTION 8
#> Use the Data Explorer package to plot boxplots of all of the numeric variables in the dataset
#> grouped by city, then do the same thing grouped by month. 
#> Finally, use plot_correlation to investigate correlations between the continuous variables only
#> Check the documentation for plot_correlation for an easy way to do this

plot_boxplot(ds, by = "city")
plot_boxplot(ds, by = "month")
plot_correlation(ds, type ="continuous")

# QUESTION 9
#> Create a scatterplot of actual_mean_temp (y axis) by date (x axis)
#> Use facet_wrap to make a separate plot for each city (3 columns)
#> Make the points different colors according to month

# Convert date to Date class
ds <- ds %>%
  mutate(date = as.Date(date))

# Create the scatterplot
ggplot(ds, aes(x = date, y = actual_mean_temp, color = month)) +
  geom_point() +
  facet_wrap(~city, ncol = 3) +  # Create separate plots for each city (aka 3 columns)
  labs(title = "Actual Mean Temperature by Date",
       x = "Date",
       y = "Actual Mean Temperature (°C)",
       color = "Month") +
  theme_minimal()

# QUESTION 10
#> Write a function that takes the dataset and the abbreviate month as arguments
#> and creates a scatter and line plot of actual temperature (y axis) by date (x axis)
#> Note, just add geom_line() to your ggplot call to get the lines
#> use the ggtitle() function to add the month as a title
#> The function should save the plot as "eda/month_name.png"
#> The eda folder has an example of what each plot should look like
#> Call the function in a map or loop to generate graphs for each month

# Function to generate and save plot for a given month
create_temp_plot <- function(ds, month_abbr) {
  # Filter the dataset for the specific month
  ds_month <- ds %>% filter(month == month_abbr)
  
  # Create the scatter and line plot
  plot <- ggplot(ds_month, aes(x = date, y = actual_mean_temp, color = month)) +
    geom_point() +  # Scatter plot
    geom_line() +   # Line plot
    ggtitle(paste("Actual Temperature in", month_abbr)) +  # Title with month abbreviation
    labs(x = "Date", y = "Actual Mean Temperature (°C)", color = "Month") +
    theme_minimal()
  
  # Save the plot as a PNG file in the 'eda' folder
  ggsave(filename = paste0("eda/", month_abbr, ".png"), plot = plot, width = 8, height = 6)
}

# Call the function for each month
months_abbr <- levels(ds$month)  # Get all unique month abbreviations

# Use map() to generate and save plots for each month
walk(months_abbr, create_temp_plot, ds = ds)

  
  





