# Load library
library(tidyverse)

# Define and map station and product codes and names
station_codes = c('023000', '023034')
station_names = c('City', 'Airport')
product_codes = c('0010', '0011')
product_names = c('Max', 'Min')

# Initialise data frame
raw_data <-
  tibble(
    Date = seq(as.Date("1887-01-01"), today(), "days"),
    Year = year(Date),
    Month = as.numeric(month(Date)),
    Day = as.numeric(day(Date))
  ) |>
  select(-Date)

# Extract raw data
for (station_code in station_codes) {
  for (product_code in product_codes) {
    # Get station and product names
    station_name <-
      station_names[station_codes == station_code]
    product_name <-
      product_names[product_codes == product_code]
    
    # Unzip compressed file
    temp <- tempfile()
    unzip(
      zipfile = paste0(
        'Data/IDCJAC',
        product_code,
        '_',
        station_code,
        '_1800.zip'
      ),
      exdir = temp
    )
    
    # Read and format data file
    this_raw_data <-
      read_csv(
        file.path(
          temp,
          paste0(
            'IDCJAC',
            product_code,
            '_',
            station_code,
            '_1800_Data.csv'
          )
        ),
        col_select = c(Year, Month, Day, starts_with(product_name)),
        show_col_types = FALSE
      ) |>
      mutate(Month = as.numeric(Month),
             Day = as.numeric(Day))
    
    # Rename measurement variable
    variable_name <-
      paste(station_name, product_name, sep = '_')
    this_raw_data <- this_raw_data |>
      rename({
        {
          variable_name
        }
      } := starts_with(product_name))
    
    # Merge data frames
    raw_data <-
      left_join(raw_data, this_raw_data, by = c('Year', 'Month', 'Day'))
  }
}

# Remove any rows where all measurements are missing
raw_data <- raw_data |>
  filter(!if_all(-c(Year, Month, Day), is.na))

# Convert from long to wide data frame
raw_data <- raw_data |>
  pivot_longer(-c(Year, Month, Day),
               names_to = "key",
               values_to = "Temperature") %>%
  separate(key, c("Location", "Type"), sep = "_")

# Output data
write_csv(raw_data, 'Data/data_clean.csv')