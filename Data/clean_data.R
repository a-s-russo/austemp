# Load library
library(tidyverse)
library(padr)

# Define and map station and product codes and names
station_codes = c('023000', '023034')
station_names = c('City', 'Airport')
product_codes = c('0010', '0011')
product_names = c('Max', 'Min')

# Initialise datasets list
raw_datasets <- list()

# Extract raw datasets
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
    
    # Read and format dataset
    raw_dataset <-
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
    raw_dataset <- raw_dataset |>
      rename({
        {
          variable_name
        }
      } := starts_with(product_name))
    
    # Merge datasets
    raw_datasets <- append(raw_datasets, list(raw_dataset))
  }
}

# Combine datasets
combined_dataset <- raw_datasets |>
  reduce(merge, all = TRUE, by = c("Year", "Month", "Day")) |>
  arrange(Year, Month, Day) |>
  as_tibble()

# Insert rows for any missing dates
combined_dataset <- combined_dataset |>
  mutate(Date = as.Date(paste(Year, Month, Day, sep = '-'))) |>
  pad(interval = 'day') |>
  select(-Date)

# Remove any starting or ending rows where all measurements are missing
no_temperature_vars <-
  ncol(select(combined_dataset, -c(Year, Month, Day)))
combined_dataset <- combined_dataset |>
  mutate(no_missing = rowSums(across(-c(Year, Month, Day), ~ is.na(.x)))) |>
  slice(first(which(no_missing < no_temperature_vars)):n()) |>
  slice(1:last(which(no_missing < no_temperature_vars))) |>
  select(-no_missing)

# Convert from wide to long dataset
long_dataset <- combined_dataset |>
  pivot_longer(-c(Year, Month, Day),
               names_to = "key",
               values_to = "Temperature") %>%
  separate(key, c("Location", "Type"), sep = "_") |>
  arrange(Year, Month, Day, Location, Type)

# Output data
write_csv(long_dataset, 'Data/data_clean.csv')