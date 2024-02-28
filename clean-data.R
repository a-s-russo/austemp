# Load libraries
suppressMessages(library(tidyverse))
library(padr, warn.conflicts = FALSE)

# Links to download datasets
URLs <- c(
  "http://www.bom.gov.au/jsp/ncc/cdio/weatherData/av?p_display_type=dailyZippedDataFile&p_stn_num=023000&p_c=-105799246&p_nccObsCode=122&p_startYear=2024",
  "http://www.bom.gov.au/jsp/ncc/cdio/weatherData/av?p_display_type=dailyZippedDataFile&p_stn_num=023000&p_c=-105799442&p_nccObsCode=123&p_startYear=2024",
  "http://www.bom.gov.au/jsp/ncc/cdio/weatherData/av?p_display_type=dailyZippedDataFile&p_stn_num=023034&p_c=-106112277&p_nccObsCode=122&p_startYear=2024",
  "http://www.bom.gov.au/jsp/ncc/cdio/weatherData/av?p_display_type=dailyZippedDataFile&p_stn_num=023034&p_c=-106112473&p_nccObsCode=123&p_startYear=2024"
)

# Initialise raw datasets list
raw_datasets <- list()

# Extract downloaded datasets
for (URL in URLs) {
  # Define headers for user-agent validation
  headers = c(`user-agent` = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/102.0.5005.61 Safari/537.36')
  
  # Create temporary file to download zipped file in to
  temp_file <- tempfile(fileext = ".zip")
  
  # Create temporary directory to store zipped file in
  temp_dir <- tempfile()
  
  # Download zipped file
  download.file(URL,
                temp_file,
                mode = "wb",
                headers = headers,
                quiet = TRUE)
  
  # Unzip downloaded file
  unzip(zipfile = temp_file, exdir = temp_dir)
  
  # Extract filenames
  data_file <- list.files(temp_dir, '*.csv')
  note_file <- list.files(temp_dir, '*.txt')
  
  # Extract dataset notes
  notes <-
    read_lines(paste(temp_dir, note_file, sep = '/'), skip_empty_rows = TRUE)
  
  # Extract location
  relevant_row_number <- grep('^Station name:', notes)
  relevant_row <- notes[relevant_row_number]
  location <-
    str_to_title(str_replace(relevant_row, 'Station name: ', ''))
  
  # Extract temperature type
  relevant_row_number <- grep('^Notes for Daily', notes)
  relevant_row <- notes[relevant_row_number]
  type <- str_split_i(relevant_row, ' ', 4)
  
  # Extract raw dataset
  raw_dataset <-
    read_csv(
      file.path(temp_dir, data_file),
      col_select = c(Year, Month, Day, contains('degree')),
      show_col_types = FALSE
    ) |>
    mutate(
      Month = as.numeric(Month),
      Day = as.numeric(Day),
      Location = location,
      Type = type
    ) |>
    rename(Temperature := matches('(degree)'))
  
  # Insert rows for any missing dates
  raw_dataset <- raw_dataset |>
    mutate(Date = as.Date(paste(Year, Month, Day, sep = '-'))) |>
    pad(interval = 'day') |>
    select(Year, Month, Day, Location, Type, Temperature, -Date)
  
  # Remove any starting or ending rows where all measurements are missing
  raw_dataset <- raw_dataset |>
    slice(first(which(!is.na(Temperature))):n()) |>
    slice(1:last(which(!is.na(Temperature))))
  
  # Merge datasets
  raw_datasets <- append(raw_datasets, list(raw_dataset))
}

# Combine datasets
combined_dataset <- bind_rows(raw_datasets) |>
  arrange(Year, Month, Day)

# Output dataset
write_csv(combined_dataset, 'data-clean.csv')