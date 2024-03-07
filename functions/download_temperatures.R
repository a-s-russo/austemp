# Define function to download temperature data
download_temperatures <- function() {

  # Links to download datasets
  URLs <- c(
    "http://www.bom.gov.au/jsp/ncc/cdio/weatherData/av?p_nccObsCode=122&p_display_type=dailyDataFile&p_startYear=&p_c=&p_stn_num=023000",
    "http://www.bom.gov.au/jsp/ncc/cdio/weatherData/av?p_nccObsCode=123&p_display_type=dailyDataFile&p_startYear=&p_c=&p_stn_num=023000",
    "http://www.bom.gov.au/jsp/ncc/cdio/weatherData/av?p_nccObsCode=122&p_display_type=dailyDataFile&p_startYear=&p_c=&p_stn_num=023034",
    "http://www.bom.gov.au/jsp/ncc/cdio/weatherData/av?p_nccObsCode=123&p_display_type=dailyDataFile&p_startYear=&p_c=&p_stn_num=023034"
  )
  
  # Initialise raw datasets list
  raw_datasets <- list()
  
  # Extract downloaded datasets
  for (URL in URLs) {
    # Define user-agent and headers
    user_agent <-
      "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/89.0.4389.90 Safari/537.36"
    headers = c(`user-agent` = user_agent)
    
    # Extract download link
    download_link <- GET(URL, user_agent(user_agent)) |>
      read_html() |>
      html_element("#content-block > ul.downloads > li:nth-child(2) > a") |>
      html_attr('href') |>
      paste0('http://www.bom.gov.au', . = _)
    
    # Create temporary file to download zipped file in to
    temp_file <- tempfile(fileext = ".zip")
    
    # Create temporary directory to store zipped file in
    temp_dir <- tempfile()
    
    # Download zipped file
    download.file(
      download_link,
      temp_file,
      mode = "wb",
      headers = headers,
      quiet = TRUE
    )
    
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
    type <-
      str_extract(str_to_title(relevant_row), pattern = str_to_title(c("maximum|minimum")))
    
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
  
  # Return combined datasets
  return(combined_dataset)
}
