# Load pipeline infrastructure libraries
library(targets)
library(tarchetypes)
library(future)
library(future.callr)
plan(callr)

# Load libraries into the pipeline
tar_option_set(packages = c(
  'readr',
  'stringr',
  'dplyr',
  'padr',
  'lubridate',
  'ggplot2',
  'zoo',
  'rvest',
  'httr'
))

# Load functions into the pipeline
source("functions/download_temperatures.R")
source("functions/plot_temperatures.R")

# Create pipeline
list(
  
  # Download temperature data
  tar_target(
    data,
    download_temperatures()),
  
  # Create summer plot
  tar_target(
    plot_summer,
    plot_temperatures(
      data = data,
      season = 'summer',
      thresholds = c(30, 35, 40)
    )
  ),
  
  # Save summer plot
  tar_target(
    image_summer,
    ggsave(filename = 'graph-airport-summer.png',
           plot = plot_summer,
           width = 7.5,
           height = 6.5,
           units = 'in')
  ),
  
  # Create winter plot
  tar_target(
    plot_winter,
    plot_temperatures(
      data = data,
      season = 'winter',
      thresholds = c(0, 3, 5),
      start_year = year(today()) - 32
    )
  ),
  
  # Save winter plot
  tar_target(
    image_winter,
    ggsave(filename = 'graph-airport-winter.png',
           plot = plot_winter,
           width = 7.5,
           height = 6.5,
           units = 'in')
  )
)
