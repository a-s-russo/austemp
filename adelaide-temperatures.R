# Load library
library(zoo)
library(tidyverse)

# Read data
raw_data <- read_csv('Data/data_clean.csv', show_col_types = FALSE)

# Define function to generate graph
plot_temp <- function(season = 'summer',
                      start_year = year(today()) - 31,
                      end_year = year(today()),
                      location = 'Airport') {
  # season <-
  #   'winter'
  # start_year <- 1993
  # end_year <- 2015
  # location <- 'Airport'
  
  # Extract relevant months
  relevant_data <- raw_data |>
    filter(if (season == 'summer') {
      (Month < 4 |
         Month > 10)
    } else {
      (Month >= 5 & Month <= 9)
    },
    Type == ifelse(season == 'summer', 'Max', 'Min'),
    Location == location)
  
  # Adjust start year
  min_start_year <- relevant_data |>
    pull(Year) |>
    min()
  start_year <-
    max(start_year, min_start_year) - ifelse(season == 'summer', 1, 0)
  
  # Adjust end year
  max_end_year <- relevant_data |>
    pull(Year) |>
    max()
  end_year <-
    min(end_year, max_end_year) + ifelse(season == 'summer', 1, 0)
  
  # Extract relevant years
  relevant_data <- relevant_data |>
    filter(Year >= start_year,
           Year <= end_year)
  
  # Group months into relevant seasons
  if (season == 'summer') {
    relevant_data <- relevant_data |>
      mutate(
        Season = if_else(Month > 10, Year - start_year + 1, Year - start_year),
        Seasons_ago = end_year - start_year - Season + 1
      )
  } else {
    relevant_data <- relevant_data |>
      mutate(Season = Year - start_year + 1,
             Seasons_ago = end_year - start_year - Season + 2)
  }
  
  # Remove irrelevant months of starting year
  if (season == 'summer') {
    relevant_data <- relevant_data |>
      filter(Season > 0,
             Seasons_ago > 0)
  }
  
  # Add rows for February 29th for non-leap-years to align February end date
  leap_years_in_data <-
    any(leap_year(unique(pull(
      relevant_data, Year
    ))))
  if (leap_years_in_data) {
    leap_year_rows <- relevant_data |>
      filter(Month == 2 & Day == 28) |>
      mutate(Day = 29, Temperature = NA) |>
      filter(!leap_year(Year))
    relevant_data <- relevant_data |>
      bind_rows(leap_year_rows) |>
      arrange(Year, Month, Day)
  }
  
  # Determine day number within the season
  relevant_data <- relevant_data |>
    group_by(Season) |>
    mutate(Day_number = row_number()) |>
    ungroup()
  
  # Determine extreme temperature categories
  if (season == 'summer')
    temp_cutoffs <- c(30, 35, 40, 48)
  if (season == 'winter')
    temp_cutoffs <- c(5, 3, 0, -10)
  extreme_days <-
    filter(relevant_data, if (season == 'summer') {
      Temperature > 30
    } else {
      Temperature <= 5
    }) |>
    mutate(Temp_category = cut(Temperature, temp_cutoffs)) # Define the classes to display
  
  # Determine graph properties based on season
  padding <-
    4 # Padding for right margin for counts of very extreme temperatures
  if (season == 'summer') {
    colours <- c("salmon", "red2", "black")
    year_breaks <- seq(1, end_year - start_year)
    year_labels <-
      str_c(
        as.character(seq(end_year - 1, start_year)),
        rep('-', end_year - start_year),
        str_sub(as.character(seq(
          end_year, start_year + 1
        )), -2, -1)
      )
    if (leap_years_in_data)
      month_breaks <- c(0, 30, 61, 92, 121, 152)
    else
      month_breaks <- c(0, 30, 61, 92, 120, 151)
    month_labels <-
      c("November", "December", "January", "February", "March")
    legend_labels <-
      c("30.1-35.0\u00B0C",
        "35.1-40.0\u00B0C",
        "Above 40.0\u00B0C")
    direction <- 'above'
    threshold <- '40.0'
    measure_label <- 'Maximum'
  }
  if (season == 'winter') {
    colours <- c("black", "cornflowerblue", "lightblue")
    year_breaks <- seq(1, end_year - start_year + 1)
    year_labels <-
      rev(as.character(unique(pull(
        relevant_data, Year
      ))))
    month_breaks <- c(0, 31, 61, 92, 123, 153)
    month_labels <- c("May", "June", "July", "August", "September")
    legend_labels <-
      c("Below 0.1\u00B0C",
        "0.1-3.0\u00B0C",
        "3.1-5.0\u00B0C")
    direction <- 'below'
    threshold <- '0.1'
    measure_label <- 'Minimum'
  }
  
  # Count days of very extreme temperatures to display on right side of graph
  very_extreme_counts <- extreme_days |>
    group_by(Seasons_ago) |>
    summarise(Very_extreme_days = sum(
      if (season == 'summer')
        Temperature > as.numeric(threshold)
      else
        Temperature < as.numeric(threshold)
    )) |>
    mutate(x_position = max(month_breaks) + padding)
  
  # Generate graph
  graph <-
    ggplot(extreme_days,
           aes(Day_number, Seasons_ago, fill = Temp_category)) +
    geom_tile() +
    scale_fill_manual(
      values = colours,
      name = paste(measure_label, "temperature:"),
      labels = legend_labels
    ) +
    geom_vline(xintercept = month_breaks + 0.5) +
    scale_y_continuous(breaks = year_breaks,
                       labels = year_labels,
                       expand = c(0, 0)) +
    scale_x_continuous(
      # 'breaks' is defined as the midpoints of 'month_breaks' to centre-align 'month_labels'
      breaks = rollapply(
        month_breaks,
        width = 2,
        FUN = sum,
        by = 1
      ) / 2,
      labels = month_labels,
      expand = expansion(add = c(0, padding))
    ) +
    theme(
      panel.border = element_rect(color = 'black', fill = NA),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      axis.ticks = element_blank(),
      plot.title = element_text(hjust = 0.5),
      plot.title.position = "plot",
      plot.subtitle = element_text(hjust = 0.5),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      legend.position = "bottom"
    ) +
    labs(
      title = paste(
        "Adelaide",
        tolower(location),
        "daily",
        tolower(measure_label),
        "temperatures"
      ),
      subtitle = paste0(
        "Numbers on right are counts of days with temperatures ",
        direction,
        " ",
        threshold,
        "\u00B0C"
      )
    ) +
    geom_text(
      data = very_extreme_counts,
      aes(x_position, Seasons_ago, label = Very_extreme_days),
      inherit.aes = FALSE
    )
  
  return(graph)
}
plot_temp()
plot_temp(start_year = 1992, season = 'winter')