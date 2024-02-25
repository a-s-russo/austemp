# Load library
library(zoo)
library(tidyverse)

# Read data
raw_data <- read_csv('data-clean.csv', show_col_types = FALSE)

# Define function to generate graph
plot_temperatures <- function(season = 'summer',
                              start_year = year(today()) - 31,
                              end_year = year(today()),
                              location = 'Adelaide Airport') {
  # Extract relevant months
  relevant_data <- raw_data |>
    filter(
      if (season == 'summer') {
        (Month < 4 |
           Month > 10)
      } else {
        (Month >= 5 & Month <= 9)
      },
      Type == ifelse(season == 'summer', 'Maximum', 'Minimum'),
      Location == location
    )
  
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
  if (season == 'summer') {
    threshold1 <- 30
    threshold2 <- 35
    threshold3 <- 40
    inf_threshold <- 99
  }
  if (season == 'winter') {
    threshold1 <- 5
    threshold2 <- 3
    threshold3 <- 0
    inf_threshold <- -99
  }
  temp_cutoffs <-
    c(threshold1, threshold2, threshold3, inf_threshold)
  extreme_days <-
    filter(relevant_data, if (season == 'summer') {
      Temperature > threshold1
    } else {
      Temperature <= threshold1
    }) |>
    mutate(Temp_category = cut(Temperature, temp_cutoffs)) # Define the classes to display
  
  # Determine graph properties based on season
  measure_label <- unique(pull(relevant_data, Type))
  temperature_symbol <- "\u00B0C"
  decimal_places <- 1
  one_padding <- str_pad('1', decimal_places, pad = '0')
  zero_padding <- str_pad('0', decimal_places, pad = '0')
  graph_edge_padding <-
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
    direction <- 'above'
    subtitle_threshold <- threshold3
    legend_labels <-
      c(
        paste0(
          threshold1,
          ".",
          one_padding,
          "-",
          threshold2,
          ".",
          zero_padding,
          temperature_symbol
        ),
        paste0(
          threshold2,
          ".",
          one_padding,
          "-",
          threshold3,
          ".",
          zero_padding,
          temperature_symbol
        ),
        paste0(
          str_to_title(direction),
          " ",
          threshold3,
          ".",
          zero_padding,
          temperature_symbol
        )
      )
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
    direction <- 'below'
    subtitle_threshold <- paste0(threshold3, '.', one_padding)
    legend_labels <-
      c(
        paste0(
          str_to_title(direction),
          " ",
          threshold3,
          ".",
          one_padding,
          temperature_symbol
        ),
        paste0(
          threshold3,
          ".",
          one_padding,
          "-",
          threshold2,
          ".",
          zero_padding,
          temperature_symbol
        ),
        paste0(
          threshold2,
          ".",
          one_padding,
          "-",
          threshold1,
          ".",
          zero_padding,
          temperature_symbol
        )
      )
  }
  
  # Count days of very extreme temperatures to display on right side of graph
  very_extreme_counts <- extreme_days |>
    group_by(Seasons_ago) |>
    summarise(Very_extreme_days = sum(
      if (season == 'summer')
        Temperature > as.numeric(threshold3)
      else
        Temperature < as.numeric(paste0(threshold3, '.', one_padding))
    )) |>
    mutate(x_position = max(month_breaks) + graph_edge_padding)
  
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
      expand = expansion(add = c(0, graph_edge_padding))
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
        str_to_title(location),
        "daily",
        tolower(measure_label),
        "temperatures"
      ),
      subtitle = paste0(
        "Numbers on right are counts of days with temperatures ",
        direction,
        " ",
        subtitle_threshold,
        temperature_symbol
      )
    ) +
    geom_text(
      data = very_extreme_counts,
      aes(x_position, Seasons_ago, label = Very_extreme_days),
      inherit.aes = FALSE
    )
  
  return(graph)
}
plot_temperatures()
plot_temperatures(start_year = 1992, season = 'winter')