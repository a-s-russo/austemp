# Load library
library(zoo)
library(tidyverse)

# Read data
raw_data <- read_csv('Data/data_clean.csv', show_col_types = FALSE)

# Define function to generate graph
plot_temp <- function(season = 'summer',
                      start_year = 1997,
                      end_year = year(today()),
                      location = 'Airport') {
  # Extract and reformat relevant months
  relevant_months <- raw_data |>
    filter(
      Year >= start_year & Year <= end_year,
      if (season == 'summer') {
        (Month < 4 |
           Month > 10)
      } else {
        (Month >= 5 & Month <= 9)
      },
      Type == ifelse(season == 'summer', 'Max', 'Min'),
      Location == location
    ) |>
    mutate(
      # Group relevant months in the same season
      Season = if_else(Month > 10, Year - start_year + 1, Year - start_year),
      # Want the most recent season plotted at bottom of graph
      Seasons_ago = end_year - start_year - Season
    ) |>
    filter(Season > 0)  # Remove irrelevant months of starting year
  
  # Add rows for February 29th for non-leap-years to align February end date
  leap_years_in_data <-
    any(leap_year(unique(pull(
      relevant_months, Year
    ))))
  if (leap_years_in_data) {
    leap_year_rows <- relevant_months |>
      filter(Month == 2 & Day == 28) |>
      mutate(Day = 29, Temp = NA) |>
      filter(!leap_year(Year))
    relevant_months <- relevant_months |>
      bind_rows(leap_year_rows) |>
      arrange(Year, Month, Day)
  }
  
  # Determine day number within the season
  relevant_months <- relevant_months |>
    group_by(Season) |>
    mutate(Day_number = row_number()) |>
    ungroup()
  
  # Determine extreme temperatures
  if (season == 'summer')
    temp_cutoffs <- c(30, 35, 40, 48)
  if (season == 'winter')
    temp_cutoffs <- c(5, 3, 0, -10)
  extreme_days <-
    filter(relevant_months, if (season == 'summer') {
      Temp > 30
    } else {
      Temp <= 5
    }) |>
    mutate(Temp_category = cut(Temp, temp_cutoffs)) # Define the classes to display
  
  # Create text label for the years
  year_labels <-
    str_c(as.character(seq(end_year - 1, start_year)),
          rep('-', end_year - start_year),
          str_sub(as.character(seq(
            end_year, start_year + 1
          )), -2, -1))
  
  # Determine graph properties
  measure_label <- ifelse(season == 'summer', 'Maximum', 'Minimum')
  if (season == 'summer') {
    colours <- c("salmon", "red2", "black")
    if (leap_years_in_data)
      month_breaks <- c(0, 30, 61, 92, 121, 152)
    else
      month_breaks <- c(0, 30, 61, 92, 120, 151)
    month_labels <-
      c("November", "December", "January", "February", "March")
    legend_labels <- c("30.1-35.0", "35.1-40.0", "Above 40.0")
  }
  if (season == 'winter') {
    colours <- c("black", "cornflowerblue", "lightblue")
    month_breaks <- c(0, 31, 61, 92, 123, 153)
    month_labels <- c("May", "June", "July", "August", "September")
    legend_labels <- c("Below 0.1", "0.1-3.0", "3.1-5.0")
  }
  
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
    geom_vline(xintercept = month_breaks + 0.5, ) +
    scale_y_continuous(
      breaks = seq(0, end_year - start_year - 1),
      labels = year_labels,
      expand = c(0, 0)
    ) +
    scale_x_continuous(
      # 'breaks' is defined as the midpoints of 'month_breaks' to centre-align 'month_labels'
      breaks = rollapply(
        month_breaks,
        width = 2,
        FUN = sum,
        by = 1
      ) / 2,
      labels = month_labels,
      expand = c(0, 0)
    ) +
    theme(
      panel.border = element_rect(color = 'black', fill = NA),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      axis.ticks = element_blank(),
      plot.title = element_text(hjust = 0.5),
      plot.title.position = "plot",
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      legend.position = "bottom"
    ) +
    labs(title = paste(
      "Adelaide",
      tolower(location),
      "daily",
      tolower(measure_label),
      "temperatures"
    ))
  
  return(graph)
}
plot_temp(start_year = 1993)
plot_temp(season = 'winter', start_year = 1992)





# Plot Airport maximums and minimums ------------------------
y1 <- 1997
cyr <- 2024

# Create text label for the years
yearlab <-
  str_c(as.character(seq(cyr - 1, y1)),
        rep('-', cyr - y1),
        str_sub(as.character(seq(cyr, y1 + 1)),-2,-1))

ySummer <-
  raw_data %>% filter(Year >= y1, (Month < 4 |
                                     Month > 10), Location == 'Airport') %>%
  mutate(
    MonthSum = if_else(Month > 10, Month - 10, Month + 2),
    Summer = if_else(Month > 10, Year - y1 + 1, Year - y1),
    SummerAgo = cyr - y1 - Summer
  ) %>%
  filter(Summer > 0)  # Remove Jan-Mar of y1
ySummer <-
  ySummer %>% group_by(Summer, Type) %>% mutate(DaySum = row_number()) %>% ungroup()

extreme_days <-
  filter(ySummer, Temp > 30 &
           Type == 'Max' | Temp > 21 & Type == 'Min') %>%
  mutate(Temp_category = cut(Temp, c(21, 25, 30, 35, 40, 48))) %>%
  mutate(Summer_place = if_else(Type == 'Max', SummerAgo + 0.15, SummerAgo -
                                  0.15))

# Get counts of the number of days above 40, to display on the right
countHot <-
  filter(extreme_days, Type == 'Max') %>% group_by(Summer_place) %>% summarise(FortyPlus = sum(Type == 'Max' &
                                                                                                 Temp > 40)) %>%
  mutate(xv = 155)

ggplot(extreme_days, aes(DaySum, Summer_place)) +
  geom_point(aes(color = Temp_category), shape = 15) +
  scale_colour_manual(
    values = c("orchid2", "orchid4", "salmon", "red2", "black"),
    name = "Temp",
    labels = c("21.1-25.0", "25.1-30", "30.1-35.0", "35.1-40.0", "Above 40.0")
  ) +
  geom_vline(xintercept = c(0, 30, 61, 92, 120, 151) + 0.5) +
  scale_y_continuous(breaks = seq(0, cyr - y1 - 1), labels = yearlab) +
  scale_x_continuous(
    breaks = c(15, 45, 76, 105, 134),
    labels = c("Nov", "Dec", "Jan", "Feb", "Mar")
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    plot.title.position = "plot",
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "bottom"
  ) +
  coord_cartesian(xlim = c(2, 155)) +
  labs(title = "Adelaide Airport high maximum and minimum temperatures",
       subtitle = "Within each year, top square is maximim and bottom square is minimum") +
  geom_text(data = countHot,  aes(xv, Summer_place, label = FortyPlus))
