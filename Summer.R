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
  pivot_longer(-c(Year, Month, Day), names_to = "key", values_to = "Temp") %>%
  separate(key, c("Location", "Type"), sep = "_")

# Read in the data, derive basic variables --------------------------------
y1 <- 1997
cyr <- 2024
ySummer <- raw_data %>% filter(Year >= y1, (Month < 4 | Month > 10), Type == 'Max') %>% 
  mutate(MonthSum = if_else(Month > 10, Month - 10, Month + 2),
         Summer = if_else(Month > 10, Year - y1 + 1, Year - y1),  # Deriving a variable to group Nov-Mar in the same summer
         SummerAgo = cyr - y1 - Summer,   # Want the most recent summer plotted at bottom
         Hot = if_else(Temp >= 30, 1, 0)) %>%
  filter(Summer > 0)  # Remove Jan-Mar of y1

# Derive a variable to denote the day number within the Summer
ySummer <- ySummer %>% group_by(Summer, Location) %>% mutate(DaySum = row_number()) %>% ungroup()

# Compare hot days for Airport and City ------------------------

ySumHot <- filter(ySummer, Temp > 30) %>% 
  mutate(max_tempC = cut(Temp, c(30, 35, 40, 48))) %>%  # Define the classes to display
  mutate(SummerPl = if_else(Location == 'Airport', SummerAgo+0.15, SummerAgo-0.15))  # Put the Airport values on top of the city values

# Create text label for the years
yearlab <- str_c(as.character(seq(cyr-1, y1)), rep('-', cyr-y1), str_sub(as.character(seq(cyr, y1+1)), -2, -1))

ggplot(ySumHot, aes(DaySum, SummerPl)) + 
  geom_point(aes(color = max_tempC), shape=15) + 
  scale_colour_manual(values = c("salmon", "red2", "black"),
                      name="Max temp", 
                      labels=c("30.1-35.0", "35.1-40.0", "Above 40.0")) +
  geom_vline(xintercept = c(0, 30, 61, 92, 120, 151) + 0.5) +
  scale_y_continuous(breaks=seq(0,cyr-y1 - 1), labels=yearlab) + 
  scale_x_continuous(breaks=c(15, 45, 76, 105, 134), labels=c("Nov", "Dec", "Jan", "Feb", "Mar")) +
  theme(plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5),
    plot.title.position = "plot",
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "bottom") +
  labs(title="Adelaide daily maximum temperatures",
       subtitle="Within each year, top square is Adelaide Airport and bottom square is City")
  #+ annotate("rect", xmin = 122, xmax = 139, ymin = 14.5, ymax = 15.5,
  #       alpha = .4)


# Plot Airport maximums and minimums ------------------------
y1 <- 1997
cyr <- 2024

# Create text label for the years
yearlab <- str_c(as.character(seq(cyr-1, y1)), rep('-', cyr-y1), str_sub(as.character(seq(cyr, y1+1)), -2, -1))

ySummer <- raw_data %>% filter(Year >= y1, (Month < 4 | Month > 10), Location == 'Airport') %>% 
  mutate(MonthSum = if_else(Month > 10, Month - 10, Month + 2),
         Summer = if_else(Month > 10, Year - y1 + 1, Year - y1),
         SummerAgo = cyr - y1 - Summer) %>%
  filter(Summer > 0)  # Remove Jan-Mar of y1
ySummer <- ySummer %>% group_by(Summer, Type) %>% mutate(DaySum = row_number()) %>% ungroup()

ySumHot <- filter(ySummer, Temp > 30 & Type == 'Max' | Temp > 21 & Type == 'Min') %>% 
  mutate(max_tempC = cut(Temp, c(21, 25, 30, 35, 40, 48))) %>%
  mutate(SummerPl = if_else(Type == 'Max', SummerAgo+0.15, SummerAgo-0.15))

# Get counts of the number of days above 40, to display on the right
countHot <- filter(ySumHot, Type == 'Max') %>% group_by(SummerPl) %>% summarise(FortyPlus = sum(Type == 'Max' & Temp > 40)) %>%
            mutate(xv = 155)

ggplot(ySumHot, aes(DaySum, SummerPl)) + 
  geom_point(aes(color = max_tempC), shape=15) + 
  scale_colour_manual(values = c("orchid2", "orchid4", "salmon", "red2", "black"),
                      name="Temp", 
                      labels=c("21.1-25.0", "25.1-30","30.1-35.0", "35.1-40.0", "Above 40.0")) +
  geom_vline(xintercept = c(0, 30, 61, 92, 120, 151) + 0.5) +
  scale_y_continuous(breaks=seq(0,cyr-y1 - 1), labels=yearlab) + 
  scale_x_continuous(breaks=c(15, 45, 76, 105, 134), labels=c("Nov", "Dec", "Jan", "Feb", "Mar")) +
  theme(plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5),
        plot.title.position = "plot",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "bottom") +
  coord_cartesian(xlim=c(2,155)) +
  labs(title="Adelaide Airport high maximum and minimum temperatures",
       subtitle="Within each year, top square is maximim and bottom square is minimum") +
  geom_text(data = countHot,  aes(xv, SummerPl, label=FortyPlus))
