# Inititalisation ---------------------------------------------------------

library(tidyverse)
library(readxl)
library(lubridate)

infile <- "C:\\Users\\asrus\\OneDrive\\Desktop\\Airport temperatures.xlsx"

rawdat <- infile %>% excel_sheets() %>%
  set_names() %>%
  map_dfr(read_excel, path=infile) %>%
  pivot_longer(c('Airport_Min', 'Airport_Max', 'City_Min', 'City_Max'), names_to = "key", values_to = "Temp") %>%
  separate(key, c("Location", "Type"), sep = "_")


# Read in the data, derive basic variables --------------------------------
y1 <- 1997
cyr <- 2023
ySummer <- rawdat %>% filter(Year >= y1, (Month < 4 | Month > 10), Type == 'Max') %>% 
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
cyr <- 2023

# Create text label for the years
yearlab <- str_c(as.character(seq(cyr-1, y1)), rep('-', cyr-y1), str_sub(as.character(seq(cyr, y1+1)), -2, -1))

ySummer <- rawdat %>% filter(Year >= y1, (Month < 4 | Month > 10), Location == 'Airport') %>% 
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