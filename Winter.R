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
cyr <- 2022
yWinter <- rawdat %>% filter(Year >= y1, (Month >= 5 & Month <= 9), Type == 'Min') %>% 
  mutate(Winter = Year - y1,
         WinterAgo = cyr - y1 - Winter,   # Want the most recent winter plotted at bottom
         Cold = if_else(Temp <= 5, 1, 0))

# Derive a variable to denote the day number within the Winter
yWinter <- yWinter %>% group_by(Winter, Location) %>% mutate(DaySum = row_number()) %>% ungroup()

# Compare cold days for Airport and City ------------------------

ySumCold <- filter(yWinter, Temp <= 5) %>% 
  mutate(min_tempC = cut(Temp, c(5, 3, 0, -10))) %>%  # Define the classes to display
  mutate(WinterPl = if_else(Location == 'Airport', WinterAgo+0.15, WinterAgo-0.15))  # Put the Airport values on top of the city values

# Create text label for the years
yearlab <- str_c(as.character(seq(cyr, y1)))

# Get counts of the number of days below 0, to display on the right
countColdAirport <- filter(ySumCold, Type == 'Min', Location == 'Airport') %>% group_by(WinterPl) %>% summarise(BelowZero = sum(Type == 'Min' & Temp < 0.1)) %>%
  mutate(xv = 156)
countColdCity <- filter(ySumCold, Type == 'Min', Location == 'City') %>% group_by(WinterPl) %>% summarise(BelowZero = sum(Type == 'Min' & Temp < 0.1)) %>%
  mutate(xv = 160)

ggplot(ySumCold, aes(DaySum, WinterPl)) + 
  geom_point(aes(color = min_tempC), shape=15) + 
  scale_colour_manual(values = c("black", "cornflowerblue", "lightblue"),
                      name="Min temperature", 
                      labels=c("Below 0.1", "0.1-3.0", "3.1-5.0"),
                      guide = guide_legend(reverse = TRUE)) +
  geom_vline(xintercept = c(0, 31, 61, 92, 123, 153) + 0.5) +
  scale_y_continuous(breaks=seq(0,cyr-y1), labels=yearlab) + 
  scale_x_continuous(breaks=c(15.5, 46, 76.5, 107.5, 138), labels=c("May", "Jun", "Jul", "Aug", "Sep")) +
  theme(plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5),
        plot.title.position = "plot",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "bottom") +
  labs(title="Adelaide Daily Minimum Temperatures",
       subtitle="Within each year, top square is Adelaide Airport and bottom square is City") +
  geom_text(data = countColdAirport,  aes(xv, WinterPl, label=BelowZero)) +
  geom_text(data = countColdCity,  aes(xv, WinterPl, label=BelowZero))