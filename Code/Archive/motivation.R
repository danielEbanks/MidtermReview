library(readr)
library(tidyverse)
library(ggplot2)

HistoricalHouse <- read_csv("Data/HistoricalHouse.csv")

HistoricalHouse <- HistoricalHouse %>% 
  mutate(PresSeats = case_when(President == "Democrat" ~ Democrats, TRUE ~ Republicans),
         midterm = rep(c(0,1), 27),
         change = PresSeats - lag(PresSeats, default = first(PresSeats)), 
         elec_year = Years - 1) 

ggplot(HistoricalHouse %>% filter(midterm == 1, Years <= 2023), aes(x = Years, y = change)) + 
  geom_line() + 
  geom_point() + 
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = -9, color = "red", lty = 4) +
  labs(y = "Net House Seats for Presidential Party", x = "") +
  theme_classic() +
  theme(axis.text.x = element_blank()) 


ggplot(HistoricalHouse %>% filter(midterm == 1, Years <= 2023), aes( x = Democrats/`# of House Seats`)) + 
  geom_histogram(binwidth = 0.05, alpha = 0.1) +
  geom_density() +
  geom_histogram(binwidth = 0.05, alpha = 0.1, HistoricalHouse %>% filter(midterm == 0, Years <= 2023), mapping = aes(x  = Democrats/`# of House Seats`, fill = President))+
  geom_density(lwd = 3, HistoricalHouse %>% filter(midterm == 0, Years <= 2023), mapping = aes(x  = Democrats/`# of House Seats`, color = President))



ggplot(HistoricalHouse %>% filter(midterm == 1, Years <= 2023), aes( x = Years, y = Democrats/`# of House Seats`)) + 
  geom_line() +
  geom_line( HistoricalHouse %>% filter(midterm == 0, Years <= 2023), mapping = aes(x = Years, y = Democrats/`# of House Seats`, color = President))
