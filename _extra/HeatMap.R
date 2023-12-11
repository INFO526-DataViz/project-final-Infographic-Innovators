# Install and load required libraries
install.packages(c("tidyverse", "plotly"))
library(tidyverse)
library(plotly)
library(dplyr)
library(janitor)

flights_ntsb <- read_csv("data/flight_crash_data_NTSB.csv")

# selecting columns which are required for our analysis
flights_ntsb <- flights_ntsb |>
  select(
    EventType, EventDate,
    City, State,
    ReportType, HighestInjuryLevel,
    FatalInjuryCount, SeriousInjuryCount,
    MinorInjuryCount, ProbableCause,
    Latitude, Longitude,
    AirCraftCategory, NumberOfEngines, 
    AirCraftDamage, WeatherCondition
  ) |>
  # cleaning column names using janitor package
  clean_names()

#pf_study_map <- subset(pf_study, !is.na(Longitude))
#pf_study_map_Loc <- pf_study_map[
#  (duplicated(pf_study_map[c("Latitude","Longitude")]) |
#     duplicated(pf_study_map[c("Latitude","Longitude")], fromLast = TRUE)), ]

#pf_study_map_Loc <- subset(pf_study_map_Loc, !is.na(Longitude))
#pf_study_map_Loc$Coordinates<- paste(pf_study_map_Loc$Latitude, 
#                                     pf_study_map_Loc$Longitude,sep = "/")

ggplot(subset(flights_ntsb, !is.na(longitude) & !is.na(latitude) 
              & latitude < 75 & latitude > 10 & longitude < -60),  
       aes(y = latitude, x = longitude)) +
  geom_point(shape=21, alpha = 0.2) +
  scale_y_continuous(breaks = seq(-70, 100, 10)) +
  scale_x_continuous(breaks = seq(-180, 180, 10),
                     limits = c(-180, -50)) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank()
  )
