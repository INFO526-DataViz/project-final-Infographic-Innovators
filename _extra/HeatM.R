

library(ggplot2)
library(gganimate)
library(transformr)
library(sf)
library(readr)
library(dplyr)
library(lubridate)




# Load and prepare data
flight_crash_data <- read_csv("data/flight_crash_data_NTSB.csv")
flight_crash_data <- flight_crash_data |>
  mutate(Year = year(Date),  # Extract year from the date
         Latitude = as.numeric(Latitude),  # Ensure Latitude is numeric
         Longitude = as.numeric(Longitude)) # Ensure Longitude is numeric




# Basic heat map
p <- ggplot(flight_crash_data, aes(x = Longitude, y = Latitude)) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
  scale_fill_gradient(low = "blue", high = "red") +
  theme_minimal() +
  labs(fill = "Density", x = "Longitude", y = "Latitude", title = "Heat Map of Aircraft Crashes Over Time")

# Animate the heat map
animated_heat_map <- p + 
  transition_time(Year) +
  labs(title = 'Year: {frame_time}')

# Render the animation
anim_save("animated_heat_map.gif", animated_heat_map)

