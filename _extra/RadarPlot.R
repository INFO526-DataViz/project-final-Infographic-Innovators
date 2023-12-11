
library(fmsb)
library(tidyverse)
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

flights_ntsb <- flights_ntsb |>
  mutate(event_time = format(event_date, "%H:%M"),
         .after = event_date) |>
  mutate(
    event_date = as.Date(event_date),
    flight_phase = case_when(
      grepl("Landing", probable_cause, ignore.case = TRUE) ~ "Landing",
      grepl("Approach", probable_cause, ignore.case = TRUE) ~ "Approach",
      grepl("Takeoff", probable_cause, ignore.case = TRUE) ~ "Takeoff",
      grepl("Maneuvering", probable_cause, ignore.case = TRUE) ~ "Maneuvering",
      grepl("Climb", probable_cause, ignore.case = TRUE) ~ "Climb",
      grepl("Descent", probable_cause, ignore.case = TRUE) ~ "Descent",
      grepl("Taxi", probable_cause, ignore.case = TRUE) ~ "Taxi",
      grepl("Cruise", probable_cause, ignore.case = TRUE) ~ "Cruise",
      grepl("Hover", probable_cause, ignore.case = TRUE) ~ "Hover",
      grepl("Standing", probable_cause, ignore.case = TRUE) ~ "Standing",
      grepl("Uncontrolled Descent", probable_cause, ignore.case = TRUE) ~ "Uncontrolled Descent",
      grepl("Emergency", probable_cause, ignore.case = TRUE) ~ "Emergency",
      grepl("Holding", probable_cause, ignore.case = TRUE) ~ "Holding",
    ),
    .after = probable_cause
  ) |>
  mutate(
    event_year = year(event_date),
    event_month = month(event_date)
  )

flights_ntsb_radar <- flights_ntsb |>
  mutate(
    weather_condition = case_when(
      weather_condition == "IFR" ~ "IMC",
      weather_condition == "VFR" ~ "VMC",
      weather_condition == "Unknown" ~ "UNK",
      is.na(weather_condition) ~ "UNK",
      TRUE ~ weather_condition
    )
  ) |>
  group_by(event_month, weather_condition) |>
  summarise(
    total_crashes = n()
  )

flights_ntsb_radar_fmsb <- flights_ntsb_radar |>
  pivot_wider(
    names_from = event_month,
    values_from = total_crashes
  ) |>
  setNames(c("weather_condition" , "Jan" , "Feb" , "Mar" , "Apr", "May", "Jun", "Jul", "Aug", "Sept", "Oct", "Nov", "Dec")
  ) |>
  column_to_rownames("weather_condition")

flights_ntsb_radar_fmsb <- rbind(rep(10500, 12), rep(0, 12), flights_ntsb_radar_fmsb)


create_radarchart <- function(data,
                              color = "#00AFBB",
                              vlabels = colnames(data),
                              vlcex = 0.7,
                              caxislabels = NULL,
                              title = NULL) {
  radarchart(
    data,
    axistype = 1,
    # Customize the polygon
    pcol = color,
    pfcol = scales::alpha(color, 0.5),
    plwd = 2,
    plty = 1,
    # Customize the grid
    cglcol = "grey",
    cglty = 1,
    cglwd = 0.8,
    # Customize the axis
    axislabcol = "grey",
    # Variable labels
    vlcex = vlcex,
    vlabels = vlabels,
    caxislabels = caxislabels,
    title = title
  )
}

op <- par(mar = c(1, 1, 1, 1), cex.lab = 5)
# Create the radar charts
create_radarchart(
  data = flights_ntsb_radar_fmsb, caxislabels = c(0, 2500, 5000, 7500, 10500),
  color = c("#00AFBB", "#E7B800", "#FC4E07")
)
# Add an horizontal legend
legend(
  x=1.1, y=1.2, legend = rownames(flights_ntsb_radar_fmsb[-c(1,2),]),
  bty = "n", pch = 20 , col = c("#00AFBB", "#E7B800", "#FC4E07"),
  text.col = "black", cex = 1, pt.cex = 1.5
)
par(op)

flights_ntsb_radar <- flights_ntsb_radar |>
  mutate(
    weather_condition = factor(weather_condition, levels = c("UNK", "IMC", "VMC"))
  )


radar_plot <- ggplot(flights_ntsb_radar, aes(x = event_month, y = total_crashes, 
                                             group = weather_condition, color = weather_condition, fill = weather_condition)) +
  geom_point() +
  geom_area(alpha = 0.75, position = "identity") +
  coord_polar() +
  theme_minimal() +
  labs(title = "Radial Bar Plot of Flight Phases and Crash Counts",
       y = "Number of Crashes occuring")

radar_plot


ggplot(flights_ntsb_radar, aes(x = event_month, y = total_crashes, 
                               group = weather_condition, color = weather_condition, fill = weather_condition)) +
  geom_bar(stat = "identity", 
           width = 1) +
  scale_x_discrete(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
                   limits = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
                   labels = c("Jan" , "Feb" , "Mar" , "Apr", "May", "Jun", "Jul", "Aug", "Sept", "Oct", "Nov", "Dec")) +
  coord_cartesian(expand = FALSE) +
  coord_polar() +
  theme(panel.border = element_blank(),
        panel.grid.minor = element_blank())


