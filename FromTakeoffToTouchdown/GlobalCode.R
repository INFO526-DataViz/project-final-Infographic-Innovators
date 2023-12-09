# global.R----
library(shiny)

# ... (other libraries)

# Load data----
flights_ntsb <- read_csv(here("data", "flight_crash_data_NTSB.csv"))
# ... (other data preparation steps)
# GETTING THE LIBRARIES----
if (!require(pacman))
  install.packages(pacman)


pacman::p_load(tidyverse,
               dplyr,
               janitor,
               dlookr,
               here,
               ggpubr,
               maps,
               plotly,
               gganimate,
               MetBrewer,
               ggsci,
               scales,
               fmsb,
               gifski,
               ggimage,
               emojifont,
               magick,
               lubridate,
               patchwork,
               viridis,
               usmap,
               sf,
               sp,
               animation)

pacman::p_load_gh("BlakeRMills/MoMAColors")

#global settings ----
#### setting theme for ggplot2
ggplot2::theme_set(ggplot2::theme_minimal(base_size = 14, base_family = "sans"))

# setting width of code output
options(width = 65)

# setting figure parameters for knitr----
knitr::opts_chunk$set(
  fig.width = 8,        # 8" width
  fig.asp = 0.618,      # the golden ratio
  fig.retina = 1,       # dpi multiplier for displaying HTML output on retina
  fig.align = "center", # center align figures
  dpi = 200,            # higher dpi, sharper image
  message = FALSE
)


#Data wrangling ----
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

# Modifying the data using mutate()----
flights_ntsb <- flights_ntsb |>
  # getting event time from the date time column
  mutate(event_time = format(event_date, "%H:%M"),
         # places the column after event_date
         .after = event_date) |>
  mutate(
    # getting event date from the date time column
    event_date = as.Date(event_date),
    # extracting flight phase based on NTSB's designated terminology
    flight_phase = case_when(
      grepl("Landing", probable_cause, ignore.case = TRUE) ~ "Landing",
      grepl("Stop", probable_cause, ignore.case = TRUE) ~ "Landing",
      grepl("Approach", probable_cause, ignore.case = TRUE) ~ "Approach",
      grepl("Takeoff", probable_cause, ignore.case = TRUE) ~ "Takeoff",
      grepl("Take-off", probable_cause, ignore.case = TRUE) ~ "Takeoff",
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
    # places the flight phase column after probable cause
    .after = probable_cause 
  ) |>
  # getting year and month of the events
  mutate(
    event_year = year(event_date),
    event_month = month(event_date)
  )



# Assigning different weather conditions to variables----
# IFR and IMC are same conditions so we are combining them
# VFR and VMC are same conditions so we are combining them
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
  # getting total crashes and total injuries
  group_by(event_month, weather_condition) |>
  summarise(
    total_crashes = n(),
    total_injuries = sum(fatal_injury_count, na.rm = TRUE) + sum(serious_injury_count, na.rm = TRUE) + sum(minor_injury_count, na.rm = TRUE)
  ) |>
  arrange(event_month)
# Filtering out weather condition 'IMC'
radar_trace_r1 <- flights_ntsb_radar |>
  filter(weather_condition %in% c("IMC")) |>
  pull(total_crashes)

radar_trace_r1 <-
  c(
    radar_trace_r1,
    flights_ntsb_radar |> 
      filter(weather_condition == "IMC" & event_month == 1) |> 
      pull(total_crashes)
  )

# Filtering out weather condition 'VMC'
radar_trace_r2 <- flights_ntsb_radar |>
  filter(weather_condition %in% c("VMC")) |>
  pull(total_crashes)

radar_trace_r2 <-
  c(
    radar_trace_r2,
    flights_ntsb_radar |> 
      filter(weather_condition == "VMC" & event_month == 1) |> 
      pull(total_crashes)
  )

radar_theta <- c("Jan" , "Feb" , "Mar" , "Apr", "May", "Jun", "Jul", "Aug", "Sept", "Oct", "Nov", "Dec", "Jan")





# Creating a filtered dataset for Radial Plot----
flights_ntsb_radial <- flights_ntsb |>
  # getting total crashes and total injuries
  group_by(flight_phase) |>
  summarise(
    total_crashes = n(),
    total_injuries = sum(fatal_injury_count, na.rm = TRUE) + sum(serious_injury_count, na.rm = TRUE) + sum(minor_injury_count, na.rm = TRUE)
  ) |>
  drop_na() |>
  arrange(desc(total_crashes))

# getting top 5 flight phases where flight crashes occured
flights_ntsb_radial <- flights_ntsb_radial |>
  slice(1:5) |>
  bind_rows(
    flights_ntsb_radial |>
      slice(-(1:5)) |>
      summarize(
        flight_phase = "Other",
        total_crashes = sum(total_crashes),
        total_injuries = sum(total_injuries)
      )
  ) |>
  mutate(flight_phase = fct_inorder(flight_phase))

flights_ntsb_radial



# Filtering out NA and invalid Latitude and Longitude values
flights_ntsb_maps <- flights_ntsb |>
  subset(!is.na(longitude) & !is.na(latitude)
         & latitude < 75 & latitude > 10 & longitude < -60)

# Creating a dataset for the map plot and picking the columns that we need
flights_ntsb_maps <- flights_ntsb_maps |>
  # getting total crashes and total injuries
  group_by(state, event_year) |>
  summarise(
    total_crashes = n(),
    total_injuries = sum(fatal_injury_count, na.rm = TRUE) + sum(serious_injury_count, na.rm = TRUE) + sum(minor_injury_count, na.rm = TRUE)
  ) |>
  drop_na()

# getting unique states in the data
unique_states <- unique(flights_ntsb_maps$state)

# getting all years involved in the data
all_years <- unique(flights_ntsb_maps$event_year)

# combining above data so that we will be generating data of the states 
# which are missing in the original data
all_states_data <-
  expand.grid(state = unique_states, event_year = all_years)

flights_ntsb_maps <-
  # using left_join() to combine the data based on state and event_year
  left_join(all_states_data,
            flights_ntsb_maps,
            by = c("state", "event_year")) |>
  mutate(
    total_crashes = ifelse(is.na(total_crashes), 0, total_crashes),
    total_injuries = ifelse(is.na(total_injuries), 0, total_injuries)
  )


# Creating a list of Years from dataset
years_list <- flights_ntsb_shp |>
  arrange(event_year) |>
  distinct(event_year) |>
  pull()

for (i in seq_along(years_list)) {
  my_maps <-
    paste0("images/map_plot/flight_crash_us_states",
           years_list[i],
           ".jpg")
  getHexBinMap(input_year = years_list[i])
  ggsave(
    my_maps,
    height = 9,
    width  = 15,
    unit   = "in",
    dpi    = 200
  )
}
















