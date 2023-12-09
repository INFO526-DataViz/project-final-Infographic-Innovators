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

# setting figure parameters for knitr
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

# Modifying the data using mutate()
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




