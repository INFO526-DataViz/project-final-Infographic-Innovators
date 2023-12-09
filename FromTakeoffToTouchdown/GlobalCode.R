# global.R
library(shiny)
library(tidyverse)
# ... (other libraries)

# Load data
flights_ntsb <- read_csv(here("data", "flight_crash_data_NTSB.csv"))
# ... (other data preparation steps)
# GETTING THE LIBRARIES
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

#global settings 
# setting theme for ggplot2
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