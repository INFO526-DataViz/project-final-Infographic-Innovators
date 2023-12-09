# global.R
library(shiny)
library(tidyverse)
# ... (other libraries)

# Load data
flights_ntsb <- read_csv(here("data", "flight_crash_data_NTSB.csv"))
# ... (other data preparation steps)
