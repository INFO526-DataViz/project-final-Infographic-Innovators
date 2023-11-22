

#load libraries ----
if(!require(pacman))
  install.packages("pacman")
pacman::p_load(tidyverse, 
               janitor, 
               colorspace,
               broom,
               fs,
               dplyr,
               readr,
               lubridate, 
               plotly, 
               timetk) 





# flight_crash_data_NTSB <- read_csv("data/flight_crash_data_NTSB.csv")
# View(flight_crash_data_NTSB)
fcdata <- read_csv("data/flight_crash_data_NTSB.csv")
glimpse(fcdata)


# Aggregating data by year----
fcdata <- fcdata |>
  mutate(Year = year(EventDate)) |>
  group_by(Year) |>
  summarise(Total_Fatalities = sum(FatalInjuryCount, na.rm = TRUE),
            Total_Serious_Injuries = sum(SeriousInjuryCount, na.rm = TRUE),
            Total_Minor_Injuries = sum(MinorInjuryCount, na.rm = TRUE))


#time series plot ----
ggplot(fcdata, aes(x = Year, y = Total_Fatalities)) +
  geom_line() + 
  labs(title = "Yearly Aircraft Crash Fatalities",
       x = "Year", 
       y = "Total Fatalities") +
  theme_minimal()


# Interactive plot with ploty----
p <- ggplot(fcdata, aes(x = Year, y = Total_Fatalities)) +
  geom_line() +
  labs(title = "Yearly Aircraft Crash Fatalities",
       x = "Year", 
       y = "Total Fatalities")

ggplotly(p)


# Advanced time series---- I can't get it to work 
library(timetk)

# Example: Time-series decomposition

fcdata <- fcdata |>
  mutate(Year = year(EventDate))

fcdata_ts <- fcdata |>
  group_by(Year) |>
  summarise(Total_Fatalities = sum(fatal_injury_count, na.rm = TRUE)) |>
  tk_ts(start = min(fcdata$Year), end = max(fcdata$Year), frequency = 1)

decomposed_data <- decompose(fcdata_ts, type = "multiplicative")
autoplot(decomposed_data)






fcdata |>
  tk_ts(start = c(Year[1]), end = c(Year[length(Year)]), frequency = 1) |>
  decompose(type = "multiplicative") |>
  autoplot()


ggplot(fcdata, aes(x = Year, y = Total_Fatalities)) +
  geom_line() +
  geom_point(data = HighestInjuryLevel, aes(x = Year, y = Total_Fatalities), color = "red") +
  geom_text(data = HighestInjuryLevel, aes(x = Year, y = Total_Fatalities, label = Crash_Name), vjust = -1) +
  labs(title = "Yearly Aircraft Crash Fatalities",
       x = "Year", 
       y = "Total Fatalities") +
  theme_minimal()




