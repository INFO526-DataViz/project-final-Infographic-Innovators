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
View(fcdata)


# Aggregating data by year
fcdata <- fcdata |>
  mutate(Year = year(EventDate)) |>
  group_by(Year) |>
  summarise(Total_Fatalities = sum(FatalInjuryCount, na.rm = TRUE),
            Total_Serious_Injuries = sum(SeriousInjuryCount, na.rm = TRUE),
            Total_Minor_Injuries = sum(MinorInjuryCount, na.rm = TRUE))


#time series plot 
ggplot(fcdata, aes(x = Year, y = Total_Fatalities)) +
  geom_line() + 
  labs(title = "Yearly Aircraft Crash Fatalities",
       x = "Year", 
       y = "Total Fatalities") +
  theme_minimal()