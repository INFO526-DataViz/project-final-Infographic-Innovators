

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
               timetk, 
               gganimate) 





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

# animates for 2 see 

p <- ggplot(fcdata, aes(x = Year, y = Total_Fatalities)) +
  geom_line() + 
  labs(title = "Yearly Aircraft Crash Fatalities: {frame_time}",
       x = "Year", 
       y = "Total Fatalities") +
  theme_minimal()

# Animate the plot
animated_plot <- p +
  transition_time(Year) +
  ease_aes('linear') +
  shadow_mark()

# To animate in steps of 2 years, we use 'transition_states' with 'transition_length' and 'state_length'
animated_plot <- p + 
  transition_states(Year, transition_length = 2, state_length = 1) +
  ease_aes('linear') +
  shadow_mark()

# Save or render the animation
anim_save("animated_yearly_fatalities.gif", animated_plot)





# Advanced time series---- I can't get it to work with this random package I found 
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




