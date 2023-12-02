

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
               viridis,
               timetk, 
               gganimate,
               DT, 
               crosstalk, 
               emojifont, 
               gifski, 
               magick) 





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
  geom_text(aes(label = "✈️"), 
            vjust = -0.5, 
            hjust = 0.5, 
            size = 5)
  labs(title = "Yearly Aircraft Crash Fatalities",
       x = "Year", 
       y = "Total Fatalities")

ggplotly(p)

# animates for 2 see 

# p <- ggplot(fcdata, aes(x = Year, y = Total_Fatalities)) +
#   geom_line() + 
#   geom_point() +
#   scale_color_viridis(discrete = TRUE) +
#   labs(title = "Yearly Aircraft Crash Fatalities: Total Fatalities",
#        x = "Year", 
#        y = "Total Fatalities") +
#   theme_minimal()








# Animated the plot - This one is better 
animated_plot <- p +
  geom_text(aes(label = ifelse(Year == Year, "✈️", "")), 
            vjust = -0.5, 
            hjust = 0.5, 
            color = "blue",
            size = 5) +
  transition_reveal(Year) +#revealing the year 
  ease_aes('linear') +
  shadow_mark()

animated_plot

# To animate in steps of 2 years, we use 'transition_states' with 'transition_length' and 'state_length'
animated_plot2 <- p + 
  transition_states(Year, transition_length = 2, state_length = 1) +
  ease_aes('linear') +
  shadow_mark()

animated_plot2

# Save or render the animation
anim_save("animated_yearly_fatalities.gif", animated_plot)



# adjusted code I found
# Filter data for the most recent year for each frame-https://stackoverflow.com/questions/58439944/how-to-use-your-own-image-for-geom-point-in-gganimate
fcdata_emoji <- fcdata |>
  group_by(Year) |>
  filter(Year == max(Year)) |>
  ungroup()

# Use this filtered data for placing the emoji
animated_plot <- p +
  geom_text(data = fcdata_emoji, 
            aes(label = "✈️"), 
            vjust = -0.5, 
            hjust = 0.5, 
            color = "blue", 
            size = 5) +
  transition_reveal(Year) +
  ease_aes('linear') +
  shadow_mark()

animate(animated_plot, 
        nframes = 200, 
        width = 800, 
        height = 600, 
        renderer = gifski_renderer())




















# https://deepshamenghani.quarto.pub/dmenghani/tidytuesday/plotly/


state_data_imputed <-  fcdata |> 
  filter(state == 'Washington') |>
  arrange(subsector) |>
  select(state_abbr, year, month, subsector, change_yoy) |>
  mutate(change_yoy = as.numeric(change_yoy |> str_remove('S'))) |> 
  group_by(subsector) |> 
  fill(change_yoy, .direction = "up") |> #Replace missing data with next good value within the group
  ungroup() %>% 
  mutate(date = ifelse(month < 10, paste0(year,'-0',month, '-01'), paste0(year,'-',month, '-01'))) %>% # Create a readable date column
  select(state_abbr, subsector, date, change_yoy)

datatable(state_data_imputed)



















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




