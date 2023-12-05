

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
               magick,
               ggimage, 
               patchwork, 
               animation) 


# flight_crash_data_NTSB <- read_csv("data/flight_crash_data_NTSB.csv")
# View(flight_crash_data_NTSB)
fcdata <- read_csv("data/flight_crash_data_NTSB.csv")
glimpse(fcdata)

fcdata <- fcdata |>
  mutate(Year = year(EventDate)) |>
  group_by(Year) |>
  summarise(Total_Fatalities = sum(FatalInjuryCount, na.rm = TRUE),
            Total_Serious_Injuries = sum(SeriousInjuryCount, na.rm = TRUE),
            Total_Minor_Injuries = sum(MinorInjuryCount, na.rm = TRUE))

Image <- "images/airplane.png"  

# Total fatality plot
p_fatalities <- ggplot(fcdata, aes(x = Year, y = Total_Fatalities)) +
  geom_line() +
  geom_image(aes(image = Image), size = 0.05) +  
  labs(title = "Yearly Aircraft Crash Fatalities",
       x = "Year", 
       y = "Total Fatalities") +
  theme_minimal()

animated_fatalities <- p_fatalities +
  transition_reveal(Year) +
  ease_aes('linear') +
  shadow_mark()

animate(animated_fatalities, 
        nframes = 200, 
        width = 800, height = 600, 
        renderer = gifski_renderer())

#Total serious plot
p_serious_injuries <- ggplot(fcdata, aes(x = Year, 
                                         y = Total_Serious_Injuries)) +
  geom_line() +
  geom_image(aes(image = Image), 
             size = 0.05) +  
  labs(title = "Yearly Aircraft Crash Serious Injuries",
       x = "Year", 
       y = "Total Serious Injuries") +
  theme_minimal()

animated_serious_injuries <- p_serious_injuries +
  transition_reveal(Year) +
  ease_aes('linear') +
  shadow_mark()

animate(animated_serious_injuries, 
        nframes = 200, 
        width = 800, 
        height = 600, 
        renderer = gifski_renderer())


#total minor injuries 
p_minor_injuries <- ggplot(fcdata, 
                           aes(x = Year, 
                                       
                               y = Total_Minor_Injuries)) +
  geom_line() +
  geom_image(aes(image = Image), size = 0.05) +  
  labs(title = "Yearly Aircraft Crash Minor Injuries",
       x = "Year", 
       y = "Total Minor Injuries") +
  theme_minimal()

animated_minor_injuries <- p_minor_injuries +
  transition_reveal(Year) +
  ease_aes('linear') +
  shadow_mark()

animate(animated_minor_injuries, 
        nframes = 200, 
        width = 800, 
        height = 600, 
        renderer = gifski_renderer())















#interactive data plot 
# Aggregating data by year----
fcdata <- fcdata |>
  mutate(Year = year(EventDate)) |>
  group_by(Year) |>
  summarise(Total_Fatalities = sum(FatalInjuryCount, na.rm = TRUE),
            Total_Serious_Injuries = sum(SeriousInjuryCount, na.rm = TRUE),
            Total_Minor_Injuries = sum(MinorInjuryCount, na.rm = TRUE))


#time series plot ----
tp <-ggplot(fcdata, aes(x = Year, y = Total_Fatalities)) +
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

#Total transition with plane ----
Image <- "images/airplane.png"  


p <- ggplot(fcdata, aes(x = Year, y = Total_Fatalities)) +
  geom_line() +
  geom_image(aes(image = Image), size = 0.05) +  # Adjust size as needed
  labs(title = "Yearly Aircraft Crash Fatalities",
       x = "Year", 
       y = "Total Fatalities") +
  theme_minimal()

# If you are animating the plot
animated_plot <- p +
  transition_reveal(Year) +
  ease_aes('linear') +
  shadow_mark()

animate(animated_plot, 
        nframes = 200, 
        width = 800, 
        height = 600, 
        renderer = gifski_renderer())






















# 3 animations
# Static plot - interactive 
fcdata <- fcdata |>
  mutate(Year = year(EventDate)) |>
  group_by(Year) |>
  summarise(Total_Fatalities = sum(FatalInjuryCount, na.rm = TRUE),
            Total_Serious_Injuries = sum(SeriousInjuryCount, na.rm = TRUE),
            Total_Minor_Injuries = sum(MinorInjuryCount, na.rm = TRUE))

# Reshaping the data to a longer format
long_fcdata <- fcdata |>
  pivot_longer(cols = starts_with("Total_"), 
               names_to = "Category", 
               values_to = "Count")



# Time series plot with all categories
tp <- ggplot(long_fcdata, 
             aes(x = Year, 
                 y = Count, 
                 color = Category)) +
  geom_line() +
  labs(title = "Yearly Aircraft Crash Statistics",
       x = "Year", 
       y = "Count") +
  theme_minimal()

# Interactive plot with plotly
p <- ggplot(long_fcdata, aes(x = Year, 
                             y = Count, 
                             color = Category)) +
  geom_line() +
  geom_text(aes(label = "✈️"), 
            vjust = -0.5, 
            hjust = 0.5, 
            size = 5) +
  labs(title = "Yearly Aircraft Crash Statistics",
       x = "Year", 
       y = "Count")

ggplotly(p)


# layers in gif? didn't really work for me 


# trying for animated version of 3 categories 
fcdata <- fcdata |>
  mutate(Year = year(EventDate)) |>
  group_by(Year) |>
  summarise(Total_Fatalities = sum(FatalInjuryCount, na.rm = TRUE),
            Total_Serious_Injuries = sum(SeriousInjuryCount, na.rm = TRUE),
            Total_Minor_Injuries = sum(MinorInjuryCount, na.rm = TRUE))

# Reshaping the data to a longer format
long_fcdata <- fcdata |>
  pivot_longer(cols = starts_with("Total_"), 
               names_to = "Category", 
               values_to = "Count")

# Image for the plot
Image <- "images/airplane.png"

# Creating the animated plot
p <- ggplot(long_fcdata, aes(x = Year, 
                             y = Count, 
                             color = Category,
                             group = Category)) +
  geom_line() +
  geom_image(aes(image = Image), size = 0.04) +  
  labs(title = "Yearly Aircraft Crash Statistics",
       x = "Year", 
       y = "Count") +
  theme_minimal()

# Animating the plot
animated_plot <- p +
  transition_reveal(Year) +
  ease_aes('linear') +
  shadow_mark()

animate(animated_plot, 
        nframes = 200, 
        width = 800, 
        height = 600, 
        renderer = gifski_renderer())

#colors squares above 



















# trying for animated version of 3 categories 
fcdata <- fcdata |>
  mutate(Year = year(EventDate)) |>
  group_by(Year) |>
  summarise(Total_Fatalities = sum(FatalInjuryCount, na.rm = TRUE),
            Total_Serious_Injuries = sum(SeriousInjuryCount, na.rm = TRUE),
            Total_Minor_Injuries = sum(MinorInjuryCount, na.rm = TRUE))

# Reshaping the data to a longer format
long_fcdata <- fcdata |>
  pivot_longer(cols = starts_with("Total_"), 
               names_to = "Category", 
               values_to = "Count")

# Image for the plot
Image <- "images/airplane.png"

# Creating the animated plot
p <- ggplot(long_fcdata, aes(x = Year, 
                             y = Count, 
                             color = Category,
                             group = Category)) +
  geom_line() +
  geom_image(data = long_fcdata |>#each it's own layer?
               filter(Category == "Total_Fatalities"), aes(image = Image), size = 0.04) +
  geom_image(data = long_fcdata |>
               filter(Category == "Total_Serious_Injuries"), aes(image = Image), size = 0.04) +
  geom_image(data = long_fcdata |>
               filter(Category == "Total_Minor_Injuries"), aes(image = Image), size = 0.04) +  
  labs(title = "Yearly Aircraft Crash Statistics",
       x = "Year", 
       y = "Count") +
  theme_minimal()

# Animating the plot
animated_plot <- p +
  transition_reveal(Year) +
  ease_aes('linear') +
  shadow_mark()

animate(animated_plot, 
        nframes = 200, 
        width = 800, 
        height = 600, 
        renderer = gifski_renderer())
























#Below is invis squares 
fcdata <- fcdata |>
  mutate(Year = year(EventDate)) |>
  group_by(Year) |>
  summarise(Total_Fatalities = sum(FatalInjuryCount, na.rm = TRUE),
            Total_Serious_Injuries = sum(SeriousInjuryCount, na.rm = TRUE),
            Total_Minor_Injuries = sum(MinorInjuryCount, na.rm = TRUE))

# Reshaping the data to a longer format
long_fcdata <- fcdata |>
  pivot_longer(cols = starts_with("Total_"), 
               names_to = "Category", 
               values_to = "Count")

# Image for the plot
Image <- "images/airplane.png"

# Creating the animated plot
p <- ggplot(long_fcdata, aes(x = Year, 
                             y = Count, 
                             color = Category, 
                             group = Category)) +
  geom_line() +
  geom_point(aes(y = ifelse(Year == max(Year), 
                            Count, NA)), 
             size = 0) + # Invisible points
  geom_image(aes(y = ifelse(Year == max(Year), 
                            Count, NA), 
                 image = Image), size = 0.05, na.rm = TRUE) +
  labs(title = "Yearly Aircraft Crash Statistics",
       x = "Year", 
       y = "Count") +
  theme_minimal()

# Animating the plot
animated_plot <- p +
  transition_reveal(Year) +
  ease_aes('linear') +
  shadow_mark()

animate(animated_plot, nframes = 200, 
        width = 800, height = 600, renderer = gifski_renderer())

#invis above 






# Creating separate data frames for each category----
fatalities_df <- fcdata %>%
  select(Year, Total_Fatalities)

serious_injuries_df <- fcdata %>%
  select(Year, Total_Serious_Injuries)

minor_injuries_df <- fcdata %>%
  select(Year, Total_Minor_Injuries)
# Function to create and animate a plot for a given data frame and y-value
animate_category <- function(data, y_value, y_label) {
  p <- ggplot(data, aes(x = Year, y = !!sym(y_value))) +
    geom_line() +
    geom_image(aes(image = Image), size = 0.05) +
    labs(title = paste("Yearly Aircraft Crash Statistics:", y_label),
         x = "Year", 
         y = y_label) +
    theme_minimal()
  
  animated_plot <- p +
    transition_reveal(Year) +
    ease_aes('linear') +
    shadow_mark()
  
  animate(animated_plot, nframes = 200, width = 800, height = 600, renderer = gifski_renderer())
}

# Animate each category
animate_category(fatalities_df, "Total_Fatalities", "Total Fatalities")
animate_category(serious_injuries_df, "Total_Serious_Injuries", "Total Serious Injuries")
animate_category(minor_injuries_df, "Total_Minor_Injuries", "Total Minor Injuries")










# Patchwork is B----
# Extract the latest point for each year and each category

fcdata <- fcdata %>%
  mutate(Year = year(EventDate)) %>%
  group_by(Year) %>%
  summarise(Total_Fatalities = sum(FatalInjuryCount, na.rm = TRUE),
            Total_Serious_Injuries = sum(SeriousInjuryCount, na.rm = TRUE),
            Total_Minor_Injuries = sum(MinorInjuryCount, na.rm = TRUE)) %>%
  ungroup() %>%
  pivot_longer(cols = starts_with("Total_"), names_to = "Category", values_to = "Count") %>%
  mutate(Category = case_when(Category == "Total_Fatalities" ~ "Fatalities",
                              Category == "Total_Serious_Injuries" ~ "Serious Injuries",
                              Category == "Total_Minor_Injuries" ~ "Minor Injuries"))



# Creating the animated plot
animation_fcdata <- ggplot(fcdata, aes(Year, Count, group = Category, color = Category)) +
  geom_line(show.legend = FALSE) +
  geom_segment(aes(xend = max(Year) + 0.1, yend = Count), linetype = 2, colour = 'grey', show.legend = FALSE) +
  geom_point(size = 2, show.legend = FALSE) +
  geom_text(aes(x = max(Year) + 0.1, label = Category, color = "#000000"), hjust = 0, show.legend = FALSE) +
  transition_reveal(Year) +
  coord_cartesian(clip = 'off') +
  theme(plot.title = element_text(size = 20)) +
  labs(title = 'Aircraft Crash Statistics Over Time',
       y = 'Count',
       x = element_blank()) +
  theme(plot.margin = margin(5.5, 40, 5.5, 5.5))

# Animating the plot
animated_fcdata <- animate(animation_fcdata,
                           fps = 10,
                           duration = 25,
                           width = 800, height = 200,
                           renderer = gifski_renderer("images/animated_fcdata.gif"))

# Display or save the animated plot
animated_fcdata






















# Animated the plot - This one is better 
animated_plot <- tp +
  geom_text(aes(label = ifelse(Year == Year, "✈️️", "")), 
            vjust = -0.5,
            hjust = 0.5,
            color = "blue",
            size = 5) +
  transition_reveal(Year) +#revealing the year 
  ease_aes('linear') +
  shadow_mark()

animated_plot
# anim_save("animated_yearly_fatalities12.gif", animated_plot)
# To animate in steps of 2 years, we use 'transition_states' with 'transition_length' and 'state_length'
# animated_plot2 <- p + 
#   transition_states(Year, transition_length = 2, state_length = 1) +
#   ease_aes('linear') +
#   shadow_mark()
# 
# animated_plot2
# 
# # Save or render the animation
# anim_save("animated_yearly_fatalities.gif", animated_plot)



# adjusted code I found
# Filter data for the most recent year for each frame-https://stackoverflow.com/questions/58439944/how-to-use-your-own-image-for-geom-point-in-gganimate
# fcdata_emoji <- fcdata |>
#   group_by(Year) |>
#   filter(Year == max(Year)) |>
#   ungroup()
# 
# # Use this filtered data for placing the emoji
# animated_plot <- p +
#   geom_text(data = fcdata_emoji, 
#             aes(label = "✈️"), 
#             vjust = -0.5, 
#             hjust = 0.5, 
#             color = "blue", 
#             size = 5) +
#   transition_reveal(Year) +
#   ease_aes('linear') +
#   shadow_mark()
# 
# animate(animated_plot, 
#         nframes = 200, 
#         width = 800, 
#         height = 600, 
#         renderer = gifski_renderer())




















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




