#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic
server <- function(input, output) {
  
  # Function to render plot based on input
  output$plot <- renderPlot({
    # Switch case to handle different plots
    switch(input$plotType,
           "Time Series - Interactive - Total Fatalies" = { 
             # Creating a dataset for the timeSeries plot
             flights_ntsb_timeseries <- flights_ntsb |>
               group_by(event_year) |>
               summarise(
                 total_fatalities = sum(fatal_injury_count, na.rm = TRUE),
                 total_serious_injuries = sum(serious_injury_count, na.rm = TRUE),
                 total_minor_injuries = sum(minor_injury_count, na.rm = TRUE)
               )
             
             #time series plot ----
             tp <-
               ggplot(flights_ntsb_timeseries,
                      aes(x = event_year, y = total_fatalities)) +
               geom_line() +
               labs(title = "Yearly Aircraft Crash Fatalities",
                    x = "Year",
                    y = "Total Fatalities") +
               theme_minimal()
             
             # Interactive plot with ploty---- total fatalities
             p <-
               ggplot(flights_ntsb_timeseries,
                      aes(x = event_year, y = total_fatalities)) +
               geom_line() +
               geom_text(aes(label = "✈️"),
                         vjust = -0.5,
                         hjust = 0.5,
                         size = 5) +
               labs(title = "Yearly Aircraft Crash Fatalities",
                    x = "Year",
                    y = "Total Fatalities")
             
             ggplotly(p)
             
           },
           "Time Series - Animated" = { # all 3 plots with label - animated 
             
             
             fcdata <- flights_ntsb_timeseries |>
               ungroup() |>
               pivot_longer(cols = starts_with("total_"),
                            names_to = "Category",
                            values_to = "Count") |>
               mutate(
                 Category = case_when(
                   Category == "total_fatalities" ~ "Fatalities",
                   Category == "total_serious_injuries" ~ "Serious Injuries",
                   Category == "total_minor_injuries" ~ "Minor Injuries"
                 )
               )
             
             
             
             # Creating the animated plot
             animation_fcdata <- ggplot(fcdata,
                                        aes(event_year,
                                            Count,
                                            group = Category,
                                            color = Category)) +
               geom_line(show.legend = FALSE) +
               geom_segment(
                 aes(xend = max(event_year) +
                       0.1,
                     yend = Count),
                 linetype = 2,
                 colour = 'grey',
                 show.legend = FALSE
               ) +
               geom_point(size = 2,
                          show.legend = FALSE) +
               geom_text(
                 aes(
                   x = max(event_year) + 0.1,
                   label = Category,
                   color = "#000000"
                 ),
                 hjust = 0,
                 show.legend = FALSE
               ) +
               transition_reveal(event_year) +
               coord_cartesian(clip = 'off') +
               theme(plot.title = element_text(size = 20)) +
               labs(title = 'Aircraft Crash Statistics Over Time',
                    y = 'Count',
                    x = element_blank()) +
               theme(plot.margin = margin(5.5, 40, 5.5, 5.5))
             
             # Animating the plot
             animated_fcdata <- animate(
               animation_fcdata,
               fps = 10,
               duration = 25,
               width = 800,
               height = 350,
               renderer = gifski_renderer("images/animated_fcdata.gif")
             )
           },
           "Plot 3" = { # Replace with your actual plot code or object
             # Example: boxplot(rnorm(100))
           },
           "Plot 2" = { # Replace with your actual plot code or object
             # Example: hist(rnorm(100))
           },
           "Plot 2" = { # Replace with your actual plot code or object
             # Example: hist(rnorm(100))
           },
           "Plot 2" = { # Replace with your actual plot code or object
             # Example: hist(rnorm(100))
           },
           "Plot 2" = { # Replace with your actual plot code or object
             # Example: hist(rnorm(100))
           },
           "Plot 2" = { # Replace with your actual plot code or object
             # Example: hist(rnorm(100))
           },
           )
  })
}
