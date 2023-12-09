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
           "Time Series - Interactive" = { 
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
           "Plot 2" = { # Replace with your actual plot code or object
             # Example: hist(rnorm(100))
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
