#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(tidyverse)









# Load data
flights_ntsb <- read_csv(here("data", "flight_crash_data_NTSB.csv"))

# Define UI
ui <- fluidPage(theme = shinytheme("lumen"),
                titlePanel("Flight Data Visualization"),
                sidebarLayout(
                  sidebarPanel(
                    
                    # Select variable type of trend to plot
                   
                    # Sidebar layout with input and output elements
                    sidebarLayout(
                      # Sidebar panel for inputs
                      sidebarPanel(
                        selectInput("plotType", 
                                    "Choose a Plot:", 
                                    choices = c("Time Series - Interactive - Total Fatalies", 
                                                "Time Series - Animated", 
                                                "Radar Chart",
                                                "Radial Plot Total Crashes", 
                                                "Radial Plot Injuries",
                                                "US Map"))
                      ),
                      
                      # Main panel for displaying outputs
                      mainPanel(
                        plotOutput("plot")
                      )
                    )
                  )
                    
                    
                    # Descriptor text
                    HTML("Total aircraft crashes, and injuries"),
                    
                 

# Define server function
server <- function(input, output) {
    
    output$plot <- renderPlot({
      if (input$plotType == "Time Series - Interactive - Total Fatalies") {
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
        
      } else if (input$plotType == "Time Series - Animated") {
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
      } else if (input$plotType == "Radar Chart") {
        # Code to generate this plot
      }
      # ... (other plot options)
    })
    
  }  
  
}

# Create Shiny object
shinyApp(ui = ui, server = server)
