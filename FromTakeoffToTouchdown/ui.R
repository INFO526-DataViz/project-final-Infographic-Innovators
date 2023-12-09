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
  
  
}

# Create Shiny object
shinyApp(ui = ui, server = server)
