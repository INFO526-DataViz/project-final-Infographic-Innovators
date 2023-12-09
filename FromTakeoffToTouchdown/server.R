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
           "Plot 1" = { # Replace with your actual plot code or object
             # Example: plot(x = rnorm(100), y = rnorm(100))
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
