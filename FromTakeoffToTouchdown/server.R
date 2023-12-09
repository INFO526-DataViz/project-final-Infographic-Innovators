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
          
            "Radar Chart" = { 
             # Plotting an Interactive Radar Plot using Plotly
             radar_chart <- plot_ly(type = 'scatterpolar',
                                    fill = 'toself',
                                    mode = 'lines+markers')
             
             # first plot tracing of VMC data
             radar_chart <- radar_chart |>
               add_trace(
                 r = radar_trace_r2,
                 theta = radar_theta,
                 name = 'VMC'
               )
             # second plot tracing of IMC data
             radar_chart <- radar_chart |>
               add_trace(
                 r = radar_trace_r1,
                 theta = radar_theta,
                 name = 'IMC'
               )
             # plot layout configuration
             radar_chart <- radar_chart |>
               layout(polar = list(radialaxis = list(
                 visible = T,
                 range = c(0, 10500)
               )))
             
             radar_chart
             
           },
           "Radial Plot Total Crashes" = { # Plotting a Radial Barplot to show the Total Count of Crashes and phase of flight 
             flights_radial_bar_crashes <-
               ggplot(flights_ntsb_radial,
                      aes(x = fct_rev(flight_phase), y = total_crashes, 
                          fill = flight_phase)) +
               geom_bar(stat = "identity", width = 0.8) +
               geom_text(hjust = 1.2, size = 4.2, 
                         aes(y = 0, label = comma(total_crashes))) +
               coord_polar(theta = "y") +
               labs(
                 x = NULL,
                 y = NULL,
                 fill = "Phase of Flight",
                 title = "A Radial View of Total Crashes",
                 subtitle = "as per the phase of flight",
                 caption = ""
               ) +
               scale_y_continuous(
                 breaks = seq(0, 27000, by = 5000),
                 limits = c(0, 27000)
               ) +
               scale_x_discrete(expand = c(0.35, 0)) + 
               scale_fill_frontiers() +
               theme(
                 legend.position = "bottom",
                 axis.text = element_blank(),
                 panel.grid.minor = element_blank(),
                 panel.grid.major = element_blank()
               ) +
               guides(
                 fill = guide_legend(
                   nrow = 1,
                   direction = "horizontal",
                   title.position = "top",
                   title.hjust = 0.5,
                   label.position = "bottom",
                   label.hjust = 1,
                   label.vjust = 1,
                   label.theme = element_text(lineheight = 0.25, size = 14),
                   keywidth = 1.5,
                   keyheight = 0.5
                 )
               )
             flights_radial_bar_crashes
             
           },
           "Radial Plot Injuries" = { 
             flights_radial_bar_injuries <-
             ggplot(flights_ntsb_radial,
                    aes(x = fct_rev(flight_phase), y = total_injuries, 
                        fill = flight_phase)) +
             geom_bar(stat = "identity", width = 0.8) +
             geom_text(hjust = 1.2, size = 4.2, 
                       aes(y = 0, label = comma(total_injuries))) +
             coord_polar(theta = "y") +
             labs(
               x = NULL,
               y = NULL,
               fill = "Phase of Flight",
               title = "A Radial View of Total Injuries",
               subtitle = "as per the phase of flight",
               caption = ""
             ) +
             scale_y_continuous(
               breaks = seq(0, 13200, by = 1000),
               limits = c(0, 13200)
             ) +
             scale_x_discrete(expand = c(0.35, 0)) + 
             scale_fill_manual(
               values = moma.colors("VanGogh")
             ) +
             theme(
               legend.position = "bottom",
               axis.text = element_blank(),
               panel.grid.minor = element_blank(),
               panel.grid.major = element_blank()
             ) +
             guides(
               fill = guide_legend(
                 nrow = 1,
                 direction = "horizontal",
                 title.position = "top",
                 title.hjust = 0.5,
                 label.position = "bottom",
                 label.hjust = 1,
                 label.vjust = 1,
                 label.theme = element_text(lineheight = 0.25, size = 14),
                 keywidth = 1.5,
                 keyheight = 0.5
               )
             ) 
           
           flights_radial_bar_injuries
           
           },
           "US Map" = { 
             
             # making gif using gganimate package
             hex_bin_maps <- list.files(path = "images/map_plot/", full.names = TRUE)
             hex_bin_maps_list <- lapply(hex_bin_maps, image_read)
             
             # Joining all the saved images
             joined_plots <- image_join(hex_bin_maps_list)
             
             # Animating the images using image_animate() and restting the resolution
             # Setting fps = 1
             hex_bin_maps_animation <- image_animate(image_scale(joined_plots, "2000x1000"), fps = 2)
             
             # Saving gif to the repository
             image_write(image = hex_bin_maps_animation,
                         path = "images/flight_crash_us_states.gif")
             
             hex_bin_maps_animation
             
           },
           )
  })
}
