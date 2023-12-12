# installing packages!

if (!require(pacman))
  install.packages(pacman)

pacman::p_load(
  tidyverse,
  dplyr,
  janitor,
  dlookr,
  here,
  ggpubr,
  maps,
  plotly,
  gganimate,
  MetBrewer,
  ggsci,
  scales,
  ggimage,
  emojifont,
  viridis,
  usmap,
  readr,
  animation,
  shiny,
  shinythemes,
  shinyjs
)


# Load datasets for each plot
flights_ntsb_maps <-
  read_csv(here("data/shinyapp_data", "flights_ntsb_maps.csv"))
flights_ntsb_radar <-
  read_csv(here("data/shinyapp_data", "flights_ntsb_radar.csv"))
flights_ntsb_radial <-
  read_csv(here("data/shinyapp_data", "flights_ntsb_radial.csv"))
flights_ntsb_timeseries <-
  read_csv(here("data/shinyapp_data", "flights_ntsb_timeseries.csv"))
flights_ntsb_waffle <-
  read_csv(here("data/shinyapp_data", "flights_ntsb_waffle.csv"))
flights_ntsb_density <-
  read_csv(here("data/shinyapp_data", "flights_ntsb_density.csv"))


# Function to generate plots
generate_plots <- function(plot_type) {
  # Replace this with your actual plot code based on the selected type
  if (plot_type == "Time Series Analysis") {
    flight_plot <- plot_ly(flights_ntsb_timeseries, x = ~event_year) %>%
      add_trace(
        type = "scatter",
        mode = "lines",
        name = "Fatalities",
        x = ~event_year,
        y = ~total_fatalities
      ) %>%
      add_trace(
        type = "scatter",
        mode = "lines",
        name = "Serious Injuries",
        x = ~event_year,
        y = ~total_serious_injuries
      ) %>% 
      add_trace(
        type = "scatter",
        mode = "lines",
        name = "Minor Injuries",
        x = ~event_year,
        y = ~total_minor_injuries
      ) %>%
      layout(
        title = "Aviation Accidents Over Time",
        yaxis = list(title = "Number of People"),
        xaxis = list(
          title = "Event Year",
          tickvals = seq(1980, max(flights_ntsb_timeseries$event_year), by = 5),
          ticktext = seq(1980, max(flights_ntsb_timeseries$event_year), by = 5)
        ),
        shapes = list(
          list(
            type = "line",
            x0 = 2001,
            x1 = 2001,
            y0 = 0,
            y1 = 1,
            yref = "paper",
            line = list(color = "red", width = 2)
          )
        )
      )
  } else if (plot_type == "Flight Crashes vs Flight Phases") {
    flights_ntsb_radial <- flights_ntsb_radial |>
      arrange(desc(total_crashes))
    
    flights_ntsb_radial$flight_phase <- factor(flights_ntsb_radial$flight_phase, levels = flights_ntsb_radial$flight_phase)
    
    flight_plot <- plot_ly(
      flights_ntsb_radial,
      x = ~ flight_phase,
      y = ~ total_crashes,
      type = 'bar',
      text = ~ scales::comma(total_crashes),
      marker = list(
        color = ~ total_crashes,
        colorscale = viridis::viridis(10, direction = -1)
      )
    ) %>%
      layout(
        xaxis = list(title = NULL),
        yaxis = list(title = NULL),
        barmode = 'stack',
        showlegend = FALSE,
        polar = list(
          radialaxis = list(
            visible = TRUE,
            tickformat = ",d",
            tickmode = "linear"
          )
        )
      )
  } else if (plot_type == "Flight Crashes vs Weather Conditions") {
    radar_trace_r1 <- flights_ntsb_radar |>
      filter(weather_condition %in% c("IMC")) |>
      pull(total_crashes)
    
    radar_trace_r1 <-
      c(
        radar_trace_r1,
        flights_ntsb_radar |>
          filter(weather_condition == "IMC" & event_month == 1) |>
          pull(total_crashes)
      )
    
    # Filtering out weather condition 'VMC'
    radar_trace_r2 <- flights_ntsb_radar |>
      filter(weather_condition %in% c("VMC")) |>
      pull(total_crashes)
    
    radar_trace_r2 <-
      c(
        radar_trace_r2,
        flights_ntsb_radar |>
          filter(weather_condition == "VMC" & event_month == 1) |>
          pull(total_crashes)
      )
    
    radar_theta <-
      c(
        "Jan" ,
        "Feb" ,
        "Mar" ,
        "Apr",
        "May",
        "Jun",
        "Jul",
        "Aug",
        "Sept",
        "Oct",
        "Nov",
        "Dec",
        "Jan"
      )
    
    # Plotting an Interactive Radar Plot using Plotly
    flight_plot <- plot_ly(type = 'scatterpolar',
                           fill = 'toself',
                           mode = 'lines+markers')
    
    # first plot tracing of VMC data
    flight_plot <- flight_plot |>
      add_trace(r = radar_trace_r2,
                theta = radar_theta,
                name = 'VMC')
    # second plot tracing of IMC data
    flight_plot <- flight_plot |>
      add_trace(r = radar_trace_r1,
                theta = radar_theta,
                name = 'IMC')
    # plot layout configuration
    flight_plot <- flight_plot |>
      layout(polar = list(radialaxis = list(
        visible = T,
        range = c(0, 10500)
      )))
  } else if (plot_type == "Flight Crashes vs US states") {
    flights_ntsb_maps$hover <-
      with(
        flights_ntsb_maps,
        paste(
          state,
          '<br>',
          "Total Crashes",
          total_crashes,
          "Total Injuries",
          total_injuries
        )
      )
    
    # Create a tibble with state names and abbreviations
    state_data <- tibble(state_name = state.name,
                         state_abb = state.abb)
    
    flights_ntsb_maps <- flights_ntsb_maps |>
      left_join(state_data, by = c("state" = "state_name"))
    
    # specify some map projection/options
    g <- list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      showlakes = TRUE,
      lakecolor = toRGB('white')
    )
    
    flight_plot <-
      plot_geo(flights_ntsb_maps, locationmode = 'USA-states')
    
    flight_plot <- flight_plot %>% add_trace(
      z = ~ total_crashes,
      text = ~ hover,
      locations = ~ state_abb,
      frame = ~ event_year,
      color = ~ total_crashes,
      colors = "Purples",
      colorbar = list(title = 'Total Crashes', len = 0.7),
      colorscale = met.brewer(name = "Tam"),
      zmin = 0,
      zmax = 400
    )
    
    flight_plot <-
      flight_plot %>% colorbar(title = "Number of Crashes")
    flight_plot <-
      flight_plot %>% layout(title = 'Flight crashes in US states',
                             geo = g)
    
    flight_plot
  }else if (plot_type == "Flight Crashes vs Causes") {
    waffle_data <- flights_ntsb_waffle %>%
      slice(rep(1:n(), times = number)) %>%
      mutate(fill_value = ifelse(row_number() <= sum(floored), floored, floored + 1))
    
    plot_data <- expand.grid(x = 0:9,y = 0:9) %>%
      rowwise() |>
      mutate(index = 1+sum(x * 10 + y >= cumsum(flights_ntsb_waffle$number)),
             cause = flights_ntsb_waffle$cause_summary[[index]])
    
    # Create a waffle plot
    p<-ggplot(plot_data, aes(x, y, color = cause), show.legend=F) +
      geom_text(label = "✈",
                family = 'sans',
                size = 8) +
      scale_color_manual(values=met.brewer("Redon")) +
      coord_equal() +
      theme_void()+
      theme(
        legend.position = 'top',
        legend.margin = margin(1, 3, 1, 1, unit = 'mm'),
        plot.margin = margin(3,3,3,3,unit = 'mm'),
        legend.background = element_rect(fill = 'grey100', color = 'grey')
      )+
      labs(title="Waffle chart showing different causes of crashes",
           colour = 'Cause')
    flight_plot <- ggplotly(p, tooltip = c("color")) |>
      layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, title=""),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, title=""))
    
    flight_plot
    
  } else if (plot_type == "Distribution of cause - Pilot's failure"){
    p <- ggplot(
      subset(
        probable_cause_flights,
        probable_cause_flights$cause_summary == "pilot's failure"
      )
    ) +
      geom_density(aes(
        x = year(event_date),
        fill = factor(highest_injury_level, levels = c("Fatal", "Serious", "Minor"))
      ), alpha = 0.7) +
      scale_fill_manual(values = c("#bcd67c", "#d398ff", "#82dfe2")) +
      labs(title = "Distribution of crashes caused by pilot's negligence over time",
           x = "Year",
           y = "Density",
           fill = "Highest Injury Level") + 
      theme_minimal() +
      theme(
        panel.grid.major = element_blank(),
        plot.title = element_text(size = 20, face = "bold"),
        plot.subtitle = element_text(size = 15),
        axis.text = element_text(size = 12),
        axis.title = element_text(hjust = 0, size = 12)
      )
    flight_plot <- ggplotly(p)
  }
  
  return(flight_plot)
}

# Defining UI
# Defining UI
ui <- navbarPage(
  title = "From Takeoff to Touchdown",
  theme = shinytheme("flatly"),
  tabPanel("Analysis",
           sidebarLayout(
             sidebarPanel(
               width = 3,
               br(),
               span(
                 "From Takeoff to Touchdown: Dissecting Data on Air Disasters",
                 tags$a("Flight Crashes NTSB", href = "https://info526-dataviz.github.io/project-final-Infographic-Innovators/")
               ),
               br(),
               hr(),
               helpText("Choose plots you want to analyze"),
               selectInput(
                 "flight_plots_list",
                 "Flight Crashes Plots",
                 choices = c(
                   "Time Series Analysis",
                   "Flight Crashes vs Flight Phases",
                   "Flight Crashes vs Weather Conditions",
                   "Flight Crashes vs US states",
                   "Flight Crashes vs Causes",
                   "Distribution of cause - Pilot's failure"
                 ),
                 selected = "Flight Crashes vs US states"
               ),
               br(),
               br(),
               hr(),
               span(
                 "Data source:",
                 tags$a("NTSB", href = "https://www.ntsb.gov/Pages/AviationQueryV2.aspx")
               ),
               br(),
               br(),
               em(span(
                 "Created by",
                 a(href = "https://github.com/INFO526-DataViz/project-final-Infographic-Innovators", "Infographic Innovators")
               ),
               br(),
               span(
                 "Code",
                 a(href = "https://github.com/INFO526-DataViz/project-final-Infographic-Innovators", "on GitHub")
               ))
             ),
             mainPanel(tabsetPanel(
               tabPanel(
                 "Flight_Plots",
                 br(),
                 h4(textOutput("infoText")),
                 hr(),
                 plotlyOutput("flightplots", height = 400)
               )
             ))
           ))
)

# Defining server logic
server <- function(input, output, session) {
  # Render the selected plot
  output$flightplots <- renderPlotly({
    plot_type <- input$flight_plots_list
    generate_plots(plot_type)
  })
  
  output$infoText <- renderText({
    paste("Selected Plot Type: ", input$flight_plots_list)
  })
}



# Running the application
shinyApp(ui = ui, server = server)
