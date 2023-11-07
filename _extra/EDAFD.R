# GETTING THE LIBRARIES
if (!require("pacman"))
  install.packages("pacman")

pacman::p_load(DataExplorer, skimr, correlationfunnel, janitor, readr)

flights_ntsb <- read_csv("data/flight_crash_data_NTSB.csv")
View(flights_ntsb)

structure(flights_ntsb)

head(flights_ntsb
     )
aircraft_crashes <- flights_ntsb |> clean_names()
structure(aircraft_crashes)


#quick data exploration - DataExplorer package
introduce(aircraft_crashes)
# A tibble: 1 × 9
# rows columns discrete_columns continuous_columns all_missing_columns
# <int>   <int>            <int>              <int>               <int>
#   1 89134      38               31                  6                   1
# ℹ 4 more variables: total_missing_values <int>, complete_rows <int>,
#   total_observations <int>, memory_usage <dbl>

# visualize missing
plot_missing(aircraft_crashes)

# Provides a detailed summary of each variable in the dataset - skimr package
skim(aircraft_crashes)
#At this point we can remove any empty variables 

# Calculate correlations - https://github.com/business-science/correlationfunnel
correlations <- cor(aircraft_crashes[, sapply(aircraft_crashes, is.numeric)], use="complete.obs")

# Eisenhower Box visualization - couldn't get this package or code to work 
# correlation_funnel(correlations)
# plot_correlation_funnel(dfcor)
# dfcor <- as.data.frame(correlations)
# dfcor |>
#   plot_correlation_funnel(interactive = FALSE)



plot_histogram(aircraft_crashes)


plot_bar(aircraft_crashes)


plot_correlation(aircraft_crashes)

# Event type below
# ACC accident
# ICC incident
# OCC occurence

# https://www.ntsb.gov/Pages/AviationDownloadDataDictionary.aspx


# categories to look at 
# Event date, serious injury count, fatal injury count, minor injury count, event type, latitude, longitude, 

###### Possible questions and how we can visualize them ----

# How have the number of crashes and injuries evolved over time? Q2
# Time-based Analysis -Visualization: Line graphs with "Event date" on the x-axis, stacked time serious count showing highest injury count, and counts of crashes or injuries on the y-axis. This can help in visualizing trends over time.


# What is the distribution of injury severity (fatal, serious, minor) for the aircraft crashes?
#   Injury Severity Analysis-Visualization: Stacked bar charts or pie charts representing the proportion of fatal, serious, and minor injuries for each crash.


# Which types of events lead to the most injuries or fatalities? plus Q2
#   Event Type Analysis- Visualization: Bar graphs with "Event type" on the x-axis and counts of injuries or fatalities on the y-axis. 


# Where do most aircraft crashes occur? #question 1 
#   Geographical Analysis-Visualization: A geographical heatmap or scatter plot using latitude and longitude to pinpoint crash locations. Areas with a high concentration of crashes will be highlighted.


# Is there a correlation between the event type and the severity of injuries?
#   Correlation Analysis-Visualization: A matrix plot or correlation heatmap showing correlations between different event types and injury counts.


# Are there specific months or seasons that witness a higher number of crashes? - #plus q 1 
#   Monthly or Seasonal Trends-Visualization: Bar graphs or line graphs showing crash counts per month or season.


# Are there specific regions that experience more crashes during particular times or seasons?
#   Spatial-Temporal Analysis-Visualization: Animated scatter plots or heatmaps over time on a geographical map to visualize the temporal and spatial distribution of crashes.

