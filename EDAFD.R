

library(DataExplorer)
library(skimr)
library(correlationfunnel)
library(janitor)
library(readr)

flights_ntsb <- read_csv("data/flights_ntsb.csv")
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

# Calculate correlations
correlations <- cor(aircraft_crashes[, sapply(aircraft_crashes, is.numeric)], use="complete.obs")

# Eisenhower Box visualization - couldn't get this to work 
# correlation_funnel(correlations)
# plot_correlation_funnel(dfcor)
# dfcor <- as.data.frame(correlations)
# dfcor |>
#   plot_correlation_funnel(interactive = FALSE)



plot_histogram(aircraft_crashes)


plot_bar(aircraft_crashes)


plot_correlation(aircraft_crashes)



