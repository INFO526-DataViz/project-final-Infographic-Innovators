# Data

**National Transportation Safety Board (NTSB) Flight Crash Dataset**:

The dataset central to our investigation was procured through a request to the National Transportation Safety Board (NSTB). It encompasses detailed records of aircraft crashes in the U.S. from January 1, 1980, to December 31, 2022.

# Codebook for NTSB FLight Crash Dataset

## Variable Names and Descriptions:

The dataset is structured as a tibble with 89,134 rows and 38 columns, out of which a few unnecessary columns has been removed for our purpose which resulted in a tibble with 89,134 rows and 20 columns.

| Variable Name        | Description                                                                                 |
|------------------|------------------------------------------------------|
| event_type           | Type of event - accident, incident or occurrence.                                           |
| event_date           | Date time of when the event has occurred.                                                   |
| city                 | The city or place location closest to the site of the event.                                |
| state                | The state in which the site of the event is present.                                        |
| has_safety_rec       | If the aircraft has a safety recommendation.                                                |
| report_type          | The report type of the incident, what is the highest level it was reported to.              |
| highest_injury_level | Indicate the highest level of injury among all injuries sustained as a result of the event. |
| fatal_injury_count   | The total number of fatal injuries from an event.                                           |
| serious_injury_count | The total number of serious injuries from an event.                                         |
| minor_injury_count   | The total number of minor injuries from an event.                                           |
| probable_cause       | The probable cause for the aircraft crash as per the NTSB report.                           |
| latitude             | Latitude for the event site in degrees and decimal degrees.                                 |
| longitude            | Longitude for the event site in degrees and decimal degrees.                                |
| air_craft_category   | The type of aircraft.                                                                       |
| airport_id           | The airport ID.                                                                             |
| airport_name         | Airport name where the event happened.                                                      |
| amateur_built        | Aircraft is a homebuilt (Y/N).                                                              |
| number_of_engines    | Number of engines of the aircraft                                                           |
| air_craft_damage     | The level of aircraft damage                                                                |
| weather_condition    | Weather condition at the time of event                                                      |

## Data Types:

| Variable Name        | Datatype  |
|----------------------|-----------|
| event_type           | Character |
| event_date           | POSIXct   |
| city                 | Character |
| state                | Character |
| has_safety_rec       | Logical   |
| report_type          | Character |
| highest_injury_level | Character |
| fatal_injury_count   | Numeric   |
| serious_injury_count | Numeric   |
| minor_injury_count   | Numeric   |
| probable_cause       | Character |
| latitude             | Numeric   |
| longitude            | Numeric   |
| air_craft_category   | Character |
| airport_id           | Character |
| airport_name         | Character |
| amateur_built        | Character |
| number_of_engines    | Numeric   |
| air_craft_damage     | Character |
| weather_condition    | Character |
