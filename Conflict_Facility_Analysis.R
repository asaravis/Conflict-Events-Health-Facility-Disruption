
# Title: Exploring Predictive Composite Measures for Global Facility Disruptions in Conflict Zones
# Output: Conflict event types and time intervals that are most/least predictive of health facility disruption status 
# Date: "2024-04-04"
# Author: Arden Saravis

# Clear environment
rm(list=ls())


######################################## DATA CLEANING ########################################
# Load libraries for data cleaning
library(tidyverse)
library(dplyr)

######## ACLED Data Cleaning #########

###### 1) Load ACLED data
# Due to the volume of data included in ACLED, data will likely need to be downloaded in sections. Load all sections into R and merge the datasets together. 

# Read in ACLED data
data <- read.csv("/Users/arden/Desktop/Capstone/Data - Capstone/ACLED_Data_pt1.csv")
data2 <- read.csv("/Users/arden/Desktop/Capstone/Data - Capstone/ACLED_Data_pt2.csv")

# Merge the datasets together (if ACLED is downloaded in sections)
acled <- rbind(data, data2)


###### 2) Keep relevant variables for the analysis
# event_id_cnty : unique ID for each conflict event
# event_date : date that the conflict event occurred. If event occurred over 24 hours, each day will be recorded as a separate event.
# event_type : the type of event (battles, riots, etc.)
# sub_event_type : a subcategory of event type
# region : region of the world where the event took place (e.g. Eastern Africa)
# country : the country or territory in which the event took place
# latitude : the latitude of the location in four decimal degrees notation
# longitude : the longitude of the location in four decimal degrees notation
acled <- acled[, c("event_id_cnty", "event_date", "event_type", "sub_event_type", "region", "country", "latitude", "longitude")]

###### 3) Filter data by violent event types
# include all Battles
# include all Riots
# include all Explosions/Remote Violence
# include all Violence Against Civilians
# **exclude** all Strategic Developments
# for Protests, only include Excessive Force Against Protesters subcategory

# Exclude all Strategic Developments
acled <- acled %>% filter(event_type == "Battles" | event_type == "Riots" |
                            event_type == "Explosions/remote violence" |
                            event_type == "Violence against civilians" | 
                            event_type == "Protests")
# Only include 'Excessive Force Against Protesters' subcategory in 'Protests'
acled <- acled %>% filter(!(sub_event_type %in% c("Protest with intervention",
                                                  "Peaceful protest")))

###### 4) Save the cleaned dataset (acled_clean.csv)

write.csv(acled, file = "acled_clean.csv")

######### Health Facility Disruption Dataset Creation/Cleaning #########
# This code uses data from the Humanitarian Data Exchange (HDE) to mimic a dataset that has regular health facility disruption status updates. 
# Variables in the HDE include: 'X', 'latutide', 'longitude'

###### 1) Load in relevant HDE dataset(s) 
# For code creation, Nigeria is used as the country of interest
nigeria_hf <- read.csv("/Users/arden/Desktop/Capstone/Data - Capstone/nigeria.csv")

## 2) Keep relevant variables for the analysis
# X : the longitude of the health facility 
# Y : the latitude of a health facility
# **only keep health facilities where we have latutide and longitude points**

# Subset to only include 'X' and 'Y'
nigeria_hf <- nigeria_hf[, c("X", "Y")]

# Exclude any rows where 'X' or 'Y' is NA
nigeria_hf <- nigeria_hf %>%
  filter(!is.na(X) & !is.na(Y))

# Rename columns X to longitude and Y to latitude
names(nigeria_hf)[names(nigeria_hf) == "X"] <- "longitude"
names(nigeria_hf)[names(nigeria_hf) == "Y"] <- "latitude"

###### 3) Create a 'date' variable representing the date of a health facility disruption status update. 
# Repeat each health facility row three times to represent different dates in time.

# Assuming your dataset is named 'nigeria_hf'
n <- nrow(nigeria_hf)

# Define the dates you want to add
dates <- as.Date(c('2019-12-10', '2021-09-01', '2023-06-27'))

# Replicate each row three times
nigeria_hf <- nigeria_hf[rep(1:n, each = 3), ]

# Add the 'date' variable
nigeria_hf$date <- rep(dates, each = n)

###### 4) Add a unique identifier row for each health facility 'X'
nigeria_hf$X <- 1:nrow(nigeria_hf)


######################################## ANALYSIS ########################################
# Load libraries for analysis
library(ggplot2)
library(maps)
library(mapdata)
library(spatstat) # Geospatial library
library(sf) # Geospatial library
library(nnet) # multinominal logistic regression


###### 1) Read in datasets

# Load ACLED dataset into environment
acled <- read.csv("/Users/arden/Desktop/Capstone/Data - Capstone/acled.csv")
# Show a summary of the dataset
summary(acled)

# Load health facilities (nigeria_clean) dataset into environment
# Dataset can be changed according to region of interest or with HeRAMS data
facilities <- read.csv("/Users/arden/facilities_clean.csv")
# Show a summary of the dataset
summary(facilities)


###### 2) Visualize point data of conflict events and health facilities

## Plot ACLED Conflict Event points
# region = "Nigeria" - can be substituted for another country/region of interest or deleted for a world map
world_map <- map_data("world", region = "Nigeria")

# Plot conflict events on the map of Nigeria
ggplot() +
  geom_map(data = world_map, map = world_map,
           aes(x = long, y = lat, map_id = region),
           fill = "lightgray", color = "black") +
  geom_point(data = acled,
             aes(x = longitude, y = latitude),
             color = "darkred", size = 0.1) +
  labs(title = "Conflict Events from January 2019 to January 2024") +
  theme_minimal()

## Plot health facility points
# region = "Nigeria" - can be substituted for another country/region of interest or deleted for a world map
world_map <- map_data("world", region = "Nigeria")
# Plot health facilities
ggplot() +
  geom_map(data = world_map, map = world_map,
           aes(x = long, y = lat, map_id = region),
           fill = "lightgray", color = "black") +
  geom_point(data = facilities,
             aes(x = longitude, y = latitude),
             color = "darkblue", size = 0.1) +
  labs(title = "Health Facility Updates from January 2019 to January 2024") +
  theme_minimal()


###### 3) Create an observation window for the analysis
# This selects the data that falls into the geographic area of interest** 
# For this test analysis, the observation window is Nigeria plus a 50km buffer around the border. The purpose of this expansion is to include data points that may lay in bordering countries that could affect health facility functionality that would be excluded by restricting to data only in Nigeria.

# Coordinates for Nigeria
nigeria_coords <- list(
  x = c(3.5, 14, 14, 3.5, 3.5),
  y = c(4, 4, 13.9, 13.9, 4)
)

# Create an owin object for Nigeria 
## This restricts the analysis to run solely for data points laying in Nigeria
nigeria_owin <- owin(poly = nigeria_coords)

# Expand the window by 50km in each direction 
nigeria_owin_buffer <- owin(c(nigeria_coords$x[1] - 0.45, nigeria_coords$x[3] + 0.45),
                            c(nigeria_coords$y[1] - 0.45, nigeria_coords$y[3] + 0.45))


###### 4) Make conversions to variables to appropriate formats for the analysis

# Convert 'facilities' and 'acled' to sf object
## This converts latitude and longitude points into geospatial marks for analysis using the WGS84 projection
facilities_sf <- st_as_sf(facilities, coords = c("longitude", "latitude"), crs = "+proj=longlat +datum=WGS84", agr = "constant")
acled_sf <- st_as_sf(acled, coords = c("longitude", "latitude"), crs = "+proj=longlat +datum=WGS84", agr = "constant")

# Convert date variables to class "Date" to make it usable for analysis
facilities_sf$date <- as.Date(facilities_sf$date)
acled_sf$event_date <- dmy(acled_sf$event_date) 

# Calculate 4 weeks prior to the health facility status update 
## This prepares the data to only capture conflict events that occurred within the four-week time period before the update
facilities_sf$four_weeks <- facilities_sf$date - weeks(4)

# Calculate 1 year prior to the health facility status update
## This prepares the data to only capture conflict events that occurred within the one-year time period before the update
facilities_sf$one_year <- facilities_sf$date - weeks(52)


###### 5) Run the Inverse Distance Weighting function for the 4-week time horizon

#### FOUR MONTHS ####
# Create function to process each health facility
facility_run <- function(i, p=2) {    # Define the power parameter for IDW (decay rate)
  
  # Subset the data to only include conflict events that occurred within 4 weeks prior to the health facility update
  acled_sf_4wk <- acled_sf[acled_sf$event_date >= facilities_sf[i, ]$four_weeks & acled_sf$event_date <= facilities_sf[i, ]$date, ] 
  
  # Calculate the distance between the health facility and each conflict event 
  acled_sf_4wk$dist <- as.vector(st_distance(facilities_sf[i, ], acled_sf_4wk))
  
  # Select conflict events that fall within 20km of the health facility **distance in meters (20 km = 20,000 meters)
  ## This selects conflict events that would be relevant for the analysis assuming that health facilities over 20km away would have minimal effect on health facility functionality
  acled_sf_4wk <- acled_sf_4wk[acled_sf_4wk$dist <= 20000, ]
  
  # Check if the subset is empty
  ## This allows for a health facility to not have any proximal conflict events by returning 0
  if (nrow(acled_sf_4wk) == 0) {
    return(data.frame(event_type = 'no_event', weights = 0, facility = facilities_sf$X[i]))
  }
  
  # Compute inverse distance weights for each conflict event
  acled_sf_4wk$weights <- 1 / (acled_sf_4wk$dist^p)
  
  # Aggregate weighted values per event type
  aggregate_values <- aggregate(weights ~ event_type, data = acled_sf_4wk, FUN = sum)
  
  # Add 'X' (facility) identifier to the aggregated values
  aggregate_values$facility <- facilities_sf$X[i]
  
  # Return the aggregate values
  return(aggregate_values)
}

# Use lapply to apply the function to each row/facility in facilities_sf dataset
aggregate_results <- lapply(1:nrow(facilities_sf), function(n) {
  facility_run(i=n, p=2) # nests of loops of functions for p values
}) 

# Combine all results into a single dataframe
idw_results_4wk <- do.call(rbind, aggregate_results)

glimpse(idw_results_4wk)

#### ONE YEAR ####

# Create function to process each health facility
facility_run <- function(i, p=2) {    # Define the power parameter for IDW (decay rate)
  
  # Subset the data to only include conflict events that occurred within 4 weeks prior to the health facility update
  acled_sf_1yr <- acled_sf[acled_sf$event_date >= facilities_sf[i, ]$one_year & acled_sf$event_date <= facilities_sf[i, ]$date, ] 
  
  # Calculate the distance between the health facility and each conflict event 
  acled_sf_1yr$dist <- as.vector(st_distance(facilities_sf[i, ], acled_sf_1yr))
  
  # Select conflict events that fall within 20km of the health facility **distance in meters (20 km = 20,000 meters)
  ## This selects conflict events that would be relevant for the analysis assuming that health facilities over 20km away would have minimal effect on health facility functionality
  acled_sf_1yr <- acled_sf_1yr[acled_sf_1yr$dist <= 20000, ]
  
  # Check if the subset is empty
  ## This allows for a health facility to not have any proximal conflict events by returning 0
  if (nrow(acled_sf_1yr) == 0) {
    return(data.frame(event_type = 'no_event', weights = 0, facility = facilities_sf$X[i]))
  }
  
  # Compute inverse distance weights for each conflict event
  acled_sf_1yr$weights <- 1 / (acled_sf_1yr$dist^p)
  
  # Aggregate weighted values per event type
  aggregate_values <- aggregate(weights ~ event_type, data = acled_sf_1yr, FUN = sum)
  
  # Add 'X' (facility) identifier to the aggregated values
  aggregate_values$facility <- facilities_sf$X[i]
  
  # Return the aggregate values
  return(aggregate_values)
}

# Use lapply to apply the function to each row/facility in facilities_sf dataset
aggregate_results <- lapply(1:nrow(facilities_sf), function(n) {
  facility_run(i=n, p=2) # nests of loops of functions for p values
}) 

# Combine all results into a single dataframe
idw_results_1yr <- do.call(rbind, aggregate_results)

glimpse(idw_results_1yr)


###### 6) Reformat the idw_results dataframe for regression analysis

# Reshape data to wide format
idw_results_regress_4wk <- idw_results_4wk %>% pivot_wider(names_from = event_type, values_from = weights, values_fill = 0) %>% select(-no_event)# four-week
idw_results_regress_1yr <- idw_results_1yr %>% pivot_wider(names_from = event_type, values_from = weights, values_fill = 0) %>% select(-no_event) # one-year

# Merge health facility disruption status information from 'facilities'
idw_results_regress_4wk <- left_join(idw_results_regress_4wk, facilities, by = c("facility" = "X")) %>%
  select(-X.1, -longitude, -latitude, -date)  

idw_results_regress_1yr <- left_join(idw_results_regress_1yr, facilities, by = c("facility" = "X")) %>%
  select(-X.1, -longitude, -latitude, -date)  

# Create one 'disruption' column for 4 week data
idw_results_regress_4wk <- idw_results_regress_4wk %>% mutate(disruption = case_when(
  building_condition == 'Partially/fully damaged' & functionality == 'Fully functioning' ~ 'Physical',
  building_condition == 'Not damaged' & functionality == 'Partially /not functioning' ~ 'Functional',
  building_condition == 'Partially/fully damaged' & functionality == 'Partially /not functioning' ~ 'Physical & Functional',
  building_condition == 'Not damaged' & functionality == 'Fully functioning' ~ 'No disruptions'))

# Create one 'disruption' column for 1 year
idw_results_regress_1yr <- idw_results_regress_1yr %>% mutate(disruption = case_when(
  building_condition == 'Partially/fully damaged' & functionality == 'Fully functioning' ~ 'Physical',
  building_condition == 'Not damaged' & functionality == 'Partially /not functioning' ~ 'Functional',
  building_condition == 'Partially/fully damaged' & functionality == 'Partially /not functioning' ~ 'Physical & Functional',
  building_condition == 'Not damaged' & functionality == 'Fully functioning' ~ 'No disruptions'))

###### 6) Run regression models to assess associations between conflict event types and functionality status. 
  # Fit multinomial logistic regression model
  model_4wk <- multinom(disruption ~ Battles + Explosions + Protests + Violence against civilians + Riots, data = idw_results_regress_4wk)
  # Print model summary
  summary(model_4wk)
  
  # Calculate p-values using the Wald Test
  z <- summary(model_4wk)$coefficients / summary(model_4wk)$standard.errors
  p <- 2 * (1 - pnorm(abs(z)))
  p
  

  # Fit multinomial logistic regression model
  model_1yr <- multinom(disruption ~ battles + explosions + protests + v_civilians + riots, data = idw_results_regress_1yr)
  # Print model summary
  summary(model_1yr)
    
  # Calculate p-values using the Wald Test
  z <- summary(model_1yr)$coefficients / summary(model_1yr)$standard.errors
  p <- 2 * (1 - pnorm(abs(z)))
  p