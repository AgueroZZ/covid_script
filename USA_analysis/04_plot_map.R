.libPaths(c("~/lib", .libPaths()))
require(tidyverse)
require(sGPfit)
require(aghq)
require(TMB)
require(Matrix)
require(lubridate)
require(OSplines)
require(ISOweek)
require(mapmisc)
library(sf)
library(dplyr)
library(RColorBrewer)
library(mapmisc)

# Load the shapefile for US states (replace 'path_to_shapefile' with the actual path)
us_states <- st_read("cb_2018_us_state_500k/cb_2018_us_state_500k.shp")


# Create a mapping between full state names and two-letter codes
load(file = "USA_monthly_result.rda")
state_name_to_code <- data.frame(
  full_name = c("Alabama", "Alaska", "Arizona", "Arkansas", "California", 
                "Colorado", "Connecticut", "Delaware", "District of Columbia", 
                "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", 
                "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", 
                "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", 
                "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", 
                "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", 
                "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", 
                "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", 
                "Washington", "West Virginia", "Wisconsin", "Wyoming"),
  code = c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL", "GA", "HI", "ID", "IL", "IN", 
           "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", 
           "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", 
           "VT", "VA", "WA", "WV", "WI", "WY")
)


# Map the full state names in your data to their two-letter codes
model_result_all <- left_join(model_result_all, state_name_to_code, 
                              by = c("state" = "full_name"))
names(model_result_all)[8] <- "State_full"
names(model_result_all)[11] <- "state"


# Function to plot U.S. state maps stratified by age and wave, focusing only on contiguous US
plot_us_state_map <- function(result_age, age_group, sex_group) {
  
  # Exclude Alaska, Hawaii, and U.S. territories
  us_states_mainland <- us_states %>% filter(!(STUSPS %in% c("AK", "HI", "AS", "PR", "MP", "VI", "GU")))
  
  for (variant in c("initial", "alpha", "delta", "omicron")) {
    
    # Filter data for the specific wave and age group
    result_age_wave <- result_age %>% filter(wave == variant & age == age_group & sex == sex_group)
    
    # Join the shapefile data with the model results
    us_states_joined <- left_join(us_states_mainland, result_age_wave, by = c("STUSPS" = "state"))
    
    # Custom color scale and breaks
    custom_palette <- RColorBrewer::brewer.pal(9, 'RdYlGn')
    custom_palette <- rev(custom_palette)
    custom_breaks <- c(-2, 0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.5, 1, 2)
    custom_labels <- as.character(c(0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.5, 1, 2))
    
    theCol = mapmisc::colourScale(us_states_joined$p_med, breaks=custom_breaks, style='fixed', col = custom_palette)
    # theCol = mapmisc::colourScale(us_states_joined$p_med, style='fixed', col = custom_palette)
    
    # Plotting
    pdf(file = paste0("maps/US_map_", age_group, "_", sex_group, "_", variant, ".pdf"), width = 10, height = 10)
    
    plot(st_geometry(us_states_joined), col = theCol$plot)
    
    mapmisc::legendBreaks("topleft", col=theCol$col, breaks = custom_labels)
    # mapmisc::legendBreaks("topleft", col=theCol$col)
    
    
    text(st_coordinates(st_centroid(us_states_joined)), labels = us_states_joined$STUSPS)
    
    dev.off()
  }
}

plot_us_state_map(model_result_all, '0-44', "F")
plot_us_state_map(model_result_all, '0-44', "M")

plot_us_state_map(model_result_all, '45-64', "F")
plot_us_state_map(model_result_all, '45-64', "M")

plot_us_state_map(model_result_all, '65-84', "F")
plot_us_state_map(model_result_all, '65-84', "M")

plot_us_state_map(model_result_all, 'Over 85', "F")
plot_us_state_map(model_result_all, 'Over 85', "M")










