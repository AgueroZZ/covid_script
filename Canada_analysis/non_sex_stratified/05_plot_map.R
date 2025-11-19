# Load required libraries
.libPaths(c("~/lib", .libPaths()))
require(tidyverse)
require(sf)
require(RColorBrewer)
require(mapmisc)

# Load the shapefile for Canadian provinces (replace 'path_to_shapefile' with the actual path)
canada_provinces <- st_read("lpr_000b21a_e/lpr_000b21a_e.shp")
canada_provinces <- st_simplify(canada_provinces, dTolerance = 1000)

province_name_to_code <- data.frame(
  full_name = c("Newfoundland and Labrador", "Prince Edward Island", "Nova Scotia", 
                "New Brunswick", "Quebec", "Ontario", "Manitoba", "Saskatchewan", 
                "Alberta", "British Columbia", "Yukon", "Northwest Territories", "Nunavut"),
  code = c("NL", "PE", "NS", "NB", "QC", "ON", "MB", "SK", "AB", "BC", "YT", "NT", "NU")
)

# Load your Canada model results
load(file = "Canada_weekly_result.rda")
model_result_all <- left_join(model_result_all, province_name_to_code, by = c("province" = "code"))


# Function to plot Canadian province maps stratified by age and wave
plot_canada_province_map <- function(result, age_group) {
  
  for (variant in c("initial1", "initial2", "alpha", "delta", "omicron1", "omicron2")) {
    
    # Filter data for the specific wave and age group
    result_age_wave <- result %>% filter(wave == variant & age == age_group)
    
    # Join the shapefile data with the model results
    canada_provinces_joined <- left_join(canada_provinces, result_age_wave, by = c("PRENAME" = "full_name"))
    
    # Custom color scale and breaks
    custom_palette <- RColorBrewer::brewer.pal(9, 'RdYlGn')
    custom_palette <- rev(custom_palette)
    custom_breaks <- c(-2, 0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.5, 1, 2)
    custom_labels <- as.character(c(0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.5, 1, 2))
    
    theCol = mapmisc::colourScale(canada_provinces_joined$p_med, breaks=custom_breaks, style='fixed', col = custom_palette)
    
    # Plotting
    pdf(file = paste0("maps/Canada_map_", age_group, "_", variant, ".pdf"), width = 10, height = 10)
    
    plot(st_geometry(canada_provinces_joined), col = theCol$plot)
    mapmisc::legendBreaks("topleft", col=theCol$col, breaks = custom_labels)
    
    text(st_coordinates(st_centroid(canada_provinces_joined)), labels = canada_provinces_joined$province)
    
    dev.off()
  }
}

plot_canada_province_map(model_result_all, '0-44')
plot_canada_province_map(model_result_all, '45-64')
plot_canada_province_map(model_result_all, '65-84')
plot_canada_province_map(model_result_all, 'over 85')
