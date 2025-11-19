.libPaths(c("~/lib", .libPaths()))
require(tidyverse)
require(sGPfit)
require(aghq)
require(TMB)
require(Matrix)
require(lubridate)
require(OSplines)
require(ISOweek)

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
wave_levels = c("initial1", "initial2", "alpha", "delta", "omicron1", "omicron2")


# Map the full state names in your data to their two-letter codes
model_result_all <- left_join(model_result_all, state_name_to_code, 
                              by = c("state" = "full_name"))
names(model_result_all)[8] <- "State_full"
names(model_result_all)[10] <- "state"

state_order_west_to_east <- data.frame(
  state = c("AK", "HI", "WA", "OR", "CA", "NV", "ID", "MT", "WY", "UT", "CO", "AZ", "NM", 
            "ND", "SD", "NE", "KS", "OK", "TX", "MN", "IA", "MO", "AR", "LA", "WI", 
            "IL", "MS", "MI", "IN", "KY", "TN", "AL", "OH", "WV", "SC", "GA", 
            "FL", "NC", "VA", "PA", "NY", "VT", "NH", "ME", "MA", "RI", "CT", "NJ", "DE", "MD", "DC"),
  order_west_to_east = 1:51
)

# Merge this ordering into your main dataframe
model_result_all <- left_join(model_result_all, state_order_west_to_east, by = "state")



# Loop through age groups
for(age_group in unique(model_result_all$age)){
  # Filter data for the current age group
  df_age = model_result_all %>% filter(age == age_group)
  
  # Loop through variants
  for(wave_var in unique(model_result_all$wave)){
    # Filter data for the current variant
    df_wave = df_age %>% filter(wave == wave_var)
    
    # Calculate the interval width
    df_wave = df_wave %>% mutate(interval_width = p_upper - p_lower, point_size = 1 / interval_width) %>% 
      mutate(wave = factor(wave, levels = wave_levels))
    
    # Create the plot
    p = ggplot(df_wave, aes(x = reorder(state, order_west_to_east), y = p_med, color = state)) +
      geom_point(aes(size = point_size)) +
      geom_errorbar(aes(ymin = p_lower, ymax = p_upper), width = 0.2) +
      xlab("State") + ylab("Aggregated P-Score") +
      ggtitle(paste("Age Group:", age_group, ", Variant:", wave_var)) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 14),
            axis.text.y = element_text(size = 14),
            axis.title = element_text(size = 16),
            plot.title = element_text(size = 18)) +
      guides(color = "none", size = "none") +
      coord_cartesian(ylim = c(-2, 2))
    
    # Save the plot
    ggsave(paste0("figure/variant_by_age_state/p_med_by_state_", age_group, "_", wave_var, ".pdf"), p, width = 17, height = 11)
  }
}

  
  
  
# Loop through age groups
for(age_group in unique(model_result_all$age)){
  # Filter data for the current age group
  df_age = model_result_all %>% filter(age == age_group) %>% filter(state %in% c("TX", "CA", "FL", "NY", "PA", "IL", "GA", "OH", "MI", "NC"))
  # Calculate the interval width
  df_age = df_age %>% mutate(interval_width = p_upper - p_lower, point_size = 1 / interval_width) %>% 
    mutate(wave = factor(wave, levels = wave_levels))
  
  p = ggplot(df_age, aes(x = reorder(state, p_med), y = p_med, color = wave)) +
    geom_point(aes(size = point_size), position = position_dodge(width = 0.5)) +  # Plot the median values
    geom_errorbar(aes(ymin = p_lower, ymax = p_upper), 
                  position = position_dodge(width = 0.5), width = 0.2) +  # Plot the intervals
    ylab("P-Score by waves") + xlab("State") +
    theme_minimal() +  # Use a minimal theme
    ggtitle(paste("Age Group:", age_group)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          text = element_text(size=20)) +  # Rotate x-axis labels for better readability 
    geom_hline(yintercept = 0, col = "black") +
    coord_cartesian(ylim = c(-1, 1)) +  # Reverse the x-axis order
    scale_size_continuous(range = c(1, 5)) + # Set the range of point sizes
    guides(size = "none")
  
  # Save the plot
  ggsave(paste0("figure/variant_by_age_state/p_med_by_state_", age_group, ".pdf"), p, width = 17, height = 11)
}


# A function to plot error bar plots:
# Function to create error bar plot for specified states and age group
create_error_bar_plot <- function(states_vector, age_group) {

  # Assuming model_result_all is your main dataset
  wave_levels_ordered <- c("initial1", "initial2", "alpha", "delta", "omicron1", "omicron2")
  
  df_filtered <- model_result_all %>%
    filter(state %in% states_vector, age == age_group) %>%
    mutate(
      interval_width = p_upper - p_lower,
      point_size = 0.5 / interval_width
    ) %>%
    mutate(wave = factor(wave, levels = wave_levels_ordered))  # Set the order
  
  
  
  dodge_width <- position_dodge(width = 0.6)
  
  # Assuming 'wave' represents the different variants
  p = ggplot(df_filtered, aes(x = reorder(state, order_west_to_east), y = p_med, color = wave)) +
    geom_point(aes(size = point_size), position = dodge_width) +
    geom_errorbar(aes(ymin = p_lower, ymax = p_upper), width = 0.2, position = dodge_width) +
    # scale_color_manual(values = c("red", "blue", "green", "yellow", "purple")) + # Adjust colors as needed
    xlab("") + ylab("") +
    # ggtitle(paste("Age Group:", age_group)) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 14),
          axis.text.y = element_text(size = 14),
          axis.title = element_text(size = 16),
          plot.title = element_text(size = 18),
          panel.spacing.x = unit(2, "lines"),
          legend.text = element_text(size = 14),  # Increase the font size of the legend text
          legend.title = element_text(size = 16)) +
    coord_cartesian(ylim = c(0, 1)) +  # Set y-axis limits without removing data
    guides(size = "none") 
  
  return(p)
}

# Example usage of the function
states_vector <- c("CA", "TX", "FL", "NY", "PA", "IL", "OH", "GA", "NC", "MI")
age_group <- "20-39"
plot <- create_error_bar_plot(states_vector, age_group)
print(plot)
ggsave("figure/p_med_by_state_20-39.pdf", plot, width = 8, height = 6)

age_group <- "40-59"
plot <- create_error_bar_plot(states_vector, age_group)
print(plot)
ggsave("figure/p_med_by_state_40-59.pdf", plot, width = 8, height = 6)

age_group <- "60-79"
plot <- create_error_bar_plot(states_vector, age_group)
print(plot)
ggsave("figure/p_med_by_state_60-79.pdf", plot, width = 8, height = 6)


age_group <- "Over 80"
plot <- create_error_bar_plot(states_vector, age_group)
print(plot)
ggsave("figure/p_med_by_state_over_80.pdf", plot, width = 8, height = 6)



  

