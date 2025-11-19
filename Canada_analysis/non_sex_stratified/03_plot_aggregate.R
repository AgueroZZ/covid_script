.libPaths(c("~/lib", .libPaths()))
require(tidyverse)
require(sGPfit)
require(aghq)
require(TMB)
require(Matrix)
require(lubridate)
require(OSplines)
require(ISOweek)

load(file = "Canada_weekly_result.rda")
load(file = "final_data.rda")
canada_weekly <- final_data
canada_weekly$date <- as.Date(canada_weekly$date)
canada_weekly$year <- year(canada_weekly$date)

wave_levels = c("initial1", "initial2", "alpha", "delta", "omicron1", "omicron2")


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
    p = ggplot(df_wave, aes(x = reorder(province, p_med), y = p_med, color = province)) +
      geom_point(aes(size = point_size)) +
      geom_errorbar(aes(ymin = p_lower, ymax = p_upper), width = 0.2) +
      xlab("Province") + ylab("Aggregated P-Score") +
      ggtitle(paste("Age Group:", age_group, ", Variant:", wave_var)) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 14),
            axis.text.y = element_text(size = 14),
            axis.title = element_text(size = 16),
            plot.title = element_text(size = 18)) +
      guides(color = "none", size = "none") +
      coord_cartesian(ylim = c(-2, 2))
    
    # Save the plot
    ggsave(paste0("figure/variant_by_age_province/p_med_by_province_", age_group, "_", wave_var, ".pdf"), p, width = 17, height = 11)
  }
}

  
# Loop through age groups
for(age_group in unique(model_result_all$age)){
  # Filter data for the current age group
  df_age = model_result_all %>% filter(age == age_group)
  
  # Calculate the interval width
  df_age = df_age %>% mutate(interval_width = p_upper - p_lower, point_size = 1 / interval_width) %>% 
    mutate(wave = factor(wave, levels = wave_levels))
  
  # Create the plot
  p = ggplot(df_age, aes(x = reorder(province, p_med), y = p_med, color = wave)) +
    geom_point(aes(size = point_size), position = position_dodge(width = 0.5)) +  # Plot the median values
    geom_errorbar(aes(ymin = p_lower, ymax = p_upper), 
                  position = position_dodge(width = 0.5), width = 0.2) +  # Plot the intervals
    ylab("P-Score by waves") + xlab("Province") +
    theme_minimal() +  # Use a minimal theme
    ggtitle(paste("Age Group:", age_group)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          text = element_text(size=20)) +  # Rotate x-axis labels for better readability 
    geom_hline(yintercept = 0, col = "black") +
    coord_cartesian(ylim = c(-1, 1)) +  # Reverse the x-axis order
    scale_size_continuous(range = c(1, 5)) + # Set the range of point sizes
    guides(size = "none")
  
  ggsave(paste0("figure/variant_by_age_province/p_med_by_province_", age_group, ".pdf"), p, width = 17, height = 11)
}




  
  
  
  

