.libPaths(c("~/lib", .libPaths()))
require(tidyverse)
require(sGPfit)
require(aghq)
require(TMB)
require(Matrix)
require(lubridate)
require(OSplines)
require(ISOweek)
source(file = "function.R")
custom_date_format <- function(x) {
  format(x, "%b %y")
}
smoother <- function(x,y){
  smooth.spline(x = x,y = y)$y
}
# Manually adding distinct colored areas for different waves
wave_colors <- c("#FF9999", "#99CC99", "#9999FF", "#FFFF66", "#FFCC99", "#CC99CC") # Example colors
wave_names <- c("Initial1", "Initial2", "Alpha", "Delta", "Omicron1", "Omicron2")
wave_ranges <- list(
  c(as.Date("2020-03-01"), as.Date("2020-07-01")),
  c(as.Date("2020-07-01"), as.Date("2020-11-01")),
  c(as.Date("2020-11-01"), as.Date("2021-07-01")),
  c(as.Date("2021-07-01"), as.Date("2022-01-01")),
  c(as.Date("2022-01-01"), as.Date("2022-04-01")),
  c(as.Date("2022-04-01"), as.Date("2023-01-01"))
)
# Load Canadian data
load(file = "Canada_weekly_result.rda")
load(file = "final_data.rda")
canada_weekly <- final_data
canada_weekly$date <- as.Date(canada_weekly$date)
canada_weekly$year <- year(canada_weekly$date)

# Unique provinces and age groups
selected_provinces <- unique(model_result_all$province)
selected_ages <- unique(model_result_all$age)

# Loop through each province
for (the_province in selected_provinces) {
  summary_p_score <- NULL
  for (the_age in selected_ages) {
    # Load the fitted model for the specific province and age group
    load(file = paste0("fitted_model/", the_province, "_age_", the_age, ".rda"))
    
    # Filter the full data for the current age group and province
    full_data <- canada_weekly %>% filter(age == the_age, province == the_province) %>% na.omit()
    data <- full_data %>% filter(year < 2020)
    
    # Calculate P-scores
    summary_p_score_new <- data.frame(time = model_pred$summary$time)
    p_score_samps <- (full_data$death - model_pred$samples) / (model_pred$samples + .Machine$double.eps)
    
    # Compute quantiles
    summary_p_score_new$med <- p_score_samps %>% apply(MARGIN = 1, quantile, p = 0.5)
    summary_p_score_new$upper <- p_score_samps %>% apply(MARGIN = 1, quantile, p = 0.975)
    summary_p_score_new$lower <- p_score_samps %>% apply(MARGIN = 1, quantile, p = 0.025)
    summary_p_score_new$age <- the_age
    
    # Combine with previous data
    summary_p_score <- rbind(summary_p_score, summary_p_score_new)
  }
  
  # Plotting
  summary_p_score$age <- factor(summary_p_score$age, levels = c("0-44", "45-64", "65-84", "over 85"))
  plot <- ggplot(summary_p_score, aes(x = as.Date(time))) +
    geom_line(aes(y = med), color = "blue") +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5, fill = "grey") +
    xlab("Time") + ylab("P-Score") +
    ggtitle(paste0(the_province, ", P-score by age group")) +
    theme_minimal() +
    theme(legend.position = "top", 
          axis.text = element_text(size = 14), 
          axis.title = element_text(size = 16)) +
    scale_x_date(breaks = seq(as.Date("2020-01-09"), to = as.Date("2023-01-01"), by = "6 months"),
                 labels = custom_date_format, 
                 limits = c(as.Date("2020-01-09"), as.Date("2023-01-01"))) +
    coord_cartesian(ylim = c(-1,2)) +
    facet_wrap(~ age, ncol = 1)
  
  for(i in seq_along(wave_ranges)){
    plot <- plot +
      geom_vline(xintercept = wave_ranges[[i]][1], linetype = "dashed", color = wave_colors[i], linewidth = 0.5) +
      geom_vline(xintercept = wave_ranges[[i]][2], linetype = "dashed", color = wave_colors[i], linewidth = 0.5) +
      annotate("text", x = mean(wave_ranges[[i]]), y = -0.8, label = wave_names[i], size = 3, angle = 45, color = wave_colors[i])
  }
  plot
  
  # Save the plot
  ggsave(file = paste0("figure/individual_province_p_score_series/rough/", the_province, ".pdf"), width = 15, height = 10)

  # Plotting
  summary_p_score_smooth <- summary_p_score %>% group_by(age) %>% mutate(med = smoother(y = med, x = time),
                                                                         lower = smoother(y = lower, x = time),
                                                                         upper = smoother(y = upper, x = time)) 
  
  plot <- ggplot(summary_p_score_smooth, aes(x = as.Date(time))) +
    geom_line(aes(y = med), color = "blue") +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5, fill = "grey") +
    xlab("Time") + ylab("P-Score") +
    ggtitle(paste0(the_province, ", P-score by age group")) +
    theme_minimal() +
    theme(legend.position = "top", 
          axis.text = element_text(size = 14), 
          axis.title = element_text(size = 16)) +
    scale_x_date(breaks = seq(as.Date("2020-01-09"), to = as.Date("2023-01-01"), by = "6 months"),
                 labels = custom_date_format, 
                 limits = c(as.Date("2020-01-09"), as.Date("2023-01-01"))) +
    coord_cartesian(ylim = c(-1,2)) +
    facet_wrap(~ age, ncol = 1)
  
  for(i in seq_along(wave_ranges)){
    plot <- plot +
      geom_vline(xintercept = wave_ranges[[i]][1], linetype = "dashed", color = wave_colors[i], linewidth = 0.5) +
      geom_vline(xintercept = wave_ranges[[i]][2], linetype = "dashed", color = wave_colors[i], linewidth = 0.5) +
      annotate("text", x = mean(wave_ranges[[i]]), y = -0.8, label = wave_names[i], size = 3, angle = 45, color = wave_colors[i])
  }
  plot
  
  # Save the plot
  ggsave(file = paste0("figure/individual_province_p_score_series/smooth/", the_province, "_smooth.pdf"), width = 15, height = 10)
  
}



### A combined figure for many provs
combined_data <- data.frame()
combined_data_smooth <- data.frame()

# Loop through each province and age group to combine data
for (the_province in selected_provinces) {
  for (the_age in selected_ages) {
    # Load the model data for the specific province and age group
    load(file = paste0("fitted_model/", the_province, "_age_", the_age, ".rda"))
    
    # Filter and preprocess data
    full_data <- canada_weekly %>% filter(age == the_age, province == the_province) %>% na.omit()
    summary_p_score_new <- data.frame(time = model_pred$summary$time)
    p_score_samps <- (full_data$death - model_pred$samples) / (model_pred$samples + .Machine$double.eps)
    
    # Calculate quantiles and add identifiers
    summary_p_score_new$med <- apply(p_score_samps, 1, quantile, p = 0.5)
    summary_p_score_new$upper <- apply(p_score_samps, 1, quantile, p = 0.975)
    summary_p_score_new$lower <- apply(p_score_samps, 1, quantile, p = 0.025)
    summary_p_score_new$age <- the_age
    summary_p_score_new$province <- the_province
    
    # Append to the combined data frame
    combined_data <- rbind(combined_data, summary_p_score_new)
  }
}

# Smooth the data
combined_data_smooth <- combined_data %>% group_by(age, province) %>% mutate(med = smoother(y = med, x = time),
                                                                             lower = smoother(y = lower, x = time),
                                                                             upper = smoother(y = upper, x = time))

# Function to create the plot
create_plot <- function(data, age_group, provinces, title_suffix) {
  if(is.null(provinces)){
    provinces <- unique(data$province)
  }
  filtered_data <- data %>% filter(age == age_group, province %in% provinces)
  
  plot <- ggplot(filtered_data, aes(x = as.Date(time), y = med, color = province)) +
    geom_line() +
    xlab("Time") + ylab("P-Score") +
    ggtitle(paste0(title_suffix, ", Age group: ", age_group)) +
    theme_minimal() +
    theme(legend.position = "top", 
          axis.text = element_text(size = 14), 
          axis.title = element_text(size = 16)) +
    scale_x_date(breaks = seq(as.Date("2020-01-09"), to = as.Date("2023-01-01"), by = "6 months"),
                 labels = custom_date_format, 
                 limits = c(as.Date("2020-01-09"), as.Date("2023-01-01"))) +
    coord_cartesian(ylim = c(-1,2))
  
  for(i in seq_along(wave_ranges)){
    plot <- plot +
      geom_vline(xintercept = wave_ranges[[i]][1], linetype = "dashed", color = wave_colors[i], linewidth = 0.5) +
      geom_vline(xintercept = wave_ranges[[i]][2], linetype = "dashed", color = wave_colors[i], linewidth = 0.5) +
      annotate("text", x = mean(wave_ranges[[i]]), y = -0.8, label = wave_names[i], size = 3, angle = 45, color = wave_colors[i])
  }
  return(plot)
}

# Example usage
plot_original <- create_plot(combined_data, "45-64", c("ON", "QC", "BC", "AB", "SK"), "P-Score by Province")
ggsave(file = "figure/selected_provinces_pseries_original.pdf", plot = plot_original, width = 15, height = 10)

plot_smooth <- create_plot(combined_data_smooth, "45-64", c("ON", "QC", "BC", "AB", "SK"), "P-Score by Province, Smoothed")
ggsave(file = "figure/selected_provinces_pseries_smooth.pdf", plot = plot_smooth, width = 15, height = 10)








