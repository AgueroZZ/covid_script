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

wave_levels = c("initial", "alpha", "delta", "omicron")


create_plot <- function(data, age_group, sex_group, provinces = NULL, TEXT = "") {
  if(is.null(provinces)){
    provinces <- unique(data$province)
  }
  # Filter data for the specified age group, sex group, and provinces
  df_age <- data %>% filter(age == age_group, sex == sex_group, province %in% provinces)

  # Calculate the interval width and set the wave factor
  df_age <- df_age %>%
    mutate(interval_width = p_upper - p_lower,
           point_size = 1 / interval_width) %>%
    mutate(wave = factor(wave, levels = wave_levels))

  # Create the plot
  p <- ggplot(df_age, aes(x = province, y = p_med, color = wave)) +
    geom_point(aes(size = point_size), position = position_dodge(width = 0.5)) +
    geom_errorbar(aes(ymin = p_lower, ymax = p_upper), position = position_dodge(width = 0.5), width = 0.2) +
    ylab("P-Score by waves") + xlab("Province") +
    theme_minimal() +
    ggtitle(TEXT) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 20), text = element_text(size = 20)) +
    geom_hline(yintercept = 0, col = "black") +
    coord_cartesian(ylim = c(-1, 1)) +
    scale_size_continuous(range = c(1, 5)) +
    guides(size = "none")

  # Return the plot object
  return(p)
}
create_plot(model_result_all, age_group = "0-44", sex_group = "Males", TEXT = "Males, 0-44")
create_plot(model_result_all, age_group = "0-44", sex_group = "Females", TEXT = "Females, 0-44")
# BC has MUCH higher mortality for young male than young female.

create_plot(model_result_all, age_group = "65-84", sex_group = "Males", TEXT = "Males, 65-84")
create_plot(model_result_all, age_group = "65-84", sex_group = "Females", TEXT = "Females, 65-84")
# Not much sex difference for old people.

create_plot(model_result_all, age_group = "45-64", sex_group = "Males", TEXT = "Males, 45-64")
create_plot(model_result_all, age_group = "45-64", sex_group = "Females", TEXT = "Females, 45-64")




