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

smoother <- function(x,y){
  smooth.spline(x = x,y = y)$y
}

# Manually adding distinct colored areas for different waves
wave_colors <- c("#FF9999", "#99CC99", "#9999FF", "#FFFF66", "#FFCC99", "#CC99CC") # Example colors
wave_names <- c("Initial", "Alpha", "Delta", "Omicron")
wave_ranges <- list(
  c(as.Date("2020-03-01"), as.Date("2020-11-01")),
  c(as.Date("2020-11-01"), as.Date("2021-07-01")),
  c(as.Date("2021-07-01"), as.Date("2022-01-01")),
  c(as.Date("2022-01-01"), as.Date("2023-01-01"))
)


load(file = "USA_monthly.rda")
USA_monthly <- left_join(USA_monthly, state_name_to_code,
                              by = c("state" = "full_name"))
names(USA_monthly)[3] <- "State_full"
names(USA_monthly)[7] <- "state"

load(file = "USA_monthly_result.rda")
model_result_all <- left_join(model_result_all, state_name_to_code,
                              by = c("state" = "full_name"))
names(model_result_all)[8] <- "State_full"
names(model_result_all)[11] <- "state"



### all groups:
selected_states <- unique(model_result_all$State_full)
selected_ages <- unique(model_result_all$age)
selected_sexes <- unique(model_result_all$sex)

for (the_state in selected_states) {
  for (the_age in selected_ages) {
    for(the_sex in selected_sexes){
      summary_p_score <- NULL
      if(!file.exists(paste0("fitted_model/", the_state, "_age_", the_age, "_sex_", the_sex, ".rda"))){
        warning(paste0("fitted_model/", the_state, "_age_", the_age, "_", the_sex, ".rda does not exist."))
        next
      }
      load(file = paste0("fitted_model/", the_state, "_age_", the_age, "_sex_", the_sex, ".rda"))
      full_data <- USA_monthly %>% filter(age == the_age, State_full == the_state, sex == the_sex)
      summary_p_score_new <- data.frame(time = model_pred$summary$time)
      p_score_samps <- (full_data$Deaths - model_pred$samples)/(model_pred$samples + .Machine$double.eps)
      summary_p_score_new$med <- p_score_samps %>% apply(MARGIN = 1, quantile, p = 0.5)
      summary_p_score_new$upper <- p_score_samps %>% apply(MARGIN = 1, quantile, p = 0.975)
      summary_p_score_new$lower <- p_score_samps %>% apply(MARGIN = 1, quantile, p = 0.025)
      summary_p_score_new$age <- the_age
      summary_p_score <- rbind(summary_p_score, summary_p_score_new)

      plot <- ggplot(summary_p_score, aes(x = as.Date(time))) +
        geom_line(aes(y = med)) +  # Line for the median values
        geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +  # Shaded region for lower and upper values
        coord_cartesian(ylim = c(-1,2)) +  # Set y limits
        xlab("Time") + ylab("P-Score") +  # Axis labels
        ggtitle(paste0("P-Score for ", the_state, ", ", the_age, ", ", the_sex)) +  # Title
        theme_minimal() +  # Use a minimal theme
        theme(legend.position = "top", # Legend at the top
              axis.text = element_text(size = 14),  # Increase axis text size
              axis.title = element_text(size = 16)) +
        scale_x_date(breaks = seq(as.Date("2020-01-01"), to = as.Date("2023-01-01"), by = "6 months"),
                     labels = custom_date_format,
                     limits = c(as.Date("2020-01-01"), as.Date("2023-01-01"))) # Custom x-axis breaks and labels

      for(i in seq_along(wave_ranges)){
        plot <- plot +
          geom_vline(xintercept = wave_ranges[[i]][1], linetype = "dashed", color = wave_colors[i], linewidth = 0.5) +
          geom_vline(xintercept = wave_ranges[[i]][2], linetype = "dashed", color = wave_colors[i], linewidth = 0.5) +
          annotate("text", x = mean(wave_ranges[[i]]), y = -0.8, label = wave_names[i], size = 3, angle = 45, color = wave_colors[i])
      }
      plot
      ggsave(file = paste0("figure/individual_pscore_series/", the_state, "_", the_age, "_", the_sex, ".pdf"), width = 10, height = 6)
    }
  }
}


