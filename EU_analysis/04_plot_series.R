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
  ifelse(y >= 0, y, 0)
}

data_direct <- "../demo_r_mwk_20_linear.csv"

# Manually adding distinct colored areas for different waves
wave_colors <- c("#FF9999", "#99CC99", "#9999FF", "#FFFF66", "#FFCC99", "#CC99CC") # Example colors
wave_names <- c("Initial", "Alpha", "Delta", "Omicron")
wave_ranges <- list(
  c(as.Date("2020-03-01"), as.Date("2020-11-01")),
  c(as.Date("2020-11-01"), as.Date("2021-07-01")),
  c(as.Date("2021-07-01"), as.Date("2022-01-01")),
  c(as.Date("2022-01-01"), as.Date("2023-01-01"))
)

#### Select the country with post-pandemic data:
all_data <- read.csv(data_direct)
all_data$date <- ISOweek2date(paste0(all_data$TIME_PERIOD, "-1"))
all_data$Year <- isoyear(all_data$date)
all_data$week <- isoweek(all_data$date)
selected_countries <- unique(all_data$geo)
selected_ages <- unique(all_data$age)
selected_sexes <- unique(all_data$sex)


### Make all series in a single plot:
combined_data <- data.frame()
combined_data_smooth <- data.frame()

# Assuming selected_countries is a vector of country codes
for (the_country in selected_countries) {
  for (the_age in selected_ages) {
    for (the_sex in selected_sexes) {
      if(!file.exists(paste0("../", "fitted_model/", the_country, "_", the_age, "_", the_sex,".rda"))){
        next
      }
      # Load the model data for each country
      load(file = paste0("../", "fitted_model/", the_country, "_", the_age, "_", the_sex,".rda"))
      # Preprocess the data
      full_data <- all_data %>% filter(geo == the_country, sex == the_sex, age == the_age)
      if (the_country == "ES") {
        # Remove the suspicious observation
        full_data <- full_data %>% filter(date != as.Date("2020-12-28"))
      }
      data <- full_data %>% filter(Year < 2020)
      x <- (as.numeric(data$date) - min(as.numeric(data$date))) / 365
      data$x <- x
      x_full <- (as.numeric(full_data$date) - min(as.numeric(full_data$date))) / 365
      full_data$x <- x_full

      # Calculate the P-Score and its bounds
      summary_p_score <- data.frame(time = model_pred$summary$time)
      p_score_samps <- (full_data$OBS_VALUE - model_pred$samples) / (model_pred$samples + .Machine$double.eps)
      summary_p_score$med <- apply(p_score_samps, 1, quantile, p = 0.5)
      summary_p_score$upper <- apply(p_score_samps, 1, quantile, p = 0.975)
      summary_p_score$lower <- apply(p_score_samps, 1, quantile, p = 0.025)

      # Add country identifier to the data
      summary_p_score$country <- the_country
      summary_p_score$age <- the_age
      summary_p_score$sex <- the_sex

      # Append to the combined data frame
      combined_data <- rbind(combined_data, summary_p_score)
    }
  }
}

save(combined_data, file = "../result/combined_data_pseries.rda")

# Smooth the data
combined_data_smooth <- combined_data %>% group_by(age, country, sex) %>% mutate(med = smoother(y = med, x = time),
                                                                                  lower = smoother(y = lower, x = time),
                                                                                  upper = smoother(y = upper, x = time))


# Function to create the plot
create_plot <- function(data, age_group, sex_group, countries, title_suffix) {
  if(is.null(countries)){
    countries <- unique(data$country)
  }
  filtered_data <- data %>% filter(age == age_group, country %in% countries, sex == sex_group)

  plot <- ggplot(filtered_data, aes(x = as.Date(time), y = med)) +
    geom_line() +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1, linestyle = "dashed") +
    xlab("Time") + ylab("P-Score") +
    ggtitle(paste0(title_suffix)) +
    theme_minimal() +
    theme(legend.position = "none",
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

# create_plot(data = combined_data, age_group = "Y20-39", "F", countries = c("FR", "ES", "IT", "BE", "NL", "PT"), "Western, Female, 20-39")
# create_plot(data = combined_data, age_group = "Y20-39", "M", countries = c("FR", "ES", "IT", "BE", "NL", "PT"), "Western, Male, 20-39")
# create_plot(data = combined_data, age_group = "Y_GE80", "F", countries = c("FR", "ES", "IT", "BE", "NL", "PT"), "Western, Female, >= 80")
# create_plot(data = combined_data, age_group = "Y_GE80", "M", countries = c("FR", "ES", "IT", "BE", "NL", "PT"), "Western, Male, >= 80")
# ## For ES, we don't have sex-stratified data after 2022 for age-group 60-79.
#
# ### Compare young female between W and E
# create_plot(data = combined_data_smooth, age_group = "Y20-39", "F", countries = c("FR", "ES", "IT", "BE", "NL", "PT"), "Western, Female, 20-39")
# create_plot(data = combined_data_smooth, age_group = "Y20-39", "F", countries = c("RS", "RO", "BG", "SK", "PL", "HU"), "Eastern, Female, 20-39")
# # For young female, both W and E countries do not suffer much from the initial wave. W countries also didnt suffer from later waves, but
# # E countries suffer much more from alpha, delta, and omicron.
#
# ### Compare young male between W and E
# create_plot(data = combined_data_smooth, age_group = "Y20-39", "M", countries = c("FR", "ES", "IT", "BE", "NL", "PT"), "Western, Male, 20-39")
# create_plot(data = combined_data_smooth, age_group = "Y20-39", "M", countries = c("RS", "RO", "BG", "SK", "PL", "HU"), "Eastern, Male, 20-39")
# # Conclusion for young males is the same for young female.
#
# ### Compare old female between W and E
# create_plot(data = combined_data_smooth, age_group = "Y_GE80", "F", countries = c("FR", "ES", "IT", "BE", "NL", "PT"), "Western, Female, >= 80")
# create_plot(data = combined_data_smooth, age_group = "Y_GE80", "F", countries = c("RS", "RO", "BG", "SK", "PL", "HU"), "Eastern, Female, >= 80")
# # For old females, the first initial wave hits badly in W, but not in E. In E, mortality is mostly from alpha and delta waves.
#
# ### Compare old male between W and E
# create_plot(data = combined_data_smooth, age_group = "Y_GE80", "M", countries = c("FR", "ES", "IT", "BE", "NL", "PT"), "Western, Male, >= 80")
# create_plot(data = combined_data_smooth, age_group = "Y_GE80", "M", countries = c("RS", "RO", "BG", "SK", "PL", "HU"), "Eastern, Male, >= 80")
# # Conclusion mostly the same for old males.




## Produce time series for each group
all_countries <- unique(combined_data$country)
all_ages <- unique(combined_data$age)
all_sexes <- unique(combined_data$sex)

for (the_country in all_countries) {
  for (the_age in all_ages) {
    for (the_sex in all_sexes) {
      if(nrow(combined_data %>% filter(country == the_country, age == the_age, sex == the_sex, time > "2020-03-01")) == 0){
        next
      }
      fig <- create_plot(data = combined_data, age_group = the_age, sex_group = the_sex, countries = the_country, title_suffix = paste(the_country,the_sex, the_age))
      ggsave(paste0("../figure/individual_pscore_series/", the_country, "_", the_age, "_", the_sex, ".pdf"), plot = fig, width = 10, height = 6)
    }
  }
}





