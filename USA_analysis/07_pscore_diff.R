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


wave_names <- c("Initial", "Alpha", "Delta", "Omicron")
wave_ranges <- list(
  c(as.Date("2020-03-01"), as.Date("2020-11-01")),
  c(as.Date("2020-11-01"), as.Date("2021-07-01")),
  c(as.Date("2021-07-01"), as.Date("2022-01-01")),
  c(as.Date("2022-01-01"), as.Date("2023-10-01"))
)


load(file = "USA_monthly.rda")
USA_monthly <- left_join(USA_monthly, state_name_to_code,
                         by = c("state" = "full_name"))
names(USA_monthly)[3] <- "State_full"
names(USA_monthly)[4] <- "Age"
names(USA_monthly)[7] <- "State"

USA_monthly <- USA_monthly %>% filter(Age %in% c("0-44", "45-64", "65-84"))

USA_monthly_pooled <- USA_monthly %>% group_by(State, State_full, sex, year, date) %>%
  summarise(Deaths = sum(Deaths), Age = "all")

USA_monthly <- rbind(USA_monthly, USA_monthly_pooled)

## functions
compute_pscore_diff <- function(state, age){
  # check if the following exists:
  if((!file.exists(paste0("fitted_model/", state, "_age_", age, "_sex_", "F",".rda"))) & (age != "all")){
    stop(paste0("No corresponding female result! ", state))
  }
  if((!file.exists(paste0("fitted_model/", state, "_age_", age, "_sex_", "M",".rda"))) & (age != "all")){
    stop(paste0("No corresponding male result! ", state))
  }

  if(age != "all"){
  # Predicted counts in male/female
  load(file = paste0("fitted_model/", state, "_age_", age, "_sex_", "F",".rda"))
  count_f <- model_pred
  load(file = paste0("fitted_model/", state, "_age_", age, "_sex_", "M",".rda"))
  count_m <- model_pred
  common_years_dat <- inner_join(count_f$summary, count_m$summary, by = "time")
  common_years <- common_years_dat$time[common_years_dat$time >= "2020-01-01"]
  which_male <- which(count_m$summary$time %in% common_years)
  which_female <- which(count_f$summary$time %in% common_years)
  count_f <- count_f$samples[which_female, ]
  count_m <- count_m$samples[which_male, ]
  }
  else if(age == "all"){
    # Predicted counts
    load(file = paste0("fitted_model/", state, "_age_", "0-44","_sex_", "F",".rda"))
    count_f_0_44 <- model_pred
    load(file = paste0("fitted_model/", state, "_age_", "0-44","_sex_", "M",".rda"))
    count_m_0_44 <- model_pred
    load(file = paste0("fitted_model/", state, "_age_", "45-64", "_sex_", "F",".rda"))
    count_f_45_64 <- model_pred
    load(file = paste0("fitted_model/", state, "_age_", "45-64", "_sex_", "M",".rda"))
    count_m_45_64 <- model_pred
    load(file = paste0("fitted_model/", state, "_age_", "65-84", "_sex_", "F",".rda"))
    count_f_65_84 <- model_pred
    load(file = paste0("fitted_model/", state, "_age_", "65-84", "_sex_", "M",".rda"))
    count_m_65_84 <- model_pred
    common_years_dat_0_44 <- inner_join(count_f_0_44$summary, count_m_0_44$summary, by = "time")
    common_years_dat_45_64 <- inner_join(count_f_45_64$summary, count_m_45_64$summary, by = "time")
    common_years_dat_65_84 <- inner_join(count_f_65_84$summary, count_m_65_84$summary, by = "time")
    common_years_dat <- inner_join(inner_join(common_years_dat_0_44, common_years_dat_45_64, by = "time"), common_years_dat_65_84, by = "time")
    common_years <- common_years_dat$time[common_years_dat$time >= "2020-01-01"]
    which_female_0_44 <- which(count_f_0_44$summary$time %in% common_years)
    which_male_0_44 <- which(count_m_0_44$summary$time %in% common_years)
    which_female_45_64 <- which(count_f_45_64$summary$time %in% common_years)
    which_male_45_64 <- which(count_m_45_64$summary$time %in% common_years)
    which_female_65_84 <- which(count_f_65_84$summary$time %in% common_years)
    which_male_65_84 <- which(count_m_65_84$summary$time %in% common_years)
    ## sum all the count for each sex
    count_f <- count_f_0_44$samples[which_female_0_44, ] + count_f_45_64$samples[which_female_45_64, ] + count_f_65_84$samples[which_female_65_84, ]
    count_m <- count_m_0_44$samples[which_male_0_44, ] + count_m_45_64$samples[which_male_45_64, ] + count_m_65_84$samples[which_male_65_84,]
  }

  # True counts in male/female
  USA_monthly_select_f <- USA_monthly %>% filter(State_full == state, Age == age, sex == "F")
  USA_monthly_select_m <- USA_monthly %>% filter(State_full == state, Age == age, sex == "M")
  true_count_f <- USA_monthly_select_f$Deaths[USA_monthly_select_f$date %in% common_years]
  true_count_m <- USA_monthly_select_m$Deaths[USA_monthly_select_m$date %in% common_years]

  # P-score in male/female
  pscore_f <- (true_count_f - count_f)  / count_f
  pscore_m <- (true_count_m - count_m)  / count_m
  pscore_diff <- pscore_f - pscore_m

  return(list(samples = pscore_diff, time = common_years))
}
obtained_aggregate_summary <- function(state_vec, age){
  result_list <- list()
  time_list <- list()
  # Compute all the required p score differences:
  for (state in state_vec) {
    result_list[[state]] <- compute_pscore_diff(state, age)
    time_list[[state]] <- data.frame(time = result_list[[state]]$time)
  }

  # Compute the intersection of all the times:
  common_time <- Reduce(inner_join, time_list)$time

  # Filter all the results:
  for (state in state_vec) {
    which_state <- which(result_list[[state]]$time %in% common_time)
    result_list[[state]]$samples <- result_list[[state]]$samples[which_state, ]
  }

  # Compute the weights of each state based on inverse of the variance:
  weights <- sapply(state_vec, function(state) {
    variances <- apply(result_list[[state]]$samples, 1, var)
    1/variances
  })

  # Normalize the weights for each row:
  weights <- weights / rowSums(weights)

  # Overall mean and var:
  mean_vec <- sapply(result_list, function(result) {
    rowMeans(result$samples)
  })
  var_vec <- sapply(result_list, function(result) {
    apply(result$samples, 1, var)
  })

  # Compute the weighted mean:
  weighted_mean <- rowSums(weights * mean_vec)
  weighted_var <- rowSums((weights^2) * var_vec)

  return(data.frame(time = common_time, mean = weighted_mean, var = weighted_var))
}
plot_aggregate_summary <- function(state_vec, age, ylim_set = c(-1,1)){
  state_vec_old <- state_vec
  for (i in 1:length(state_vec_old)) {
    state <- state_vec_old[i]
    # Remove elements if that element does not exist in both sex
    if((!file.exists(paste0("fitted_model/", state, "_age_", age, "_sex_", "F",".rda"))) & (age != "all")){
      warning(paste0("No corresponding female result in ", state))
      state_vec <- state_vec[state_vec != state]
    }
    if((!file.exists(paste0("fitted_model/", state, "_age_", age, "_sex_", "M",".rda"))) & (age != "all") ){
      warning(paste0("No corresponding male result in ", state))
      state_vec <- state_vec[state_vec != state]
    }
  }

  # call obtain_aggregate_result
  try <- obtained_aggregate_summary(state_vec, age)

  # Make plot
  plot(try$time, try$mean, type = "l", col = "black", lty = 1, lwd = 2,
       ylim = ylim_set,
       xlab = "", ylab = "P-score difference (F-M)",
       cex.lab = 1.5, cex.axis = 1.5, bty = "n")
  # lines(try$time, try$mean - 1.96 * sqrt(try$var), col = "black", lty = 2, lwd = 1)
  # lines(try$time, try$mean + 1.96 * sqrt(try$var), col = "black", lty = 2, lwd = 1)
  polygon(c(try$time, rev(try$time)), c(try$mean - 1.96 * sqrt(try$var), rev(try$mean + 1.96 * sqrt(try$var))),
          border = NA, col = rgb(0, 0, 0, 0.2))
  abline(h=0, lty="dashed", col="black", lwd=0.5)
  for(i in seq_along(wave_ranges)) {
    # Draw the start line
    # abline(v=wave_ranges[[i]][1], lty="dashed", col=wave_colors[i], lwd=1)
    abline(v=wave_ranges[[i]][1], lty="dashed", col = "black", lwd=1)
    # Draw the end line
    # abline(v=wave_ranges[[i]][2], lty="dashed", col=wave_colors[i], lwd=1)
    abline(v=wave_ranges[[i]][2], lty="dashed", col = "black", lwd=1)
    # Add annotation
    # text(x=mean(wave_ranges[[i]]), y=(max(ylim_set) - 0.1), labels=wave_names[i], srt=45, cex=1, col=wave_colors[i])
    text(x=mean(wave_ranges[[i]]), y=(max(ylim_set) - 0.1), labels=wave_names[i], srt=45, cex=1.5, col= "black")
  }
}


load(file = "us_state_vaccinations_select.rda")
all_files <- list.files("fitted_model/")
all_states <- unique(gsub("_.*", "", all_files))
plot_aggregate_summary(all_states[c(1, 3:7, 10:11, 13:19, 21:26, 29, 31:34, 36:39, 41, 43:45, 47:50)],
                       "0-44", ylim_set = c(-0.3,1)) # Low vaccination states
selected_states <- all_states[c(1, 3:7, 10:11, 13:19, 21:26, 29, 31:34, 36:39, 41, 43:45, 47:50)]
us_state_vaccinations_select <- us_state_vaccinations_select %>%
  filter(location %in% selected_states)
as.numeric(quantile(us_state_vaccinations_select$people_vaccinated_per_hundred, 0.15))
as.numeric(quantile(us_state_vaccinations_select$people_vaccinated_per_hundred, 0.85))
threshold_low <- 42
threshold_high <- 62
high_vac_state <- us_state_vaccinations_select %>% filter(people_vaccinated_per_hundred >= threshold_high) %>% arrange(-people_vaccinated_per_hundred) %>% pull(location)
low_vac_state <- us_state_vaccinations_select %>% filter(people_vaccinated_per_hundred <= threshold_low) %>% arrange(people_vaccinated_per_hundred) %>% pull(location)

png(filename = "p_score_diff_high_vac_0_44_US.png", width = 600, height = 400)
plot_aggregate_summary(high_vac_state, "0-44", ylim_set = c(-0.2,0.4))
dev.off()

png(filename = "p_score_diff_low_vac_0_44_US.png", width = 600, height = 400)
plot_aggregate_summary(low_vac_state, "0-44", ylim_set = c(-0.2,0.4))
dev.off()

png(filename = "p_score_diff_high_vac_45_64_US.png", width = 600, height = 400)
plot_aggregate_summary(high_vac_state, "45-64", ylim_set = c(-0.2,0.4))
dev.off()

png(filename = "p_score_diff_low_vac_45_64_US.png", width = 600, height = 400)
plot_aggregate_summary(low_vac_state, "45-64", ylim_set = c(-0.2,0.4))
dev.off()

png(filename = "p_score_diff_high_vac_65_84_US.png", width = 600, height = 400)
plot_aggregate_summary(high_vac_state, "65-84", ylim_set = c(-0.2,0.4))
dev.off()

png(filename = "p_score_diff_low_vac_65_84_US.png", width = 600, height = 400)
plot_aggregate_summary(low_vac_state, "65-84", ylim_set = c(-0.2,0.4))
dev.off()

png(filename = "p_score_diff_high_vac_all_US.png", width = 600, height = 400)
plot_aggregate_summary(high_vac_state, "all", ylim_set = c(-0.2,0.4))
dev.off()

png(filename = "p_score_diff_low_vac_all_US.png", width = 600, height = 400)
plot_aggregate_summary(low_vac_state, "all", ylim_set = c(-0.2,0.4))
dev.off()



plot_contrast_summary <- function(state_vec1, state_vec2, age, ylim_set = c(-1, 1), color1 = "blue", color2 = "red") {
  # Helper function to process each state vector
  process_state_vec <- function(state_vec, age) {
    state_vec_old <- state_vec
    for (i in 1:length(state_vec_old)) {
      state <- state_vec_old[i]
      # Remove elements if that element does not exist in both sexes
      if ((!file.exists(paste0("fitted_model/", state, "_age_", age, "_sex_", "F", ".rda"))) & (age != "all")) {
        warning(paste0("No corresponding female result in ", state))
        state_vec <- state_vec[state_vec != state]
      }
      if ((!file.exists(paste0("fitted_model/", state, "_age_", age, "_sex_", "M", ".rda"))) & (age != "all")) {
        warning(paste0("No corresponding male result in ", state))
        state_vec <- state_vec[state_vec != state]
      }
    }
    # Call obtain_aggregate_summary
    obtained_aggregate_summary(state_vec, age)
  }

  # Process both state vectors
  summary1 <- process_state_vec(state_vec1, age)
  summary2 <- process_state_vec(state_vec2, age)

  # Make plot
  plot(summary1$time, summary1$mean, type = "l", col = color1, lty = 1, lwd = 2,
       ylim = ylim_set,
       xlim = c(as.Date("2020-01-01"), as.Date("2023-05-01")),
       xlab = "", ylab = "P-score difference (F-M)",
       cex.lab = 1.5, cex.axis = 1.5, bty = "n")
  lines(summary1$time, (summary1$mean - 1.96 * sqrt(summary1$var)), col = color1, lty = "dotted", lwd = 0.5)
  lines(summary1$time, (summary1$mean + 1.96 * sqrt(summary1$var)), col = color1, lty = "dotted", lwd = 0.5)

  # polygon(c(summary1$time, rev(summary1$time)),
  #         c(summary1$mean - 1.96 * sqrt(summary1$var), rev(summary1$mean + 1.96 * sqrt(summary1$var))),
  #         border = NA, col = adjustcolor(color1, alpha.f = 0.2))

  lines(summary2$time, summary2$mean, type = "l", col = color2, lty = 1, lwd = 2)
  lines(summary2$time, (summary2$mean - 1.96 * sqrt(summary2$var)) ,col = color2, lty = "dotted", lwd = 0.5)
  lines(summary2$time, (summary2$mean + 1.96 * sqrt(summary2$var)) ,col = color2, lty = "dotted", lwd = 0.5)

  # polygon(c(summary2$time, rev(summary2$time)),
  #         c(summary2$mean - 1.96 * sqrt(summary2$var), rev(summary2$mean + 1.96 * sqrt(summary2$var))),
  #         border = NA, col = adjustcolor(color2, alpha.f = 0.2))

  abline(h = 0, lty = "dashed", col = "black", lwd = 0.5)
  for (i in seq_along(wave_ranges)) {
    abline(v = wave_ranges[[i]][1], lty = "dashed", col = "black", lwd = 1)
    abline(v = wave_ranges[[i]][2], lty = "dashed", col = "black", lwd = 1)
    text(x = mean(wave_ranges[[i]]), y = (max(ylim_set) - 0.1), labels = wave_names[i], srt = 45, cex = 1.5, col = "black")
  }

  # legend("topright", legend = c("State Set 1", "State Set 2"), col = c(color1, color2), lty = 1, lwd = 2, cex = 1.5)
}





png(filename = "p_score_diff_contrast_vac_0_44_US.png", width = 600, height = 400)
plot_contrast_summary(high_vac_state, low_vac_state, "0-44", ylim_set = c(-0.2, 0.4))
dev.off()

png(filename = "p_score_diff_contrast_vac_45_64_US.png", width = 600, height = 400)
plot_contrast_summary(high_vac_state, low_vac_state, "45-64", ylim_set = c(-0.2, 0.4))
dev.off()

png(filename = "p_score_diff_contrast_vac_65_84_US.png", width = 600, height = 400)
plot_contrast_summary(high_vac_state, low_vac_state, "65-84", ylim_set = c(-0.2, 0.4))
dev.off()

png(filename = "p_score_diff_contrast_vac_all_age_US.png", width = 600, height = 400)
plot_contrast_summary(high_vac_state, low_vac_state, "all", ylim_set = c(-0.2, 0.4))
dev.off()


# > high_vac_state
# [1] "Massachusetts" "Connecticut"   "New Mexico"    "New Jersey"    "Pennsylvania"
# [6] "Maryland"
# > low_vac_state
# [1] "Mississippi" "Louisiana"   "Idaho"       "Alabama"     "Tennessee"





