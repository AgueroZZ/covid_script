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

wave_names <- c("Initial", "Alpha", "Delta", "Omicron")
wave_ranges <- list(
  c(as.Date("2020-03-01"), as.Date("2020-11-01")),
  c(as.Date("2020-11-01"), as.Date("2021-07-01")),
  c(as.Date("2021-07-01"), as.Date("2022-01-01")),
  c(as.Date("2022-01-01"), as.Date("2023-10-01"))
)


load(file = "final_data.rda")

final_data <- final_data %>% filter(age %in% c("0-44", "45-64", "65-84"))

final_data_pooled <- final_data %>% group_by(date, province, Sex) %>%
  summarise(death = sum(death), age = "all")

final_data_weekly <- rbind(final_data, final_data_pooled)
final_data_weekly <- rename(final_data_weekly, c("Deaths" = "death"))
final_data_weekly <- rename(final_data_weekly, c("Age" = "age"))
final_data_weekly <- rename(final_data_weekly, c("sex" = "Sex"))


compute_pscore_diff <- function(state, age){
  # check if the following exists:
  if((!file.exists(paste0("fitted_model/", state, "_age_", age, "_sex_", "Females",".rda"))) & (age != "all")){
    stop(paste0("No corresponding female result! ", state))
  }
  if((!file.exists(paste0("fitted_model/", state, "_age_", age, "_sex_", "Males",".rda"))) & (age != "all")){
    stop(paste0("No corresponding male result! ", state))
  }

  if(age != "all"){
    # Predicted counts in male/female
    load(file = paste0("fitted_model/", state, "_age_", age, "_sex_", "Females",".rda"))
    count_f <- model_pred
    load(file = paste0("fitted_model/", state, "_age_", age, "_sex_", "Males",".rda"))
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
    load(file = paste0("fitted_model/", state, "_age_", "0-44","_sex_", "Females",".rda"))
    count_f_0_44 <- model_pred
    load(file = paste0("fitted_model/", state, "_age_", "0-44","_sex_", "Males",".rda"))
    count_m_0_44 <- model_pred
    load(file = paste0("fitted_model/", state, "_age_", "45-64", "_sex_", "Females",".rda"))
    count_f_45_64 <- model_pred
    load(file = paste0("fitted_model/", state, "_age_", "45-64", "_sex_", "Males",".rda"))
    count_m_45_64 <- model_pred
    load(file = paste0("fitted_model/", state, "_age_", "65-84", "_sex_", "Females",".rda"))
    count_f_65_84 <- model_pred
    load(file = paste0("fitted_model/", state, "_age_", "65-84", "_sex_", "Males",".rda"))
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
  final_data_weekly_select_f <- final_data_weekly %>% filter(province == state, Age == age, sex == "Females")
  final_data_weekly_select_m <- final_data_weekly %>% filter(province == state, Age == age, sex == "Males")
  true_count_f <- final_data_weekly_select_f$Deaths[final_data_weekly_select_f$date %in% common_years]
  true_count_m <- final_data_weekly_select_m$Deaths[final_data_weekly_select_m$date %in% common_years]

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
    if((!file.exists(paste0("fitted_model/", state, "_age_", age, "_sex_", "Females",".rda"))) & (age != "all")){
      warning(paste0("No corresponding female result in ", state))
      state_vec <- state_vec[state_vec != state]
    }
    if((!file.exists(paste0("fitted_model/", state, "_age_", age, "_sex_", "Males",".rda"))) & (age != "all") ){
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

all_pro <- unique(final_data$province)

png(filename = "p_score_diff_selected_CA.png", width = 600, height = 400)
plot_aggregate_summary(c("AB", "QC", "ON", "BC", "SK"),
                       "all", ylim_set = c(-0.3,1))
dev.off()

# plot_aggregate_summary("AB", "all", ylim_set = c(-0.3,1))
# plot_aggregate_summary("QC", "all", ylim_set = c(-0.3,1))
# plot_aggregate_summary("ON", "all", ylim_set = c(-0.3,1))
# plot_aggregate_summary("BC", "all", ylim_set = c(-0.3,1))
# plot_aggregate_summary("MB", "all", ylim_set = c(-0.3,1))

