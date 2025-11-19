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

wave_names <- c("Initial", "Alpha", "Delta", "Omicron")
wave_ranges <- list(
  c(as.Date("2020-03-01"), as.Date("2020-11-01")),
  c(as.Date("2020-11-01"), as.Date("2021-07-01")),
  c(as.Date("2021-07-01"), as.Date("2022-01-01")),
  c(as.Date("2022-01-01"), as.Date("2023-10-01"))
)
# Manually adding distinct colored areas for different waves
wave_colors <- c("#FF9999", "#99CC99", "#9999FF", "#FFFF66") # Example colors

load(file = "../result/result_all_age.rda")
data_path <- "../demo_r_mwk_20_linear.csv"
all_age <- read.csv(data_path)
all_age$date <- ISOweek2date(paste0(all_age$TIME_PERIOD, "-1"))
all_age$Year <- year(all_age$date)
names(all_age)[9] <- "Deaths"
names(all_age)[7] <- "country"
all_age <- all_age %>% select(date, country,Deaths, Year, sex, age) %>% filter(age %in% c("Y20-39", "Y40-59", "Y60-79"))
all_age_pooled <- all_age %>% group_by(date, country, Year, sex) %>%
  summarise(Deaths = sum(Deaths), age = "all_age")
all_age <- rbind(all_age, all_age_pooled)

## P-score difference between male and female:
compute_pscore_diff <- function(country, age){
  # check if the following exists:
  if((!file.exists(paste0("../fitted_model/", country, "_", age, "_F",".rda"))) & (age != "all_age")){
    stop(paste0("No corresponding female result! ", country))
    return(NA)
  }
  if((!file.exists(paste0("../fitted_model/", country, "_", age, "_M",".rda"))) & (age != "all_age")){
    stop(paste0("No corresponding male result! ", country))
    return(NA)
  }

  if(age != "all_age"){
    # Predicted counts in male/female
    load(file = paste0("../fitted_model/", country, "_", age, "_", "F",".rda"))
    count_f <- model_pred
    load(file = paste0("../fitted_model/", country, "_", age, "_", "M",".rda"))
    count_m <- model_pred
    common_years_dat <- inner_join(count_f$summary, count_m$summary, by = "time")
    common_years <- common_years_dat$time[common_years_dat$time >= "2020-01-01"]
    which_male <- which(count_m$summary$time %in% common_years)
    which_female <- which(count_f$summary$time %in% common_years)
    count_f <- count_f$samples[which_female, ]
    count_m <- count_m$samples[which_male, ]
  }
  else if(age == "all_age"){
    # Predicted counts
    # load(file = paste0("../fitted_model/", country, "_", "Y20-39", "_", "F",".rda"))
    # count_f_20_39 <- model_pred
    # load(file = paste0("../fitted_model/", country, "_", "Y20-39", "_", "M",".rda"))
    # count_m_20_39 <- model_pred
    load(file = paste0("../fitted_model/", country, "_", "Y40-59", "_", "F",".rda"))
    count_f_40_59 <- model_pred
    load(file = paste0("../fitted_model/", country, "_", "Y40-59", "_", "M",".rda"))
    count_m_40_59 <- model_pred
    load(file = paste0("../fitted_model/", country, "_", "Y60-79", "_", "F",".rda"))
    count_f_60_79 <- model_pred
    load(file = paste0("../fitted_model/", country, "_", "Y60-79", "_", "M",".rda"))
    count_m_60_79 <- model_pred
    # common_years_dat_20_39 <- inner_join(count_f_20_39$summary, count_m_20_39$summary, by = "time")
    common_years_dat_40_59 <- inner_join(count_f_40_59$summary, count_m_40_59$summary, by = "time")
    common_years_dat_60_79 <- inner_join(count_f_60_79$summary, count_m_60_79$summary, by = "time")
    # common_years_dat <- inner_join(inner_join(common_years_dat_20_39, common_years_dat_40_59, by = "time"), common_years_dat_60_79, by = "time")
    common_years_dat <- inner_join(common_years_dat_40_59, common_years_dat_60_79, by = "time")
    common_years <- common_years_dat$time[common_years_dat$time >= "2020-01-01"]
    # which_male_20_39 <- which(count_m_20_39$summary$time %in% common_years)
    # which_female_20_39 <- which(count_f_20_39$summary$time %in% common_years)
    which_male_40_59 <- which(count_m_40_59$summary$time %in% common_years)
    which_female_40_59 <- which(count_f_40_59$summary$time %in% common_years)
    which_male_60_79 <- which(count_m_60_79$summary$time %in% common_years)
    which_female_60_79 <- which(count_f_60_79$summary$time %in% common_years)
    # count_f <- count_f_20_39$samples[which_female_20_39, ] + count_f_40_59$samples[which_female_40_59, ] + count_f_60_79$samples[which_female_60_79, ]
    # count_m <- count_m_20_39$samples[which_male_20_39, ] + count_m_40_59$samples[which_male_40_59, ] + count_m_60_79$samples[which_male_60_79, ]
    count_f <- count_f_40_59$samples[which_female_40_59, ] + count_f_60_79$samples[which_female_60_79, ]
    count_m <- count_m_40_59$samples[which_male_40_59, ] + count_m_60_79$samples[which_male_60_79, ]
  }
  country_to_compare <- country
  age_to_compare <- age

  # True counts in male/female
  all_age_select_f <- all_age %>% filter(country == country_to_compare, age == age_to_compare, sex == "F")
  all_age_select_m <- all_age %>% filter(country == country_to_compare, age == age_to_compare, sex == "M")
  true_count_f <- all_age_select_f$Deaths[all_age_select_f$date %in% common_years]
  true_count_m <- all_age_select_m$Deaths[all_age_select_m$date %in% common_years]

  # P-score in male/female
  pscore_f <- (true_count_f - count_f)  / (count_f)
  pscore_m <- (true_count_m - count_m)  / (count_m)
  pscore_diff <- pscore_f - pscore_m

  return(list(samples = pscore_diff, time = common_years))
}
obtained_aggregate_summary <- function(country_vec, age){
  result_list <- list()
  time_list <- list()
  # Compute all the required p score differences:
  for (country in country_vec) {
    result_list[[country]] <- compute_pscore_diff(country, age)
    time_list[[country]] <- data.frame(time = result_list[[country]]$time)
  }

  # Compute the intersection of all the times:
  common_time <- suppressMessages(Reduce(inner_join, time_list)$time)

  # handle RS separately: so it only has weight in the original observation, not the fill in observation
  if("RS" %in% country_vec){

    time_list_except_RS <- time_list[country_vec[country_vec != "RS"]]
    common_time_except_RS <- suppressMessages(Reduce(inner_join, time_list_except_RS)$time)
    extra_time <- common_time_except_RS[!common_time_except_RS %in% common_time]

    # insert extra_time to the result of RS
    result_list_new_RS <- result_list[["RS"]]

    result_list_new_RS$time <- sort(c(result_list[["RS"]]$time, extra_time))
    # the indices of insertions
    insert_idx <- match(extra_time, result_list_new_RS$time)

    # Create a matrix of zeros with the same number of columns as the original samples
    new_samples <- matrix(0, nrow = length(extra_time), ncol = ncol(result_list_new_RS$samples))

    # Initialize a matrix to hold the combined samples
    combined_samples <- matrix(NA, nrow = length(result_list_new_RS$time), ncol = ncol(result_list_new_RS$samples))

    # Place the original samples in the combined matrix
    original_idx <- setdiff(1:nrow(combined_samples), insert_idx)
    combined_samples[original_idx, ] <- result_list_new_RS$samples
    combined_samples[insert_idx, ] <- new_samples

    result_list$RS <- list(samples = combined_samples, time = result_list_new_RS$time)

    for (country in country_vec) {
      which_country <- which(result_list[[country]]$time %in% common_time_except_RS)
      result_list[[country]]$samples <- result_list[[country]]$samples[which_country, ]
    }

    # Compute the weights of each country based on inverse of the variance:
    weights <- sapply(country_vec, function(country) {
      variances <- apply(result_list[[country]]$samples, 1, var)
      1/variances
    })

    # For RS, we only consider the original observation, so change Inf to zero
    weights[is.infinite(weights)] <- 0
    # Normalize the weights for each row:
    weights <- ifelse(is.na(weights)|is.nan(weights)|is.infinite(weights), 0, weights)

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
    return(data.frame(time = common_time_except_RS, mean = weighted_mean, var = weighted_var))

  }
  else{
    # Filter all the results:
    for (country in country_vec) {
      which_country <- which(result_list[[country]]$time %in% common_time)
      result_list[[country]]$samples <- result_list[[country]]$samples[which_country, ]
    }

    # Compute the weights of each country based on inverse of the variance:
    weights <- sapply(country_vec, function(country) {
      variances <- apply(result_list[[country]]$samples, 1, var)
      1/variances
    })

    # Normalize the weights for each row:
    weights <- ifelse(is.na(weights)|is.nan(weights)|is.infinite(weights), 0, weights)

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

}
plot_aggregate_summary <- function(country_vec, age, ylim_set = c(-0.3,1)){
  country_vec_old <- country_vec
  for (i in 1:length(country_vec_old)) {
    country <- country_vec_old[i]
    # Remove elements if that element does not exist in both sex
    if((!file.exists(paste0("../fitted_model/", country, "_", age, "_", "F",".rda"))) & (age != "all_age")){
      warning(paste0("No corresponding female result in ", country))
      country_vec <- country_vec[country_vec != country]
    }
    if((!file.exists(paste0("../fitted_model/", country, "_", age, "_", "M",".rda"))) & (age != "all_age")){
      warning(paste0("No corresponding male result in ", country))
      country_vec <- country_vec[country_vec != country]
    }
  }

  # call obtain_aggregate_result and suppress the printing
  try <- suppressMessages(obtained_aggregate_summary(country_vec, age))

  # Make plot
  plot(try$time, try$mean, type = "l", col = "black", lty = 1, lwd = 2,
       ylim = ylim_set, cex = 0.5,
       xlab = "", ylab = "P-score difference (F-M)",
       cex.lab = 1.5, cex.axis = 1.5, bty = "n")
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

### All countries:
all_fitted_mods <- list.files(path = "../fitted_model/")
## extract the first two characters
all_fitted_mods <- unique(gsub("_.*", "", all_fitted_mods))
length(all_fitted_mods)
# countries_exclude <- c("AD", "DE", "IS", "LI", "LU", "ME", "MT")
# countries_select <- all_fitted_mods[!all_fitted_mods %in% countries_exclude]
# plot_aggregate_summary(countries_select, "Y60-79")
countries_exclude <- c("AD","CY","DE","EE","IS","LI","LU","ME","MT","SI")
selected_countries <- all_fitted_mods[!all_fitted_mods %in% countries_exclude]
plot_aggregate_summary(selected_countries, "Y60-79")
plot_aggregate_summary(selected_countries, "Y40-59")

load(file = "vac_data_eu.rda")
vac_data_eu$iso2[50] <- "UK"
# plot_aggregate_summary(all_fitted_mods[c(2:8, 10:17, 19, 21, 23, 26, 27:34)], "Y60-79")
# selected_countries <- all_fitted_mods[c(2:8, 10:17, 19, 21, 23, 26, 27:34)]
vac_data_eu <- vac_data_eu %>% filter(iso2 %in% selected_countries)
threshold_high <- quantile(vac_data_eu$people_vaccinated_per_hundred, probs = c(0.7))
# 53.188
threshold_low <- quantile(vac_data_eu$people_vaccinated_per_hundred, probs = c(0.3))
# 41.232
high_vac_country <- vac_data_eu %>% filter(people_vaccinated_per_hundred >= threshold_high) %>% pull(iso2)
low_vac_country <- vac_data_eu %>% filter(people_vaccinated_per_hundred <= threshold_low) %>% pull(iso2)

png(filename = "p_score_diff_high_vac_60_79_EU.png", width = 600, height = 400)
plot_aggregate_summary(high_vac_country, "Y60-79", ylim_set = c(-0.2, 0.5))
dev.off()

png(filename = "p_score_diff_low_vac_60_79_EU.png", width = 600, height = 400)
plot_aggregate_summary(low_vac_country, "Y60-79", ylim_set = c(-0.2, 0.5))
dev.off()

png(filename = "p_score_diff_high_vac_40_59_EU.png", width = 600, height = 400)
plot_aggregate_summary(high_vac_country, "Y40-59", ylim_set = c(-0.2, 0.5))
dev.off()

png(filename = "p_score_diff_low_vac_40_59_EU.png", width = 600, height = 400)
plot_aggregate_summary(low_vac_country, "Y40-59", ylim_set = c(-0.2, 0.5))
dev.off()

png(filename = "p_score_diff_high_vac_all_age_EU.png", width = 600, height = 400)
plot_aggregate_summary(high_vac_country, "all_age", ylim_set = c(-0.2, 0.5))
dev.off()

png(filename = "p_score_diff_low_vac_all_age_EU.png", width = 600, height = 400)
plot_aggregate_summary(low_vac_country, "all_age", ylim_set = c(-0.2, 0.5))
dev.off()



plot_contrast_summary <- function(country_vec1, country_vec2, age, ylim_set = c(-0.3, 1), color1 = "blue", color2 = "red", include.legend = FALSE) {
  # Helper function to process each country vector
  process_country_vec <- function(country_vec, age) {
    country_vec_old <- country_vec
    for (i in 1:length(country_vec_old)) {
      country <- country_vec_old[i]
      # Remove elements if that element does not exist in both sex
      if ((!file.exists(paste0("../fitted_model/", country, "_", age, "_", "F", ".rda"))) & (age != "all_age")) {
        warning(paste0("No corresponding female result in ", country))
        country_vec <- country_vec[country_vec != country]
      }
      if ((!file.exists(paste0("../fitted_model/", country, "_", age, "_", "M", ".rda"))) & (age != "all_age")) {
        warning(paste0("No corresponding male result in ", country))
        country_vec <- country_vec[country_vec != country]
      }
    }
    # Call obtain_aggregate_summary and suppress the printing
    suppressMessages(obtained_aggregate_summary(country_vec, age))
  }

  # Process both country vectors
  summary1 <- process_country_vec(country_vec1, age)
  summary2 <- process_country_vec(country_vec2, age)

  # Make plot
  plot(summary1$time, summary1$mean, type = "l", col = color1, lty = 1, lwd = 2,
       ylim = ylim_set, cex = 0.5,
       xlab = "", ylab = "P-score difference (F-M)",
       cex.lab = 1.5, cex.axis = 1.5, bty = "n")
  polygon(c(summary1$time, rev(summary1$time)),
          c(summary1$mean - 1.96 * sqrt(summary1$var), rev(summary1$mean + 1.96 * sqrt(summary1$var))),
          border = NA, col = adjustcolor(color1, alpha.f = 0.2))

  lines(summary2$time, summary2$mean, type = "l", col = color2, lty = 1, lwd = 2)
  polygon(c(summary2$time, rev(summary2$time)),
          c(summary2$mean - 1.96 * sqrt(summary2$var), rev(summary2$mean + 1.96 * sqrt(summary2$var))),
          border = NA, col = adjustcolor(color2, alpha.f = 0.2))

  abline(h = 0, lty = "dashed", col = "black", lwd = 0.5)
  for (i in seq_along(wave_ranges)) {
    abline(v = wave_ranges[[i]][1], lty = "dashed", col = "black", lwd = 1)
    abline(v = wave_ranges[[i]][2], lty = "dashed", col = "black", lwd = 1)
    text(x = mean(wave_ranges[[i]]), y = (max(ylim_set) - 0.1), labels = wave_names[i], srt = 45, cex = 1.5, col = "black")
  }
  if (include.legend){
    legend(x = as.Date("2022-06-01"), y = 0.35, legend = c("high vac", "low vac"), col = c(color1, color2), lty = 1, lwd = 2, cex = 1.5, bty = "n")
  }
}
plot_contrast_summary_smooth <- function(country_vec1, country_vec2, age, ylim_set = c(-0.3, 1), color1 = "blue", color2 = "red", include.legend = FALSE, bandwidth = 14) {
  # Helper function to process each country vector
  process_country_vec <- function(country_vec, age) {
    country_vec_old <- country_vec
    for (i in 1:length(country_vec_old)) {
      country <- country_vec_old[i]
      # Remove elements if that element does not exist in both sex
      if ((!file.exists(paste0("../fitted_model/", country, "_", age, "_", "F", ".rda"))) & (age != "all_age")) {
        warning(paste0("No corresponding female result in ", country))
        country_vec <- country_vec[country_vec != country]
      }
      if ((!file.exists(paste0("../fitted_model/", country, "_", age, "_", "M", ".rda"))) & (age != "all_age")) {
        warning(paste0("No corresponding male result in ", country))
        country_vec <- country_vec[country_vec != country]
      }
    }
    # Call obtain_aggregate_summary and suppress the printing
    suppressMessages(obtained_aggregate_summary(country_vec, age))
  }

  # Process both country vectors
  summary1 <- process_country_vec(country_vec1, age)
  summary2 <- process_country_vec(country_vec2, age)

  # Smooth the summary
  summary1$t <- as.numeric(summary1$time)
  summary2$t <- as.numeric(summary2$time)

  ## Smoothing through moving average
  summary1$mean <- stats::ksmooth(x = summary1$t, y = summary1$mean, kernel = "box", bandwidth = bandwidth)$y
  summary1$var <- stats::ksmooth(x = summary1$t, y = summary1$var, kernel = "box", bandwidth = bandwidth)$y

  summary2$mean <- stats::ksmooth(x = summary2$t, y = summary2$mean, kernel = "box", bandwidth = bandwidth)$y
  summary2$var <- stats::ksmooth(x = summary2$t, y = summary2$var, kernel = "box", bandwidth = bandwidth)$y
  # summary1$mean <- smooth.spline(summary1$t, summary1$mean, spar = spar)$y
  # summary1$var <- smooth.spline(summary1$t, summary1$var, spar = spar)$y

  # summary2$mean <- smooth.spline(summary2$t, summary2$mean, spar = spar)$y
  # summary2$var <- smooth.spline(summary2$t, summary2$var, spar = spar)$y

  # Make plot
  plot(summary1$time, summary1$mean, type = "l", col = color1, lty = 1, lwd = 2,
       ylim = ylim_set, cex = 0.5,
       xlab = "", ylab = "P-score difference (F-M)",
       cex.lab = 1.5, cex.axis = 1.5, bty = "n")
  lines(summary1$time, (summary1$mean - 1.96 * sqrt(summary1$var)), lty = "dotted", col = color1, lwd = 0.5)
  lines(summary1$time, (summary1$mean + 1.96 * sqrt(summary1$var)), lty = "dotted", col = color1, lwd = 0.5)
  # polygon(c(summary1$time, rev(summary1$time)),
  #         c(summary1$mean - 1.96 * sqrt(summary1$var), rev(summary1$mean + 1.96 * sqrt(summary1$var))),
  #         border = NA, col = adjustcolor(color1, alpha.f = 0.2))

  lines(summary2$time, summary2$mean, type = "l", col = color2, lty = 1, lwd = 2)
  lines(summary2$time, (summary2$mean - 1.96 * sqrt(summary2$var)), lty = "dotted", col = color2, lwd = 0.5)
  lines(summary2$time, (summary2$mean + 1.96 * sqrt(summary2$var)), lty = "dotted", col = color2, lwd = 0.5)
  # polygon(c(summary2$time, rev(summary2$time)),
  #         c(summary2$mean - 1.96 * sqrt(summary2$var), rev(summary2$mean + 1.96 * sqrt(summary2$var))),
  #         border = NA, col = adjustcolor(color2, alpha.f = 0.2))

  abline(h = 0, lty = "dashed", col = "black", lwd = 0.5)
  for (i in seq_along(wave_ranges)) {
    abline(v = wave_ranges[[i]][1], lty = "dashed", col = "black", lwd = 1)
    abline(v = wave_ranges[[i]][2], lty = "dashed", col = "black", lwd = 1)
    text(x = mean(wave_ranges[[i]]), y = (max(ylim_set) - 0.1), labels = wave_names[i], srt = 45, cex = 1.5, col = "black")
  }
  if (include.legend){
    legend(x = as.Date("2022-06-01"), y = 0.35, legend = c("high vac", "low vac"), col = c(color1, color2), lty = 1, lwd = 2, cex = 1.5, bty = "n")
  }
}

# par(mfrow = c(2, 1))
# plot_contrast_summary_smooth(high_vac_country, low_vac_country, "all_age", ylim_set = c(-0.2, 0.5), include.legend = TRUE)
# plot_contrast_summary_smooth(high_vac_country, low_vac_country, "all_age", ylim_set = c(-0.2, 0.5), include.legend = TRUE)
# par(mfrow = c(1, 1))

png(filename = "p_score_diff_contrast_all_age_EU.png", width = 600, height = 400)
plot_contrast_summary_smooth(high_vac_country, low_vac_country, "all_age", ylim_set = c(-0.2, 0.5), include.legend = TRUE)
dev.off()

png(filename = "p_score_diff_contrast_60_79_EU.png", width = 600, height = 400)
plot_contrast_summary_smooth(high_vac_country, low_vac_country, "Y60-79", ylim_set = c(-0.2, 0.5))
dev.off()

png(filename = "p_score_diff_contrast_40_59_EU.png", width = 600, height = 400)
plot_contrast_summary_smooth(high_vac_country, low_vac_country, "Y40-59", ylim_set = c(-0.2, 0.5))
dev.off()


# > high_vac_country
# [1] "AT" "BE" "DK" "FI" "HU" "IT" "ES"

# > low_vac_country
# [1] "AM" "BG" "HR" "LV" "RO" "RS" "SK"



