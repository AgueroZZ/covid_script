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


data_path <- "../stratified/demo_r_mwk_20_linear.csv"
all_age <- read.csv(data_path)
all_age$date <- ISOweek2date(paste0(all_age$TIME_PERIOD, "-1"))
all_age$Year <- year(all_age$date)
names(all_age)[9] <- "Deaths"
names(all_age)[7] <- "country"

all_age <- all_age %>% filter(sex == "T", age %in% c("Y40-59", "Y60-79")) %>% select(date, country,Deaths, Year, age)
all_age_pooled <- all_age %>% group_by(date, country, Year) %>%
  summarise(Deaths = sum(Deaths), age = "all_age")
all_age <- rbind(all_age, all_age_pooled)


### P-score:
compute_pscore <- function(country, age){
  # check if the following exists:
  if((!file.exists(paste0("../stratified/fitted_model/", country, "_", age,"_T.rda"))) & (age != "all_age") ){
    stop(paste0("No corresponding result! ", country))
    return(NA)
  }
  all_age_select <- all_age %>% filter(country == country)
  true_count <- all_age_select$Deaths
  if(age == "all_age"){
    # load(file = paste0("../stratified/fitted_model/", country, "_Y", "20-39", "_T.rda"))
    # count1 <- model_pred
    load(file = paste0("../stratified/fitted_model/", country, "_Y", "40-59", "_T.rda"))
    count2 <- model_pred
    load(file = paste0("../stratified/fitted_model/", country, "_Y", "60-79", "_T.rda"))
    count3 <- model_pred
    # common_years_dat <- inner_join(count1$summary, count2$summary, by = "time") %>%
    #   inner_join(count3$summary, by = "time")
    common_years_dat <- inner_join(count2$summary, count3$summary, by = "time")
    common_years <- common_years_dat$time[common_years_dat$time >= "2020-01-01"]
    # which_count1 <- which(count1$summary$time %in% common_years)
    # count1 <- count1$samples[which_count1, ]
    which_count2 <- which(count2$summary$time %in% common_years)
    count2 <- count2$samples[which_count2, ]
    which_count3 <- which(count3$summary$time %in% common_years)
    count3 <- count3$samples[which_count3, ]
    # count <- count1 + count2 + count3
    count <- count2 + count3
  }
  else{
  # Predicted counts in male/female
  load(file = paste0("../stratified/fitted_model/", country, "_", age, "_T",".rda"))
  count <- model_pred
  common_years_dat <- count$summary
  common_years <- common_years_dat$time[common_years_dat$time >= "2020-01-01"]
  which_count <- which(count$summary$time %in% common_years)
  count <- count$samples[which_count, ]
  }
  country_to_compare <- country
  age_to_compare <- age

  # True counts in male/female
  all_age_select <- all_age %>% filter(country == country_to_compare, age == age_to_compare)
  true_count <- all_age_select$Deaths[all_age_select$date %in% common_years]

  # P-score in male/female
  pscore <- (true_count - count)  / (count)

  return(list(samples = pscore, time = common_years))
}
obtained_aggregate_summary <- function(country_vec, age){
  result_list <- list()
  time_list <- list()
  # Compute all the required p score differences:
  for (country in country_vec) {
    result_list[[country]] <- compute_pscore(country, age)
    time_list[[country]] <- data.frame(time = result_list[[country]]$time)
  }

  # Compute the intersection of all the times:
  common_time <- Reduce(inner_join, time_list)$time

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
plot_aggregate_summary <- function(country_vec, age, ylim_set = c(-0.3,1)){
  country_vec_old <- country_vec
  for (i in 1:length(country_vec_old)) {
    country <- country_vec_old[i]
    # Remove elements if that element does not exist in both sex
    if((!file.exists(paste0("../stratified/fitted_model/", country, "_", age,"_T.rda"))) & (age != "all_age")){
      warning(paste0("No corresponding result in ", country))
      country_vec <- country_vec[country_vec != country]
    }
  }

  # call obtain_aggregate_result
  try <- obtained_aggregate_summary(country_vec, age)

  # Make plot
  plot(try$time, try$mean, type = "l", col = "black", lty = 1, lwd = 2,
       ylim = ylim_set, cex = 0.5,
       xlab = "", ylab = "P-score",
       xlim = c(as.Date("2020-01-01"), as.Date("2023-04-01")),
       cex.lab = 1.5, cex.axis = 1.5, bty = "n")
  polygon(c(try$time, rev(try$time)), c(try$mean - 1.96 * sqrt(try$var), rev(try$mean + 1.96 * sqrt(try$var))),
          border = NA, col = rgb(0, 0, 0, 0.2))
  abline(h=0, lty="dashed", col="black", lwd=0.5)
  for(i in seq_along(wave_ranges)) {
    abline(v=wave_ranges[[i]][1], lty="dashed", col = "black", lwd=1)
    abline(v=wave_ranges[[i]][2], lty="dashed", col = "black", lwd=1)
    text(x=mean(wave_ranges[[i]]), y=(max(ylim_set) - 0.1), labels=wave_names[i], srt=45, cex=1.5, col= "black")
  }
}

load(file = "vac_data_eu.rda")
vac_data_eu$iso2[50] <- "UK"

all_fitted_mods <- list.files(path = "../stratified/fitted_model/")
all_fitted_mods <- unique(gsub("_.*", "", all_fitted_mods))
countries_exclude <- c("AD","CY","DE","EE","IS","LI","LU","ME","MT","SI")
selected_countries <- all_fitted_mods[!all_fitted_mods %in% countries_exclude]
vac_data_eu <- vac_data_eu %>% filter(iso2 %in% selected_countries)

threshold_high <- quantile(vac_data_eu$people_vaccinated_per_hundred, probs = c(0.7))
threshold_low <- quantile(vac_data_eu$people_vaccinated_per_hundred, probs = c(0.3))

high_vac_country <- vac_data_eu %>% filter(people_vaccinated_per_hundred >= threshold_high) %>% pull(iso2)
low_vac_country <- vac_data_eu %>% filter(people_vaccinated_per_hundred <= threshold_low) %>% pull(iso2)

png("p_score_high_vac_all_age_EU.png", width = 600, height = 400)
plot_aggregate_summary(high_vac_country, "all_age", ylim_set = c(-0.3,1))
dev.off()

png("p_score_low_vac_all_age_EU.png", width = 600, height = 400)
plot_aggregate_summary(low_vac_country, "all_age", ylim_set = c(-0.3,1))
dev.off()

png("p_score_high_vac_60_79_EU.png", width = 600, height = 400)
plot_aggregate_summary(high_vac_country, "Y60-79", ylim_set = c(-0.3,1))
dev.off()

png("p_score_low_vac_60_79_EU.png", width = 600, height = 400)
plot_aggregate_summary(low_vac_country, "Y60-79", ylim_set = c(-0.3,1))
dev.off()

png("p_score_high_vac_40_59_EU.png", width = 600, height = 400)
plot_aggregate_summary(high_vac_country, "Y40-59", ylim_set = c(-0.3,1))
dev.off()

png("p_score_low_vac_40_59_EU.png", width = 600, height = 400)
plot_aggregate_summary(low_vac_country, "Y40-59", ylim_set = c(-0.3,1))
dev.off()


plot_contrast_summary <- function(country_vec1, country_vec2, age, ylim_set = c(-0.3, 1), color1 = "blue", color2 = "red", include.legend = FALSE) {
  # Helper function to process each country vector
  process_country_vec <- function(country_vec, age) {
    country_vec_old <- country_vec
    for (i in 1:length(country_vec_old)) {
      country <- country_vec_old[i]
      # Remove elements if that element does not exist in both sex
      if ((!file.exists(paste0("../stratified/fitted_model/", country, "_", age, "_T.rda"))) & (age != "all_age")) {
        warning(paste0("No corresponding result in ", country))
        country_vec <- country_vec[country_vec != country]
      }
    }
    # Call obtain_aggregate_result
    obtained_aggregate_summary(country_vec, age)
  }

  # Process both country vectors
  summary1 <- process_country_vec(country_vec1, age)
  summary2 <- process_country_vec(country_vec2, age)

  # Make plot
  plot(summary1$time, summary1$mean, type = "l", col = color1, lty = 1, lwd = 2,
       ylim = ylim_set, cex = 0.5,
       xlab = "", ylab = "P-score",
       xlim = c(as.Date("2020-01-01"), as.Date("2023-04-01")),
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
  if(include.legend){
    legend(x = as.Date("2022-01-01"), y = 0.6, legend = c("high vac", "low vac"), col = c(color1, color2), lty = 1, lwd = 2, cex = 1.5, bty = "n")
  }
}

png("p_score_contrast_40_59_EU.png", width = 600, height = 400)
plot_contrast_summary(high_vac_country, low_vac_country, "Y40-59", ylim_set = c(-0.3, 1), color1 = "blue", color2 = "red")
dev.off()

png("p_score_contrast_60_79_EU.png", width = 600, height = 400)
plot_contrast_summary(high_vac_country, low_vac_country, "Y60-79", ylim_set = c(-0.3, 1), color1 = "blue", color2 = "red")
dev.off()

png("p_score_contrast_all_age_EU.png", width = 600, height = 400)
plot_contrast_summary(high_vac_country, low_vac_country, "all_age", ylim_set = c(-0.3, 1), color1 = "blue", color2 = "red", include.legend = TRUE)
dev.off()


# > high_vac_country
# [1] "AT" "BE" "DK" "FI" "HU" "IT" "ES"

# > low_vac_country
# [1] "AM" "BG" "HR" "LV" "RO" "RS" "SK"

