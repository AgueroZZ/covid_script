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
data_path <- "../demo_r_mwk_20_linear.csv"
all_age <- read.csv(data_path)
all_age <- all_age %>% filter(age %in% c("Y20-39","Y40-59","Y60-79","Y_GE80"))
compile(file = "../tut.cpp")
dyn.load(dynlib("../tut"))

####################################################
####### 1. Setting up the prior of SD: #############
####################################################
p = 2; d1 = 5 ## 5 years PSD for IWP-2
a = 2*pi; d2 = 1 ## 1 year PSD for the aggregated sGP
m = 4 ## Include its four harmonics
prior_overdis <- list(u = 0.1, a = 0.01)
prior_PSD1 <- list(u = 0.1, a = 0.01)
prior_PSD2 <- list(u = 0.1, a = 0.01)
prior_SD1 <- OSplines::prior_conversion(d = d1, prior = prior_PSD1, p = p)
prior_SD2 <- prior_conversion_sGP_m(d = d2, prior = prior_PSD2, a = a, m = m)

######################################################################
####### 2. Running model for each country at each age-group: #########
######################################################################
selected_countries <- unique(all_age$geo)
selected_ages <- unique(all_age$age)
selected_sexs <- unique(all_age$sex)

all_age$date <- ISOweek2date(paste0(all_age$TIME_PERIOD, "-1"))
all_age$Year <- year(all_age$date)
model_result_all <- data.frame()
for (selected_country in selected_countries) {
  cat(selected_country, "\n")
  for (selected_age in selected_ages) {
    cat(selected_age, "\n")
    for (selected_sex in selected_sexs) {
      cat(selected_sex, "\n")
      selected_data <- all_age %>% filter(geo == selected_country, age == selected_age, sex == selected_sex)
      tryCatch({
        if(diff(range(selected_data$Year)) >= 10){
          k_IWP <- 100
          k_sGP <- 40
        }
        else{
          k_IWP <- 50
          k_sGP <- 20
        }

        if(max(selected_data$date) <= as.Date("2022-01-01")){
          next
        }

        if(!file.exists(paste0("../fitted_model/", selected_country, "_", selected_age, "_",selected_sex, ".rda"))){
          model_list <- fit_mod_IWP_sGP(world_death = selected_data, prior_IWP = prior_SD1, prior_sGP = prior_PSD2, prior_overdis = prior_overdis, k_IWP = k_IWP, k_sGP = k_sGP, country = selected_country, m = m)
          model_pred <- pred_mortality_obs(model_list = model_list, refined_pred = model_list$x_full, M1 = 3000, M2 = 1)
          save(file = paste0("../fitted_model/", selected_country, "_", selected_age, "_", selected_sex, ".rda"), model_pred)
        }
        load(file = paste0("../fitted_model/", selected_country, "_", selected_age, "_", selected_sex, ".rda"))
        pdf(file = paste0("../figure/individual_country_series/", selected_country, "_", selected_age, "_", selected_sex, ".pdf"), height = 10, width = 10)
        plot_pred_series(country = selected_country, model_pred = model_pred, world_death = selected_data)
        dev.off()
        model_result <- excess_mortality_aggregate(world_death = selected_data, model_pred = model_pred, country = selected_country)
        model_result$country <- selected_country
        model_result$age <- selected_age
        model_result$sex <- selected_sex
        model_result_all <- rbind(model_result_all, model_result)
      }, error = function(e) {
        cat("An error occurred for country", selected_country, ":", selected_age, ":", selected_sex, e$message, "\n")
      })
    }

  }
}
save(file = "../result/result_all_age.rda", model_result_all)




