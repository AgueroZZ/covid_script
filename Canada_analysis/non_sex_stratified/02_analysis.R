.libPaths(c("~/lib", .libPaths()))
require(tidyverse)
require(sGPfit)
require(aghq)
require(TMB)
require(Matrix)
require(lubridate)
require(OSplines)
require(ISOweek)
require(BayesGP)
source(file = "function.R")

load(file = "final_data.rda")
canada_weekly <- final_data %>% arrange(date)
canada_weekly$date <- as.Date(canada_weekly$date)
canada_weekly$year <- year(canada_weekly$date)

####################################################
####### 1. Setting up the prior of SD: ############# 
####################################################
p = 2; d1 = 5 ## 5 years PSD for IWP-2
a = 2*pi; d2 = 1 ## 1 year PSD for the aggregated sGP
m = 4 ## Include its four harmonics
prior_overdis <- list(u = 0.1, alpha = 0.01)
prior_PSD1 <- list(u = 0.1, alpha = 0.01)
prior_PSD2 <- list(u = 0.1, alpha = 0.01)
# prior_SD1 <- prior_conversion_IWP(d = d1, prior = prior_PSD1, p = p)
# prior_SD2 <- prior_conversion_sGP_m(d = d2, prior = prior_PSD2, a = a, m = m)

prior_SD1 <- list(prior = "exp", param = prior_PSD1, h = d1)
prior_SD2 <- list(prior = "exp", param = prior_PSD2, h = d2)
prior_overdis <- list(prior = "exp", param = prior_overdis)

###################################################################### 
####### 2. Running model for each country at each age-group: ######### 
######################################################################
selected_prov <- unique(canada_weekly$province)
selected_ages <- unique(canada_weekly$age)
model_result_all <- data.frame()
for (prov in selected_prov) {
  for (Age in selected_ages) {
    cat(prov,":",Age, "\n")
    selected_CA <- canada_weekly %>% filter(province == prov & age == Age) %>% arrange(date)
    selected_CA <- na.omit(selected_CA)
    if(max(selected_CA$year) <= 2021){
      next
    }
    tryCatch({
      if(diff(range(selected_CA$year)) >= 10){
        k_IWP <- 100
        k_sGP <- 40
      }
      else{
        k_IWP <- 50
        k_sGP <- 20
      }  
      if(file.exists(paste0("fitted_model/", prov, "_age_", Age, ".rda"))){
        load(file = paste0("fitted_model/", prov, "_age_", Age, ".rda"))
      }
      else{
        model_list <- fit_mod_IWP_sGP(canada_death = selected_CA, prior_IWP = prior_SD1, prior_sGP = prior_SD2, prior_overdis = prior_overdis, k_IWP = k_IWP, k_sGP = k_sGP, prov = prov, Age = Age, m = m)
        model_pred <- pred_mortality_obs(model_list = model_list, refined_pred = model_list$x_full)
        save(file = paste0("fitted_model/", prov, "_age_", Age, ".rda"), model_pred)
      }

      pdf(file = paste0("figure/individual_prov_series/age_group_", Age, "_", prov, ".pdf"), height = 10, width = 10)
      plot_pred_series(prov = prov, Age = Age, model_pred = model_pred, canada_death = canada_weekly)
      dev.off()
      model_result <- excess_mortality_aggregate(Age = Age, model_pred = model_pred, prov = prov, canada_death = canada_weekly)
      model_result$province <- prov
      model_result$age <- Age
      model_result_all <- rbind(model_result_all, model_result)
    }, error = function(e) {
      cat("An error occurred for province:", prov, ": age group:", Age, ":", "\n", e$message, "\n")
    })
  }
}
save(file = "Canada_weekly_result.rda", model_result_all)







