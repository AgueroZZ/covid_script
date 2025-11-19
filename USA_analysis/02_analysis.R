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

load(file = "USA_monthly.rda")
USA_monthly <- USA_monthly %>% arrange(date)
USA_monthly$log_days <- log(as.numeric(lubridate::days_in_month(USA_monthly$date)))

####################################################
####### 1. Setting up the prior of SD: ############# 
####################################################
p = 2; d1 = 5 ## 5 years PSD for IWP-2
a = 2*pi; d2 = 1 ## 1 year PSD for the aggregated sGP
m = 4 ## Include its four harmonics
prior_overdis <- list(u = 0.1, alpha = 0.01)
prior_PSD1 <- list(u = 0.1, alpha = 0.01)
prior_PSD2 <- list(u = 0.1, alpha = 0.01)
# prior_SD1 <- BayesGP::prior_conversion_IWP(d = d1, prior = prior_PSD1, p = p)
# prior_SD2 <- BayesGP::prior_conversion_sGP(d = d2, prior = prior_PSD2, a = a, m = m)

prior_SD1 <- list(prior = "exp", param = prior_PSD1, h = d1)
prior_SD2 <- list(prior = "exp", param = prior_PSD2, h = d2)
prior_overdis <- list(prior = "exp", param = prior_overdis)


###################################################################### 
####### 2. Running model for each country at each age-group: ######### 
######################################################################
selected_states <- unique(USA_monthly$state)[-length(unique(USA_monthly$state))]
selected_ages <- unique(USA_monthly$age)[1:4]
selected_sexs <- unique(USA_monthly$sex)[1:2]

model_result_all <- data.frame()
for (State in selected_states) {
  for (Age in selected_ages) {
    for (Sex in selected_sexs) {
      cat(State,":",Age,":",Sex, "\n")
      selected_USA <- USA_monthly %>% filter(state == State , age == Age, sex == Sex, date <= "2023-08-31") %>% arrange(date)
      
      tryCatch({
        if(diff(range(selected_USA$year)) >= 10){
          k_IWP <- 100
          k_sGP <- 40
        }
        else{
          k_IWP <- 50
          k_sGP <- 20
        }  
        if(file.exists(paste0("fitted_model/", State, "_age_", Age, "_sex_", Sex, ".rda"))){
          load(file = paste0("fitted_model/", State, "_age_", Age, "_sex_", Sex, ".rda"))
        }
        else{
          model_list <- fit_mod_IWP_sGP(monthly_death = selected_USA, prior_IWP = prior_SD1, prior_sGP = prior_SD2, prior_overdis = prior_overdis, k_IWP = k_IWP, k_sGP = k_sGP, State = State, Age = Age, m = m)
          model_pred <- pred_mortality_obs(model_list = model_list, refined_pred = model_list$x_full, days_month = exp(selected_USA$log_days))
          save(file = paste0("fitted_model/", State, "_age_", Age, "_sex_", Sex, ".rda"), model_pred)
        }
        model_pred$summary <- model_pred$summary %>% filter(time <= "2023-08-31")
        model_pred$samples <- model_pred$samples[1:nrow(model_pred$summary),]
        pdf(file = paste0("figure/individual_state_series/", State, "_age_", Age, "_sex_", Sex, ".pdf"), height = 10, width = 10)
        plot_pred_series(State = State, Age = Age, model_pred = model_pred, monthly_death = selected_USA)
        dev.off()
        model_result <- excess_mortality_aggregate(Age = Age, model_pred = model_pred, State = State, monthly_death = selected_USA)
        model_result$state <- State
        model_result$age <- Age
        model_result$sex <- Sex
        model_result_all <- rbind(model_result_all, model_result)
      }, error = function(e) {
        cat("An error occurred for state:", State, ": age group:", Age, ": sex",  Sex, "\n", e$message, "\n")
      })
    }
  }
}

save(file = "USA_monthly_result.rda", model_result_all)




