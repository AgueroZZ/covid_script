library(readr)
library(tidyverse)
library(lubridate)
require(sGPfit)
require(aghq)
require(TMB)
require(Matrix)
require(OSplines)
require(ISOweek)
source(file = "function.R")
compile(file = "tut.cpp")
dyn.load(dynlib("tut"))
load(file = "UK_results/UK_combined_final.rda")
summary_p_score <- NULL

# Sample sizes:
UK_combined_final %>%
  group_by(Age, `Region code`) %>%
  summarise(sample_sizes = n()) %>%
  as.data.frame()

####################################################
####### 1. Setting up the prior of SD: ############# 
####################################################
p = 2; d1 = 5 ## 5 years PSD for IWP-2
a = 2*pi; d2 = 1 ## 1 year PSD for the aggregated sGP
m = 4 ## Include its 4 harmonics
prior_overdis <- list(u = 0.1, a = 0.01)
prior_PSD1 <- list(u = 0.1, a = 0.01)
prior_PSD2 <- list(u = 0.1, a = 0.01)
prior_SD1 <- OSplines::prior_conversion(d = d1, prior = prior_PSD1, p = p)
prior_SD2 <- prior_conversion_sGP_m(d = d2, prior = prior_PSD2, a = a, m = m)
k_IWP <- 100
k_sGP <- 40
final_data <- UK_combined_final %>% filter(`Region code` == "All") 
final_data$geo <- "UK"
final_data$OBS_VALUE <- final_data$Total_Deaths


## For age group 1:
# model_list <- fit_mod_IWP_sGP(world_death = final_data[final_data$Age == "Under 65", ], prior_IWP = prior_SD1, prior_sGP = prior_PSD2, prior_overdis = prior_overdis, k_IWP = k_IWP, k_sGP = k_sGP, country = "UK", m = m, accuracy = 0.001)
# model_pred <- pred_mortality_obs(model_list = model_list, refined_pred = model_list$x_full, M1 = 3000, M2 = 1)
# model_pred$summary <- arrange(model_pred$summary, time)
# save(model_pred, file = "fitted_mod/model_pred_less_65.rda")
load(file = "fitted_mod/model_pred_less_65.rda")
model_result <- excess_mortality_aggregate(model_pred = model_pred, full_data = final_data[final_data$Age == "Under 65", ])
model_result$country = "UK"
save(model_result, file = "UK_result_under_65.rda")

### For age group 2:
model_list <- fit_mod_IWP_sGP(world_death = final_data[final_data$Age == "65-85", ], prior_IWP = prior_SD1, prior_sGP = prior_PSD2, prior_overdis = prior_overdis, k_IWP = k_IWP, k_sGP = k_sGP, country = "UK", m = m, accuracy = 0.001)
model_pred <- pred_mortality_obs(model_list = model_list, refined_pred = model_list$x_full, M1 = 3000, M2 = 1)
model_pred$summary <- arrange(model_pred$summary, time)
save(model_pred, file = "fitted_mod/model_pred_65_85.rda")
load(file = "fitted_mod/model_pred_65_85.rda")
model_result <- excess_mortality_aggregate(model_pred = model_pred, full_data = final_data[final_data$Age == "65-85", ])
model_result$country = "UK"
save(model_result, file = "UK_result_65_85.rda")


### For age group 3:
model_list <- fit_mod_IWP_sGP(world_death = final_data[final_data$Age == "over 85", ], prior_IWP = prior_SD1, prior_sGP = prior_PSD2, prior_overdis = prior_overdis, k_IWP = k_IWP, k_sGP = k_sGP, country = "UK", m = m, accuracy = 0.001)
model_pred <- pred_mortality_obs(model_list = model_list, refined_pred = model_list$x_full, M1 = 3000, M2 = 1)
model_pred$summary <- arrange(model_pred$summary, time)
save(model_pred, file = "fitted_mod/model_pred_85.rda")
load(file = "fitted_mod/model_pred_85.rda")
model_result <- excess_mortality_aggregate(model_pred = model_pred, full_data = final_data[final_data$Age == "over 85", ])
model_result$country = "UK"
save(model_result, file = "UK_result_85.rda")

