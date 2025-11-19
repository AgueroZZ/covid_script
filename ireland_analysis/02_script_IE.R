require(sGPfit)
require(aghq)
require(TMB)
require(tidyverse)
require(Matrix)
require(lubridate)
require(OSplines)
require(ISOweek)
source(file = "function.R")
compile(file = "mixed.cpp")
dyn.load(dynlib("mixed"))
load(file = "final.data")
ireland_summary$Year <- year(ireland_summary$date)


# sample size
ireland_summary %>% group_by(new_age_group) %>% summarise(n = n())


####################################################
####### 1. Fit the model with the mixed data type ##
####################################################
p = 2; d1 = 5 ## 5 years PSD for IWP-2
a = 2*pi; d2 = 1 ## 1 year PSD for the aggregated sGP
m = 1 ## Include its two harmonics instead of four, since the data is quarterly
k_IWP <- 100; k_sGP <- 40
prior_overdis <- list(u = 0.1, a = 0.01)
prior_PSD1 <- list(u = 0.1, a = 0.01)
prior_PSD2 <- list(u = 0.1, a = 0.01)
prior_SD1 <- OSplines::prior_conversion(d = d1, prior = prior_PSD1, p = p)
prior_SD2 <- prior_conversion_sGP_m(d = d2, prior = prior_PSD2, a = a, m = m)

ireland_25_45 <- ireland_summary %>% filter(new_age_group == "25-45")
# model_list_25_45 <- fit_mod_IWP_sGP(quarterly_data = ireland_25_45, prior_IWP = prior_SD1, prior_sGP = prior_PSD2, prior_overdis = prior_overdis, k_IWP = k_IWP, k_sGP = k_sGP, m = m)
# model_pred_25_45 <- pred_mortality_obs(model_list = model_list_25_45, refined_pred = model_list_25_45$x_full, M1 = 3000, M2 = 1)
# save(model_pred_25_45, file = "IE_age_25_45.rda")
load(file = "IE_result/IE_age_25_45.rda")
plot_pred_series(model_pred_25_45, ireland_25_45)
model_result_25_45 <- excess_mortality_aggregate(model_pred = model_pred_25_45, full_data = ireland_25_45)
save(file = "IE_result_age_25_45.rda", model_result_25_45)

ireland_45_65 <- ireland_summary %>% filter(new_age_group == "45-65")
# model_list_45_65 <- fit_mod_IWP_sGP(quarterly_data = ireland_45_65, prior_IWP = prior_SD1, prior_sGP = prior_PSD2, prior_overdis = prior_overdis, k_IWP = k_IWP, k_sGP = k_sGP, m = m)
# model_pred_45_65 <- pred_mortality_obs(model_list = model_list_45_65, refined_pred = model_list_45_65$x_full, M1 = 3000, M2 = 1)
# save(model_pred_45_65, file = "IE_age_45_65.rda")
load(file = "IE_result/IE_age_45_65.rda")
plot_pred_series(model_pred_45_65, ireland_45_65)
model_result_45_65 <- excess_mortality_aggregate(model_pred = model_pred_45_65, full_data = ireland_45_65)
save(file = "IE_result_age_45_65.rda", model_result_45_65)

ireland_65_85 <- ireland_summary %>% filter(new_age_group == "65-85")
# model_list_65_85 <- fit_mod_IWP_sGP(quarterly_data = ireland_65_85, prior_IWP = prior_SD1, prior_sGP = prior_PSD2, prior_overdis = prior_overdis, k_IWP = k_IWP, k_sGP = k_sGP, m = m)
# model_pred_65_85 <- pred_mortality_obs(model_list = model_list_65_85, refined_pred = model_list_65_85$x_full, M1 = 3000, M2 = 1)
# save(model_pred_65_85, file = "IE_age_65_85.rda")
load(file = "IE_result/IE_age_65_85.rda")
plot_pred_series(model_pred_65_85, ireland_65_85)
model_result_65_85 <- excess_mortality_aggregate(model_pred = model_pred_65_85, full_data = ireland_65_85)
save(file = "IE_result_age_65_85.rda", model_result_65_85)

ireland_85 <- ireland_summary %>% filter(new_age_group == "over 85")
# model_list_85 <- fit_mod_IWP_sGP(quarterly_data = ireland_85, prior_IWP = prior_SD1, prior_sGP = prior_PSD2, prior_overdis = prior_overdis, k_IWP = k_IWP, k_sGP = k_sGP, m = m)
# model_pred_85 <- pred_mortality_obs(model_list = model_list_85, refined_pred = model_list_85$x_full, M1 = 3000, M2 = 1)
# save(model_pred_85, file = "IE_age_85.rda")
load(file = "IE_result/IE_age_85.rda")
plot_pred_series(model_pred_85, ireland_85)
model_result_85 <- excess_mortality_aggregate(model_pred = model_pred_85, full_data = ireland_85)
save(file = "IE_result_age_85.rda", model_result_85)




