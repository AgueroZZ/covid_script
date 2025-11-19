require(tidyverse)

load(file = "result/result_all_age.rda")
##Remove UK:
model_result_all <- model_result_all %>% filter(country != "UK")

load(file = "IE_results_update/IE_result_age_25_45.rda")
model_result_25_45$country <- "IE"
model_result_25_45$age <- "Y20-39"
model_result_25_45$sex <- "T"
model_result_all <- rbind(model_result_all, model_result_25_45)

load(file = "IE_results_update/IE_result_age_45_65.rda")
model_result_45_65$country <- "IE"
model_result_45_65$age <- "Y40-59"
model_result_45_65$sex <- "T"

model_result_all <- rbind(model_result_all, model_result_45_65)

load(file = "IE_results_update/IE_result_age_65_85.rda")
model_result_65_85$country <- "IE"
model_result_65_85$age <- "Y60-79"
model_result_65_85$sex <- "T"
model_result_all <- rbind(model_result_all, model_result_65_85)

load(file = "IE_results_update/IE_result_age_85.rda")
model_result_85$country <- "IE"
model_result_85$age <- "Y_GE80"
model_result_85$sex <- "T"
model_result_all <- rbind(model_result_all, model_result_85)

load(file = "UK_results_update/UK_result_under_65.rda")
model_result_20_39 <- model_result
model_result_20_39$country <- "GB"
model_result_20_39$age <- "Y20-39"
model_result_20_39$sex <- "T"
model_result_all <- rbind(model_result_all, model_result_20_39)

model_result_40_59 <- model_result
model_result_40_59$country <- "GB"
model_result_40_59$age <- "Y40-59"
model_result_40_59$sex <- "T"
model_result_all <- rbind(model_result_all, model_result_40_59)

load(file = "UK_results_update/UK_result_65_85.rda")
model_result_60_79 <- model_result
model_result_60_79$country <- "GB"
model_result_60_79$age <- "Y60-79"
model_result_60_79$sex <- "T"
model_result_all <- rbind(model_result_all, model_result_60_79)

load(file = "UK_results_update/UK_result_85.rda")
model_result_80 <- model_result
model_result_80$country <- "GB"
model_result_80$age <- "Y_GE80"
model_result_80$sex <- "T"
model_result_all <- rbind(model_result_all, model_result_80)

save(model_result_all, file = "result/result_all_age_complete.rda")









