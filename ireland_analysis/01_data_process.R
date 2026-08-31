source(file = "../R/ireland_model.R")

# This compatibility script writes the historical object name from the tracked
# quarterly CSV. The canonical pipeline uses read_ireland_model_input() directly.
ireland_model_input <- read_ireland_model_input("ireland_quater.csv")
ireland_summary <- ireland_model_input
ireland_summary$new_age_group <- ireland_summary$age_group
ireland_summary$Quarter <- ireland_summary$quarter
ireland_summary$deaths <- ireland_summary$observed_deaths

save(ireland_summary, file = "final.data")
