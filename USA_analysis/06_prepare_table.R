load(file = "USA_monthly_result.rda")
require(tidyverse)



load(file = "us_state_vaccinations_select.rda")
all_files <- list.files("fitted_model/")
all_states <- unique(gsub("_.*", "", all_files))
selected_states <- all_states[c(1, 3:7, 10:11, 13:19, 21:26, 29, 31:34, 36:39, 41, 43:45, 47:50)]
us_state_vaccinations_select <- us_state_vaccinations_select %>%
  filter(location %in% selected_states)
as.numeric(quantile(us_state_vaccinations_select$people_vaccinated_per_hundred, 0.15))
as.numeric(quantile(us_state_vaccinations_select$people_vaccinated_per_hundred, 0.85))
threshold_low <- 42
threshold_high <- 62
high_vac_state <- us_state_vaccinations_select %>% filter(people_vaccinated_per_hundred >= threshold_high) %>% arrange(-people_vaccinated_per_hundred) %>% pull(location)
low_vac_state <- us_state_vaccinations_select %>% filter(people_vaccinated_per_hundred <= threshold_low) %>% arrange(people_vaccinated_per_hundred) %>% pull(location)

age_group <- "65-84"

model_result_high_vac <- model_result_all %>%
  filter(state %in% high_vac_state, age == age_group) %>%
  select(p_med, state, sex, wave)


model_result_low_vac <- model_result_all %>%
  filter(state %in% low_vac_state, age == age_group) %>%
  select(p_med, state, sex, wave)

model_result_both <- bind_rows(
  model_result_high_vac %>% mutate(vac_group = "high"),
  model_result_low_vac %>% mutate(vac_group = "low")
)



table_out <- model_result_both %>%
  pivot_wider(
    id_cols = c(state, wave),
    names_from = sex,
    values_from = p_med
  ) %>%
  mutate(
    M = round(M, 3),
    F = round(F, 3),
    value = paste0(M, " (", F, ")")
  ) %>%
  select(state, wave, value) %>%
  pivot_wider(
    names_from = wave,
    values_from = value
  ) %>%
  arrange(state) %>%
  # attach vaccination rate
  left_join(
    us_state_vaccinations_select %>%
      select(location, people_vaccinated_per_hundred),
    by = c("state" = "location")
  ) %>%
  # rename vaccination column if you like
  rename(vaccinated_per_hundred = people_vaccinated_per_hundred) %>%
  # arrange by vaccination rate
  arrange(desc(vaccinated_per_hundred)) %>%
  # remove vaccination rate column
  select(-vaccinated_per_hundred)

print(table_out)



library(knitr)
library(kableExtra)

table_out %>%
  kbl(
    format = "latex",
    booktabs = TRUE,
    align = "lrrrr",
    caption = "P-score by state and wave (male [female])",
    escape = FALSE
  ) %>%
  kable_styling(
    latex_options = c("striped", "hold_position"),
    font_size = 9,
    full_width = FALSE
  )





