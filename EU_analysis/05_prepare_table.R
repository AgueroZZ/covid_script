load(file = "../result/result_all_age_complete.rda")
require(tidyverse)
countries_exclude <- c("AD","CY","DE","EE","IS","LI","LU","ME","MT","SI")
all_countries <- unique(model_result_all$country)
selected_countries <- all_countries[!all_countries %in% countries_exclude]

load(file = "../vac_data_eu.rda")
vac_data_eu$iso2[50] <- "UK"
vac_data_eu <- vac_data_eu %>% filter(iso2 %in% selected_countries)
threshold_high <- quantile(vac_data_eu$people_vaccinated_per_hundred, probs = c(0.7))
threshold_low <- quantile(vac_data_eu$people_vaccinated_per_hundred, probs = c(0.3))
high_vac_country <- vac_data_eu %>% filter(people_vaccinated_per_hundred >= threshold_high) %>% pull(iso2)
low_vac_country <- vac_data_eu %>% filter(people_vaccinated_per_hundred <= threshold_low) %>% pull(iso2)



model_result_high_vac <- model_result_all %>% filter(country %in% high_vac_country, age == "Y60-79", sex %in% c("F", "M")) %>%
  select(p_med, country, sex, wave)

model_result_low_vac <- model_result_all %>% filter(country %in% low_vac_country, age == "Y60-79", sex %in% c("F", "M")) %>%
  select(p_med, country, sex, wave)

model_result_both <- bind_rows(
  model_result_high_vac %>% mutate(vac_group = "high"),
  model_result_low_vac %>% mutate(vac_group = "low")
)



table_out <- model_result_both %>%
  pivot_wider(
    id_cols = c(country, wave),
    names_from = sex,
    values_from = p_med
  ) %>%
  mutate(
    M = round(M, 3),
    F = round(F, 3),
    value = paste0(M, " (", F, ")")
  ) %>%
  select(country, wave, value) %>%
  pivot_wider(
    names_from = wave,
    values_from = value
  ) %>%
  arrange(country) %>%
  # attach vaccination rate
  left_join(
    vac_data_eu %>%
      select(iso2, people_vaccinated_per_hundred),
    by = c("country" = "iso2")
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
    caption = "P-score by country and wave (male [female])",
    escape = FALSE
  ) %>%
  kable_styling(
    latex_options = c("striped", "hold_position"),
    font_size = 9,
    full_width = FALSE
  )









