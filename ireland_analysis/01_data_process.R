library(readxl)
library(dplyr)
library(tidyverse)
library(lubridate)

ireland_quarter <- read.csv("~/Desktop/mortality/ireland/ireland_quater.csv")

# Convert Quarter to Date
ireland_quarter <- ireland_quarter %>%
  mutate(year = substr(Quarter, 1, 4),
         qtr = substr(Quarter, 5, 6)) %>%
  mutate(date = case_when(
    qtr == "Q1" ~ paste0(year, "-03-31"),
    qtr == "Q2" ~ paste0(year, "-06-30"),
    qtr == "Q3" ~ paste0(year, "-09-30"),
    qtr == "Q4" ~ paste0(year, "-12-31")
  )) %>%
  mutate(date = as.Date(date))

# Aggregate the age group
ireland_quarter <- ireland_quarter %>%
  mutate(new_age_group = case_when(
    Age.Group %in% c("Under 1 year", "1 - 4 years", "5 - 14 years", "15 - 24 years") ~ "less than 25",
    Age.Group %in% c("25 - 34 years", "35 - 44 years") ~ "25-45",
    Age.Group %in% c("45 - 54 years", "55 - 64 years") ~ "45-65",
    Age.Group %in% c("65 - 74 years", "75 - 84 years") ~ "65-85",
    Age.Group == "85 years and over" ~ "over 85",
    TRUE ~ "Other"
  ))

# Summarize the data
ireland_summary <- ireland_quarter %>%
  filter(new_age_group != "Other") %>%
  group_by(date, new_age_group, Quarter) %>%
  summarise(deaths = sum(VALUE))

save(ireland_summary, file = "final.data")
