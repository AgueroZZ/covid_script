library(readxl)
library(dplyr)
library(tidyverse)
library(lubridate)
ca_data <- read.csv(paste0(getwd(), "/13100768.csv"))

all_geo <- unique(ca_data$GEO)
all_age <- unique(ca_data$Age.at.time.of.death)

filtered_data <- ca_data %>%
  filter(GEO != "Canada, place of occurrence",
         Age.at.time.of.death != "Age at time of death, all ages",
         Sex != "Both sexes")

# Re-categorize age groups
filtered_data <- filtered_data %>%
  mutate(Age.at.time.of.death = recode(Age.at.time.of.death,
                                       "Age at time of death, 0 to 44 years" = "0-44",
                                       "Age at time of death, 45 to 64 years" = "45-64",
                                       "Age at time of death, 65 to 84 years" = "65-84",
                                       "Age at time of death, 85 years and over" = "over 85"))

# Re-categorize provinces using their abbreviations
filtered_data <- filtered_data %>%
  mutate(GEO = recode(GEO,
                      "Newfoundland and Labrador, place of occurrence" = "NL",
                      "Prince Edward Island, place of occurrence" = "PE",
                      "Nova Scotia, place of occurrence" = "NS",
                      "New Brunswick, place of occurrence" = "NB",
                      "Quebec, place of occurrence" = "QC",
                      "Ontario, place of occurrence" = "ON",
                      "Manitoba, place of occurrence" = "MB",
                      "Saskatchewan, place of occurrence" = "SK",
                      "Alberta, place of occurrence" = "AB",
                      "British Columbia, place of occurrence" = "BC",
                      "Yukon, place of occurrence" = "YT",
                      "Northwest Territories, place of occurrence" = "NT",
                      "Nunavut, place of occurrence" = "NU"))

colnames(filtered_data)[1] <- "date"
colnames(filtered_data)[2] <- "province"
colnames(filtered_data)[4] <- "age"
colnames(filtered_data)[13] <- "death"


final_data <- filtered_data %>% select(date, province, age, death, Sex)
save(final_data, file = "final_data.rda")

