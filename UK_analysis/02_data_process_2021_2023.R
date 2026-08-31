library(readxl)
library(dplyr)
library(tidyverse)
library(lubridate)
source(file = "../R/england_wales_model.R")

publishedweek522021 <- read_excel("publishedweek522021.xlsx", 
                                  sheet = "Weekly figures 2021", skip = 16, n_max = 20)
colnames(publishedweek522021)[2:53] <- 1:52
colnames(publishedweek522021)[1] <- "age"

UK_2021 <- publishedweek522021 %>% pivot_longer(cols = 2:53, names_to = "week", values_to = "deaths")
UK_2021$Year <- 2021
UK_2021$date <- england_wales_iso_monday(UK_2021$Year, UK_2021$week)

publicationfile2022 <- read_excel("publicationfileweek522022.xlsx", sheet = "2", skip = 6, n_max = 52)
UK_2022 <- publicationfile2022 %>% pivot_longer(cols = 4:23, names_to = "age", values_to = "deaths") %>% select(`Week number`, `Week ending`, age, deaths)
colnames(UK_2022) <- c("week", "date", "age", "deaths")
UK_2022$date <- as.Date(UK_2022$date)
UK_2022$Year <- 2022
UK_2022$date <- england_wales_iso_monday(UK_2022$Year, UK_2022$week)

publicationfile2023 <- read_excel("publicationfileweek352023.xlsx", sheet = "2", skip = 6, n_max = 35)
UK_2023 <- publicationfile2023 %>% pivot_longer(cols = 4:23, names_to = "age", values_to = "deaths") %>% select(`Week number`, `Week ending`, age, deaths)
colnames(UK_2023) <- c("week", "date", "age", "deaths")
UK_2023$date <- as.Date(UK_2023$date)
UK_2023$Year <- 2023
UK_2023$date <- england_wales_iso_monday(UK_2023$Year, UK_2023$week)

UK_recent <- rbind(UK_2021, UK_2022, UK_2023)

UK_recent_final <- UK_recent %>%
  mutate(
    Age = case_when(
      age %in% c("<1", "1-4", "01-04", "05-09", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64") ~ "Under 65",
      age %in% c("65-69", "70-74") ~ "65-74",
      age %in% c("75-79", "80-84") ~ "75-84",
      age %in% c("85-89", "90+") ~ "85 and over",
      TRUE ~ "Other"  # for unexpected categories, optional
    )
  ) %>%
  group_by(Year, week, date, Age) %>%
  summarise(deaths = sum(deaths))


#### combine with the old data:
load(file = "final_data.rda")
final_data <- final_data %>% filter(`Region code` == "All") %>% filter(date < min(UK_recent_final$date))
final_data$Year <- as.numeric(final_data$ISOYear)
UK_recent_final$`Region code` = "All"
UK_recent_final$`Region name` = "All"
UK_recent_final$week <- as.numeric(UK_recent_final$week)
colnames(final_data)[c(5)] <- c("week")
colnames(UK_recent_final)[5] <- "Total_Deaths"
UK_recent_final <- arrange(UK_recent_final, date)
UK_combined_final <- rbind(final_data, UK_recent_final)

UK_combined_final <- UK_combined_final %>%
  mutate(
    Age = case_when(
      Age %in% c("Under 65") ~ "Under 65",
      Age %in% c("65-74", "75-84") ~ "65-85",
      Age %in% c("85 and over") ~ "over 85",
      TRUE ~ "Other"  # for unexpected categories, optional
    )
  ) %>%
  group_by(Year, ISOYear, week, date, Age, `Region code`) %>%
  summarise(Total_Deaths = sum(Total_Deaths))

save(UK_combined_final, file = "UK_combined_final.rda")
