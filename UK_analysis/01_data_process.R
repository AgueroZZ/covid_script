library(readxl)
library(dplyr)
library(tidyverse)
library(lubridate)
source(file = "../R/england_wales_model.R")

dailydeaths19812020 <- read_excel("dailydeaths19812020.xlsx", sheet = "Table 2", skip = 3)
head(dailydeaths19812020)

# Convert to long format
long_data <- dailydeaths19812020 %>%
  pivot_longer(cols = matches("^\\d{4}$"), names_to = "Year", values_to = "Deaths")

# Create date column and compute ISO week
long_data <- long_data %>%
  mutate(Date = ISOdate(as.integer(Year), Month, Day),
         ISOWeek = isoweek(Date),
         ISOYear = isoyear(Date))

# Aggregate to weekly data
weekly_data <- long_data %>%
  group_by(`Region code`, `Region name`, Age, ISOYear, ISOWeek) %>%
  summarise(Total_Deaths = sum(Deaths), Total_counts = n()) %>%
  na.omit()

weekly_data$date <- england_wales_iso_monday(
  weekly_data$ISOYear,
  weekly_data$ISOWeek
)

# Aggregate data to get total deaths across all regions
aggregated_data <- weekly_data %>%
  group_by(ISOYear, ISOWeek, Age) %>%
  summarise(Total_Deaths = sum(Total_Deaths), Total_counts = mean(Total_counts)) %>%
  mutate(`Region code` = "All",
         `Region name` = "All",
         date = england_wales_iso_monday(ISOYear, ISOWeek))

# Combine the original data with the aggregated data
final_data <- bind_rows(weekly_data, aggregated_data) %>% filter(Total_counts == 7)
save(final_data, file = "final_data.rda")




