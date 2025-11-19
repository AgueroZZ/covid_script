library(readxl)
library(dplyr)
library(tidyverse)
library(lubridate)

USA_1999_2002 <- read.delim("~/Desktop/mortality/USA/monthly_data/USA_monthly_1999_2002.txt")
USA_1999_2002 <- USA_1999_2002 %>% mutate(state = State, year = Year.Code,
                                          date = Month.Code, age = Five.Year.Age.Groups,
                                          Deaths = Deaths) %>% select(state, year, date, age, Deaths)
USA_1999_2002 <- USA_1999_2002 %>% 
  mutate(date = as.Date(paste0(date, "/01")),  # Convert to date format
         date = ceiling_date(date, "month") - days(1))  # Move to the last day of the month
USA_1999_2002 <- USA_1999_2002 %>%
  mutate(
    age = case_when(
      age %in% c("< 1 year", "1-4 years", "5-9 years", "10-14 years", "15-19 years") ~ "Under 20",
      age %in% c("20-24 years", "25-29 years", "30-34 years", "35-39 years") ~ "20-39",
      age %in% c("40-44 years", "45-49 years", "50-54 years", "55-59 years") ~ "40-59",
      age %in% c("60-64 years ", "65-69 years", "70-74 years", "75-79 years") ~ "60-79",
      age %in% c("80-84 years", "85-89 years", "90-94 years", "95-99 years", "100+ years") ~ "Over 80",
      age == "Not Stated" | age == "" ~ "Other",
      TRUE ~ "Other"  # for unexpected categories, optional
    )
  )%>%
  group_by(year, date, state, age) %>%  # Add any other variables you want to group by
  summarise(Deaths = sum(Deaths)) %>%
  ungroup()



USA_2003_2006 <- read.delim("~/Desktop/mortality/USA/monthly_data/USA_monthly_2003_2006.txt")
USA_2003_2006 <- USA_2003_2006 %>% mutate(state = State, year = Year.Code,
                                          date = Month.Code, age = Five.Year.Age.Groups,
                                          Deaths = Deaths) %>% select(state, year, date, age, Deaths)
USA_2003_2006 <- USA_2003_2006 %>% 
  mutate(date = as.Date(paste0(date, "/01")),  # Convert to date format
         date = ceiling_date(date, "month") - days(1))  # Move to the last day of the month
USA_2003_2006 <- USA_2003_2006 %>%
  mutate(
    age = case_when(
      age %in% c("< 1 year", "1-4 years", "5-9 years", "10-14 years", "15-19 years") ~ "Under 20",
      age %in% c("20-24 years", "25-29 years", "30-34 years", "35-39 years") ~ "20-39",
      age %in% c("40-44 years", "45-49 years", "50-54 years", "55-59 years") ~ "40-59",
      age %in% c("60-64 years ", "65-69 years", "70-74 years", "75-79 years") ~ "60-79",
      age %in% c("80-84 years", "85-89 years", "90-94 years", "95-99 years", "100+ years") ~ "Over 80",
      age == "Not Stated" | age == "" ~ "Other",
      TRUE ~ "Other"  # for unexpected categories, optional
    )
  )%>%
  group_by(year, date, state, age) %>%  # Add any other variables you want to group by
  summarise(Deaths = sum(Deaths)) %>%
  ungroup()





USA_2007_2010 <- read.delim("~/Desktop/mortality/USA/monthly_data/USA_monthly_2007_2010.txt")
USA_2007_2010 <- USA_2007_2010 %>% mutate(state = State, year = Year.Code,
                                          date = Month.Code, age = Five.Year.Age.Groups,
                                          Deaths = Deaths) %>% select(state, year, date, age, Deaths)
USA_2007_2010 <- USA_2007_2010 %>% 
  mutate(date = as.Date(paste0(date, "/01")),  # Convert to date format
         date = ceiling_date(date, "month") - days(1))  # Move to the last day of the month
USA_2007_2010 <- USA_2007_2010 %>%
  mutate(
    age = case_when(
      age %in% c("< 1 year", "1-4 years", "5-9 years", "10-14 years", "15-19 years") ~ "Under 20",
      age %in% c("20-24 years", "25-29 years", "30-34 years", "35-39 years") ~ "20-39",
      age %in% c("40-44 years", "45-49 years", "50-54 years", "55-59 years") ~ "40-59",
      age %in% c("60-64 years ", "65-69 years", "70-74 years", "75-79 years") ~ "60-79",
      age %in% c("80-84 years", "85-89 years", "90-94 years", "95-99 years", "100+ years") ~ "Over 80",
      age == "Not Stated" | age == "" ~ "Other",
      TRUE ~ "Other"  # for unexpected categories, optional
    )
  )%>%
  group_by(year, date, state, age) %>%  # Add any other variables you want to group by
  summarise(Deaths = sum(Deaths)) %>%
  ungroup()



USA_2011_2014 <- read.delim("~/Desktop/mortality/USA/monthly_data/USA_monthly_2011_2014.txt")
USA_2011_2014 <- USA_2011_2014 %>% mutate(state = State, year = Year.Code,
                                          date = Month.Code, age = Five.Year.Age.Groups,
                                          Deaths = Deaths) %>% select(state, year, date, age, Deaths)
USA_2011_2014 <- USA_2011_2014 %>% 
  mutate(date = as.Date(paste0(date, "/01")),  # Convert to date format
         date = ceiling_date(date, "month") - days(1))  # Move to the last day of the month
USA_2011_2014 <- USA_2011_2014 %>%
  mutate(
    age = case_when(
      age %in% c("< 1 year", "1-4 years", "5-9 years", "10-14 years", "15-19 years") ~ "Under 20",
      age %in% c("20-24 years", "25-29 years", "30-34 years", "35-39 years") ~ "20-39",
      age %in% c("40-44 years", "45-49 years", "50-54 years", "55-59 years") ~ "40-59",
      age %in% c("60-64 years ", "65-69 years", "70-74 years", "75-79 years") ~ "60-79",
      age %in% c("80-84 years", "85-89 years", "90-94 years", "95-99 years", "100+ years") ~ "Over 80",
      age == "Not Stated" | age == "" ~ "Other",
      TRUE ~ "Other"  # for unexpected categories, optional
    )
  )%>%
  group_by(year, date, state, age) %>%  # Add any other variables you want to group by
  summarise(Deaths = sum(Deaths)) %>%
  ungroup()




USA_2015_2017 <- read.delim("~/Desktop/mortality/USA/monthly_data/USA_monthly_2015_2017.txt")
USA_2015_2017 <- USA_2015_2017 %>% mutate(state = State, year = Year.Code,
                                          date = Month.Code, age = Five.Year.Age.Groups,
                                          Deaths = Deaths) %>% select(state, year, date, age, Deaths)
USA_2015_2017 <- USA_2015_2017 %>% 
  mutate(date = as.Date(paste0(date, "/01")),  # Convert to date format
         date = ceiling_date(date, "month") - days(1))  # Move to the last day of the month
USA_2015_2017 <- USA_2015_2017 %>%
  mutate(
    age = case_when(
      age %in% c("< 1 year", "1-4 years", "5-9 years", "10-14 years", "15-19 years") ~ "Under 20",
      age %in% c("20-24 years", "25-29 years", "30-34 years", "35-39 years") ~ "20-39",
      age %in% c("40-44 years", "45-49 years", "50-54 years", "55-59 years") ~ "40-59",
      age %in% c("60-64 years ", "65-69 years", "70-74 years", "75-79 years") ~ "60-79",
      age %in% c("80-84 years", "85-89 years", "90-94 years", "95-99 years", "100+ years") ~ "Over 80",
      age == "Not Stated" | age == "" ~ "Other",
      TRUE ~ "Other"  # for unexpected categories, optional
    )
  )%>%
  group_by(year, date, state, age) %>%  # Add any other variables you want to group by
  summarise(Deaths = sum(Deaths)) %>%
  ungroup()





USA_2018_2020 <- read.delim("~/Desktop/mortality/USA/monthly_data/USA_monthly_2018_2020.txt")
USA_2018_2020 <- USA_2018_2020 %>% mutate(state = Residence.State, year = Year.Code,
                                          date = Month.Code, age = Five.Year.Age.Groups,
                                          Deaths = Deaths) %>% select(state, year, date, age, Deaths)
USA_2018_2020 <- USA_2018_2020 %>% 
  mutate(date = as.Date(paste0(date, "/01")),  # Convert to date format
         date = ceiling_date(date, "month") - days(1))  # Move to the last day of the month
USA_2018_2020 <- USA_2018_2020 %>%
  mutate(
    age = case_when(
      age %in% c("< 1 year", "1-4 years", "5-9 years", "10-14 years", "15-19 years") ~ "Under 20",
      age %in% c("20-24 years", "25-29 years", "30-34 years", "35-39 years") ~ "20-39",
      age %in% c("40-44 years", "45-49 years", "50-54 years", "55-59 years") ~ "40-59",
      age %in% c("60-64 years ", "65-69 years", "70-74 years", "75-79 years") ~ "60-79",
      age %in% c("80-84 years", "85-89 years", "90-94 years", "95-99 years", "100+ years") ~ "Over 80",
      age == "Not Stated" | age == "" ~ "Other",
      TRUE ~ "Other"  # for unexpected categories, optional
    )
  )%>%
  group_by(year, date, state, age) %>%  # Add any other variables you want to group by
  summarise(Deaths = sum(Deaths)) %>%
  ungroup()




USA_2021_2023 <- read.delim("~/Desktop/mortality/USA/monthly_data/USA_monthly_2021_2023.txt")
USA_2021_2023 <- USA_2021_2023 %>% mutate(state = Residence.State, year = Year.Code,
                                          date = Month.Code, age = Five.Year.Age.Groups,
                                          Deaths = Deaths) %>% select(state, year, date, age, Deaths)
USA_2021_2023 <- USA_2021_2023 %>% 
  mutate(date = as.Date(paste0(date, "/01")),  # Convert to date format
         date = ceiling_date(date, "month") - days(1))  # Move to the last day of the month
USA_2021_2023 <- USA_2021_2023 %>%
  mutate(
    age = case_when(
      age %in% c("< 1 year", "1-4 years", "5-9 years", "10-14 years", "15-19 years") ~ "Under 20",
      age %in% c("20-24 years", "25-29 years", "30-34 years", "35-39 years") ~ "20-39",
      age %in% c("40-44 years", "45-49 years", "50-54 years", "55-59 years") ~ "40-59",
      age %in% c("60-64 years ", "65-69 years", "70-74 years", "75-79 years") ~ "60-79",
      age %in% c("80-84 years", "85-89 years", "90-94 years", "95-99 years", "100+ years") ~ "Over 80",
      age == "Not Stated" | age == "" ~ "Other",
      TRUE ~ "Other"  # for unexpected categories, optional
    )
  )%>%
  group_by(year, date, state, age) %>%  # Add any other variables you want to group by
  summarise(Deaths = sum(Deaths)) %>%
  ungroup()

USA_monthly <- rbind(USA_1999_2002, USA_2003_2006, USA_2007_2010, USA_2011_2014, USA_2015_2017, USA_2018_2020, USA_2021_2023)
save(USA_monthly, file = "USA_monthly.rda")

