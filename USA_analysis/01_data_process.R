library(readxl)
library(dplyr)
library(tidyverse)
library(lubridate)


process_data <- function(data) {
  data %>% 
    mutate(state = State, year = Year.Code, date = Month.Code, age = Ten.Year.Age.Groups, sex = Gender.Code, Deaths = Deaths) %>% 
    select(state, year, date, age, sex, Deaths) %>% 
    mutate(date = as.Date(paste0(date, "/01")),  # Convert to date format
           date = ceiling_date(date, "month") - days(1),  # Move to the last day of the month
           age = case_when(
             age %in% c("< 1 year", "1-4 years", "5-14 years", "15-24 years", "25-34 years", "35-44 years") ~ "0-44",
             age %in% c("45-54 years", "55-64 years") ~ "45-64",
             age %in% c("65-74 years", "75-84 years") ~ "65-84",
             age == "85+ years" ~ "Over 85",
             age == "Not Stated" | age == "" ~ "Other",
             TRUE ~ "Other"  # for unexpected categories, optional
           )) %>%
    group_by(year, date, state, age, sex) %>% 
    summarise(Deaths = sum(Deaths), .groups = 'drop') %>%
    ungroup()
}

USA_1999_2002 <- read.delim("monthly_data/Multiple Cause of Death, 1999-2002.txt")
USA_1999_2002 <-  process_data(USA_1999_2002)

USA_2003_2006 <- read.delim("monthly_data/Multiple Cause of Death, 2003-2006.txt")
USA_2003_2006 <- process_data(USA_2003_2006)

USA_2007_2010 <- read.delim("monthly_data/Multiple Cause of Death, 2007-2010.txt")
USA_2007_2010 <- process_data(USA_2007_2010)

USA_2011_2014 <- read.delim("monthly_data/Multiple Cause of Death, 2011-2014.txt")
USA_2011_2014 <- process_data(USA_2011_2014)

USA_2015_2018 <- read.delim("monthly_data/Multiple Cause of Death, 2015-2018.txt")
USA_2015_2018 <- process_data(USA_2015_2018)

USA_2019_2020 <- read.delim("monthly_data/Multiple Cause of Death, 2019-2020.txt")
USA_2019_2020 <- process_data(USA_2019_2020)

USA_2021_2023 <- read.delim("monthly_data/Multiple Cause of Death, 2021-2023.txt")
colnames(USA_2021_2023)[2] <- "State"
USA_2021_2023 <- process_data(USA_2021_2023)

USA_monthly <- rbind(USA_1999_2002, USA_2003_2006, USA_2007_2010, USA_2011_2014, USA_2015_2018, USA_2019_2020, USA_2021_2023)
save(USA_monthly, file = "USA_monthly.rda")

