##### Packages #####

library(tidyverse)
library(readxl)
library(openxlsx) 
library(lubridate)

select <- dplyr::select
filter <- dplyr::filter


##### Initial Data Processing and Cleaning #####

# Format Met Data #

met.wide <- read_xlsx("13-22 Met Data.xlsx", sheet = "all")

met.long <- met.wide %>%
  gather(
    key = "Meteorological Metric",
    value = "Value",
    "Historical Highest Temperature in Luqa.13":"Historical Lowest Temperature in Luqa.22",
    na.rm = FALSE,
    convert = FALSE,
    factor_key = FALSE
  ) %>%
  mutate(Year = case_when(
    row_number() < 559 ~ "2013",
    row_number() < 1117 ~ "2014",
    row_number() < 1675 ~ "2015",
    row_number() < 2233 ~ "2016",
    row_number() < 2791 ~ "2017",
    row_number() < 3349 ~ "2018",
    row_number() < 3907 ~ "2019",
    row_number() < 4465 ~ "2020",
    row_number() < 5023 ~ "2021",
    row_number() >= 5023 ~ "2022",
  ),
  .before = 1
  ) %>%
  rename(Date = "0") %>%
  mutate(`Meteorological Metric` = str_remove(`Meteorological Metric`, "\\.\\d{2}$"))


##Format Admissions Data ##

# A&E Admissions #
MDH.a_e <- read_xlsx("15-23 MDH Data.xlsx", sheet = "A&E Attendances")

MDH.a_e.long <- MDH.a_e %>%
  rename(Date = ...1) %>%
  pivot_longer(
    cols = "2023":"2015",
    names_to = "Year",
    values_to = "Value"
  ) %>%
  relocate(Year, .before = 1) %>%
  mutate("Type of Admission" = "All A&E Attendances", .after = 2)


# Admissions (all specialties ) #
MDH.ad.all <- read_xlsx("15-23 MDH Data.xlsx", sheet = "Ad (all specialties)")

MDH.ad.all.long <- MDH.ad.all %>%
  rename(Date = ...1) %>%
  pivot_longer(
    cols = "2023":"2015",
    names_to = "Year",
    values_to = "Value"
  ) %>%
  relocate(Year, .before = 1) %>%
  mutate("Type of Admission" = "Admissions (All Specialties)", .after = 2)


# Admissions (Med. Dept. Consultancies) # 
MDH.ad.con <- read_xlsx("15-23 MDH Data.xlsx", sheet = "Ad (Consults)")

MDH.ad.con.long <- MDH.ad.con %>%
  rename(Date = ...1) %>%
  pivot_longer(
    cols = "2023":"2015",
    names_to = "Year",
    values_to = "Value"
  ) %>%
  relocate(Year, .before = 1) %>%
  mutate("Type of Admission" = "Admissions (Med. Dept. Consultancies)", .after = 2)


# Combine admissions # 
ad.15_23 <- bind_rows(MDH.a_e.long, MDH.ad.all.long, MDH.ad.con.long)


# Met 2023 #
met.2023.wide <- read_xlsx("2023 Data.xlsx", sheet = "Met")

met.2023.long <- met.2023.wide %>%
  gather(
    key = "Meteorological Metric",
    value = "Value",
    "average temp":"7day feels like",
    na.rm = FALSE,
    convert = FALSE,
    factor_key = FALSE
  ) %>%
  mutate(Year = "2023", .before = 1)


# Admissions 2023 #
ad.2023.wide <- read_xlsx("2023 Data.xlsx", sheet = "Ad")

ad.2023.long <- ad.2023.wide %>%
  gather(
    key = "Type of Admission",
    value = "Value",
    "Resident deaths":"Medical Admissions",
    na.rm = FALSE,
    convert = FALSE,
    factor_key = FALSE
  ) %>%
  mutate(Year = "2023", .before = 1)


# Met 2024 #
met.2024.wide <- read_xlsx("2024 Data.xlsx", sheet = "Met")

met.2024.long <- met.2024.wide %>%
  gather(
    key = "Meteorological Metric",
    value = "Value",
    "Historical Highest Temperature in Luqa":"Historical Lowest Temperature in Luqa",
    na.rm = FALSE,
    convert = FALSE,
    factor_key = FALSE
  ) %>%
  mutate(Year = "2024", .before = 1)


# Admissions  2024 #
ad.2024.wide <- read_xlsx("2024 Data.xlsx", sheet = "Ad")

ad.2024.long <- ad.2024.wide %>%
  gather(
    key = "Type of Admission",
    value = "Value",
    "A&E":"Telemedicine calls",
    na.rm = FALSE,
    convert = FALSE,
    factor_key = FALSE
  ) %>%
  mutate(Year = "2024", .before = 1)

# Met 2025 #
met.2025.wide <- read_xlsx("2025 Data.xlsx", sheet = "Met")

met.2025.long <- met.2025.wide %>%
  gather(
    key = "Meteorological Metric",
    value = "Value",
    "Historical Highest Temperature in Luqa":"Historical Lowest Temperature in Luqa",
    na.rm = FALSE,
    convert = FALSE,
    factor_key = FALSE
  ) %>%
  mutate(Year = "2025", .before = 1)

# Combined Met #
met.long$Value <- as.numeric(met.long$Value)
met.2024.long$Value <- as.numeric(met.2024.long$Value)
met.2025.long$Value <- as.numeric(met.2025.long$Value)


combined_met <- bind_rows(met.long, met.2023.long, met.2024.long, met.2025.long)

# Ad 2025 #
ad.2025.wide <- read_xlsx("2025 Data.xlsx", sheet = "Ad")

ad.2025.long <- ad.2025.wide %>%
  gather(
    key = "Type of Admission",
    value = "Value",
    "A&E":"Telemedicine calls",
    na.rm = FALSE,
    convert = FALSE,
    factor_key = FALSE
  ) %>%
  mutate(Year = "2025", .before = 1)

# 2024 + 2025 Ad #
ad.2024.long$Value <- as.numeric(ad.2024.long$Value)
ad.2025.long$Value <- as.numeric(ad.2025.long$Value)

ad.24_25 <- bind_rows(ad.2024.long, ad.2025.long)

# 13 - 22 Mortality #
mort.13_23.wide <- read_xlsx("13-23 Mort Data.xlsx", sheet = "Copy")
mort.13_22.wide <- mort.13_23.wide %>%
  rename("Date"= `...1`) %>%
  select(!`total 2023`)

mort.13_22.long <- mort.13_22.wide %>%
  pivot_longer(
    cols         = starts_with("total"),
    names_to     = "Total Number of Deaths",
    values_to    = "value",
    values_drop_na = FALSE
  )

mort.13_22.long <- mort.13_22.long %>%
  mutate(
    year = str_extract(`Total Number of Deaths`, "\\d{4}"),
    Date = dmy(paste(Date, year, sep = "-"))
  ) %>%
  select(-year)

mort.13_22.long <- mort.13_22.long %>%
  mutate(`Total Number of Deaths` = value) %>%
  select(-value)

# 23 - 25 Mort #
mort.23_25 <- bind_rows(ad.2023.long, ad.2024.long, ad.2025.long) %>%
  filter(`Type of Admission` %in% "Total deaths") %>%
  mutate(`Type of Admission` = Value) %>%
  select(-Value) %>%
  rename("Total Number of Deaths" = `Type of Admission`) %>%
  mutate(Date = dmy(paste(Date, Year, sep = "-"))) %>%
  select(-Year)

# 13 - 25 Mort #
mort.13_25 <- bind_rows(mort.13_22.long, mort.23_25) 

##### Further Combining and Reorganising Data #####

# Tasmax #
tasmax.13_25 <- combined_met %>%
  filter(`Meteorological Metric` %in% c("Historical Highest Temperature in Luqa", "high temp"))  %>%
  mutate(Date = as.Date(paste(Date, Year), format = "%d-%b %Y")) %>%
  select(-Year)

tasmax.15_25 <- tasmax.13_25 %>%
  filter(!year(Date) %in% c(2013, 2014))

tasmax.23_25 <- tasmax.13_25 %>%
  filter(!year(Date) %in% c(2023, 2024, 2025))

## Formatting the Data ##

# total admissions + A&E # 
a_e.totad.15_23 <- ad.15_23 %>%
  filter(`Type of Admission` %in% c("All A&E Attendances", "Admissions (All Specialties)")) 

a_e.totad.24_25 <- ad.24_25 %>%
  filter(`Type of Admission` %in% c("A&E", "Total admissions"))


a_e.totad.15_25 <- bind_rows(a_e.totad.15_23, a_e.totad.24_25) %>%
  mutate(`Type of Admission` = case_when(
    `Type of Admission` %in% c("All A&E Attendances", "A&E") ~ "A&E Attendances",
    `Type of Admission` %in% c("Admissions (All Specialties)", "Total admissions") ~ "Total Admissions",
    TRUE ~ `Type of Admission`
  ))

# Create individual A&E and Total Admissions, and remove any missing dates #

a_e.15_25 <- a_e.totad.15_25 %>%
  filter(`Type of Admission` == "A&E Attendances") %>%
  mutate(Date = as.Date(paste(Date, Year), format = "%d-%b %Y"))

totad.15_25 <- a_e.totad.15_25 %>%
  filter(`Type of Admission` == "Total Admissions") %>%
  mutate(Date = as.Date(paste(Date, Year), format = "%d-%b %Y")) 

#-----------#

missing_from_y <- as.Date(setdiff(a_e.15_25$Date, mort.13_25$Date), origin = "1970-01-01")

a_e.15_25 <- a_e.15_25 %>%
  filter(!Date %in% missing_from_y)

mort.13_25 <- mort.13_25 %>%
  filter(!Date %in% missing_from_y)


## Final Dataframe ##

# year, date, ae, temp, mon - sun #
a_e.15_25.dow <- a_e.15_25 %>%
  mutate(Day_of_Week = (as.POSIXlt(Date)$wday + 6) %% 7 + 1) %>% # 1 = Mon, 7 = Sun
  select(!Year) 

# year, data, mort, temp, mon - sun #
mort.13_25.dow <- mort.13_25 %>%
  mutate(
    `Type of Admission` = "Total Deaths",
    Day_of_Week = (as.POSIXlt(Date)$wday + 6) %% 7 + 1
  ) %>%
  rename(Value = `Total Number of Deaths`) %>%
  select(Date, `Type of Admission`, Value, Day_of_Week)

# A&E + Mort Joined #
a_e.mort.13_25.dow <- rbind(mort.13_25.dow, a_e.15_25.dow)

# A&E + Mort + Temp #
a_e.mort.temp.13_25 <- a_e.mort.13_25.dow %>%
  left_join(
    tasmax.13_25 %>%
      mutate(Date = as.Date(Date)) %>%
      rename(`MaxT` = Value),
    by = "Date",
    relationship = "many-to-many"
  )

# A&E + Mort + Temp w/lag #
a_e.mort.13_25.full <- a_e.mort.temp.13_25 %>%
  group_by(`Type of Admission`) %>%
  arrange(Date) %>%
  mutate(!!!setNames(
    lapply(1:7, function(i) expr(lag(`MaxT`, !!i))),
    paste0("L", 1:7)
  )) %>%
  ungroup() %>%
  select(-`Meteorological Metric`)

# A&E + Temp Full #
a_e.15_25.full <- a_e.mort.13_25.full %>%
  filter(`Type of Admission` == "A&E Attendances") %>%
  select(!`Type of Admission`) %>%
  rename("n_ae" = Value)

# Mort + Temp  Full #
mort.13_25.full <- a_e.mort.13_25.full %>%
  filter(`Type of Admission` == "Total Deaths") %>%
  select(!`Type of Admission`) %>%
  rename("n_deaths" = Value)
