library(tidyverse)
library(lubridate)
library(augsynth)

# Loading in prepared RDS & filtering to Kenema district
sierra_leone <- readRDS("data/sierra_leone_combined.RDS")

# Getting unique list of facilities
facilities <- sierra_leone %>%
  dplyr::select(
    district_orgunitid,
    district_ou_name,
    organisationunitid,
    name
  ) %>%
  dplyr::distinct()

# Getting unique list of quarters & months
quarters <- data.frame(
  quarter = as.Date(
    c(
      "2017-01-01", "2017-04-01", "2017-07-01", "2017-10-01",
      "2018-01-01", "2018-04-01", "2018-07-01", "2018-10-01",
      "2019-01-01", "2019-04-01", "2019-07-01", "2019-10-01",
      "2020-01-01", "2020-04-01", "2020-07-01", "2020-10-01"
      )
    )
)

months <- data.frame(month = seq.Date(as.Date("2017-01-01"), as.Date("2020-12-01"), by = "month"))

# Merging the two together
treatment_data <- merge(facilities, months)
treatment_data$live_births <- NA
treatment_data$low_birth_weights <- NA

# Selecting a dozen facilities at random to use for treatment group
treatment_orgunitids <- dplyr::slice_sample(facilities, n = 24) %>% 
  dplyr::select(organisationunitid) 

# Creating treatment and control groups & time periods
treatment_data$treatment_group <- ifelse(treatment_data$organisationunitid %in% treatment_orgunitids$organisationunitid, "Treatment", "Control")
treatment_data$treated <- ifelse(treatment_data$treatment_group == "Treatment" & treatment_data$month >= "2019-07-01", 1, 0)

# Saving wire frame data set
saveRDS(treatment_data, "data/treatment_data.RDS")
