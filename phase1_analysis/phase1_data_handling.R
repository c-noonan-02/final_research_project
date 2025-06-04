# clear environment
rm(list=ls())

# load required packages
library(dplyr)
# library(stringr)
# library(tools)
# library(readr)
library(readxl)
# library(writexl)
# library(tidyverse)

# import data set
BD_pilot_data <- read_xlsx("./audiomoth_data/BD2025_BirdNETOutput2.xlsx") # times preserved in xlsx format
head(BD_pilot_data)


#### Sub-samples to assess distances between devices ####

##### Devices 50m apart #####

##### Devices 100m apart #####

##### Devices 150m apart #####

##### Devices 200m apart #####


#### Sub-samples to assess number of days recording ####

# check all unique recording dates and times
unique(BD_pilot_data$recording_date)
unique(BD_pilot_data$recording_time)

# try and subset all data recorded on one date
practice_subset <- BD_pilot_data %>% 
  filter(recording_date == as.Date("2025-05-15"), recording_time == "000000")

# generate combined key for easier matching of dates and times
BD_pilot_data <- BD_pilot_data %>%
  mutate(join_key = paste(recording_date, recording_time))

##### One day #####

# generate reference table for all recordings which would be covered by recording day one
one_day_times <- tibble::tibble(
  
  # specify recording dates
  recording_date = as.Date(c("2025-05-15", "2025-05-15", "2025-05-15", "2025-05-15", "2025-05-16", "2025-05-16", "2025-05-16", "2025-05-16", "2025-05-16", "2025-05-16", # day one in woodland
                             "2025-05-21", "2025-05-21", "2025-05-21", "2025-05-21", "2025-05-22", "2025-05-22", "2025-05-22", "2025-05-22", "2025-05-22", "2025-05-22")), # day one in moorland
  
  # specify their matching times
  recording_time = c("000000", "000001", "000002", "000003", "122541", "122543", "122542", "122544", "122545", "122540", # woodland
                     "000000", "000001", "000002", "000003", "122541", "122543", "122542", "122544", "122545", "122540")) # moorland

# add the same join key in the reference list
one_day_times <- one_day_times %>%
  mutate(join_key = paste(recording_date, recording_time))

# add logical column in main dataframe to highlight rows which were recorded within day one
BD_pilot_data <- BD_pilot_data %>% 
  mutate(one_day = join_key %in% one_day_times$join_key)

# check some have been labelled true and some false
unique(BD_pilot_data$one_day)


# then generate dataset containing only recordings made from a recording schedule of one day
BD_pilot_oneday <- BD_pilot_data %>%
  filter(one_day == TRUE)
# check it has worked
unique(BD_pilot_oneday$one_day)
# remove obselete rows
BD_pilot_oneday <- BD_pilot_oneday %>% 
  select(-join_key, -one_day)
# check dataset
head(BD_pilot_oneday)
unique(BD_pilot_oneday$site)
unique(BD_pilot_oneday$audiomoth_ID)

# generate one_day species list
one_day_species <- unique(BD_pilot_oneday$scientific_n)
one_day_speciesno <- n_distinct(BD_pilot_oneday$scientific_n)




##### Two days #####

# generate reference table for all recordings which would be covered by recording day one
two_day_times <- tibble::tibble(
  
  # specify recording dates
  recording_date = as.Date(c("2025-05-15", "2025-05-15", "2025-05-15", "2025-05-15", "2025-05-16", "2025-05-16", "2025-05-16", "2025-05-16", "2025-05-16", "2025-05-16", # day one in woodland
                             "2025-05-21", "2025-05-21", "2025-05-21", "2025-05-21", "2025-05-22", "2025-05-22", "2025-05-22", "2025-05-22", "2025-05-22", "2025-05-22", # day one in moorland
                             "2025-05-16", "2025-05-16", "2025-05-16", "2025-05-16", "2025-05-17", "2025-05-17", "2025-05-17", "2025-05-17", "2025-05-17", "2025-05-17", # day two in woodland
                             "2025-05-22", "2025-05-22", "2025-05-22", "2025-05-22", "2025-05-23", "2025-05-23", "2025-05-23", "2025-05-23", "2025-05-23", "2025-05-23")), # day two in moorland
  
  # specify their matching times
  recording_time = c("000000", "000001", "000002", "000003", "122541", "122543", "122542", "122544", "122545", "122540", # woodland
                     "000000", "000001", "000002", "000003", "122541", "122543", "122542", "122544", "122545", "122540", # moorland
                     "000000", "000001", "000002", "000003", "122541", "122543", "122542", "122544", "122545", "122540", # woodland
                     "000000", "000001", "000002", "000003", "122541", "122543", "122542", "122544", "122545", "122540")) # moorland

# add the same join key in the reference list
two_day_times <- two_day_times %>%
  mutate(join_key = paste(recording_date, recording_time))

# add logical column in main dataframe to highlight rows which were recorded within day one
BD_pilot_data <- BD_pilot_data %>% 
  mutate(two_day = join_key %in% two_day_times$join_key)

# check some have been labelled true and some false
unique(BD_pilot_data$two_day)


# then generate dataset containing only recordings made from a recording schedule of one day
BD_pilot_twoday <- BD_pilot_data %>%
  filter(two_day == TRUE)
# check it has worked
unique(BD_pilot_twoday$two_day)
# remove obselete rows
BD_pilot_twoday <- BD_pilot_twoday %>% 
  select(-join_key, -one_day, -two_day)
# check dataset
head(BD_pilot_twoday)
unique(BD_pilot_twoday$site)
unique(BD_pilot_twoday$audiomoth_ID)

# generate one_day species list
two_day_species <- unique(BD_pilot_twoday$scientific_n)
two_day_speciesno <- n_distinct(BD_pilot_twoday$scientific_n)




##### Three days #####

# save whole data set as the data from three-day recording schedule
BD_pilot_threeday <- BD_pilot_data
# remove obselete rows
BD_pilot_threeday <- BD_pilot_threeday %>% 
  select(-join_key, -one_day, -two_day)
# check dataset
head(BD_pilot_threeday)

# generate one_day species list
three_day_species <- unique(BD_pilot_threeday$scientific_n)
three_day_speciesno <- n_distinct(BD_pilot_threeday$scientific_n)




#### Sub-samples to assess recording periods ####

##### Dawn-only (7:00-9:00) #####

##### Dusk-only (20:00 - 22:00) #####

##### All day (00:00 - 24:00) #####


#### Sub-samples to assess sampling schedule ####

##### 5mins/hour #####

##### 10mins/hour #####

##### 15mins/hour #####

##### 30mins/hour #####

##### 60mins/hour #####