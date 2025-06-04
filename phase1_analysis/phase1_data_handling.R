# clear environment
rm(list=ls())

# load required packages
library(dplyr)
library(tidyr)
# library(stringr)
# library(tools)
# library(readr)
library(readxl)
library(writexl)
# library(tidyverse)

# import data set
BD_pilot_data <- read_xlsx("./audiomoth_data/BD2025_BirdNETOutput2.xlsx") # times preserved in xlsx format
head(BD_pilot_data)


#### Sub-samples to assess distances between devices ####

BD_pilot_dist <- BD_pilot_data

##### Devices 50m apart #####

##### Devices 100m apart #####

##### Devices 150m apart #####

##### Devices 200m apart #####


#### Sub-samples to assess number of days recording ####

BD_pilot_days <- BD_pilot_data

# check all unique recording dates and times
unique(BD_pilot_days$recording_date)
unique(BD_pilot_days$recording_time)

# try and subset all data recorded on one date
practice_subset <- BD_pilot_days %>% 
  filter(recording_date == as.Date("2025-05-15"), recording_time == "000000")

# generate combined key for easier matching of dates and times
BD_pilot_days <- BD_pilot_days %>%
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
BD_pilot_days <- BD_pilot_days %>% 
  mutate(one_day = join_key %in% one_day_times$join_key)

# check some have been labelled true and some false
unique(BD_pilot_days$one_day)


# then generate dataset containing only recordings made from a recording schedule of one day
BD_pilot_oneday <- BD_pilot_days %>%
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

# unique woodland species
BD_pilot_oneday %>%
  filter(site == "BDWD") %>%
  summarise(unique_species = n_distinct(scientific_n))

# unique moorland species
BD_pilot_oneday %>%
  filter(site == "BDMD") %>%
  summarise(unique_species = n_distinct(scientific_n))




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
BD_pilot_days <- BD_pilot_days %>% 
  mutate(two_day = join_key %in% two_day_times$join_key)

# check some have been labelled true and some false
unique(BD_pilot_days$two_day)


# then generate dataset containing only recordings made from a recording schedule of one day
BD_pilot_twoday <- BD_pilot_days %>%
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

# unique woodland species
BD_pilot_twoday %>%
  filter(site == "BDWD") %>%
  summarise(unique_species = n_distinct(scientific_n))

# unique moorland species
BD_pilot_twoday %>%
  filter(site == "BDMD") %>%
  summarise(unique_species = n_distinct(scientific_n))



##### Three days #####

# mark all data as part of the three day recording schedule
BD_pilot_days$three_day <- TRUE

# save whole data set as the data from three-day recording schedule
BD_pilot_threeday <- BD_pilot_days
# remove obselete rows
BD_pilot_threeday <- BD_pilot_threeday %>% 
  select(-join_key, -one_day, -two_day)
# check dataset
head(BD_pilot_threeday)

# generate one_day species list
three_day_species <- unique(BD_pilot_threeday$common_n)
three_day_speciesno <- n_distinct(BD_pilot_threeday$scientific_n)

# unique woodland species
BD_pilot_threeday %>%
  filter(site == "BDWD") %>%
  summarise(unique_species = n_distinct(scientific_n))

# unique moorland species
BD_pilot_threeday %>%
  filter(site == "BDMD") %>%
  summarise(unique_species = n_distinct(scientific_n))


##### Presence/Absence Summary #####
# this might be better at the end, with all sampling designs in one for ease
# but doing by each division for now

# list of all devices and all species
audiomoths <- unique(BD_pilot_days$audiomoth_ID)
species_c <- unique(BD_pilot_days$common_n)
species_s <- unique(BD_pilot_days$scientific_n)

# create grid of all combinations
full_grid <- expand.grid(audiomoth_ID = audiomoths, common_n = species_c, scientific_n = species_s)

# summarise presence/absences
BD_pilot_days_pa <- BD_pilot_days %>% 
  group_by(audiomoth_ID, common_n, scientific_n) %>% 
  summarise(
    one_day_sample = as.integer((any(one_day))),
    two_day_sample = as.integer((any(two_day))),
    three_day_sample = as.integer((any(three_day))),
    .groups = "drop"
  )

# merge with the full grid to record 'missing' species as absences
BD_pilot_days_pa <- full_grid %>% 
  left_join(BD_pilot_days_pa, by = c("audiomoth_ID", "common_n", "scientific_n")) %>% 
  mutate(
    one_day_sample = replace_na(one_day_sample, 0),
    two_day_sample = replace_na(two_day_sample, 0),
    three_day_sample = replace_na(three_day_sample, 0)
  )

# check dataframe
View(BD_pilot_days_pa)



##### No. Detections Summary #####

# summarise detection counts
BD_pilot_days_ab <- BD_pilot_days %>% 
  group_by(audiomoth_ID, common_n, scientific_n) %>% 
  summarise(
    one_day_sample = sum(one_day),
    two_day_sample = sum(two_day),
    three_day_sample = sum(three_day),
    .groups = "drop"
  )

# merge with the full grid to record 'missing' species as absences
BD_pilot_days_ab <- full_grid %>% 
  left_join(BD_pilot_days_ab, by = c("audiomoth_ID", "common_n", "scientific_n")) %>% 
  mutate(
    one_day_sample = replace_na(one_day_sample, 0),
    two_day_sample = replace_na(two_day_sample, 0),
    three_day_sample = replace_na(three_day_sample, 0)
  )

# check dataframe
View(BD_pilot_days_ab)




#### Sub-samples to assess recording periods ####

BD_pilot_period <- BD_pilot_data

##### Dawn-only (7:00-9:00) #####

##### Dusk-only (20:00 - 22:00) #####

##### All day (00:00 - 24:00) #####


#### Sub-samples to assess sampling schedule ####

BD_pilot_sched <- BD_pilot_data

##### 5mins/hour #####

##### 10mins/hour #####

##### 15mins/hour #####

##### 30mins/hour #####

##### 60mins/hour #####