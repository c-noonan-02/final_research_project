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
library(hms)

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


##### Presence/Absence Summary #####
# this might be better at the end, with all sampling designs in one for ease
# but doing by each division for now

# list of all devices and all species
sites <- unique(BD_pilot_data$site)
audiomoths <- unique(BD_pilot_data$audiomoth_ID)
species_c <- unique(BD_pilot_data$common_n)
species_s <- unique(BD_pilot_data$scientific_n)

# create grid of all combinations
full_grid <- expand.grid(site = sites, audiomoth_ID = audiomoths, common_n = species_c, scientific_n = species_s)

# summarise presence/absences
BD_pilot_days_pa <- BD_pilot_days %>% 
  group_by(site, audiomoth_ID, common_n, scientific_n) %>% 
  summarise(
    one_day_sample = as.integer((any(one_day))),
    two_day_sample = as.integer((any(two_day))),
    three_day_sample = as.integer((any(three_day))),
    .groups = "drop"
  )

# merge with the full grid to record 'missing' species as absences
BD_pilot_days_pa <- full_grid %>% 
  left_join(BD_pilot_days_pa, by = c("site", "audiomoth_ID", "common_n", "scientific_n")) %>% 
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
  group_by(site, audiomoth_ID, common_n, scientific_n) %>% 
  summarise(
    one_day_sample = sum(one_day),
    two_day_sample = sum(two_day),
    three_day_sample = sum(three_day),
    .groups = "drop"
  )

# merge with the full grid to record 'missing' species as absences
BD_pilot_days_ab <- full_grid %>% 
  left_join(BD_pilot_days_ab, by = c("site", "audiomoth_ID", "common_n", "scientific_n")) %>% 
  mutate(
    one_day_sample = replace_na(one_day_sample, 0),
    two_day_sample = replace_na(two_day_sample, 0),
    three_day_sample = replace_na(three_day_sample, 0)
  )

# check dataframe
View(BD_pilot_days_ab)




#### Sub-samples to assess recording periods ####

BD_pilot_period <- BD_pilot_data

# list all detection times
unique(BD_pilot_period$detect_start_time)


##### Dawn-only (7:00-9:00) #####

# add logical column in main data frame to highlight rows which were recorded within a dawn survey
BD_pilot_period <- BD_pilot_period %>% 
  mutate(dawn = detect_start_time >= "07:00:00" & detect_start_time < "09:00:00")

# check dataset
View(BD_pilot_period)
unique(BD_pilot_period$dawn)

##### Dusk-only (20:00 - 22:00) #####

# add logical column in main data frame to highlight rows which were recorded within a dusk survey
BD_pilot_period <- BD_pilot_period %>% 
  mutate(dusk = detect_start_time >= "20:00:00" & detect_start_time < "22:00:00")

# check dataset
View(BD_pilot_period)
unique(BD_pilot_period$dusk)

##### All day (00:00 - 24:00) #####

# add logical column in main data frame to highlight rows which were recorded within the full 24hrs
BD_pilot_period$all_day <- TRUE

# check dataset
View(BD_pilot_period)
unique(BD_pilot_period$all_day)

##### Presence/Absence Summary #####
# this might be better at the end, with all sampling designs in one for ease
# but doing by each division for now

# summarise presence/absences
BD_pilot_period_pa <- BD_pilot_period %>% 
  group_by(site, audiomoth_ID, common_n, scientific_n) %>% 
  summarise(
    dawn_sample = as.integer((any(dawn))),
    dusk_sample = as.integer((any(dusk))),
    day_sample = as.integer((any(all_day))),
    .groups = "drop"
  )

# merge with the full grid to record 'missing' species as absences
BD_pilot_period_pa <- full_grid %>% 
  left_join(BD_pilot_period_pa, by = c("site", "audiomoth_ID", "common_n", "scientific_n")) %>% 
  mutate(
    dawn_sample = replace_na(dawn_sample, 0),
    dusk_sample = replace_na(dusk_sample, 0),
    day_sample = replace_na(day_sample, 0)
  )

# check dataframe
View(BD_pilot_days_pa)



##### No. Detections Summary #####

# summarise detection counts
BD_pilot_period_ab <- BD_pilot_period %>% 
  group_by(site, audiomoth_ID, common_n, scientific_n) %>% 
  summarise(
    dusk_sample = sum(dusk),
    dawn_sample = sum(dawn),
    day_sample = sum(all_day),
    .groups = "drop"
  )

# merge with the full grid to record 'missing' species as absences
BD_pilot_period_ab <- full_grid %>% 
  left_join(BD_pilot_period_ab, by = c("site", "audiomoth_ID", "common_n", "scientific_n")) %>% 
  mutate(
    dawn_sample = replace_na(dawn_sample, 0),
    dusk_sample = replace_na(dusk_sample, 0),
    day_sample = replace_na(day_sample, 0)
  )

# check dataframe
View(BD_pilot_period_ab)




#### Sub-samples to assess sampling schedule ####

BD_pilot_sched <- BD_pilot_data

##### 5mins/hour #####

# add logical column in main data frame to highlight rows which were recorded in the first 5 mins of every hour
BD_pilot_sched <- BD_pilot_sched %>% 
  mutate(five_mins = lubridate::minute(hms::as_hms(detect_start_time)) < 5)

# check dataset
View(BD_pilot_sched)
unique(BD_pilot_sched$five_mins)

##### 10mins/hour #####

# add logical column in main data frame to highlight rows which were recorded in the first 10 mins of every hour
BD_pilot_sched <- BD_pilot_sched %>% 
  mutate(ten_mins = lubridate::minute(hms::as_hms(detect_start_time)) < 10)

# check dataset
View(BD_pilot_sched)
unique(BD_pilot_sched$ten_mins)

##### 15mins/hour #####

# add logical column in main data frame to highlight rows which were recorded in the first 15 mins of every hour
BD_pilot_sched <- BD_pilot_sched %>% 
  mutate(fithtn_mins = lubridate::minute(hms::as_hms(detect_start_time)) < 15)

# check dataset
View(BD_pilot_sched)
unique(BD_pilot_sched$fithtn_mins)

##### 30mins/hour #####

# add logical column in main data frame to highlight rows which were recorded in the first 30 mins of every hour
BD_pilot_sched <- BD_pilot_sched %>% 
  mutate(thirty_mins = lubridate::minute(hms::as_hms(detect_start_time)) < 30)

# check dataset
View(BD_pilot_sched)
unique(BD_pilot_sched$thirty_mins)

##### 60mins/hour #####

# add logical column in main data frame to highlight rows which were recorded using continuous recording (full hour)

BD_pilot_sched$full_hour <- TRUE

# check dataset
View(BD_pilot_sched)
unique(BD_pilot_sched$full_hour)


##### Presence/Absence Summary #####
# this might be better at the end, with all sampling designs in one for ease
# but doing by each division for now

# summarise presence/absences
BD_pilot_sched_pa <- BD_pilot_sched %>% 
  group_by(site, audiomoth_ID, common_n, scientific_n) %>% 
  summarise(
    five_mins = as.integer((any(five_mins))),
    ten_mins = as.integer((any(ten_mins))),
    fithtn_mins = as.integer((any(fithtn_mins))),
    thirty_mins = as.integer((any(thirty_mins))),
    full_hour = as.integer((any(full_hour))),
    .groups = "drop"
  )

# merge with the full grid to record 'missing' species as absences
BD_pilot_sched_pa <- full_grid %>% 
  left_join(BD_pilot_sched_pa, by = c("site", "audiomoth_ID", "common_n", "scientific_n")) %>% 
  mutate(
    five_mins = replace_na(five_mins, 0),
    ten_mins = replace_na(ten_mins, 0),
    fithtn_mins = replace_na(fithtn_mins, 0),
    thirty_mins = replace_na(thirty_mins, 0),
    full_hour = replace_na(full_hour, 0)
  )

# check dataframe
View(BD_pilot_sched_pa)



##### No. Detections Summary #####

# summarise detection counts
BD_pilot_sched_ab <- BD_pilot_sched %>% 
  group_by(site, audiomoth_ID, common_n, scientific_n) %>% 
  summarise(
    five_mins = sum(five_mins),
    ten_mins = sum(ten_mins),
    fithtn_mins = sum(fithtn_mins),
    thirty_mins = sum(thirty_mins),
    full_hour = sum(full_hour),
    .groups = "drop"
  )

# merge with the full grid to record 'missing' species as absences
BD_pilot_sched_ab <- full_grid %>% 
  left_join(BD_pilot_sched_ab, by = c("site", "audiomoth_ID", "common_n", "scientific_n")) %>% 
  mutate(
    five_mins = replace_na(five_mins, 0),
    ten_mins = replace_na(ten_mins, 0),
    fithtn_mins = replace_na(fithtn_mins, 0),
    thirty_mins = replace_na(thirty_mins, 0),
    full_hour = replace_na(full_hour, 0)
  )

# check dataframe
View(BD_pilot_sched_ab)
