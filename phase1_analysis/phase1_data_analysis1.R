# This R script contains all code relating to sub-sets of the phase one data set
# used to assess the differences in the number of species detected when using
# different 'sampling effort' on different spatial and temporal scales, in
# isolation from each other.

# clear environment
rm(list=ls())

# load required packages
library(dplyr)
library(tidyr)
library(readxl)
library(writexl)
library(hms)
library(ggplot2)
library(cowplot)

# import data set
BD_pilot_data <- read_xlsx("./audiomoth_data/BD2025_BirdNETOutput2.xlsx") # times preserved in xlsx format
head(BD_pilot_data)


#### Sub-samples to assess distances between devices ####

BD_pilot_dist <- BD_pilot_data


##### Devices 200m apart #####

# generate reference table for all recordings which would be obtained with 200m spacing
combo_200 <- tibble::tibble(
  
  # specify sites
  site = c("BDWD", "BDWD", "BDWD", "BDWD", "BDWD", "BDWD", "BDWD", "BDWD", # woodland
           
           "BDMD", "BDMD", "BDMD", "BDMD"), # moorland
  
  # specify their devices
  audiomoth_ID = c("Audiomoth_2", "Audiomoth_14", "Audiomoth_9", "Audiomoth_3", # woodland
                   "Audiomoth_15", "Audiomoth_16", "Audiomoth_4", "Audiomoth_18", # woodland
                   
                     "Audiomoth_16", "Audiomoth_2", "Audiomoth_5", "Audiomoth_3"), # moorland
  
  # pair audiomoths that are 200m apart
  dist_200 = c("200_pair1", "200_pair1", "200_pair2", "200_pair2", # woodland
               "200_pair3", "200_pair3", "200_pair4", "200_pair4", # woodland
               
              "200_pair5", "200_pair5", "200_pair6", "200_pair6")) # moorland

# add logical column in main dataframe to highlight rows which would be obtained with 200m spacing
BD_pilot_dist <- BD_pilot_dist %>% 
  left_join(combo_200, by = c("site", "audiomoth_ID"))

# check data
View(BD_pilot_dist)
unique(BD_pilot_dist$dist_200)


##### Devices 150m apart #####

# generate reference table for all recordings which would be obtained with 200m spacing
combo_150 <- tibble::tibble(
  
  # specify sites
  site = c("BDWD", "BDWD", "BDWD", "BDWD", "BDWD", "BDWD", "BDWD", "BDWD", # woodland
           "BDWD", "BDWD", "BDWD", "BDWD", "BDWD", "BDWD", "BDWD", "BDWD", # woodland
           "BDWD", "BDWD", "BDWD", "BDWD", "BDWD", "BDWD", "BDWD", "BDWD", # woodland
           "BDWD", "BDWD", # woodland
           
           "BDMD", "BDMD", "BDMD", "BDMD", "BDMD", "BDMD", "BDMD", "BDMD", # moorland
           "BDMD", "BDMD", "BDMD", "BDMD", "BDMD", "BDMD", "BDMD", "BDMD"), # moorland
  
  # specify their devices
  audiomoth_ID = c("Audiomoth_2", "Audiomoth_10", "Audiomoth_9", "Audiomoth_8", # woodland
                   "Audiomoth_15", "Audiomoth_20", "Audiomoth_4", "Audiomoth_5", # woodland
                   "Audiomoth_13", "Audiomoth_14", "Audiomoth_12", "Audiomoth_3", # woodland
                   "Audiomoth_1", "Audiomoth_16", "Audiomoth_7", "Audiomoth_18", # woodland
                   "Audiomoth_2", "Audiomoth_4", "Audiomoth_13", "Audiomoth_7", # woodland
                   "Audiomoth_6", "Audiomoth_19", "Audiomoth_10", "Audiomoth_5", # woodland
                   "Audiomoth_14", "Audiomoth_18", # woodland
                   
                   "Audiomoth_12", "Audiomoth_7", "Audiomoth_16", "Audiomoth_14", # moorland
                   "Audiomoth_4", "Audiomoth_15", "Audiomoth_6", "Audiomoth_2", # moorland 
                   "Audiomoth_12", "Audiomoth_5", "Audiomoth_11", "Audiomoth_20", # moorland
                   "Audiomoth_13", "Audiomoth_17", "Audiomoth_6", "Audiomoth_2"), # moorland
  
  
  # pair audiomoths that are 200m apart
  dist_150 = c("150_pair1", "150_pair1", "150_pair2", "150_pair2", # woodland
               "150_pair3", "150_pair3", "150_pair4", "150_pair4", # woodland
               "150_pair5", "150_pair5", "150_pair6", "150_pair6", # woodland
               "150_pair7", "150_pair7", "150_pair8", "150_pair8", # woodland
               "150_pair9", "150_pair9", "150_pair10", "150_pair10", # woodland
               "150_pair11", "150_pair11", "150_pair12", "150_pair12", # woodland
               "150_pair13", "150_pair13", # woodland
               
               "150_pair14", "150_pair14", "150_pair15", "150_pair15", # moorland
               "150_pair16", "150_pair16", "150_pair17", "150_pair17", # moorland
               "150_pair18", "150_pair18", "150_pair19", "150_pair19", # moorland
               "150_pair20", "150_pair20", "150_pair21", "150_pair21")) # moorland

# add logical column in main dataframe to highlight rows which would be obtained with 200m spacing
BD_pilot_dist <- BD_pilot_dist %>% 
  left_join(combo_150, by = c("site", "audiomoth_ID"))

# check data
View(BD_pilot_dist)
unique(BD_pilot_dist$dist_150)


##### Devices 100m apart #####

# generate reference table for all recordings which would be obtained with 200m spacing
combo_100 <- tibble::tibble(
  
  # specify sites
  site = c("BDWD", "BDWD", "BDWD", "BDWD", "BDWD", "BDWD", "BDWD", "BDWD", # woodland
           "BDWD", "BDWD", "BDWD", "BDWD", "BDWD", "BDWD", "BDWD", "BDWD", # woodland
           "BDWD", "BDWD", "BDWD", "BDWD", "BDWD", "BDWD", "BDWD", "BDWD", # woodland
           "BDWD", "BDWD", "BDWD", "BDWD", "BDWD", "BDWD", "BDWD", "BDWD", # woodland
           "BDWD", "BDWD", "BDWD", "BDWD", "BDWD", "BDWD", "BDWD", "BDWD", # woodland
           "BDWD", "BDWD", "BDWD", "BDWD", # woodland
           
           "BDMD", "BDMD", "BDMD", "BDMD", "BDMD", "BDMD", "BDMD", "BDMD", # moorland
           "BDMD", "BDMD", "BDMD", "BDMD", "BDMD", "BDMD", "BDMD", "BDMD", # moorland
           "BDMD", "BDMD", "BDMD", "BDMD", "BDMD", "BDMD", "BDMD", "BDMD", # moorland
           "BDMD", "BDMD", "BDMD", "BDMD", "BDMD", "BDMD", "BDMD", "BDMD", # moorland
           "BDMD", "BDMD"), # moorland
  
  # specify their devices
  audiomoth_ID = c("Audiomoth_2", "Audiomoth_6", "Audiomoth_9", "Audiomoth_11", # woodland
                   "Audiomoth_15", "Audiomoth_17", "Audiomoth_4", "Audiomoth_19", # woodland
                   "Audiomoth_13", "Audiomoth_10", "Audiomoth_12", "Audiomoth_8", # woodland
                   "Audiomoth_1", "Audiomoth_20", "Audiomoth_7", "Audiomoth_5", # woodland
                   "Audiomoth_6", "Audiomoth_14", "Audiomoth_11", "Audiomoth_3", # woodland
                   "Audiomoth_17", "Audiomoth_16", "Audiomoth_19", "Audiomoth_18", # woodland
                   "Audiomoth_2", "Audiomoth_15", "Audiomoth_9", "Audiomoth_4", # woodland
                   "Audiomoth_13", "Audiomoth_1", "Audiomoth_12", "Audiomoth_7", # woodland
                   "Audiomoth_6", "Audiomoth_17", "Audiomoth_11", "Audiomoth_19", # woodland
                   "Audiomoth_10", "Audiomoth_20", "Audiomoth_8", "Audiomoth_5", # woodland
                   "Audiomoth_14", "Audiomoth_16", "Audiomoth_3", "Audiomoth_18", # woodland
                   
                   
                   "Audiomoth_12", "Audiomoth_13", "Audiomoth_16", "Audiomoth_18", # moorland
                   "Audiomoth_4", "Audiomoth_10", "Audiomoth_5", "Audiomoth_17", # moorland
                   "Audiomoth_11", "Audiomoth_7", "Audiomoth_6", "Audiomoth_14", # moorland
                   "Audiomoth_1", "Audiomoth_15", "Audiomoth_18", "Audiomoth_2", # moorland
                   "Audiomoth_17", "Audiomoth_3","Audiomoth_12", "Audiomoth_4", # moorland
                   "Audiomoth_16", "Audiomoth_5", "Audiomoth_11", "Audiomoth_1", # moorland
                   "Audiomoth_6", "Audiomoth_20", "Audiomoth_13", "Audiomoth_10", # moorland
                   "Audiomoth_18", "Audiomoth_17", "Audiomoth_7", "Audiomoth_15", # moorland
                   "Audiomoth_2", "Audiomoth_3"), # moorland

  # pair audiomoths that are 200m apart
  dist_100 = c("100_pair1", "100_pair1", "100_pair2", "100_pair2", # woodland
               "100_pair3", "100_pair3", "100_pair4", "100_pair4", # woodland
               "100_pair5", "100_pair5", "100_pair6", "100_pair6", # woodland
               "100_pair7", "100_pair7", "100_pair8", "100_pair8", # woodland
               "100_pair9", "100_pair9", "100_pair10", "100_pair10", # woodland
               "100_pair11", "100_pair11", "100_pair12", "100_pair12", # woodland
               "100_pair13", "100_pair13", "100_pair14", "100_pair14", # woodland
               "100_pair15", "100_pair15", "100_pair16", "100_pair16", # woodland
               "100_pair17", "100_pair17", "100_pair18", "100_pair18", # woodland
               "100_pair19", "100_pair19", "100_pair20", "100_pair20", # woodland
               "100_pair21", "100_pair21", "100_pair22", "100_pair22", # woodland
               
               "100_pair23", "100_pair23", "100_pair24", "100_pair24", # moorland
               "100_pair25", "100_pair25", "100_pair26", "100_pair26", # moorland
               "100_pair27", "100_pair27", "100_pair28", "100_pair28", # moorland
               "100_pair29", "100_pair29", "100_pair30", "100_pair30", # moorland
               "100_pair31", "100_pair31", "100_pair32", "100_pair32", # moorland
               "100_pair33", "100_pair33", "100_pair34", "100_pair34", # moorland
               "100_pair35", "100_pair35", "100_pair36", "100_pair36", # moorland
               "100_pair37", "100_pair37", "100_pair38", "100_pair38", # moorland
               "100_pair39", "100_pair39")) # moorland

# add logical column in main dataframe to highlight rows which would be obtained with 100m spacing
# duplicate rows for any device in multiple pairs
BD_pilot_dist <- BD_pilot_dist %>% 
  left_join(combo_100, by = c("site", "audiomoth_ID"))

# check data
View(BD_pilot_dist)
unique(BD_pilot_dist$dist_100)


##### Devices 50m apart #####

# generate reference table for all recordings which would be obtained with 200m spacing
combo_50 <- tibble::tibble(
  
  # specify sites
  site = c("BDWD", "BDWD", "BDWD", "BDWD", "BDWD", "BDWD", "BDWD", "BDWD", # woodland
           "BDWD", "BDWD", "BDWD", "BDWD", "BDWD", "BDWD", "BDWD", "BDWD", # woodland
           "BDWD", "BDWD", "BDWD", "BDWD", "BDWD", "BDWD", "BDWD", "BDWD", # woodland
           "BDWD", "BDWD", "BDWD", "BDWD", "BDWD", "BDWD", "BDWD", "BDWD", # woodland
           "BDWD", "BDWD", "BDWD", "BDWD", "BDWD", "BDWD", "BDWD", "BDWD", # woodland
           "BDWD", "BDWD", "BDWD", "BDWD", "BDWD", "BDWD", "BDWD", "BDWD", # woodland
           "BDWD", "BDWD", "BDWD", "BDWD", "BDWD", "BDWD", "BDWD", "BDWD", # woodland
           "BDWD", "BDWD", "BDWD", "BDWD", "BDWD", "BDWD", # woodland
           
           "BDMD", "BDMD", "BDMD", "BDMD", "BDMD", "BDMD", "BDMD", "BDMD", # moorland
           "BDMD", "BDMD", "BDMD", "BDMD", "BDMD", "BDMD", "BDMD", "BDMD", # moorland
           "BDMD", "BDMD", "BDMD", "BDMD", "BDMD", "BDMD", "BDMD", "BDMD", # moorland
           "BDMD", "BDMD", "BDMD", "BDMD", "BDMD", "BDMD", "BDMD", "BDMD", # moorland
           "BDMD", "BDMD", "BDMD", "BDMD", "BDMD", "BDMD", "BDMD", "BDMD", # moorland
           "BDMD", "BDMD", "BDMD", "BDMD", "BDMD", "BDMD"), # moorland
  
  # specify their devices
  audiomoth_ID = c("Audiomoth_2", "Audiomoth_6", "Audiomoth_9", "Audiomoth_11", # woodland
                   "Audiomoth_15", "Audiomoth_17", "Audiomoth_4", "Audiomoth_19", # woodland
                   "Audiomoth_13", "Audiomoth_6", "Audiomoth_12", "Audiomoth_11", # woodland
                   "Audiomoth_1", "Audiomoth_17", "Audiomoth_7", "Audiomoth_19", # woodland
                   "Audiomoth_6", "Audiomoth_10", "Audiomoth_11", "Audiomoth_8", # woodland
                   "Audiomoth_17", "Audiomoth_20", "Audiomoth_19", "Audiomoth_5", # woodland
                   "Audiomoth_10", "Audiomoth_14", "Audiomoth_8", "Audiomoth_3", # woodland
                   "Audiomoth_20", "Audiomoth_16", "Audiomoth_5", "Audiomoth_18", # woodland
                   "Audiomoth_2", "Audiomoth_9", "Audiomoth_9", "Audiomoth_15", # woodland
                   "Audiomoth_15", "Audiomoth_4", "Audiomoth_13", "Audiomoth_12", # woodland
                   "Audiomoth_12", "Audiomoth_1", "Audiomoth_1", "Audiomoth_7", # woodland
                   "Audiomoth_6", "Audiomoth_11", "Audiomoth_11", "Audiomoth_17", # woodland
                   "Audiomoth_17", "Audiomoth_19", "Audiomoth_10", "Audiomoth_8", # woodland
                   "Audiomoth_8", "Audiomoth_20", "Audiomoth_20", "Audiomoth_5", # woodland
                   "Audiomoth_14", "Audiomoth_3", "Audiomoth_3", "Audiomoth_16", # woodland
                   "Audiomoth_16", "Audiomoth_18", # woodland
                   
                   
                   "Audiomoth_12", "Audiomoth_11", "Audiomoth_16", "Audiomoth_6", # moorland
                   "Audiomoth_4", "Audiomoth_1", "Audiomoth_5", "Audiomoth_20", # moorland
                   "Audiomoth_11", "Audiomoth_13", "Audiomoth_6", "Audiomoth_18", # moorland
                   "Audiomoth_1", "Audiomoth_10", "Audiomoth_20", "Audiomoth_17", # moorland
                   "Audiomoth_13", "Audiomoth_7", "Audiomoth_18", "Audiomoth_14", # moorland
                   "Audiomoth_10", "Audiomoth_15", "Audiomoth_14", "Audiomoth_2", # moorland
                   "Audiomoth_12", "Audiomoth_16", "Audiomoth_16", "Audiomoth_4", # moorland
                   "Audiomoth_4", "Audiomoth_5", "Audiomoth_11", "Audiomoth_6", # moorland
                   "Audiomoth_6", "Audiomoth_1", "Audiomoth_1", "Audiomoth_20", # moorland
                   "Audiomoth_13", "Audiomoth_18", "Audiomoth_18", "Audiomoth_10", # moorland
                   "Audiomoth_10", "Audiomoth_17", "Audiomoth_7", "Audiomoth_14", # moorland
                   "Audiomoth_14", "Audiomoth_15"), # moorland
                   
                   
                   
  
  # pair audiomoths that are 200m apart
  dist_50 = c("50_pair1", "50_pair1", "50_pair2", "50_pair2", # woodland
              "50_pair3", "50_pair3", "50_pair4", "50_pair4", # woodland
              "50_pair5", "50_pair5", "50_pair6", "50_pair6", # woodland
              "50_pair7", "50_pair7", "50_pair8", "50_pair8", # woodland
              "50_pair9", "50_pair9", "50_pair10", "50_pair10", # woodland
              "50_pair11", "50_pair11", "50_pair12", "50_pair12", # woodland
              "50_pair13", "50_pair13", "50_pair14", "50_pair14", # woodland 
              "50_pair15", "50_pair15", "50_pair16", "50_pair16", # woodland
              "50_pair17", "50_pair17", "50_pair18", "50_pair18", # woodland
              "50_pair19", "50_pair19", "50_pair20", "50_pair20", # woodland
              "50_pair21", "50_pair21", "50_pair22", "50_pair22", # woodland
              "50_pair23", "50_pair23", "50_pair24", "50_pair24", # woodland
              "50_pair25", "50_pair25", "50_pair26", "50_pair26", # woodland
              "50_pair27", "50_pair27", "50_pair28", "50_pair28", # woodland
              "50_pair29", "50_pair29", "50_pair30", "50_pair30", # woodland
              "50_pair31", "50_pair31", # woodland
            
              "50_pair32", "50_pair32", "50_pair33", "50_pair33", # moorland
              "50_pair34", "50_pair34", "50_pair35", "50_pair35", # moorland
              "50_pair36", "50_pair36","50_pair37", "50_pair37", # moorland
              "50_pair38", "50_pair38", "50_pair39", "50_pair39", # moorland
              "50_pair40", "50_pair40", "50_pair41", "50_pair41", # moorland
              "50_pair42", "50_pair42","50_pair43", "50_pair43", # moorland
              "50_pair44", "50_pair44", "50_pair45", "50_pair45", # moorland
              "50_pair46", "50_pair46", "50_pair47", "50_pair47", # moorland
              "50_pair48", "50_pair48", "50_pair49", "50_pair49", # moorland
              "50_pair50", "50_pair50","50_pair51", "50_pair51", # moorland
              "50_pair52", "50_pair52","50_pair53", "50_pair53", # moorland
              "50_pair54", "50_pair54")) # moorland

# add logical column in main dataframe to highlight rows which would be obtained with 100m spacing
# duplicate rows for any device in multiple pairs
BD_pilot_dist <- BD_pilot_dist %>% 
  left_join(combo_50, by = c("site", "audiomoth_ID"))

# check data
View(BD_pilot_dist)
unique(BD_pilot_dist$dist_50)


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
BD_pilot_dist_pa <- BD_pilot_dist %>% 
  group_by(site, audiomoth_ID, common_n, scientific_n) %>% 
  summarise(
    dist_200_sample = as.integer((any(!is.na(dist_200)))),
    dist_150_sample = as.integer((any(!is.na(dist_150)))),
    dist_100_sample = as.integer((any(!is.na(dist_100)))),
    dist_50_sample = as.integer((any(!is.na(dist_50)))),
    .groups = "drop"
  )

# merge with the full grid to record 'missing' species as absences
BD_pilot_dist_pa <- full_grid %>% 
  left_join(BD_pilot_dist_pa, by = c("site", "audiomoth_ID", "common_n", "scientific_n")) %>% 
  mutate(
    dist_200_sample = replace_na(dist_200_sample, 0),
    dist_150_sample = replace_na(dist_150_sample, 0),
    dist_100_sample = replace_na(dist_100_sample, 0),
    dist_50_sample = replace_na(dist_50_sample, 0)
  )

# check dataframe
View(BD_pilot_dist_pa)




#### Sub-samples to assess number of days recording ####

BD_pilot_days <- BD_pilot_data

# check all unique recording dates and times
unique(BD_pilot_days$recording_date)
unique(BD_pilot_days$recording_time)

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




#### Split by habitats ####

##### Distance between devices #####

# generate data frame containing only the woodland site data
dist_wood <- BD_pilot_dist_pa %>% 
  filter(site == "BDWD")
# check dataset
head(dist_wood)
unique(dist_wood$site)

# generate data frame containing only the moorland site data
dist_moor <- BD_pilot_dist_pa %>% 
  filter(site == "BDMD")
# check dataset
head(dist_moor)
unique(dist_moor$site)


##### Number of days #####

# generate data frame containing only the woodland site data
days_wood <- BD_pilot_days_pa %>% 
  filter(site == "BDWD")
# check dataset
head(days_wood)
unique(days_wood$site)

# generate data frame containing only the moorland site data
days_moor <- BD_pilot_days_pa %>% 
  filter(site == "BDMD")
# check dataset
head(days_moor)
unique(days_moor$site)


##### Recording Period #####

# generate data frame containing only the woodland site data
period_wood <- BD_pilot_period_pa %>% 
  filter(site == "BDWD")
# check dataset
head(period_wood)
unique(period_wood$site)

# generate data frame containing only the moorland site data
period_moor <- BD_pilot_period_pa %>% 
  filter(site == "BDMD")
# check dataset
head(period_moor)
unique(period_moor$site)


##### Sampling Schedule #####

# generate data frame containing only the woodland site data
sched_wood <- BD_pilot_sched_pa %>% 
  filter(site == "BDWD")
# check dataset
head(sched_wood)
unique(sched_wood$site)

# generate data frame containing only the moorland site data
sched_moor <- BD_pilot_sched_pa %>% 
  filter(site == "BDMD")
# check dataset
head(sched_moor)
unique(sched_moor$site)




#### Data Analysis ####

##### Distance between devices #####

# generate reference table containing each audiomoth_ID and each pair_ID it belongs to
combo_all <- bind_rows(
  combo_200 %>% mutate(survey_design = "dist_200", pair_ID = dist_200) %>% select(site, audiomoth_ID, survey_design, pair_ID),
  combo_150 %>% mutate(survey_design = "dist_150", pair_ID = dist_150) %>% select(site, audiomoth_ID, survey_design, pair_ID),
  combo_100 %>% mutate(survey_design = "dist_100", pair_ID = dist_100) %>% select(site, audiomoth_ID, survey_design, pair_ID),
  combo_50 %>% mutate(survey_design = "dist_50", pair_ID = dist_50) %>% select(site, audiomoth_ID, survey_design, pair_ID))
head(combo_all)

# woodland site data
dist_wood_long <- dist_wood %>% 
  pivot_longer(
    cols = ends_with("_sample"),
    names_to = "survey_design",
    values_to = "presence"
  ) %>% 
  mutate(survey_design = gsub("_sample", "", survey_design))

# join pair IDs
dist_wood_long <- dist_wood_long %>%
  left_join(combo_all, by = c("site", "audiomoth_ID", "survey_design"))

# check data
head(dist_wood_long)

# count species detected
dist_wood_counts <- dist_wood_long %>% 
  filter(presence ==1) %>% 
  group_by(survey_design, pair_ID) %>% 
  summarise(n_species = n_distinct(common_n),.groups = "drop")
# check data
head(dist_wood_counts)

dist_wood_plot <-
  ggplot(dist_wood_counts, aes(x = factor(survey_design, levels = c("dist_50", "dist_100", "dist_150", "dist_200")),
                             y = n_species)) +
  geom_boxplot(fill = "seagreen") +
  labs(
    x = "Distance between AudioMoths",
    y = "Total species detected\nby each device pairing") +
  scale_x_discrete(labels = c(
    "dist_50_sample" = "50m",
    "dist_100_sample" = "100m",
    "dist_150_sample" = "150m",
    "dist_200_sample" = "200m")) +
  theme_minimal()+
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank())

# woodland site data
dist_moor_long <- dist_moor %>% 
  pivot_longer(
    cols = ends_with("_sample"),
    names_to = "survey_design",
    values_to = "presence"
  ) %>% 
  mutate(survey_design = gsub("_sample", "", survey_design))

# join pair IDs
dist_moor_long <- dist_moor_long %>%
  left_join(combo_all, by = c("site", "audiomoth_ID", "survey_design"))

# check data
head(dist_moor_long)

# count species detected
dist_moor_counts <- dist_moor_long %>% 
  filter(presence ==1) %>% 
  group_by(survey_design, pair_ID) %>% 
  summarise(n_species = n_distinct(common_n),.groups = "drop")
# check data
head(dist_moor_counts)

dist_moor_plot <-
  ggplot(dist_moor_counts, aes(x = factor(survey_design, levels = c("dist_50", "dist_100", "dist_150", "dist_200")),
                             y = n_species)) +
  geom_boxplot(fill = "goldenrod") +
  labs(
    x = "Distance between AudioMoths",
    y = "Total species detected\nby each device pairing") +
  scale_x_discrete(labels = c(
    "dist_50" = "50m",
    "dist_100" = "100m",
    "dist_150" = "150m",
    "dist_200" = "200m")) +
  theme_minimal() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14))


##### Number of days #####

# woodland site data
days_wood_long <- days_wood %>% 
  pivot_longer(
    cols = c(one_day_sample, two_day_sample, three_day_sample),
    names_to = "survey_design",
    values_to = "presence"
  )
# check data
head(days_wood_long)

# count species detected
days_wood_counts <- days_wood_long %>% 
  filter(presence ==1) %>% 
  group_by(audiomoth_ID, survey_design) %>% 
  summarise(n_species = n(),.groups = "drop")
# check data
head(days_wood_counts)

days_wood_plot <-
  ggplot(days_wood_counts, aes(x = factor(survey_design, levels = c("one_day_sample", "two_day_sample", "three_day_sample")),
                             y = n_species)) +
  geom_boxplot(fill = "seagreen") +
  labs(
    x = "Number of days recorded",
    y = "Number of species\ndetected per device") +
  scale_x_discrete(labels = c(
    "one_day_sample" = "1",
    "two_day_sample" = "2",
    "three_day_sample" = "3")) +
  theme_minimal()+
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank())

# moorland site data
days_moor_long <- days_moor %>% 
  pivot_longer(
    cols = c(one_day_sample, two_day_sample, three_day_sample),
    names_to = "survey_design",
    values_to = "presence"
  )
# check data
head(days_moor_long)

# count species detected
days_moor_counts <- days_moor_long %>% 
  filter(presence ==1) %>% 
  group_by(audiomoth_ID, survey_design) %>% 
  summarise(n_species = n(),.groups = "drop")
# check data
head(days_moor_counts)
unique(days_moor_counts$survey_design)

days_moor_plot <-
  ggplot(days_moor_counts, aes(x = factor(survey_design, levels = c("one_day_sample", "two_day_sample", "three_day_sample")),
                             y = n_species)) +
  geom_boxplot(fill = "goldenrod") +
  labs(
    x = "Number of days recorded",
    y = "Number of species\ndetected per device") +
  scale_x_discrete(labels = c(
    "one_day_sample" = "1",
    "two_day_sample" = "2",
    "three_day_sample" = "3")) +
  theme_minimal() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14))


##### Recording Period #####

# woodland site data
period_wood_long <- period_wood %>% 
  pivot_longer(
    cols = c(dawn_sample, dusk_sample, day_sample),
    names_to = "survey_design",
    values_to = "presence"
  )
# check data
head(period_wood_long)

# count species detected
period_wood_counts <- period_wood_long %>% 
  filter(presence ==1) %>% 
  group_by(audiomoth_ID, survey_design) %>% 
  summarise(n_species = n(),.groups = "drop")
# check data
head(period_wood_counts)

period_wood_plot <-
  ggplot(period_wood_counts, aes(x = factor(survey_design, levels = c("dawn_sample", "dusk_sample", "day_sample")),
                             y = n_species)) +
  geom_boxplot(fill = "seagreen") +
  labs(
    x = "Recording Period",
    y = "Number of species\ndetected per device") +
  scale_x_discrete(labels = c(
    "dawn_sample" = "Dawn (7-9am)",
    "dusk_sample" = "Dusk (8-10pm)",
    "day_sample" = "Full Day (24hrs)")) +
  theme_minimal()+
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank())

# moorland site data
period_moor_long <- period_moor %>% 
  pivot_longer(
    cols = c(dawn_sample, dusk_sample, day_sample),
    names_to = "survey_design",
    values_to = "presence"
  )
# check data
head(period_moor_long)

# count species detected
period_moor_plot <-
  period_moor_counts <- period_moor_long %>% 
  filter(presence ==1) %>% 
  group_by(audiomoth_ID, survey_design) %>% 
  summarise(n_species = n(),.groups = "drop")
# check data
head(period_moor_counts)
unique(period_moor_counts$survey_design)

period_moor_plot <-
  ggplot(period_moor_counts, aes(x = factor(survey_design, levels = c("dawn_sample", "dusk_sample", "day_sample")),
                             y = n_species)) +
  geom_boxplot(fill = "goldenrod") +
  labs(
    x = "Recording Period",
    y = "Number of species\ndetected per device") +
  scale_x_discrete(labels = c(
    "dawn_sample" = "Dawn (7-9am)",
    "dusk_sample" = "Dusk (8-10pm)",
    "day_sample" = "Full Day (24hrs)")) +
  theme_minimal() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14))


##### Sampling Schedule #####

# woodland site data
sched_wood_long <- sched_wood %>% 
  pivot_longer(
    cols = c(five_mins, ten_mins, fithtn_mins, thirty_mins, full_hour),
    names_to = "survey_design",
    values_to = "presence"
  )
# check data
head(sched_wood_long)

# count species detected
sched_wood_counts <- sched_wood_long %>% 
  filter(presence ==1) %>% 
  group_by(audiomoth_ID, survey_design) %>% 
  summarise(n_species = n(),.groups = "drop")
# check data
head(sched_wood_counts)

sched_wood_plot <-
  ggplot(sched_wood_counts, aes(x = factor(survey_design, levels = c("five_mins", "ten_mins", "fithtn_mins", "thirty_mins", "full_hour")),
                               y = n_species)) +
  geom_boxplot(fill = "seagreen") +
  labs(
    x = "Sampling Schedule Period",
    y = "Number of species\ndetected per device") +
  scale_x_discrete(labels = c(
    "five_mins" = "5 mins/hr",
    "ten_mins" = "10 mins/hr",
    "fithtn_mins" = "15 mins/hr",
    "thirty_mins" = "30 mins/hr",
    "full_hour" = "60 mins/hr")) +
  theme_minimal()+
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank())

# moorland site data
sched_moor_long <- sched_moor %>% 
  pivot_longer(
    cols = c(five_mins, ten_mins, fithtn_mins, thirty_mins, full_hour),
    names_to = "survey_design",
    values_to = "presence"
  )
# check data
head(sched_moor_long)

# count species detected
sched_moor_counts <- sched_moor_long %>% 
  filter(presence ==1) %>% 
  group_by(audiomoth_ID, survey_design) %>% 
  summarise(n_species = n(),.groups = "drop")
# check data
head(sched_moor_counts)
unique(sched_moor_counts$survey_design)

sched_moor_plot <-
  ggplot(sched_moor_counts, aes(x = factor(survey_design, levels = c("five_mins", "ten_mins", "fithtn_mins", "thirty_mins", "full_hour")),
                               y = n_species)) +
  geom_boxplot(fill = "goldenrod") +
  labs(
    x = "Sampling Schedule",
    y = "Number of species\ndetected per device") +
  scale_x_discrete(labels = c(
    "five_mins" = "5 mins/hr",
    "ten_mins" = "10 mins/hr",
    "fithtn_mins" = "15 mins/hr",
    "thirty_mins" = "30 mins/hr",
    "full_hour" = "60 mins/hr")) +
  theme_minimal() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14))




##### saving Data #####

# distance count data
write_xlsx(dist_wood_counts, "./phase1_analysis/data/BDWD2025_dist_subset.xlsx")
write_xlsx(dist_moor_counts, "./phase1_analysis/data/BDMD2025_dist_subset.xlsx")

# no. days count data
write_xlsx(days_wood_counts, "./phase1_analysis/data/BDWD2025_days_subset.xlsx")
write_xlsx(days_moor_counts, "./phase1_analysis/data/BDMD2025_days_subset.xlsx")

# recording period count data
write_xlsx(period_wood_counts, "./phase1_analysis/data/BDWD2025_period_subset.xlsx")
write_xlsx(period_moor_counts, "./phase1_analysis/data/BDMD2025_period_subset.xlsx")

# sampling schedule count data
write_xlsx(sched_wood_counts, "./phase1_analysis/data/BDWD2025_sched_subset.xlsx")
write_xlsx(sched_moor_counts, "./phase1_analysis/data/BDMD2025_sched_subset.xlsx")




##### Organising and Saving Plots #####

# save plot of species detected with different spatial designs
dist_plot <- plot_grid(dist_wood_plot, dist_moor_plot, ncol = 1)
ggsave("./phase1_analysis/plots/BD_dist_plot.png", plot = dist_plot, height = 7.5, width = 7.2)

# save plot of species detected with different numbers of recording days
days_plot <- plot_grid(days_wood_plot, days_moor_plot, ncol = 1)
ggsave("./phase1_analysis/plots/BD_days_plot.png", plot = days_plot, height = 7.5, , width = 7.2)

# save plot of species detected with different recording periods
period_plot <- plot_grid(period_wood_plot, period_moor_plot, ncol = 1)
ggsave("./phase1_analysis/plots/BD_period_plot.png", plot = period_plot, height = 7.5)

# save plot of species detected with different sampling schedules
sched_plot <- plot_grid(sched_wood_plot, sched_moor_plot, ncol = 1)
ggsave("./phase1_analysis/plots/BD_sched_plot.png", plot = sched_plot, height = 7.5, width = 8)

