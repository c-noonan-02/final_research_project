#### Data Exploration ####

# clear environment
rm(list=ls())

# load required packages
library(dplyr)
library(readxl)
library(writexl)

# import data set
BD_pilot_data <- read_xlsx("./audiomoth_data/BD2025_BirdNETOutput2.xlsx") # times preserved in xlsx format
head(BD_pilot_data)


#### Number of species detected ####

# unique species
unique(BD_pilot_data$scientific_n)

# unique woodland species
BD_pilot_data %>%
  filter(site == "BDWD") %>%
  summarise(unique_species = n_distinct(scientific_n))

# unique moorland species
BD_pilot_data %>%
  filter(site == "BDMD") %>%
  summarise(unique_species = n_distinct(scientific_n))


#### Confidence Filtering ####

# filter species detections by confidence
test <- BD_pilot_data %>%
  filter(conf >= 0.85) # change no for different thresholds
# see species remaining after filter
unique(test$common_n)


#### Plotting confidence against number of detections ####

# obtain min and maximum confidence scores by species
min <- tapply(BD_pilot_data$conf, BD_pilot_data$common_n, min)
max <- tapply(BD_pilot_data$conf, BD_pilot_data$common_n, max)
# obtain number of detections by species
length <- tapply(BD_pilot_data$conf, BD_pilot_data$common_n, length)

# plot maximum confidence against number of detections
plot(log(length), max)
# look at species in order of maximum confidence
sort(max)


#### Species Summary for Expert Opinion ####

# generate summary dataset for expert opinion on likelihood of detections
summary_df <- BD_pilot_data %>%
  group_by(common_n) %>%
  summarise(no_detections = n())

# add confidence scores by species
summary_df$min_conf <- tapply(BD_pilot_data$conf, BD_pilot_data$common_n, min)
summary_df$max_conf <- tapply(BD_pilot_data$conf, BD_pilot_data$common_n, max)

# save data frame to send to expert
write_xlsx(summary_df, "./phase1_analysis/data/BD2025_species_summary.xlsx")


#### To Do ####

# Determine if unfeasible detections display similar length and max conf patterns...
# Use to determine appropriate threshold
# Or remove these species regardless, and set a lower threshold for species we are certain of...?