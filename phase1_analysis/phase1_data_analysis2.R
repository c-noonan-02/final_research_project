# This R script contains all code relating to sub-sets of the phase one data set
# used to assess the differences in the number of species detected when using
# different combinations of 'sampling effort' on both spatial and temporal scales
# in combination.

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


#### Generation of Sub-samples ####

##### Survey Design A #####

# Survey Design A = devices 200m apart, recording 1 day, sampling 5 mins per hour.
# To assess use of low spatial effort and low temporal effort.

BD_pilot_A <- BD_pilot_data

# First select all data obtained when devices are placed 200m apart

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
  pair_ID = c("200_pair1", "200_pair1", "200_pair2", "200_pair2", # woodland
               "200_pair3", "200_pair3", "200_pair4", "200_pair4", # woodland
               "200_pair5", "200_pair5", "200_pair6", "200_pair6")) # moorland

# add logical column in main dataframe to highlight rows which would be obtained with 200m spacing
BD_pilot_A <- BD_pilot_A %>% 
  left_join(combo_200, by = c("site", "audiomoth_ID"))

# check data
View(BD_pilot_A)
unique(BD_pilot_A$pair_ID)

# remove any recordings not made using this design
BD_pilot_A <- BD_pilot_A %>%
  filter(!is.na(pair_ID))
# check data again
unique(BD_pilot_A$pair_ID)


# Then select only the first day of recording

BD_pilot_A <- BD_pilot_A %>% 
  mutate(day_one = (site == "BDWD" & recording_date == as.Date("2025-05-15")) |
           (site == "BDMD" & recording_date == as.Date("2025-05-21"))
  )

# check data
View(BD_pilot_A)
unique(BD_pilot_A$day_one)

# remove any recordings not made using this design
BD_pilot_A <- BD_pilot_A %>%
  filter(!is.na(day_one))
# check data again
unique(BD_pilot_A$day_one)


# Then select only the first 5 mins of recording

# add logical column in main data frame to highlight rows which were recorded in the first 5 mins of every hour
BD_pilot_A <- BD_pilot_A %>% 
  mutate(five_mins = lubridate::minute(hms::as_hms(detect_start_time)) < 5)

# check dataset
View(BD_pilot_A)
unique(BD_pilot_A$five_mins)

# remove any recordings not made within the first 5 mins
BD_pilot_A <- BD_pilot_A %>%
  filter(five_mins == "TRUE")
# check data again
unique(BD_pilot_A$day_one)


# remove redundant columns
# keep dist column for paired devices
BD_pilot_A <- BD_pilot_A %>% 
  select(-day_one, -five_mins)


##### Survey Design B #####

# Survey Design B = devices 200m apart, recording 3 days, continuous sampling.
# To assess use of low spatial effort and high temporal effort.

BD_pilot_B <- BD_pilot_data

# First select all data obtained when devices are placed 200m apart

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
  pair_ID = c("200_pair1", "200_pair1", "200_pair2", "200_pair2", # woodland
               "200_pair3", "200_pair3", "200_pair4", "200_pair4", # woodland
               "200_pair5", "200_pair5", "200_pair6", "200_pair6")) # moorland

# add logical column in main dataframe to highlight rows which would be obtained with 200m spacing
BD_pilot_B <- BD_pilot_B %>% 
  left_join(combo_200, by = c("site", "audiomoth_ID"))

# check data
View(BD_pilot_B)
unique(BD_pilot_B$pair_ID)

# remove any recordings not made using this design
BD_pilot_B <- BD_pilot_B %>%
  filter(!is.na(pair_ID))
# check data again
unique(BD_pilot_B$pair_ID)

# Do not need to write code to select days or sampling schedule as this is represented in the overall dataset

# no redundant columns to remove
# keep dist column for paired devices


##### Survey Design C #####

# Survey Design C = devices 50m apart, recording 1 day, sampling 5 mins per hour.
# To assess use of high spatial effort and low temporal effort.

BD_pilot_C <- BD_pilot_data

# First select all data obtained when devices are placed 50m apart

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
  pair_ID = c("50_pair1", "50_pair1", "50_pair2", "50_pair2", # woodland
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

# add logical column in main data frame to highlight rows which would be obtained with 100m spacing
# duplicate rows for any device in multiple pairs
BD_pilot_C <- BD_pilot_C %>% 
  left_join(combo_50, by = c("site", "audiomoth_ID"))

# check data
View(BD_pilot_C)
unique(BD_pilot_C$pair_ID)

# remove any recordings not made using this design
BD_pilot_C <- BD_pilot_C %>%
  filter(!is.na(pair_ID))
# check data again
unique(BD_pilot_C$pair_ID)


# Then select only the first day of recording

BD_pilot_C <- BD_pilot_C %>% 
  mutate(day_one = (site == "BDWD" & recording_date == as.Date("2025-05-15")) |
           (site == "BDMD" & recording_date == as.Date("2025-05-21"))
  )

# check data
View(BD_pilot_C)
unique(BD_pilot_C$day_one)

# remove any recordings not made using this design
BD_pilot_C <- BD_pilot_C %>%
  filter(!is.na(day_one))
# check data again
unique(BD_pilot_C$day_one)


# Then select only the first 5 mins of recording

# add logical column in main data frame to highlight rows which were recorded in the first 5 mins of every hour
BD_pilot_C <- BD_pilot_C %>% 
  mutate(five_mins = lubridate::minute(hms::as_hms(detect_start_time)) < 5)

# check dataset
View(BD_pilot_C)
unique(BD_pilot_C$five_mins)

# remove any recordings not made within the first 5 mins
BD_pilot_C <- BD_pilot_C %>%
  filter(five_mins == "TRUE")
# check data again
unique(BD_pilot_C$day_one)


# remove redundant columns
# keep dist column for paired devices
BD_pilot_C <- BD_pilot_C %>% 
  select(-day_one, -five_mins)


##### Survey Design D #####

# Survey Design D = devices 50m apart, recording 3 days, continuous sampling.
# To assess use of low spatial effort and high temporal effort.

BD_pilot_D <- BD_pilot_data

# First select all data obtained when devices are placed 50m apart

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
  pair_ID = c("50_pair1", "50_pair1", "50_pair2", "50_pair2", # woodland
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

# add logical column in main data frame to highlight rows which would be obtained with 100m spacing
# duplicate rows for any device in multiple pairs
BD_pilot_D <- BD_pilot_D %>% 
  left_join(combo_50, by = c("site", "audiomoth_ID"))

# check data
View(BD_pilot_D)
unique(BD_pilot_D$pair_ID)

# remove any recordings not made using this design
BD_pilot_D <- BD_pilot_D %>%
  filter(!is.na(pair_ID))
# check data again
unique(BD_pilot_D$pair_ID)

# Do not need to write code to select days or sampling schedule as this is represented in the overall dataset

# no redundant columns to remove
# keep dist column for paired devices


#### Combine Data ####

# combine all survey design data into one dataset
BD_combined <- bind_rows(
  BD_pilot_A %>% mutate(survey_design = "A"),
  BD_pilot_B %>% mutate(survey_design = "B"),
  BD_pilot_C %>% mutate(survey_design = "C"),
  BD_pilot_D %>% mutate(survey_design = "D"))

# check data
View(BD_combined)


#### Data Analysis ####

# count species detected
combined_counts <- BD_combined %>% 
  group_by(survey_design, site, pair_ID) %>% 
  summarise(n_species = n_distinct(common_n),.groups = "drop")
# check data
head(combined_counts)

designs_plot <-
  ggplot(combined_counts, aes(x = factor(survey_design, levels = c("A", "B", "C", "D")),
                                   y = n_species, fill = site)) +
  geom_boxplot() +
  labs(
    x = "Survey Design",
    y = "Total species detected\nby each device pairing",
    fill = "Habitat") +
  scale_x_discrete(labels = c(
    "A" = "A = Low spatial / \nlow temporal effort",
    "B" = "B = Low spatial / \nhigh temporal effort",
    "C" = "C = High spatial / \nlow temporal effort",
    "D" = "D = High spatial / \nhigh temporal effort")) +
  scale_fill_manual(
    values = c("BDWD" = "seagreen", "BDMD" = "goldenrod"),
    labels = c("BDWD" = "Woodland", "BDMD" = "Moorland"),
    name = "Habitat") +
  theme_minimal() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14))

# save plot of species detected with different survey designs
ggsave("./phase1_analysis/plots/BD_designs_plot.png", plot = designs_plot, height = 6, width = 10)
