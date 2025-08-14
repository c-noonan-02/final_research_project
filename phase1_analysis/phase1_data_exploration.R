#### Data Exploration ####

# clear environment
rm(list=ls())

# load required packages
library(dplyr)
library(readxl)
library(writexl)

# import data set
BD_pilot_data <- read_xlsx("./audiomoth_data/PT2025_BirdNETOutput4.xlsx") # times preserved in xlsx format
head(BD_pilot_data)


#### Number of species detected ####

# unique species
unique(BD_pilot_data$scientific_n)

# number woodland species
BD_pilot_data %>%
  filter(site == "BDWD") %>%
  summarise(unique_species = n_distinct(scientific_n))
# number moorland species
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
  group_by(common_n, scientific_n) %>%
  summarise(no_detections = n())

# add confidence scores by species
summary_df$min_conf <- tapply(BD_pilot_data$conf, BD_pilot_data$common_n, min)
summary_df$max_conf <- tapply(BD_pilot_data$conf, BD_pilot_data$common_n, max)

# save data frame to send to expert
write_xlsx(summary_df, "./phase1_analysis/data/BD2025_species_summary.xlsx")


#### Species Summaries for Appendix ####

# clear environment
rm(list=ls())

# load required packages
library(dplyr)
# library(tidyr)
library(readxl)
library(writexl)

# import data set
BD_pilot_data <- read_xlsx("./audiomoth_data/PT2025_BirdNETOutput3.xlsx") # times preserved in xlsx format
head(BD_pilot_data)

# generate summary dataset for expert opinion on likelihood of detections
species_summary <- BD_pilot_data %>%
  group_by(habitat, common_n, scientific_n) %>%
  summarise(no_detections = n())

# get species list for each habitat
woodland_species <- species_summary %>% 
  filter(habitat == "woodland") %>% 
  pull(common_n) %>% 
  unique()
# number of species
length(woodland_species)
moorland_species <- species_summary %>% 
  filter(habitat == "moorland") %>% 
  pull(common_n) %>% 
  unique()
# number of species
length(moorland_species)

# get unique species for each habitat
woodland_unique <- setdiff(woodland_species, moorland_species)
moorland_unique <- setdiff(moorland_species, woodland_species)

# generate summary dataset of most common detections
species_frequency1 <- BD_pilot_data %>%
  group_by(common_n, scientific_n) %>%
  summarise(no_detections = n())
# generate summary dataset of most common detections
species_frequency2 <- BD_pilot_data %>%
  group_by(habitat, common_n, scientific_n) %>%
  summarise(no_detections = n())

sum(species_frequency$no_detections)

# save data frame to send to expert
write_xlsx(summary_df, "./phase1_analysis/data/BD2025_confidence_summary.xlsx")


#### Summary Data ####

# post filtering summary

# import data set
BD_pilot_data <- read_xlsx("./audiomoth_data/PT2025_BirdNETOutput4.xlsx") # times preserved in xlsx format
head(BD_pilot_data)

# generate summary dataset for expert opinion on likelihood of detections
species_summary <- BD_pilot_data %>%
  group_by(habitat, common_n, scientific_n) %>%
  summarise(no_detections = n())

# get species list for each habitat
woodland_species <- species_summary %>% 
  filter(habitat == "woodland") %>% 
  pull(common_n) %>% 
  unique()
# number of species
length(woodland_species)
moorland_species <- species_summary %>% 
  filter(habitat == "moorland") %>% 
  pull(common_n) %>% 
  unique()
# number of species
length(moorland_species)

# get unique species for each habitat
woodland_unique <- setdiff(woodland_species, moorland_species)
moorland_unique <- setdiff(moorland_species, woodland_species)

# generate summary dataset of most common detections
species_frequency1 <- BD_pilot_data %>%
  group_by(common_n, scientific_n) %>%
  summarise(no_detections = n())
# generate summary dataset of most common detections
species_frequency2 <- BD_pilot_data %>%
  group_by(habitat, common_n, scientific_n) %>%
  summarise(no_detections = n())

sum(species_frequency$no_detections)

# generate summary table of all detections
species_summary2 <- BD_pilot_data %>% 
  group_by(common_n, scientific_n, habitat) %>% 
  summarise(no_detections = n(), .groups = "drop") %>% 
  pivot_wider(
    names_from = habitat,
    values_from = no_detections,
    values_fill = 0
  ) %>% 
  mutate(total_detections = coalesce(woodland, 0) + coalesce(moorland, 0))

# save data frame to send to expert
write_xlsx(species_summary2, "./phase1_analysis/data/BD2025_detections_summary.xlsx")
