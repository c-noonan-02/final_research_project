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
library(lme4)
library(lmerTest)
library(performance)

# import data set
BD_pilot_data <- read_xlsx("./audiomoth_data/PT2025_BirdNETOutput3.xlsx") # times preserved in xlsx format
head(BD_pilot_data)


#### How does the number of days recorded affect the number and identity of species detected? ####

BD_pilot_days <- BD_pilot_data

# check all unique recording dates
unique(BD_pilot_days$recording_date)

# check dates are processing as dates
BD_pilot_days <- BD_pilot_days %>%
  mutate(recording_date = as.Date(recording_date))


##### Assign day numbers to all recording dates #####

###### Day One ######

# filter for recordings from the first day of sampling for each site
BD_pilot_days <- BD_pilot_days %>% 
  mutate(day_one = (site == "BDWD" & recording_date == as.Date("2025-05-15")) |
           (site == "BDMD" & recording_date == as.Date("2025-05-21"))
  )
# check it has worked
unique(BD_pilot_days$day_one)
BD_pilot_days %>%
  filter(day_one == TRUE) %>%
  distinct(site, recording_date) %>%
  print()
head(BD_pilot_days)
unique(BD_pilot_days$site)
unique(BD_pilot_days$audiomoth_ID)


###### Day Two ######

# filter for recordings from the second day of sampling for each site
BD_pilot_days <- BD_pilot_days %>% 
  mutate(day_two = 
           
           (site == "BDWD" & recording_date == as.Date("2025-05-16")) |
           (site == "BDMD" & recording_date == as.Date("2025-05-22"))
  )

# check it has worked
unique(BD_pilot_days$day_two)
BD_pilot_days %>%
  filter(day_two == TRUE) %>%
  distinct(site, recording_date) %>%
  print()
head(BD_pilot_days)
unique(BD_pilot_days$site)
unique(BD_pilot_days$audiomoth_ID)


###### Three days ######

# filter for recordings from the third day of sampling for each site
BD_pilot_days <- BD_pilot_days %>% 
  mutate(day_three = 
           
           (site == "BDWD" & recording_date == as.Date("2025-05-17")) |
           (site == "BDMD" & recording_date == as.Date("2025-05-23"))
  )
# check it has worked
unique(BD_pilot_days$day_three)
BD_pilot_days %>%
  filter(day_three == TRUE) %>%
  distinct(site, recording_date) %>%
  print()
head(BD_pilot_days)
unique(BD_pilot_days$site)
unique(BD_pilot_days$audiomoth_ID)

# convert to long format
BD_pilot_days <- BD_pilot_days %>% 
  pivot_longer(
    cols = starts_with("day_"),
    names_to = "day",
    values_to = "from_day"
  ) %>% 
  filter(from_day == TRUE)
# check filtering has worked
unique(BD_pilot_days$from_day)
# remove redundant column
BD_pilot_days <- select(BD_pilot_days, -from_day)


##### Randomly assign each audiomoth within habitat #####

# each device within each habitat will be divided into blocks of one, two or three days

# set the pattern of randomisation for reproducability
set.seed(123) # can hash out for final run

# create a table of unique devices with habitat
device_options <- BD_pilot_days %>% 
  # extract all distinct arrays and audiomoths
  distinct(site, audiomoth_ID, habitat) %>% 
  # group the dataset by habitat
  group_by(habitat) %>% 
  # randomly assign each device/habitat combination to one of three scenarios
  mutate(option = sample(c("optionA", "optionB", "optionC"), n(), replace = TRUE)) %>% 
  ungroup()

# join this back into the full dataset
BD_pilot_days <- BD_pilot_days %>% 
  left_join(device_options, by = c("site", "audiomoth_ID", "habitat"))
# check data set
head(BD_pilot_days)

# extract sets of one, two or three days based on the assigned options
BD_pilot_days <- BD_pilot_days %>% 
  mutate(subsample_group = case_when(
    # extract day one as one day of data, and days two and three as two days of data
    option == "optionA" & day == "day_one" ~ "one_day",
    option == "optionA" & day %in% c("day_two","day_three") ~ "two_days",
    # extract day three as one day of data, and days one and two as two days of data
    option == "optionB" & day == "day_three" ~ "one_day",
    option == "optionB" & day %in% c("day_one", "day_two") ~ "two_days",
    # extract all days combined as three days worth of data
    option == "optionC" ~ "three_days",
    
    # if none of the above conditions are matched return missing value character
    TRUE ~ NA_character_
  ))


##### Count the species detected #####

days_combined_counts <- BD_pilot_days %>%
  group_by(subsample_group, site, audiomoth_ID) %>%
  summarise(n_species = n_distinct(common_n),.groups = "drop")
# check data
head(days_combined_counts)


##### Visualise the data #####

days_plot <-
  ggplot(days_combined_counts, aes(x = factor(subsample_group, levels = c("one_day", "two_days", "three_days")),
                                   y = n_species, fill = site)) +
  geom_boxplot() +
  labs(
    x = "Number of days recorded",
    y = "Total species\ndetected per device",
    fill = "Habitat") +
  scale_x_discrete(labels = c(
    "one_day" = "1",
    "two_days" = "2",
    "three_days" = "3")) +
  scale_fill_manual(
    values = c("BDWD" = "seagreen", "BDMD" = "goldenrod"),
    labels = c("BDWD" = "Woodland", "BDMD" = "Moorland"),
    name = "Habitat") +
  theme_minimal() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14))
# view the plot
days_plot

# change to use model outputs?


##### Statistically Analyse the Data #####

hist(days_combined_counts$n_species)

# formally classify the subsample_group content as a factor rather than character
days_combined_counts$subsample_group <- as.factor(days_combined_counts$subsample_group)
# check this has worked
levels(days_combined_counts$subsample_group)
# formally classify the site content as a factor rather than character
days_combined_counts$site <- as.factor(days_combined_counts$site)
# check this has worked
levels(days_combined_counts$site)

# model to test the impact of the number of days recorded
days_model <- lmer(n_species ~ subsample_group * site + (1|audiomoth_ID), data = days_combined_counts)

# check distribution using histogram
hist(residuals(days_model))
# check assumptions for glmer model
check_model(days_model)

# model output - NOT TO BE USED YET, DATA CLEANING INCOMPLETE
summary(days_model)
