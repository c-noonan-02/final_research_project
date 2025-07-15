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
library(lubridate)
library(fuzzyjoin)

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


#### How does the recording period affect the number and identity of species detected? ####

BD_pilot_period <- BD_pilot_data

# check all unique recording dates
unique(BD_pilot_period$recording_date)

# check dates are processing as dates
BD_pilot_period <- BD_pilot_period %>%
  mutate(recording_date = as.Date(recording_date))


##### Assign Recording Period to all times #####

# need to write this code, inspired by previous code used to divide by recording period

# ###### Day One ######
# 
# # filter for recordings from the first day of sampling for each site
# BD_pilot_period <- BD_pilot_period %>% 
#   mutate(day_one = (site == "BDWD" & recording_date == as.Date("2025-05-15")) |
#            (site == "BDMD" & recording_date == as.Date("2025-05-21"))
#   )
# # check it has worked
# unique(BD_pilot_period$day_one)
# BD_pilot_period %>%
#   filter(day_one == TRUE) %>%
#   distinct(site, recording_date) %>%
#   print()
# head(BD_pilot_period)
# unique(BD_pilot_period$site)
# unique(BD_pilot_period$audiomoth_ID)
# 
# 
# ###### Day Two ######
# 
# # filter for recordings from the second day of sampling for each site
# BD_pilot_period <- BD_pilot_period %>% 
#   mutate(day_two = 
#            
#            (site == "BDWD" & recording_date == as.Date("2025-05-16")) |
#            (site == "BDMD" & recording_date == as.Date("2025-05-22"))
#   )
# 
# # check it has worked
# unique(BD_pilot_period$day_two)
# BD_pilot_period %>%
#   filter(day_two == TRUE) %>%
#   distinct(site, recording_date) %>%
#   print()
# head(BD_pilot_period)
# unique(BD_pilot_period$site)
# unique(BD_pilot_period$audiomoth_ID)
# 
# 
# ###### Three period ######
# 
# # filter for recordings from the third day of sampling for each site
# BD_pilot_period <- BD_pilot_period %>% 
#   mutate(day_three = 
#            
#            (site == "BDWD" & recording_date == as.Date("2025-05-17")) |
#            (site == "BDMD" & recording_date == as.Date("2025-05-23"))
#   )
# # check it has worked
# unique(BD_pilot_period$day_three)
# BD_pilot_period %>%
#   filter(day_three == TRUE) %>%
#   distinct(site, recording_date) %>%
#   print()
# head(BD_pilot_period)
# unique(BD_pilot_period$site)
# unique(BD_pilot_period$audiomoth_ID)
# 
# # convert to long format
# BD_pilot_period <- BD_pilot_period %>% 
#   pivot_longer(
#     cols = starts_with("day_"),
#     names_to = "day",
#     values_to = "from_day"
#   ) %>% 
#   filter(from_day == TRUE)
# # check filtering has worked
# unique(BD_pilot_period$from_day)
# # remove redundant column
# BD_pilot_period <- select(BD_pilot_period, -from_day)
# 
# 
# ##### Randomly assign each audiomoth within habitat #####
# 
# # each device within each habitat will be divided into blocks of one, two or three period
# 
# # set the pattern of randomisation for reproducability
# set.seed(123) # can hash out for final run
# 
# # create a table of unique devices with habitat
# device_options <- BD_pilot_period %>% 
#   # extract all distinct arrays and audiomoths
#   distinct(site, audiomoth_ID, habitat) %>% 
#   # group the dataset by habitat
#   group_by(habitat) %>% 
#   # randomly assign each device/habitat combination to one of three scenarios
#   mutate(option = sample(c("optionA", "optionB", "optionC"), n(), replace = TRUE)) %>% 
#   ungroup()
# 
# # join this back into the full dataset
# BD_pilot_period <- BD_pilot_period %>% 
#   left_join(device_options, by = c("site", "audiomoth_ID", "habitat"))
# # check data set
# head(BD_pilot_period)
# 
# # extract sets of one, two or three days based on the assigned options
# BD_pilot_period <- BD_pilot_period %>% 
#   mutate(subsample_group = case_when(
#     # extract day one as one day of data, and days two and three as two days of data
#     option == "optionA" & day == "day_one" ~ "one_day",
#     option == "optionA" & day %in% c("day_two","day_three") ~ "two_days",
#     # extract day three as one day of data, and days one and two as two days of data
#     option == "optionB" & day == "day_three" ~ "one_day",
#     option == "optionB" & day %in% c("day_one", "day_two") ~ "two_days",
#     # extract all days combined as three days worth of data
#     option == "optionC" ~ "three_days",
#     
#     # if none of the above conditions are matched return missing value character
#     TRUE ~ NA_character_
#   ))
# 
# 
# ##### Count the species detected #####
# 
# period_combined_counts <- BD_pilot_period %>%
#   group_by(subsample_group, site, audiomoth_ID) %>%
#   summarise(n_species = n_distinct(common_n),.groups = "drop")
# # check data
# head(period_combined_counts)
# 
# 
# ##### Visualise the data #####
# 
# period_plot <-
#   ggplot(period_combined_counts, aes(x = factor(subsample_group, levels = c("one_day", "two_days", "three_days")),
#                                    y = n_species, fill = site)) +
#   geom_boxplot() +
#   labs(
#     x = "Number of days recorded",
#     y = "Total species\ndetected per device",
#     fill = "Habitat") +
#   scale_x_discrete(labels = c(
#     "one_day" = "1",
#     "two_days" = "2",
#     "three_days" = "3")) +
#   scale_fill_manual(
#     values = c("BDWD" = "seagreen", "BDMD" = "goldenrod"),
#     labels = c("BDWD" = "Woodland", "BDMD" = "Moorland"),
#     name = "Habitat") +
#   theme_minimal() +
#   theme(axis.text = element_text(size = 12),
#         axis.title = element_text(size = 14))
# # view the plot
# period_plot
# 
# # change to use model outputs?
# 
# 
# ##### Statistically Analyse the Data #####
# 
# hist(period_combined_counts$n_species)
# 
# # formally classify the subsample_group content as a factor rather than character
# period_combined_counts$subsample_group <- as.factor(period_combined_counts$subsample_group)
# # check this has worked
# levels(period_combined_counts$subsample_group)
# # formally classify the site content as a factor rather than character
# period_combined_counts$site <- as.factor(period_combined_counts$site)
# # check this has worked
# levels(period_combined_counts$site)
# 
# # model to test the impact of the number of days recorded
# period_model <- lmer(n_species ~ subsample_group * site + (1|audiomoth_ID), data = period_combined_counts)
# 
# # check distribution using histogram
# hist(residuals(period_model))
# # check assumptions for glmer model
# check_model(period_model)
# 
# # model output - NOT TO BE USED YET, DATA CLEANING INCOMPLETE
# summary(period_model)


#### How does the recording schedule affect the number and identity of species detected? ####

BD_pilot_sched <- BD_pilot_data


##### Break recordings down into 2 hour blocks #####

BD_pilot_sched <- BD_pilot_sched %>% 
  mutate(
    
    # create full datetime
    detect_datetime = ymd_hms(paste(recording_date, detect_start_time)),
    # break into 2 hour blocks
    block_start_time = floor_date(detect_datetime, unit = "2 hours"),
    
    # compute minutes since the start of the last block
    mins_since = as.numeric(difftime(detect_datetime, block_start_time, units = "mins"))
  )


##### Randomise the order of recording schedule extractions #####

schedule_orders <- BD_pilot_sched %>% 
  distinct(site, habitat, audiomoth_ID) %>% 
  mutate(
    schedule_order = list(sample(c(5, 10, 15, 30, 60)))
  )


##### Assign recording schedule to all possible times #####

# create dataframe containing the start and end time for each subblock, the label,
# the random order of blocks, and a row per subblock per device/block
block_schedules <- BD_pilot_sched %>% 
  
  distinct(site, habitat, audiomoth_ID, block_start_time) %>% 
  
  left_join(schedule_orders, by = c("site", "habitat", "audiomoth_ID")) %>% 
  # unnest to get one row per subblock
  unnest_longer(schedule_order, indices_to = "order_index") %>% 
  # group by each device/block combo
  group_by(site, habitat, audiomoth_ID, block_start_time) %>% 
  # calculate time ranges
  mutate(
    start_min = cumsum(lag(schedule_order, default = 0)),
    end_min = cumsum(schedule_order),
    schedule_label = paste0(schedule_order, "min")
    ) %>% 
  ungroup()

# ensure time comparisons are numeric
BD_pilot_sched <- BD_pilot_sched %>% 
  mutate(mins_since = as.numeric(mins_since))
block_schedules <- block_schedules %>% 
  mutate(
    start_min = as.numeric(start_min),
    end_min = as.numeric(end_min)
  )

# assign each detection to it's corresponding subblock
BD_pilot_sched_joined <- BD_pilot_sched %>% 
  left_join(block_schedules,
            by = c("site", "habitat", "audiomoth_ID", "block_start_time"))

BD_pilot_sched_assigned <- BD_pilot_sched_joined %>% 
  filter(mins_since >= start_min & mins_since < end_min)

# keep only the row where a detection falls within a matching block
# removes incorrect matches


##### Count the species detected #####

sched_combined_counts <- BD_pilot_sched_assigned %>%
  group_by(schedule_label, site, audiomoth_ID) %>%
  summarise(n_species = n_distinct(common_n),.groups = "drop")
# check data
head(sched_combined_counts)


##### Visualise the data #####

sched_plot <-
  ggplot(sched_combined_counts, aes(x = factor(schedule_label, levels = c("5min", "10min", "15min", "30min", "60min")),
                                   y = n_species, fill = site)) +
  geom_boxplot() +
  labs(
    x = "Number of minutes recorded within the hour",
    y = "Total species\ndetected per device",
    fill = "Habitat") +
  scale_fill_manual(
    values = c("BDWD" = "seagreen", "BDMD" = "goldenrod"),
    labels = c("BDWD" = "Woodland", "BDMD" = "Moorland"),
    name = "Habitat") +
  theme_minimal() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14))
# view the plot
sched_plot

# change to use model outputs?


##### Statistically Analyse the Data #####

hist(sched_combined_counts$n_species)

# formally classify the subsample_group content as a factor rather than character
sched_combined_counts$schedule_label <- as.factor(sched_combined_counts$schedule_label)
# check this has worked
levels(sched_combined_counts$schedule_label)
# formally classify the site content as a factor rather than character
sched_combined_counts$site <- as.factor(sched_combined_counts$site)
# check this has worked
levels(sched_combined_counts$site)

# model to test the impact of the number of days recorded
sched_model <- lmer(n_species ~ schedule_label * site + (1|audiomoth_ID), data = sched_combined_counts)

# check distribution using histogram
hist(residuals(sched_model))
# check assumptions for glmer model
check_model(sched_model)

# model output - NOT TO BE USED YET, DATA CLEANING INCOMPLETE
summary(sched_model)
