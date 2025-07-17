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
library(purrr)
library(geosphere)

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

# convert subsample group to numbers for easier analysis
days_combined_counts <- days_combined_counts %>% 
  mutate(subsample_group = recode(subsample_group,
                                  "one_day" = 1,
                                  "two_days" = 2,
                                  "three_days" = 3))
# check data
head(days_combined_counts)


##### Visualise the data #####

# plot the raw data
days_plot <-
  ggplot(days_combined_counts) +
  geom_point(aes(x = subsample_group,
                 y = n_species, col = site),
             position = position_dodge(width = 0.75), pch = 21)
# plot the means
days_plot <- days_plot +
  geom_point(data = days_combined_counts, 
             aes(x = subsample_group,
                 y = n_species, col = site),
             stat = "summary",
             fun = "mean",
             size = 3,
             position = position_dodge(width = 0.75))
# calculate the standard errors
days_dist_table <- days_combined_counts %>% group_by(subsample_group, site) %>% 
  summarise(mean = mean(n_species), se = sd(n_species)/sqrt(n()))
# plot the error bars
days_plot <- days_plot +
  geom_errorbar(data = days_dist_table,
                aes(x = subsample_group,
                    ymin = mean - se, ymax = mean + se, col = site),
                width = 0.2, position = position_dodge(width = 0.75))
# improve style of plot
days_plot <- days_plot +
  labs(
    x = "Number of days recorded",
    y = "Total species\ndetected per device",
    colour = "Habitat") +
  scale_colour_manual(
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

# formally classify the site content as a factor rather than character
days_combined_counts$site <- as.factor(days_combined_counts$site)
# check this has worked
levels(days_combined_counts$site)

# model to test the impact of the number of days recorded
days_model <- lmer(n_species ~ site * subsample_group + (1|audiomoth_ID), data = days_combined_counts)

# check distribution using histogram
hist(residuals(days_model))
# check assumptions for glmer model
check_model(days_model)

# model output - NOT TO BE USED YET, DATA CLEANING INCOMPLETE
summary(days_model)


##### Visualise the Data 2 #####

# create a dummy set of x values to feed through the equation
days_x <- seq(min(days_combined_counts$subsample_group),
              max(days_combined_counts$subsample_group), 1)
# how to for this data set?

# predicted values for each habitat
days_predict <- expand.grid(
  subsample_group = days_x,
  site = c("BDMD", "BDWD")
)

# add the model predictions to the plot
days_predict$predicted <- predict(days_model, newdata = days_predict, re.form = NA)

# add these to the plot
# plot the raw data
days_plot <-
  ggplot(days_combined_counts) +
  
  geom_point(aes(x = subsample_group,
                 y = n_species, col = site),
             position = position_dodge(width = 0.75), pch = 21,
             alpha = 0.6) +
  
  geom_point(data = days_combined_counts, 
             aes(x = subsample_group,
                 y = n_species, col = site),
             stat = "summary",
             fun = "mean",
             size = 3,
             position = position_dodge(width = 0.75),
             #alpha = 0.8
             ) +
  
  geom_errorbar(data = days_dist_table,
                aes(x = subsample_group,
                    ymin = mean - se, ymax = mean + se, col = site),
                width = 0.2, position = position_dodge(width = 0.75),
                #alpha = 0.8
                ) +
  
  geom_line(data = days_predict,
            aes(x = subsample_group,
                y = predicted, col = site),
            linetype = 2, linewidth = 1.2) +
  
  labs(x = "Number of days recorded", y = "Species richness detected\nper audiomoth device", col = "Habitat") +
  
  scale_colour_manual(
    values = c("BDWD" = "seagreen", "BDMD" = "goldenrod"),
    labels = c("BDWD" = "Woodland", "BDMD" = "Moorland"),
    name = "Habitat") +
  
  theme_minimal() +
  
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14))

days_plot
  


#### How does the recording period affect the number and identity of species detected? ####

BD_pilot_period <- BD_pilot_data

# check all unique recording dates
unique(BD_pilot_period$recording_date)

# check dates are processing as dates
BD_pilot_period <- BD_pilot_period %>%
  mutate(recording_date = as.Date(recording_date))


##### Assign Recording Period to all times #####

##### Dawn-only (2:30-7:30) #####

# add logical column in main data frame to highlight rows which were recorded within a dawn survey
BD_pilot_period <- BD_pilot_period %>% 
  mutate(dawn = detect_start_time >= "02:30:00" & detect_start_time < "07:00:00")

# check it has worked
unique(BD_pilot_period$dawn)
BD_pilot_period %>%
  filter(dawn == TRUE) %>%
  distinct(site, recording_date) %>%
  print()
head(BD_pilot_period)


##### Day-only (4:30-22:00) #####

# add logical column in main data frame to highlight rows which were recorded within a dawn survey
BD_pilot_period <- BD_pilot_period %>% 
  mutate(day = detect_start_time >= "07:00:00" & detect_start_time < "21:00:00")

# check it has worked
unique(BD_pilot_period$day)
BD_pilot_period %>%
  filter(day == TRUE) %>%
  distinct(site, recording_date) %>%
  print()
head(BD_pilot_period)


##### Dusk-only (21:00 - 24:00) #####

# add logical column in main data frame to highlight rows which were recorded within a dusk survey
BD_pilot_period <- BD_pilot_period %>% 
  mutate(dusk = detect_start_time >= "21:00:00" & detect_start_time < "24:00:00")

# check it has worked
unique(BD_pilot_period$dusk)
BD_pilot_period %>%
  filter(dusk == TRUE) %>%
  distinct(site, recording_date) %>%
  print()
head(BD_pilot_period)


##### Night-only (22:00 - 04:30) #####

# add logical column in main data frame to highlight rows which were recorded within a dawn survey
BD_pilot_period <- BD_pilot_period %>% 
  mutate(night = detect_start_time >= "22:00:00" | detect_start_time < "04:30:00")

# check it has worked
unique(BD_pilot_period$night)
BD_pilot_period %>%
  filter(night == TRUE) %>%
  distinct(site, recording_date) %>%
  print()
head(BD_pilot_period)


##### All day (00:00 - 24:00) #####

# add logical column in main data frame to highlight rows which were recorded within the full 24hrs
BD_pilot_period$all_day <- TRUE

# check it has worked
unique(BD_pilot_period$all_day)
BD_pilot_period %>%
  filter(all_day == TRUE) %>%
  distinct(site, recording_date) %>%
  print()
head(BD_pilot_period)

# convert to long format
BD_pilot_period <- BD_pilot_period %>%
  pivot_longer(
    cols = c("dawn", "dusk", "day", "night", "all_day"),
    names_to = "recording_period",
    values_to = "from_period"
  ) %>%
  filter(from_period == TRUE)

# check filtering has worked
unique(BD_pilot_period$from_period)
# remove redundant column
BD_pilot_period <- select(BD_pilot_period, -from_period)


##### Randomly assign each audiomoth within habitat #####

# each device within each habitat will provide data for either dawn and dusk, day and night, or a full 24hrs.

# set the pattern of randomisation for reproducability
set.seed(123) # can hash out for final run

# create a table of unique devices with habitat
device_options <- BD_pilot_period %>%
  # extract all distinct arrays and audiomoths
  distinct(site, audiomoth_ID, habitat) %>%
  # group the dataset by habitat
  group_by(habitat) %>%
  # randomly assign each device/habitat combination to one of three scenarios
  mutate(option = sample(c("optionA", "optionB", "optionC"), n(), replace = TRUE)) %>%
  ungroup()

# join this back into the full dataset
BD_pilot_period <- BD_pilot_period %>%
  left_join(device_options, by = c("site", "audiomoth_ID", "habitat"))
# check data set
head(BD_pilot_period)

# extract sets of one, two or three days based on the assigned options
BD_pilot_period2 <- BD_pilot_period %>%
  mutate(subsample_group = case_when(
    # extract day one as one day of data, and days two and three as two days of data
    option == "optionA" & recording_period == "dawn" ~ "dawn",
    option == "optionA" & recording_period == "dusk" ~ "dusk",
    # extract day three as one day of data, and days one and two as two days of data
    option == "optionB" & recording_period == "day" ~ "day",
    option == "optionB" & recording_period == "night" ~ "night",
    # extract all days combined as three days worth of data
    option == "optionC" ~ "all_day",

    # if none of the above conditions are matched return missing value character
    TRUE ~ NA_character_
  ))

# remove unselected rows
BD_pilot_period2 <- BD_pilot_period2 %>% 
  filter(!is.na(subsample_group))


##### Count the species detected #####

period_combined_counts <- BD_pilot_period2 %>%
  group_by(subsample_group, site, audiomoth_ID) %>%
  summarise(n_species = n_distinct(common_n),.groups = "drop")
# check data
head(period_combined_counts)


##### Visualise the data #####

# plot the raw data
period_plot <-
  ggplot(period_combined_counts) +
  geom_point(aes(x = subsample_group,
                 y = n_species, col = site),
             position = position_dodge(width = 0.75), pch = 21)
# plot the means
period_plot <- period_plot +
  geom_point(data = period_combined_counts, 
             aes(x = subsample_group,
                 y = n_species, col = site),
             stat = "summary",
             fun = "mean",
             size = 3,
             position = position_dodge(width = 0.75))
# calculate the standard errors
period_dist_table <- period_combined_counts %>% group_by(subsample_group, site) %>% 
  summarise(mean = mean(n_species), se = sd(n_species)/sqrt(n()))
# plot the error bars
period_plot <- period_plot +
  geom_errorbar(data = period_dist_table,
                aes(x = subsample_group,
                    ymin = mean - se, ymax = mean + se, col = site),
                width = 0.2, position = position_dodge(width = 0.75))
# improve style of plot
period_plot <- period_plot +
  labs(
    x = "Recording Period",
    y = "Species richness detected\nper audiomoth device",
    colour = "Habitat") +
  scale_x_discrete(labels = c(
    "dawn" = "Dawn\n(2:30-7:30)",
    "day" = "Day\n(4:30-22:00)",
    "dusk" = "Dusk\n(21:00-24:00)",
    "night" = "Night\n(22:00-4:30)",
    "all_day" = "Full Day\n(24hrs)")) +
  scale_colour_manual(
    values = c("BDWD" = "seagreen", "BDMD" = "goldenrod"),
    labels = c("BDWD" = "Woodland", "BDMD" = "Moorland"),
    name = "Habitat") +
  theme_minimal() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14))

# view the plot
period_plot

# change to use model outputs?


##### Statistically Analyse the Data #####

hist(period_combined_counts$n_species)

# formally classify the subsample_group content as a factor rather than character
period_combined_counts$subsample_group <- as.factor(period_combined_counts$subsample_group)
# check this has worked
levels(period_combined_counts$subsample_group)
# formally classify the site content as a factor rather than character
period_combined_counts$site <- as.factor(period_combined_counts$site)
# check this has worked
levels(period_combined_counts$site)

# model to test the impact of the number of days recorded
period_model <- lmer(n_species ~ subsample_group * site + (1|audiomoth_ID), data = period_combined_counts)

# check distribution using histogram
hist(residuals(period_model))
# check assumptions for glmer model
check_model(period_model)

# model output - NOT TO BE USED YET, DATA CLEANING INCOMPLETE
summary(period_model)


##### Visualise the Data 2 #####

# create a dummy set of x values to feed through the equation
period_x <- c("full_day", "dawn", "day", "dusk", "night")
# how to for this data set?

# predicted values for each habitat
period_predict <- expand.grid(
  subsample_group = period_x,
  site = c("BDMD", "BDWD")
)

period_predict$predicted <- predict(period_model, newdata = period_predict, re.form = NA)

# add the model predictions to the plot
period_plot <-
  ggplot(period_combined_counts) +
  
  geom_point(aes(x = factor(subsample_group, levels = c("dawn", "day", "dusk", "night", "all_day")),
                 y = n_species, col = site),
             position = position_dodge(width = 0.75), pch = 21) +
  
  geom_point(data = period_combined_counts, 
             aes(x = subsample_group,
                 y = n_species, col = site),
             stat = "summary",
             fun = "mean",
             size = 3,
             position = position_dodge(width = 0.75)) +
  
  geom_errorbar(data = period_dist_table,
                aes(x = subsample_group,
                    ymin = mean - se, ymax = mean + se, col = site),
                width = 0.2, position = position_dodge(width = 0.75)) +
  
  # geom_line(data = period_predict,
  #           aes(x = subsample_group,
  #               y = predicted, col = site),
  #           linetype = 2, linewidth = 1.2) +
  
  labs(
    x = "Recording Period",
    y = "Species richness detected\nper audiomoth device",
    colour = "Habitat") +
  
  scale_x_discrete(labels = c(
    "dawn" = "Dawn\n(2:30-7:30)",
    "day" = "Day\n(4:30-22:00)",
    "dusk" = "Dusk\n(21:00-24:00)",
    "night" = "Night\n(22:00-4:30)",
    "all_day" = "Full Day\n(24hrs)")) +
  
  scale_colour_manual(
    values = c("BDWD" = "seagreen", "BDMD" = "goldenrod"),
    labels = c("BDWD" = "Woodland", "BDMD" = "Moorland"),
    name = "Habitat") +
  
  theme_minimal() +
  
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14))

period_plot




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

# convert schedule labels to numbers for easier analysis
sched_combined_counts <- sched_combined_counts %>% 
  mutate(schedule_label = recode(schedule_label,
                                  "5min" = 5,
                                  "10min" = 10,
                                  "15min" = 15,
                                  "30min" = 30,
                                  "60min" = 60))
# check data
head(sched_combined_counts)


##### Visualise the data #####

# plot the raw data
sched_plot <-
  ggplot(sched_combined_counts) +
  geom_point(aes(x = schedule_label,
                 y = n_species, col = site),
             position = position_dodge(width = 0.75), pch = 21)
# plot the means
sched_plot <- sched_plot +
  geom_point(data = sched_combined_counts, 
             aes(x = schedule_label,
                 y = n_species, col = site),
             stat = "summary",
             fun = "mean",
             size = 3,
             position = position_dodge(width = 0.75))
# calculate the standard errors
sched_dist_table <- sched_combined_counts %>% group_by(schedule_label, site) %>% 
  summarise(mean = mean(n_species), se = sd(n_species)/sqrt(n()))
# plot the error bars
sched_plot <- sched_plot +
  geom_errorbar(data = sched_dist_table,
                aes(x = schedule_label,
                    ymin = mean - se, ymax = mean + se, col = site),
                width = 0.2, position = position_dodge(width = 0.75))
# improve style of plot
sched_plot <- sched_plot +
  labs(
    x = "Number of minutes recorded within the hour",
    y = "Species richness detected\nper audiomoth device",
    colour = "Habitat") +
  scale_x_discrete(labels = c(
    "dawn" = "Dawn\n(2:30-7:30)",
    "day" = "Day\n(4:30-22:00)",
    "dusk" = "Dusk\n(21:00-24:00)",
    "night" = "Night\n(22:00-4:30)",
    "all_day" = "Full Day\n(24hrs)")) +
  scale_colour_manual(
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

# formally classify the site content as a factor rather than character
sched_combined_counts$site <- as.factor(sched_combined_counts$site)
# check this has worked
levels(sched_combined_counts$site)

# model to test the impact of the number of days recorded
sched_model <- lmer(n_species ~ schedule_label * site + (1|audiomoth_ID), data = sched_combined_counts)
sched_model <- lmer(n_species ~ site * schedule_label + (1|audiomoth_ID), data = sched_combined_counts)


# check distribution using histogram
hist(residuals(sched_model))
# check assumptions for glmer model
check_model(sched_model)

# model output - NOT TO BE USED YET, DATA CLEANING INCOMPLETE
summary(sched_model)


##### Visualise the data 2 #####

# create a dummy set of x values to feed through the equation
sched_x <- c(5, 10, 15, 30, 60)
# how to for this data set?

# predicted values for each habitat
sched_predict <- expand.grid(
  schedule_label = sched_x,
  site = c("BDMD", "BDWD")
)

sched_predict$predicted <- predict(sched_model, newdata = sched_predict, re.form = NA)

# add the model predictions to the plot
sched_plot <-
  ggplot(sched_combined_counts) +
  
  geom_point(aes(x = schedule_label,
                 y = n_species, col = site),
             position = position_dodge(width = 0.75), pch = 21) +

  geom_point(data = sched_combined_counts, 
             aes(x = schedule_label,
                 y = n_species, col = site),
             stat = "summary",
             fun = "mean",
             size = 3,
             position = position_dodge(width = 0.75)) +
  
  geom_errorbar(data = sched_dist_table,
                aes(x = schedule_label,
                    ymin = mean - se, ymax = mean + se, col = site),
                width = 0.2, position = position_dodge(width = 0.75)) +
  
  geom_line(data = sched_predict,
            aes(x = schedule_label,
                y = predicted, col = site),
            linetype = 2, linewidth = 1.2) +

  labs(
    x = "Number of minutes recorded within the hour",
    y = "Species richness detected\nper audiomoth device",
    colour = "Habitat") +
  
  scale_colour_manual(
    values = c("BDWD" = "seagreen", "BDMD" = "goldenrod"),
    labels = c("BDWD" = "Woodland", "BDMD" = "Moorland"),
    name = "Habitat") +
  
  theme_minimal() +
  
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14))

# view the plot
sched_plot

# change x axis scale to see smaller intervals




#### How does the distance between audiomoths affect the number and identity of species detected? ####

BD_pilot_dist <- BD_pilot_data


##### Randomly pair audiomoths within each habitat #####

# extract all unique combinations of site, habitat and audiomoth_ID for reference
unique_devices <- BD_pilot_dist %>% 
  distinct(site, habitat, audiomoth_ID)

generate_pairs <- function(df) {
  df %>% 
    # keep unique devices per habitat/site
    distinct(site, habitat, audiomoth_ID, lat_coord, lon_coord) %>% 
    
    # split by habitat
    group_split(habitat, site) %>% 
    # what is happening here??
    map_dfr(function(habitat_df) {
      
      # calculate the number of devices
      n_devices <- nrow(habitat_df)
      
      # if there is an odd number, drop the last one (as it will not pair)
      if (n_devices %% 2 != 0) {
        habitat_df <- habitat_df %>%  slice(-sample(1:n_devices,1))
      }
      
      # shuffle the devices randomly within habitat
      shuffled_devices <- habitat_df %>%  sample_frac(1)
      
      # pair the devices by row
      pairs <- tibble(
        # pair the devices
        audiomoth_1 = shuffled_devices$audiomoth_ID[seq(1, nrow(shuffled_devices), by = 2)],
        audiomoth_2 = shuffled_devices$audiomoth_ID[seq(2, nrow(shuffled_devices), by = 2)],
        # extract coordinates for each pair
        lat1 = shuffled_devices$lat_coord[seq(1, nrow(shuffled_devices), by = 2)],
        lon1 = shuffled_devices$lon_coord[seq(1, nrow(shuffled_devices), by = 2)],
        lat2 = shuffled_devices$lat_coord[seq(2, nrow(shuffled_devices), by = 2)],
        lon2 = shuffled_devices$lon_coord[seq(2, nrow(shuffled_devices), by = 2)],
        # retain site and habitat information for rejoining with the main dataframe
        site = shuffled_devices$site[seq(1, nrow(shuffled_devices), by = 2)],
        # site = shuffled_devices$site[1]
        habitat = shuffled_devices$habitat[1]
        )
      
      # add column denoting pair ID
      pairs <- pairs %>% 
        mutate(pair_ID = paste0(habitat, "_pair", row_number()))
      
      # calculate the distance between paired devices in meters
      pairs <- pairs %>% 
        rowwise() %>% 
        mutate(distance = distHaversine(c(lon1, lat1), c(lon2, lat2))) %>% 
        ungroup()
  
      return(pairs)
    })
}

# run the function
audiomoth_pairs <- generate_pairs(BD_pilot_dist) 

# long format, so each device/site combination has it's own row again
audiomoth_pairs <- audiomoth_pairs %>% 
  pivot_longer(cols = c(audiomoth_1, audiomoth_2),
               values_to = "audiomoth_ID") %>% 
  select(site, habitat, audiomoth_ID, pair_ID, distance)

# join this back into the full dataset
BD_pilot_dist <- BD_pilot_dist %>% 
  left_join(audiomoth_pairs, by = c("site", "audiomoth_ID", "habitat"))
# check data set
head(BD_pilot_dist)

# filter out rows without a valid pair
BD_pilot_dist <- BD_pilot_dist %>% 
  filter(!is.na(pair_ID))


##### Count the species detected #####

dist_combined_counts <- BD_pilot_dist %>%
  group_by(site, habitat, audiomoth_ID, pair_ID, distance) %>%
  summarise(n_species = n_distinct(common_n),.groups = "drop")
# check data
head(dist_combined_counts)


##### Visualise the data #####

dist_plot <-
  ggplot(dist_combined_counts, aes(x = distance,
                                   y = n_species, colour = site)) +
  geom_point() +
  labs(
    x = "Distance between paired devices (m)",
    y = "Total species\ndetected per device pair",
    colour = "Habitat") +
  scale_colour_manual(
    values = c("BDWD" = "seagreen", "BDMD" = "goldenrod"),
    labels = c("BDWD" = "Woodland", "BDMD" = "Moorland"),
    name = "Habitat") +
  theme_minimal() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14))
# view the plot
dist_plot

# change to use model outputs?


##### Statistically Analyse the Data #####

hist(dist_combined_counts$n_species)

# formally classify the site content as a factor rather than character
dist_combined_counts$site <- as.factor(dist_combined_counts$site)
# check this has worked
levels(dist_combined_counts$site)

# model to test the impact of the number of days recorded
dist_model <- lmer(n_species ~ distance * site + (1|audiomoth_ID), data = dist_combined_counts)
dist_model <- lm(n_species ~ distance * site, data = dist_combined_counts)


# check distribution using histogram
hist(residuals(dist_model))
# check assumptions for glmer model
check_model(dist_model)

# model output - NOT TO BE USED YET, DATA CLEANING INCOMPLETE
summary(dist_model)
