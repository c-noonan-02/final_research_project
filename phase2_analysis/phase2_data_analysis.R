# clear environment
rm(list=ls())

# load required packages
library(dplyr)
library(tidyr)
library(readxl)
library(writexl)
library(lme4)
library(performance)
library(tidyverse)

# import audiomoth data sets
audiomoth_data <- read_xlsx("./audiomoth_data/phase2_BirdNETOutput.xlsx")
head(audiomoth_data)
# import bird survey data
survey_data <- read_xlsx("./birdsurvey_data/phase2_BirdSurveys.xlsx")
head(survey_data)




#### Data Arrangement ####

# ensure column names are the same
survey_data <- survey_data %>% rename(common_n = species)
audiomoth_data <- audiomoth_data %>% rename(date = recording_date)


# list all species in each data set
unique(audiomoth_data$common_n)
unique(survey_data$common_n)




#### Collectors Curve ####

# might run some code to plot the number of species detected with increasing survey effort - i.e. time
# see if they both saturate or if only one method does, compare across habitats also

# order the detection_period categories (shortest to longest ammount of survey time)
survey_data <- survey_data %>%
  mutate(detection_period = factor(detection_period, levels = c("5_mins", "10_mins", "15_mins", "20_mins", "25_mins", "30_mins", "35_mins", "40_mins", "45_mins"), ordered = TRUE))

# get all unique detection periods, even if no species were detected
periods_per_group <- survey_data %>%
  distinct(site, habitat, detection_period)

# summarise the species detected, exlcuding NAs in common_n
species_detected <- survey_data %>% 
  filter(!is.na(common_n)) %>% 
  group_by(site, habitat, detection_period) %>% 
  summarise(species_detected = list(unique(common_n)), .groups = "drop")

# join the two to ensure periods with no additional species detected are kept in the dataframe
cumulative_species <- periods_per_group %>%
  left_join(species_detected, by = c("site", "habitat", "detection_period")) %>%  # group by site, habitat and detection_period
  mutate(species_detected = replace_na(species_detected, list(character(0)))) %>% 
  arrange(site, habitat, detection_period) %>% 
  group_by(site, habitat) %>% 
  mutate(
    cumulative_species = accumulate(species_detected, ~ union(.x, .y)), 
    n_species = lengths(cumulative_species)
  )

# plot the accumulation curve for each site
cumulative_plot <-
  ggplot(cumulative_species, aes(x = detection_period, y = n_species, group = habitat, colour = habitat)) +
  geom_line(size = 1, lty = 2) +
  geom_point(pch = 19, size = 2) +
  facet_wrap(~ site, ncol = 1,
             labeller = labeller(site = c(BD = "Baddinsgill", EB = "Easter Bavelaw"))) +  # make separate panels for each site
  labs(
    x = "Survey Length (mins)",
    y = "Cumulative species detected",
    colour = "Habitat") +
  scale_x_discrete(labels = c(
    "5_mins" = "5",
    "10_mins" = "10",
    "15_mins" = "15",
    "20_mins" = "20",
    "25_mins" = "25",
    "30_mins" = "30",
    "35_mins" = "35",
    "40_mins" = "40",
    "45_mins" = "45")) +
  scale_colour_manual(
    values = c("woodland" = "seagreen", "moorland" = "goldenrod"),
    labels = c("woodland" = "Woodland", "moorland" = "Moorland"),
    name = "Habitat") +
  theme_minimal() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        strip.text = element_text(size = 14))

# plot the accumulation curve across sites
cumulative_plot_smooth <-
  ggplot(cumulative_species, aes(x = detection_period, y = n_species, group = habitat, colour = habitat)) +
  geom_smooth(aes(fill = habitat), alpha = 0.2, size = 1, lty = 2) +
  geom_point(pch = 19, size = 2) +
  labs(
    x = "Survey Length (mins)",
    y = "Cumulative species detected",
    colour = "Habitat") +
  scale_x_discrete(labels = c(
    "5_mins" = "5",
    "10_mins" = "10",
    "15_mins" = "15",
    "20_mins" = "20",
    "25_mins" = "25",
    "30_mins" = "30",
    "35_mins" = "35",
    "40_mins" = "40",
    "45_mins" = "45")) +
  scale_colour_manual(
    values = c("woodland" = "seagreen", "moorland" = "goldenrod"),
    labels = c("woodland" = "Woodland", "moorland" = "Moorland"),
    name = "Habitat") +
  scale_fill_manual(
    values = c("woodland" = "seagreen", "moorland" = "goldenrod"),
    labels = c("woodland" = "Woodland", "moorland" = "Moorland"),
    name = "Habitat") +
  theme_minimal() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14))

# save plot of species accumulation curves by site
cumulative_plot
ggsave("./phase2_analysis/plots/birdsurvey_acc_plot1.png", plot = cumulative_plot, height = 6, width = 8)
# save plot of species accumulation curves by site
cumulative_plot_smooth
ggsave("./phase2_analysis/plots/birdsurvey_acc_plot2.png", plot = cumulative_plot_smooth, height = 5, width = 8)




#### Generation of Presence Data ####

# add presence column to each dataset
audiomoth_data <- audiomoth_data %>% mutate(presence = 1)
survey_data <- survey_data %>% mutate(presence = 1)

# get all unique site-habitat combinations
site_habitat <- audiomoth_data %>% 
  select(site, habitat) %>% 
  distinct() %>% 
  bind_rows(survey_data %>%  select(site, habitat) %>%  distinct()) %>% 
  distinct()

# get all unique species
all_species <- union(audiomoth_data$common_n, survey_data$common_n)

# create full grid of unique combinations
full_grid <- expand.grid(
  site = unique(site_habitat$site),
  habitat = unique(site_habitat$habitat),
  common_n = all_species,
  stringsAsFactors = FALSE
)

# join full grid with datasets

# audiomoth presence/absence data
audiomoth_pa <- full_grid %>% 
  left_join(audiomoth_data %>% select(site, habitat, common_n) %>% 
              distinct() %>% # keep only one row per site-habitat-species
              mutate(presence = 1),
            by = c("site", "habitat", "common_n")) %>%
  mutate(presence = replace_na(presence, 0))
# add column describing the method of data collection
audiomoth_pa$survey_method <- "PAM"

# bird survey presence/absence data
survey_pa <- full_grid %>% 
  left_join(survey_data %>% select(site, habitat, common_n) %>% 
              distinct() %>% # keep only one row per site-habitat-species
              mutate(presence = 1),
            by = c("site", "habitat", "common_n")) %>%
  mutate(presence = replace_na(presence, 0))
# remove all rows where species = NA (this was only needed to evaluate species detections over time)
survey_pa <- survey_pa %>% filter(!is.na(common_n))
# add column describing the method of data collection
survey_pa$survey_method <- "bird_survey"

# combine data from both approaches into one dataframe
phase2_pa <- bind_rows(audiomoth_pa, survey_pa)
head(phase2_pa)

# rearrange the columns
phase2_pa <- phase2_pa %>% 
  select(survey_method, site, habitat, common_n, presence)
# check dataset
View(phase2_pa)


# TO DO
# incorporate date into presence absence data sets to control for date?
# or not needed due to few dates covered?




#### Visualisation of Relationship ####

##### Contingency Table: Species Richness #####
# generate contingency table of species totals

# calculate the total number of species detected by survey method and habitat
species_summary <- phase2_pa %>%
  filter(presence == 1) %>%
  group_by(survey_method, habitat) %>%
  summarise(n_species = n_distinct(common_n), .groups = "drop")

# add row totals for habitat and survey method
species_summary_pivot <- species_summary %>%
  pivot_wider(names_from = habitat, values_from = n_species, values_fill = 0) %>%
  mutate(all_habitats = rowSums(across(where(is.numeric)))) %>%
  bind_rows(
    species_summary %>%
      group_by(habitat) %>%
      summarise(across(n_species, sum), survey_method = "all_methods") %>%
      pivot_wider(names_from = habitat, values_from = n_species, values_fill = 0) %>%
      mutate(all_habitats = rowSums(across(where(is.numeric))))
  )
# print contingency table
species_summary_pivot


##### Bar Graph: Species Richness #####
# generate bar graph of the species richness detected in each habitat, by each approach

# count number of species detected in each habitat, by each survey approach
richness <- phase2_pa %>%
  group_by(site, survey_method, habitat, common_n) %>%
  summarise(present = max(presence), .groups = "drop") %>%
  group_by(site, survey_method, habitat) %>%
  summarise(n_species = sum(present), .groups = "drop")

# plot the number of species detected in each habitat by each survey approach
# split by site
richness_barplot1 <- ggplot(richness, aes(x = survey_method, y = n_species, fill = habitat)) +
  geom_col(position = "dodge") +
  facet_wrap(~ site, ncol = 1,
             labeller = labeller(site = c(BD = "Baddinsgill", EB = "Easter Bavelaw"))) +  # make separate panels for each site
  labs(
    x = "Survey Method",
    y = "Number of Species Detected",
    fill = "Habitat"
  ) +
  scale_x_discrete(labels = c(
    "bird_survey" = "Traditional Bird Survey",
    "PAM" = "Passive Acoustic Monitoring")) +
  scale_fill_manual(
    values = c("woodland" = "seagreen", "moorland" = "goldenrod"),
    labels = c("woodland" = "Woodland", "moorland" = "Moorland"),
    name = "Habitat") +
  theme_minimal() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        strip.text = element_text(size = 14))

# plot the number of species detected in each habitat by each survey approach
# not split by site
richness_barplot2 <- ggplot(richness, aes(x = survey_method, y = n_species, fill = habitat)) +
  geom_col(position = "dodge") +
  labs(
    x = "Survey Method",
    y = "Number of Species Detected",
    fill = "Habitat"
  ) +
  scale_x_discrete(labels = c(
    "bird_survey" = "Traditional Bird Survey",
    "PAM" = "Passive Acoustic Monitoring")) +
  scale_fill_manual(
    values = c("woodland" = "seagreen", "moorland" = "goldenrod"),
    labels = c("woodland" = "Woodland", "moorland" = "Moorland"),
    name = "Habitat") +
  theme_minimal() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14))

# save both plots
richness_barplot1
ggsave("./phase2_analysis/plots/richness_barplot1.png", plot = richness_barplot1, height = 7, width = 8)
richness_barplot2
ggsave("./phase2_analysis/plots/richness_barplot2.png", plot = richness_barplot2, height = 5, width = 8)



##### Bar Graph: Species Richness #####
# generate a heat map of species detections made by each approach in each habitat

# tiles of all species detected
# separated by survey method to compare
species_matrix <- phase2_pa %>%
  group_by(common_n, survey_method, habitat) %>%
  summarise(present = max(presence), .groups = "drop")

# extract the most common species
top_species <- species_matrix %>%
  filter(present == 1) %>%
  count(common_n, sort = TRUE) %>%
  slice_head(n = 50) %>%
  pull(common_n)

# filter dataset to the common species
species_matrix_top <- species_matrix %>%
  filter(common_n %in% top_species)

# create a column that combines presence and habitat - to allow to distinguish habitat by colour
species_matrix_top <- species_matrix_top %>%
  mutate(
    fill_group = case_when(
      present == 1 & habitat == "woodland" ~ "present_wood",
      present == 1 & habitat == "moorland" ~ "present_moor",
      present == 0 ~ "absent"
    )
  )

# plot a 'heatmap' of species detections by species, in each habitat
richness_heatmap <- ggplot(species_matrix_top, aes(x = survey_method, y = fct_reorder(common_n, desc(common_n)), fill = fill_group)) +
  geom_tile(color = "white") +
  facet_wrap(~ habitat,
             labeller = labeller(habitat = c(woodland = "Woodland", moorland = "Moorland"))) + # make separate panels for each site
  scale_fill_manual(values = c("absent" = "grey90", "present_wood" = "seagreen", "present_moor" = "goldenrod"),
                    labels = c("absent" = "0", "present_wood" = "1", "present_moor" = "1"),
                    name = "Detected") +
  #scale_fill_manual(values = c("0" = "grey90", "1" = "khaki1"), name = "Detected") +
  labs(
    x = "Survey Method",
    y = "Species"
  ) +
  scale_x_discrete(labels = c(
    "bird_survey" = "Traditional Bird\nSurvey",
    "PAM" = "Passive Acoustic\nMonitoring")) +
  theme_minimal() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        strip.text = element_text(size = 14))

richness_heatmap
ggsave("./phase2_analysis/plots/richness_heatmap.png", plot = richness_heatmap, height = 15, width = 10)




#### Binomial GLMMs ####

# need to run these later
# check they work, and discuss all variables to fit with supervisors


##### Model 1: No Interactions #####
phase2_mod1 <- glmer(presence~survey_method+habitat+(1|common_n)+(1|site), data = phase2_pa, family = binomial)
# check model functionality
summary(phase2_mod1)
r2(phase2_mod1)
# check distribution using histogram
hist(residuals(phase2_mod1))
# check assumptions for glmer model
check_model(phase2_mod1)

##### Model 2: Interaction between method and species #####
phase2_mod2 <- glmer(presence~survey_method+habitat+common_n+(1|site)+survey_method:common_n, data = phase2_pa, family = binomial)
# check model functionality
summary(phase2_mod2)
r2(phase2_mod2)
# check distribution using histogram
hist(residuals(phase2_mod2))
# check assumptions for glmer model
check_model(phase2_mod2)

##### Model 3: Interaction between method and habitat #####
phase2_mod3 <- glmer(presence~survey_method+habitat+(1|common_n)+(1|site)+survey_method:habitat, data = phase2_pa, family = binomial)
# check model functionality
summary(phase2_mod3)
r2(phase2_mod3)
# check distribution using histogram
hist(residuals(phase2_mod3))
# check assumptions for glmer model
check_model(phase2_mod3)

##### Model 4: Interaction between method, species and habitat #####
phase2_mod4 <- glmer(presence~survey_method+habitat+common_n+(1|site)+survey_method:common_n:habitat, data = phase2_pa, family = binomial)
# check model functionality
summary(phase2_mod4)
r2(phase2_mod4)
# check distribution using histogram
hist(residuals(phase2_mod4))
# check assumptions for glmer model
check_model(phase2_mod4)
