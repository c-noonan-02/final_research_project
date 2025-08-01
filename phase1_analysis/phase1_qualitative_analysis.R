#### Script Set-Up ####

##### Clear Environment #####

rm(list=ls())

##### Load Packages #####

library(readxl)
library(writexl)
library(dplyr)
library(ggplot2)

##### Import Dataset #####



#### Species-Specific Confidence Thresholds ####

detections <- read_xlsx("./phase1_analysis/data/BD2025_species_summary.xlsx")
head(detections)

# convert confidence thresholds to numeric
detections$threshold_0.85 <- as.numeric(detections$threshold_0.85)
detections$threshold_0.90 <- as.numeric(detections$threshold_0.90)
detections$threshold_0.95 <- as.numeric(detections$threshold_0.95)
detections$threshold_0.99 <- as.numeric(detections$threshold_0.99)

# remove Eurasian Moorhen due to failed calculation of confidence intervals
detections <- detections %>% 
  mutate(
    across(
      .cols = starts_with("threshold_"),
      .fns = ~ if_else(common_n == "Eurasian Moorhen", NA_real_, .)
    )
  )

# convert any negative confidence thresholds to zero
detections_bounded <- detections %>% 
  mutate(
         threshold_0.85 = pmax(threshold_0.85, 0),
         threshold_0.90 = pmax(threshold_0.90, 0),
         threshold_0.95 = pmax(threshold_0.95, 0),
         threshold_0.99 = pmax(threshold_0.99, 0),
         )


##### 0.85 Confidence Threshold #####

# visualise species-specific confidence thresholds
hist(detections_bounded$threshold_0.85)

# improve visualisation
conf0.85_plot <- ggplot(detections_bounded, aes(x = threshold_0.85)) +
  
  geom_histogram(binwidth = 0.1, boundary = 0,
                 fill = "tan", colour = "black") +
  
  labs(
    x = "Species-specific Confidence Threshold (p>=0.85)",
    y = "Species Count"
  ) +
  
  scale_x_continuous(breaks = seq(0, 1, by = 0.1)) +
  scale_y_continuous(breaks = seq(0, 30, by = 5)) +
  
  theme_minimal()

# print plot
conf0.85_plot


##### 0.90 Confidence Threshold #####

# visualise species-specific confidence thresholds
hist(detections_bounded$threshold_0.90)

# improve visualisation
conf0.90_plot <- ggplot(detections_bounded, aes(x = threshold_0.90)) +
  
  geom_histogram(binwidth = 0.1, boundary = 0,
                 fill = "tan", colour = "black") +
  
  labs(
    x = "Species-specific Confidence Threshold (p>=0.90)",
    y = "Species Count"
  ) +
  
  scale_x_continuous(breaks = seq(0, 1, by = 0.1)) +
  scale_y_continuous(breaks = seq(0, 30, by = 5)) +
  
  theme_minimal()

# print plot
conf0.90_plot


##### 0.95 Confidence Threshold #####

# visualise species-specific confidence thresholds
hist(detections_bounded$threshold_0.95)

# improve visualisation
conf0.95_plot <- ggplot(detections_bounded, aes(x = threshold_0.95)) +
  
  geom_histogram(binwidth = 0.1, boundary = 0,
                 fill = "tan", colour = "black") +
  
  labs(
    x = "Species-specific Confidence Threshold (p>=0.95)",
    y = "Species Count"
  ) +
  
  scale_x_continuous(breaks = seq(0, 1, by = 0.1)) +
  scale_y_continuous(breaks = seq(0, 30, by = 5)) +
  
  theme_minimal()

# print plot
conf0.95_plot


##### 0.99 Confidence Threshold #####

# visualise species-specific confidence thresholds
hist(detections_bounded$threshold_0.99)

# improve visualisation
conf0.99_plot <- ggplot(detections_bounded, aes(x = threshold_0.99)) +
  
  geom_histogram(binwidth = 0.1, boundary = 0,
                 fill = "tan", colour = "black") +
  
  labs(
    x = "Species-specific Confidence Threshold (p>=0.99)",
    y = "Species Count"
  ) +
  
  scale_x_continuous(breaks = seq(0, 1, by = 0.1)) +
  scale_y_continuous(breaks = seq(0, 30, by = 5)) +
  
  theme_minimal()

# print plot
conf0.99_plot


#### To-Do List ####

# decide if bins are the right side
# decide appropriate colour
# create cowplot or something similar with all histograms in panels for results section
# update once final species are finalised