# This R script contains all code relating to pairwise comparisons of species
# detections to determine spatial redundancy when using Audiomoths to detect
# bird species on a site.


#### Set-Up ####

# clear environment
rm(list=ls())

# load required packages
library(dplyr)
library(readxl)
library(ggplot2)
library(geosphere)
library(lubridate)
library(purrr)
library(fuzzyjoin)
library(data.table)

# import data set
BD_pilot_data <- read_xlsx("./audiomoth_data/BD2025_BirdNETOutput2.xlsx") # times preserved in xlsx format
head(BD_pilot_data)

# create column containing date and time combined
BD_pilot_data <- BD_pilot_data %>% 
  mutate(date_time = ymd_hms(paste(recording_date, detect_start_time)))


#### Create Similarity Function ####

# create function to calculate similarity for each site
compute_similarity <- function(site, BD_pilot_data, time_tolerance = 2) {
  site_data <- BD_pilot_data[BD_pilot_data$site == site, ] # filter data by site
  
  # create time window (to reduce number of pairs needing to run through later on)
  site_data$start_time <- site_data$date_time - time_tolerance
  site_data$end_time <- site_data$date_time + time_tolerance
  
  # convert to data.table
  dt1 <- as.data.table(site_data)
  dt2 <- copy(dt1)
  
  # get device coordinates
  device_locations <- unique(dt1[, .(audiomoth_ID, lat_coord, lon_coord)])
  
  # create dummy interval columns for dt2 to get past error
  dt2[, `:=`(start_time = date_time, end_time = date_time)]
  
  # ensure correct column orders
  setkey(dt1, start_time, end_time)
  setkey(dt2, start_time, end_time)
  
  # find all dt2 rows within dt1's time window
  matched <- foverlaps(dt2, dt1, by.x = c("start_time", "end_time"),
                        by.y = c("start_time", "end_time"),
                        type = "within",
                       nomatch = 0L)
  
  # filter for matching species and different devices
  matched <- matched[
    scientific_n == i.scientific_n & audiomoth_ID != i.audiomoth_ID,
    .(audiomoth_ID1 = audiomoth_ID, audiomoth_ID2 = i.audiomoth_ID, date_time)
  ]
  
  # sort to avoid duplicates
  matched[, pair_ID := paste0(pmin(audiomoth_ID1, audiomoth_ID2), "_",
                             pmax(audiomoth_ID1, audiomoth_ID2))]
  
  valid_devices <- device_locations$audiomoth_ID
  
  matched <- matched[
    audiomoth_ID1 %in% valid_devices & audiomoth_ID2 %in% valid_devices
  ]
  
  matched <- unique(matched)
  
  # count shared detections
  shared_counts <- matched[, .N, by = .(pair_ID)]
  
  # count total detections per device
  device_counts <- dt1[, .N, by = audiomoth_ID]
  
  # generate all device pairs for this device
  device_pairs <- CJ(audiomoth_ID1 = device_locations$audiomoth_ID,
                     audiomoth_ID2 = device_locations$audiomoth_ID)[audiomoth_ID1 < audiomoth_ID2]
  device_pairs[, pair_ID := paste0(pmin(audiomoth_ID1, audiomoth_ID2), "_",
                                   pmax(audiomoth_ID1, audiomoth_ID2))]

  # add coordinates and compute distances 
  device_pairs <- merge(device_pairs, device_locations, by.x = "audiomoth_ID1", by.y = "audiomoth_ID")
  setnames(device_pairs, c("lat_coord", "lon_coord"), c("lat_coord1", "lon_coord1"))
  device_pairs <- merge(device_pairs, device_locations, by.x = "audiomoth_ID2", by.y = "audiomoth_ID")
  setnames(device_pairs, c("lat_coord", "lon_coord"), c("lat_coord2", "lon_coord2"))
  device_pairs[, distance := distHaversine(cbind(lon_coord1, lat_coord1), cbind(lon_coord2, lat_coord2))]

  # add shared counts to df
  similarity_data <- copy(device_pairs)
  similarity_data <- merge(similarity_data, shared_counts, by = "pair_ID", all.x = TRUE)
  # Ensure 'shared' column is defined even if N is entirely missing
  if (!"N" %in% names(similarity_data)) {
    similarity_data[, shared := 0]
  } else {
    similarity_data[, shared := fifelse(is.na(N), 0, N)]
    similarity_data[, N := NULL]
  }
  
  # add total detections to df
  similarity_data <- merge(similarity_data, device_counts, by.x = "audiomoth_ID1", by.y = "audiomoth_ID", all.x = TRUE)
  setnames(similarity_data, "N", "total1")
  similarity_data <- merge(similarity_data, device_counts, by.x = "audiomoth_ID2", by.y = "audiomoth_ID", all.x = TRUE)
  setnames(similarity_data, "N", "total2")
  
  # calculate similarity
  similarity_data[, similarity := shared / (total1 + total2 - shared)]
  similarity_data[, site := site]
  
  #return(similarity_data) # to debug
  return(similarity_data[, .(site, audiomoth_ID1, audiomoth_ID2, distance, similarity)])
  
}


#### Run function ####

all_sites <- unique(BD_pilot_data$site)

# compute similarities for one site as test
similarity_BD <- rbindlist(
  lapply(all_sites, function(s) compute_similarity(s, BD_pilot_data)),
  use.names = TRUE
)


#### Visualise the Data ####

# all data
ggplot(similarity_BD, aes(x = distance, y = similarity, colour = site)) +
  geom_point(alpha = 0.7) +
  geom_smooth(se = FALSE) +
  scale_colour_manual(
    values = c("BDWD" = "seagreen", "BDMD" = "goldenrod"),
    labels = c("BDWD" = "Woodland", "BDMD" = "Moorland"),
    name = "Habitat") +
  labs(x = "Distance between devices (m)", y = "Proportion of shared detections",
       colour = "Site") +
  theme_minimal() +
  theme(
    legend.position = "right")

# just woodland
ggplot(similarity_BD[site == "BDWD"], aes(x = distance, y = similarity)) +
  geom_point(colour = "seagreen",alpha = 0.7) +
  geom_smooth(colour = "seagreen", se = FALSE) +
  labs(x = "Distance between devices (m)", y = "Proportion of shared detections",
       colour = "Site") +
  theme_minimal() +
  theme(
    legend.position = "right")

# just moorland
# just woodland
ggplot(similarity_BD[site == "BDMD"], aes(x = distance, y = similarity)) +
  geom_point(colour = "goldenrod",alpha = 0.7) +
  geom_smooth(colour = "goldenrod", se = FALSE) +
  labs(x = "Distance between devices (m)", y = "Proportion of shared detections",
       colour = "Site") +
  theme_minimal() +
  theme(
    legend.position = "right")


#### Save Data & Plots ####

# to be continued