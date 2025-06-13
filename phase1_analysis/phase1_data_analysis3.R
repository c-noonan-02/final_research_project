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
compute_similarity <- function(site, BD_pilot_data, time_tolerance = 0.5) { # have tried 0.5, 1, and 2 seconds
  
  # filter the full dataset to just the site(s) of interest
  site_data <- BD_pilot_data[BD_pilot_data$site == site, ]
  
  # create time window for each detection (+/- time tolerance)
  site_data$start_time <- site_data$date_time - time_tolerance
  site_data$end_time <- site_data$date_time + time_tolerance
  
  # convert to data tables for fast overlap operations
  dt1 <- as.data.table(site_data)
  dt2 <- copy(dt1)
  
  # get unique coordinates for each device
  device_locations <- unique(dt1[, .(audiomoth_ID, lat_coord, lon_coord)])
  
  # creeate dummy start/end for dt2 to pass the foverlaps check
  dt2[, `:=`(start_time = date_time, end_time = date_time)]
  
  # set keys for overlapping time window join
  setkey(dt1, start_time, end_time)
  setkey(dt2, start_time, end_time)
  
  # filter to matching species across different devices
  matched <- foverlaps(dt2, dt1, by.x = c("start_time", "end_time"),
                        by.y = c("start_time", "end_time"),
                        type = "within",
                       nomatch = 0L)
  
  # filter to matching species detections only
  matched <- matched[
    scientific_n == i.scientific_n & audiomoth_ID != i.audiomoth_ID,
    .(audiomoth_ID1 = audiomoth_ID, audiomoth_ID2 = i.audiomoth_ID, date_time)
  ]
  
  # create unique pair_IDs to avoid duplicate comparisons
  matched[, pair_ID := paste0(pmin(audiomoth_ID1, audiomoth_ID2), "_",
                             pmax(audiomoth_ID1, audiomoth_ID2))]
  
  # ensure both devices exist in the location data (remove mislabels)
  valid_devices <- device_locations$audiomoth_ID
  
  matched <- matched[
    audiomoth_ID1 %in% valid_devices & audiomoth_ID2 %in% valid_devices
  ]
  
  # keep only distinct co-detections
  matched <- unique(matched)
  
  # count number of shared detections per device per pair
  shared_counts <- matched[, .N, by = .(pair_ID)]
  
  # count total detections per device
  device_counts <- dt1[, .N, by = audiomoth_ID]
  
  # generate all unique device pairs within the site
  device_pairs <- CJ(audiomoth_ID1 = device_locations$audiomoth_ID,
                     audiomoth_ID2 = device_locations$audiomoth_ID)[audiomoth_ID1 < audiomoth_ID2]
  device_pairs[, pair_ID := paste0(pmin(audiomoth_ID1, audiomoth_ID2), "_",
                                   pmax(audiomoth_ID1, audiomoth_ID2))]

  # merge in device coordinates and compute 'Haversine' distances
  device_pairs <- merge(device_pairs, device_locations, by.x = "audiomoth_ID1", by.y = "audiomoth_ID")
  setnames(device_pairs, c("lat_coord", "lon_coord"), c("lat_coord1", "lon_coord1"))
  device_pairs <- merge(device_pairs, device_locations, by.x = "audiomoth_ID2", by.y = "audiomoth_ID")
  setnames(device_pairs, c("lat_coord", "lon_coord"), c("lat_coord2", "lon_coord2"))
  device_pairs[, distance := distHaversine(cbind(lon_coord1, lat_coord1), cbind(lon_coord2, lat_coord2))]

  # merge in the shared detection counts
  similarity_data <- copy(device_pairs)
  similarity_data <- merge(similarity_data, shared_counts, by = "pair_ID", all.x = TRUE)
  
  # handle missing shared values by replacing NA with 0
  if (!"N" %in% names(similarity_data)) {
    similarity_data[, shared := 0]
  } else {
    similarity_data[, shared := fifelse(is.na(N), 0, N)]
    similarity_data[, N := NULL]
  }
  
  # merge in total detection counts for each device
  similarity_data <- merge(similarity_data, device_counts, by.x = "audiomoth_ID1", by.y = "audiomoth_ID", all.x = TRUE)
  setnames(similarity_data, "N", "total1")
  similarity_data <- merge(similarity_data, device_counts, by.x = "audiomoth_ID2", by.y = "audiomoth_ID", all.x = TRUE)
  setnames(similarity_data, "N", "total2")
  
  # calculate proportion of shared detections
  similarity_data[, similarity := shared / (total1 + total2 - shared)]
  
  # add the site label
  similarity_data[, site := site]
  
  # return clean output
  return(similarity_data[, .(site, audiomoth_ID1, audiomoth_ID2, distance, similarity)])
  
}


##### Run function #####

all_sites <- unique(BD_pilot_data$site)

# compute similarities for all sites
similarity_BD <- rbindlist(
  lapply(all_sites, function(s) compute_similarity(s, BD_pilot_data, time_tolerance = 1)),
  use.names = TRUE
)




#### Create Similarity Function BY SPECIES ####

# determine the 40 most frequently detected species
data_table <- as.data.table(BD_pilot_data)
top_species <- data_table[, .N, by = .(scientific_n, common_n)][order(-N)][1:40]

# create function to calculate similarity for each site
compute_similarity_sp <- function(site, BD_pilot_data, species_filter = NULL, time_tolerance = 1) { # have tried 0.5, 1, and 2 seconds
  
  # filter the full dataset to just the site(s) of interest
  site_data <- BD_pilot_data[BD_pilot_data$site == site, ]
  
  # ensure date time data are in POSIXct format
  site_data$date_time <- as.POSIXct(site_data$date_time)
  time_tolerance <- as.difftime(time_tolerance, units = "secs")
  
  # convert to data.table
  site_data <- as.data.table(site_data)
  
  # optionally filter to the species of interest
  if(!is.null(species_filter)) {
    site_data <- site_data[scientific_n %in% species_filter]
  }
  
  # create time window for each detection (+/- time tolerance)
  site_data$start_time <- site_data$date_time - time_tolerance
  site_data$end_time <- site_data$date_time + time_tolerance
  
  # convert to data tables for fast overlap operations
  dt1 <- as.data.table(site_data)
  dt2 <- copy(dt1)
  
  # get unique coordinates for each device
  device_locations <- unique(dt1[, .(audiomoth_ID, lat_coord, lon_coord)])
  
  # creeate dummy start/end for dt2 to pass the foverlaps check
  dt2[, `:=`(start_time = date_time, end_time = date_time)]
  
  # set keys for overlapping time window join
  setkey(dt1, start_time, end_time)
  setkey(dt2, start_time, end_time)
  
  # filter to matching species across different devices
  matched <- foverlaps(dt2, dt1, by.x = c("start_time", "end_time"),
                       by.y = c("start_time", "end_time"),
                       type = "within",
                       nomatch = 0L)
  
  # filter to matching species detections only
  matched <- matched[
    scientific_n == i.scientific_n & audiomoth_ID != i.audiomoth_ID,
    .(audiomoth_ID1 = audiomoth_ID, audiomoth_ID2 = i.audiomoth_ID,
      scientific_n, common_n, date_time) # with addition of species
  ]
  
  # create unique pair_IDs to avoid duplicate comparisons
  matched[, pair_ID := paste0(pmin(audiomoth_ID1, audiomoth_ID2), "_",
                              pmax(audiomoth_ID1, audiomoth_ID2))]
  
  # ensure both devices exist in the location data (remove mislabels)
  valid_devices <- device_locations$audiomoth_ID
  
  matched <- matched[
    audiomoth_ID1 %in% valid_devices & audiomoth_ID2 %in% valid_devices
  ]
  
  # keep only distinct co-detections
  matched <- unique(matched)
  
  # count number of shared same-species detections per device per pair
  shared_counts <- matched[, .N, by = .(pair_ID, scientific_n, common_n)]
  
  # count total detections per device
  device_counts <- dt1[, .N, by = .(audiomoth_ID, scientific_n)]
  
  # generate all unique device pairs within the site
  device_pairs <- CJ(audiomoth_ID1 = device_locations$audiomoth_ID,
                     audiomoth_ID2 = device_locations$audiomoth_ID)[audiomoth_ID1 < audiomoth_ID2]
  device_pairs[, pair_ID := paste0(pmin(audiomoth_ID1, audiomoth_ID2), "_",
                                   pmax(audiomoth_ID1, audiomoth_ID2))]
  
  # merge in device coordinates and compute 'Haversine' distances
  device_pairs <- merge(device_pairs, device_locations, by.x = "audiomoth_ID1", by.y = "audiomoth_ID")
  setnames(device_pairs, c("lat_coord", "lon_coord"), c("lat_coord1", "lon_coord1"))
  device_pairs <- merge(device_pairs, device_locations, by.x = "audiomoth_ID2", by.y = "audiomoth_ID")
  setnames(device_pairs, c("lat_coord", "lon_coord"), c("lat_coord2", "lon_coord2"))
  device_pairs[, distance := distHaversine(cbind(lon_coord1, lat_coord1), cbind(lon_coord2, lat_coord2))]
  
  # extract all species with matched detections
  all_species <- as.data.table(unique(matched[, .(scientific_n, common_n)]))
  
  # ensure data are all data.table format
  device_pairs <- as.data.table(device_pairs)
  
  # add dummy key to each
  all_species[, tmp_key := 1]
  device_pairs[, tmp_key := 1]
  
  # expand device pairs across all species
  device_pairs_exp <- merge(all_species, device_pairs, by = "tmp_key", allow.cartesian = TRUE)
  
  # drop dummy key
  device_pairs_exp[, tmp_key := NULL]
  
  # merge in the shared detection counts
  similarity_data <- copy(device_pairs_exp) # use this expanded grid as the base
  similarity_data <- merge(similarity_data, shared_counts, by = c("pair_ID", "scientific_n", "common_n"), all.x = TRUE)
  
  # handle missing shared values by replacing NA with 0
  if (!"N" %in% names(similarity_data)) {
    similarity_data[, shared := 0]
  } else {
    similarity_data[, shared := fifelse(is.na(N), 0, N)]
    similarity_data[, N := NULL]
  }
  
  # merge in total detection counts for each device
  similarity_data <- merge(similarity_data, device_counts, by.x = c("audiomoth_ID1", "scientific_n"), by.y = c("audiomoth_ID", "scientific_n"), all.x = TRUE)
  setnames(similarity_data, "N", "total1")
  similarity_data <- merge(similarity_data, device_counts, by.x = c("audiomoth_ID2", "scientific_n"), by.y = c("audiomoth_ID", "scientific_n"), all.x = TRUE)
  setnames(similarity_data, "N", "total2")
  
  # convert NAs, produced where only one device in a pair had a detection for the species, into zeros
  similarity_data[is.na(total1), total1 := 0]
  similarity_data[is.na(total2), total2 := 0]
  
  # calculate proportion of shared detections
  similarity_data[, similarity := shared / (total1 + total2 - shared)]
  
  # add the site label
  similarity_data[, site := site]
  
  # return clean output
  return(similarity_data[, .(site, audiomoth_ID1, audiomoth_ID2, scientific_n, common_n, distance, similarity)])
  
}


##### Run function #####

# extract the top species names
species_list <- top_species$scientific_n

# compute similarities for all sites
similarity_sp_BD <- rbindlist(
  lapply(all_sites, function(s) compute_similarity_sp(s, BD_pilot_data, species_filter = species_list, time_tolerance = 1)),
  use.names = TRUE
)




#### Visualise the Data ####

##### Pooled Data #####
combined_plot <- ggplot(similarity_BD, aes(x = distance, y = similarity, colour = site)) +
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
print(combined_plot)

# just woodland
wood_plot <- ggplot(similarity_BD[site == "BDWD"], aes(x = distance, y = similarity)) +
  geom_point(colour = "seagreen", alpha = 0.7) +
  geom_smooth(colour = "seagreen", se = FALSE) +
  labs(x = "Distance between devices (m)", y = "Proportion of shared detections",
       colour = "Site") +
  theme_minimal() +
  theme(
    legend.position = "right")
print(wood_plot)

# just moorland
moor_plot <- ggplot(similarity_BD[site == "BDMD"], aes(x = distance, y = similarity)) +
  geom_point(colour = "goldenrod", alpha = 0.7) +
  geom_smooth(colour = "goldenrod", se = FALSE) +
  labs(x = "Distance between devices (m)", y = "Proportion of shared detections",
       colour = "Site") +
  theme_minimal() +
  theme(
    legend.position = "right")
print(moor_plot)


##### Species-Specific Data #####

# extract species information for plots
species_info <- unique(similarity_sp_BD[, .(scientific_n, common_n)])

# loop through each species and create the plot
plots_list <- lapply(seq_len(nrow(species_info)), function(i) {
  
  # set names
  sci_n <- species_info$scientific_n[i]
  com_n <- species_info$common_n[i]
  
  # filter data for one species and create the plot
  sp_data <- similarity_sp_BD[scientific_n == sci_n]
  
  # create the plot
  p <- ggplot(sp_data, aes(x = distance, y = similarity, colour = site)) +
    geom_point(alpha = 0.7) +
    geom_smooth(se = FALSE) +
    scale_colour_manual(
      values = c("BDWD" = "seagreen", "BDMD" = "goldenrod"),
      labels = c("BDWD" = "Woodland", "BDMD" = "Moorland"),
      name = "Habitat") +
    labs(x = "Distance between devices (m)", y = "Proportion of shared detections",
         title = com_n,
         colour = "Site") +
    theme_minimal() +
    theme(
      legend.position = "right")
  
  # return the plot object
  return(p)
})

# QUESTION FOR ME - Should I set all y limits to 1 for easier comparison between species?

# print the first plot
print(plots_list[[10]])

# print the first 10 plots
invisible({ lapply(plots_list[1:40], print) }) # invisible ensures last graph is not plotted twice





#### Save Data & Plots ####

# save each combined plot
ggsave("./phase1_analysis/plots/BD_similarity_plot.png", plot = combined_plot, height = 6, width = 10)
ggsave("./phase1_analysis/plots/BDWD_similarity_plot.png", plot = wood_plot, height = 6, width = 8)
ggsave("./phase1_analysis/plots/BDMD_similarity_plot.png", plot = moor_plot, height = 6, width = 8)


# save each species plot as a PNG
for (i in seq_along(species_list)) {
  ggsave(
    filename = paste0("./phase1_analysis/plots/sp_similarity_plots/BD_similarity_plot_", species_list[i], ".png"),
    plot = plots_list[[i]],
    height = 6,     width = 8
  )
}
