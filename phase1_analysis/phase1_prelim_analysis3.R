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
library(data.table)
library(ggtext)
library(cowplot)

# import data set
BD_pilot_data <- read_xlsx("./audiomoth_data/PT2025_BirdNETOutput4.xlsx") # times preserved in xlsx format
head(BD_pilot_data)

# create column containing date and time combined
BD_pilot_data <- BD_pilot_data %>% 
  mutate(date_time = ymd_hms(paste(recording_date, detect_start_time)))


#### Create Similarity Function ####

# create function to calculate similarity for each site
compute_similarity <- function(site, BD_pilot_data, time_tolerance = 1) {
  
  # filter the full dataset to just the site(s) of interest
  site_data <- BD_pilot_data[BD_pilot_data$site == site, ]
  
  # convert to data.table
  site_data.table <- as.data.table(site_data)
  
  # add a unique detection ID
  site_data.table[, detection_ID := .I]
  
  # create time window for each detection (+/- time tolerance)
  site_data.table$start_time <- site_data.table$date_time - time_tolerance
  site_data.table$end_time <- site_data.table$date_time + time_tolerance
  
  # make  copies of the data
  dt1 <- copy(site_data.table)
  dt2 <- copy(site_data.table)
  
  # get unique coordinates for each device
  device_locations <- unique(dt1[, .(audiomoth_ID, lat_coord, lon_coord)])
  
  # creeate dummy start/end for dt2 to pass the foverlaps check
  dt2[, `:=`(start_time = date_time, end_time = date_time)]
  
  # set keys for overlapping time window join
  setkey(dt1, start_time, end_time)
  setkey(dt2, start_time, end_time)
  
  # filter to matching species across different devices
  matched <- foverlaps(dt2, dt1, type = "within", nomatch = 0L)
  
  # # filter to matching species across different devices
  # matched <- foverlaps(dt2, dt1, by.x = c("start_time", "end_time"),
  #                      by.y = c("start_time", "end_time"),
  #                      type = "within",
  #                      nomatch = 0L)
  
  # keep species matches across different devices and store detection IDs
  matched <- matched[
    scientific_n == i.scientific_n & audiomoth_ID != i.audiomoth_ID,
    .(audiomoth_ID1 = audiomoth_ID, audiomoth_ID2 = i.audiomoth_ID,
    det_ID1 = detection_ID, det_ID2 = i.detection_ID)
  ]
  
  # make sure each detection pair is only counted once
  matched[, det_pair := paste0(pmin(det_ID1, det_ID2), "_", pmax(det_ID1, det_ID2))]
  matched <- unique(matched, by = "det_pair")
  
  # create unique pair_IDs to avoid duplicate comparisons
  matched[, pair_ID := paste0(pmin(audiomoth_ID1, audiomoth_ID2), "_",
                             pmax(audiomoth_ID1, audiomoth_ID2))]
  
  # ensure both devices exist in the location data (remove mislabels)
  valid_devices <- device_locations$audiomoth_ID
  
  matched <- matched[
    audiomoth_ID1 %in% valid_devices & audiomoth_ID2 %in% valid_devices
  ]
  
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
  
  # # handle missing shared values by replacing NA with 0
  # if (!"N" %in% names(similarity_data)) {
  #   similarity_data[, shared := 0]
  # } else {
  #   similarity_data[, shared := fifelse(is.na(N), 0, N)]
  #   similarity_data[, N := NULL]
  # }
  
  # handle missing shared values by replacing NA with 0
  similarity_data[, shared := fifelse(is.na(N), 0, N)]
  similarity_data[, N := NULL]
  
  # merge in total detection counts for each device
  similarity_data <- merge(similarity_data, device_counts, by.x = "audiomoth_ID1", by.y = "audiomoth_ID", all.x = TRUE)
  setnames(similarity_data, "N", "total1")
  similarity_data <- merge(similarity_data, device_counts, by.x = "audiomoth_ID2", by.y = "audiomoth_ID", all.x = TRUE)
  setnames(similarity_data, "N", "total2")
  
  # # SANITY CHECK
  # similarity_data[, `:=`(
  #   check_total = total1 + total2 - shared
  # )]
  # print(similarity_data[check_total < shared, ])
  # 
  # SANITY CHECK
  bad_rows <- similarity_data[shared > (total1 + total2)]
  if (nrow(bad_rows) > 0) {
    print("Warning: Still some shared > total cases!")
    print(bad_rows)
  }
  
  # calculate proportion of shared detections
  similarity_data[, similarity := shared / (total1 + total2 - shared)]
  
  # add the site label
  similarity_data[, site := site]
  
  # return clean output
  return(similarity_data[, .(site, audiomoth_ID1, audiomoth_ID2, distance, similarity)])
  
}


##### Run function #####

all_sites <- unique(BD_pilot_data$site)

# compute similarities for all sites, with 1s overlap
similarity_1 <- rbindlist(
  lapply(all_sites, function(s) compute_similarity(s, BD_pilot_data, time_tolerance = 1)),
  use.names = TRUE
)
# compute similarities for all sites, with 0.5s overlap
similarity_0.5 <- rbindlist(
  lapply(all_sites, function(s) compute_similarity(s, BD_pilot_data, time_tolerance = 0.5)),
  use.names = TRUE
)



#### Create Similarity Function BY SPECIES ####

# determine the 40 most frequently detected species
data_table <- as.data.table(BD_pilot_data)
top_species <- data_table[, .N, by = .(scientific_n, common_n)][order(-N)][1:40]

# create function to calculate similarity for each site
compute_similarity_sp <- function(site, BD_pilot_data, species_filter = NULL, time_tolerance = 1) {
  
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
  
  # Add unique detection ID
  site_data[, detection_ID := .I]
  
  # create time window for each detection (+/- time tolerance)
  site_data$start_time <- site_data$date_time - time_tolerance
  site_data$end_time <- site_data$date_time + time_tolerance
  
  # convert to data tables for fast overlap operations
  dt1 <- as.data.table(site_data)
  dt2 <- copy(dt1)
  
  # create dummy start/end for dt2 to pass the foverlaps check
  dt2[, `:=`(start_time = date_time, end_time = date_time)]
  
  # set keys for overlapping time window join
  setkey(dt1, start_time, end_time)
  setkey(dt2, start_time, end_time)
  
  # get unique coordinates for each device
  device_locations <- unique(site_data[, .(audiomoth_ID, lat_coord, lon_coord)])
  
  # filter to matching species across different devices
  matched <- foverlaps(dt2, dt1, type = "within", nomatch = 0L)
  
  # filter to matching species detections across different devices
  matched <- matched[
    scientific_n == i.scientific_n & audiomoth_ID != i.audiomoth_ID,
    .(audiomoth_ID1 = pmin(audiomoth_ID, i.audiomoth_ID),
     audiomoth_ID2 = pmax(audiomoth_ID, i.audiomoth_ID),
     det_ID1 = detection_ID,
     det_ID2 = i.detection_ID,
     scientific_n,
     common_n,
     date_time)
  ]
  
  # deduplicate by detection ID pair
  matched[, det_pair := paste0(pmin(det_ID1, det_ID2), "_", pmax(det_ID1, det_ID2))]
  matched <- unique(matched, by = "det_pair")
  
  # create unique pair IDs to avoid duplicate comparisons
  matched[, pair_ID := paste0(audiomoth_ID1, "_", audiomoth_ID2)]
  
  # ensure both devices exist in the location data (remove mislabels)
  valid_devices <- device_locations$audiomoth_ID
  
  matched <- matched[
    audiomoth_ID1 %in% valid_devices & audiomoth_ID2 %in% valid_devices
  ]
  
  # group by species, pair and time to de-duplicate co-detections
  # i.e. count only one co-detection per species-pair per time even if both devices caught multiple overlapping calls
  matched <- matched[, .SD[1], by = .(pair_ID, scientific_n, date_time)]
  
  # # keep only distinct co-detections
  # matched <- unique(matched)
  
  # count number of shared same-species detections per device per pair
  shared_counts <- matched[, .N, by = .(pair_ID, scientific_n, common_n)]
  
  # count total detections per device
  device_counts <- site_data[, .N, by = .(audiomoth_ID, scientific_n)]
  
  # generate all unique device pairs within the site
  device_pairs <- CJ(audiomoth_ID1 = device_locations$audiomoth_ID,
                     audiomoth_ID2 = device_locations$audiomoth_ID)[audiomoth_ID1 < audiomoth_ID2]
  device_pairs[, pair_ID := paste0(audiomoth_ID1, "_", audiomoth_ID2)]
  
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
  similarity_data[, shared := fifelse(is.na(N), 0, N)][, N := NULL]
  
  # merge in total detection counts for each device
  similarity_data <- merge(similarity_data, device_counts,
                           by.x = c("audiomoth_ID1", "scientific_n"),
                           by.y = c("audiomoth_ID", "scientific_n"), all.x = TRUE)
  setnames(similarity_data, "N", "total1")
  similarity_data <- merge(similarity_data, device_counts,
                           by.x = c("audiomoth_ID2", "scientific_n"),
                           by.y = c("audiomoth_ID", "scientific_n"), all.x = TRUE)
  setnames(similarity_data, "N", "total2")
  
  # convert NAs, produced where only one device in a pair had a detection for the species, into zeros
  similarity_data[is.na(total1), total1 := 0]
  similarity_data[is.na(total2), total2 := 0]
  
  # remove cases where both devices had zero detections for that species
  similarity_data <- similarity_data[!(total1 == 0 & total2 == 0)]
  
  # # SANITY CHECK
  # similarity_data[, `:=`(
  #   check_total = total1 + total2 - shared
  # )]
  # print(similarity_data[check_total < shared, ])
  # 
  # SANITY CHECK
  bad_rows <- similarity_data[shared > (total1 + total2)]
  if (nrow(bad_rows) > 0) {
    print("Warning: Still some shared > total cases!")
    print(bad_rows)
  }
  
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
# extract site list
all_sites <- unique(BD_pilot_data$site)

# compute similarities for all sites
similarity_sp <- rbindlist(
  lapply(all_sites, function(s) compute_similarity_sp(s, BD_pilot_data, species_filter = species_list, time_tolerance = 1)),
  use.names = TRUE
)




#### Visualise the Data ####

##### Pooled Data #####

# plot the relationship with 1s overlap
combined_plot_1 <- ggplot(similarity_1, aes(x = distance, y = similarity, colour = site)) +
  geom_point(alpha = 0.7) +
  geom_smooth(se = FALSE) +
  scale_colour_manual(
    values = c("BDWD" = "seagreen", "BDMD" = "goldenrod"),
    labels = c("BDWD" = "Woodland", "BDMD" = "Moorland"),
    name = "Habitat") +
  labs(title = "Overlap = 1s", x = "Distance between devices (m)", y = "Proportion of shared detections",
       colour = "Site") +
  theme_minimal() +
  theme(
    legend.position = "right")
print(combined_plot_1)

# plot the relationship with 0.5s overlap
combined_plot_0.5 <- ggplot(similarity_0.5, aes(x = distance, y = similarity, colour = site)) +
  geom_point(alpha = 0.7) +
  geom_smooth(se = FALSE) +
  scale_colour_manual(
    values = c("BDWD" = "seagreen", "BDMD" = "goldenrod"),
    labels = c("BDWD" = "Woodland", "BDMD" = "Moorland"),
    name = "Habitat") +
  labs(title = "Overlap = 0.5s", x = "Distance between devices (m)", y = "Proportion of shared detections",
       colour = "Site") +
  theme_minimal() +
  theme(
    legend.position = "right")
print(combined_plot_0.5)


##### Species-Specific Data #####

# extract species information for plots
species_info <- unique(similarity_sp[, .(scientific_n, common_n)])

# loop through each species and create the plot
plots_list <- lapply(seq_len(nrow(species_info)), function(i) {
  
  # set names
  sci_n <- species_info$scientific_n[i]
  com_n <- species_info$common_n[i]
  
  # create plot title
  title_text <- paste0("<b>", LETTERS[i], ".</b> ", com_n, " (<i>", sci_n, "</i>)")
  
  # filter data for one species and create the plot
  sp_data <- similarity_sp[scientific_n == sci_n]
  
  # create the plot
  p <- ggplot(sp_data, aes(x = distance, y = similarity, colour = site)) +
    geom_point(alpha = 0.7) +
    scale_y_continuous(limits = c(0, 1)) +
    geom_smooth(se = FALSE) +
    scale_colour_manual(
      values = c("BDWD" = "seagreen", "BDMD" = "goldenrod"),
      labels = c("BDWD" = "Woodland", "BDMD" = "Moorland"),
      name = "Habitat") +
    labs(x = "Distance between devices (m)", y = "Proportion of shared detections",
         title = title_text,
         colour = "Site") +
    theme_minimal() +
    theme(
      legend.position = "right",
      plot.title = ggtext::element_markdown(),
      plot.title.position = "plot",
      )
  
  # return the plot object
  return(p)
})

# print the first plot to check formatting
print(plots_list[[1]])

# combine plots into a single figure
species_plots <- plot_grid(
  plotlist = plots_list,
  ncol = 3,
  nrow = 10
)

# ATTEMPT TO ADD BORDERS

# # build a function to add a full-plot border
# add_border <- function(plot, colour = "black", size = 0.5) {
#   ggdraw(plot) +
#     theme(plot.margin = margin(0,0,0,0)) +
#     draw_plot(plot) +
#     draw_rect(color = colour, size = size, fill = NA)
# }
# 
# # apply borders to the plots
# species_plots <- lapply(plots_list, add_border)
# 
# # print the first plot to check formatting
# print(plots_list[[1]])


#### Save Data & Plots ####

# save each combined plot
ggsave("./phase1_analysis/plots/BD_similarity_plot_1.png", plot = combined_plot_1, height = 6, width = 10)
ggsave("./phase1_analysis/plots/BD_similarity_plot_0.5.png", plot = combined_plot_0.5, height = 6, width = 10)

# save the species plots
ggsave("./phase1_analysis/plots/BD_similarity_plot_species.png", plot = species_plots, height = 30, width = 20)
