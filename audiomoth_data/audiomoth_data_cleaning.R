# clear environment
rm(list=ls())

# load required packages
library(readxl)
library(writexl)
library(dplyr)

# add meta data to a single excel file
file_path <- "F:/dissertation_data/pilot_woodland/BirdNET_Output/Audiomoth_1/20250515_000000.BirdNET.results.csv"

# extract sections of the file path
path_parts <- strsplit(file_path, "/|\\\\")[[1]] # this splits on \ or /
path_parts # check

# extract audiomoth_ID
audiomoth_ID <- path_parts[length(path_parts) - 1]
audiomoth_ID # check

# extract the filename
file_name <- tools::file_path_sans_ext(basename(file_path))
file_name # check
# remove ".BirdNET.results"

# extract the date
date_str <- substr(file_name, 1, 8)
recording_date <- as.Date(date_str, format = "%Y%m%d")
recording_date # check

# extract the time
time_str <- substr(file_name, 10, 15)
recording_time <- format(strptime(time_str, format = "%H%M%S"), "%H%M%S")
recording_time # check
