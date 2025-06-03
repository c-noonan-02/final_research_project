# clear environment
rm(list=ls())

# load required packages
library(dplyr)
library(stringr)
library(tools)

# read in the example file
data <- read.csv(file_path)
head(data) # check data

# use a loop to extract meta data from the file path stored in each row of the data frame
data <- data %>% 
  mutate(
    
    # extract and save the audiomoth ID
    audiomoth_ID = sapply(strsplit(File, "/|\\\\"), function(x) x[length(x) - 1]), 
    
    # extract the habitat/site info
    site_ID = sapply(strsplit(File, "/|\\\\"), function(x) x[length(x) - 2]), # will give a better folder name eventually
    
    # extract the file name
    file_name = file_path_sans_ext(basename(File)),
    
    # extract the recording date
    recording_date = as.Date(substr(file_name, 1, 8), format = "%Y%m%d"), 
    
    # extract the start time of the recording
    recording_time = ifelse(
      nchar(file_name) >= 15,
      format(strptime(substr(file_name, 10, 15), format = "%H%M%S"), "%H%M%S"),
      NA
    )
  ) %>%
  select(-file_name) # do not save path_parts or file_name as new columns

head(data)
