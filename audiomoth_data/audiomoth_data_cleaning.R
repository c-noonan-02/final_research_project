# clear environment
rm(list=ls())

# load required packages
library(dplyr)
library(stringr)
library(tools)
library(readr)


#### Merging all datasets ####


##### Woodland Data #####

# provide file path to csv files
woodland_folder <- "F:/dissertation_data/pilot_woodland/BirdNET_Output"

# list all csv files within woodland_folder
woodland_files <- list.files(path = woodland_folder, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)
# check list
woodland_files

# exclude the parameters csv file
woodland_files <- woodland_files[woodland_files != "F:/dissertation_data/pilot_woodland/BirdNET_Output/BirdNET_analysis_params.csv"]
# check list
woodland_files

# read and combine all csvs into one dataframe
woodland_data <- lapply(woodland_files, function(file) {
  df <- read_csv(file, show_col_types = FALSE)
  return(df)
}) %>% bind_rows()

head(woodland_data)


##### Moorland Data #####

# provide file path to csv files
moorland_folder <- "F:/dissertation_data/pilot_moorland/BirdNET_Output"

# list all csv files within woodland_folder
moorland_files <- list.files(path = moorland_folder, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)
# check list
moorland_files

# exclude the parameters csv file
moorland_files <- moorland_files[moorland_files != "F:/dissertation_data/pilot_moorland/BirdNET_Output/BirdNET_analysis_params.csv"]
# check list
moorland_files

# force expected column types - FOUND THIS FIX FROM CHATGPT
col_types_spec <- cols(
  `Start (s)` = col_double(),
  `End (s)` = col_double(),
  `Scientific name` = col_character(),
  `Common name` = col_character(),
  Confidence = col_double(),
  File = col_character()
)

# read and combine all csvs into one dataframe
moorland_data <- lapply(moorland_files, function(file) {
  df <- read_csv(file, col_types = col_types_spec, show_col_types = FALSE)
  return(df)
}) %>% bind_rows()

head(moorland_data)


##### Merging both habitats into one df #####

# add columns to denote field site
woodland_data <- woodland_data %>%
  mutate(site = "BDWD") # i.e. baddinsgill woodland
moorland_data <- moorland_data %>%
  mutate(site = "BDMD") # i.e. baddinsgill moorland
head(woodland_data)
head(moorland_data)

BD_pilot_data <- bind_rows(woodland_data, moorland_data)
head(BD_pilot_data)




#### Extracting meta data from filepath ####

# check table structure
head(BD_pilot_data)

# extract meta data from the file path stored in each row of the data frame
BD_pilot_data <- BD_pilot_data %>% 
  mutate(
    
    # extract and save the audiomoth ID
    audiomoth_ID = sapply(strsplit(File, "/|\\\\"), function(x) x[length(x) - 1]), 
    
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

# check resulting df
head(BD_pilot_data)

# save to project directory
write_csv(BD_pilot_data, "./audiomoth_data/BD2025_BirdNETOutput.csv")
