# clear environment
rm(list=ls())

# load required packages
library(dplyr)
library(stringr)
library(tools)
library(readr)
library(readxl)
library(writexl)
library(tidyverse)


#### Easter Bavelaw Dataset ####


##### Woodland Data #####

# provide file path to csv files
woodland_folder1 <- "F:/dissertation_data/easter_bavelaw/BirdNET_Output/woodland"

# list all csv files within woodland_folder1
woodland_files1 <- list.files(path = woodland_folder1, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)
# check list
woodland_files1

# exclude the parameters csv file
woodland_files1 <- woodland_files1[woodland_files1 != "F:/dissertation_data/easter_bavelaw/BirdNET_Output/BirdNET_analysis_params.csv"]
# check list
woodland_files1

# read and combine all csvs into one dataframe
EB_wood_data <- lapply(woodland_files1, function(file) {
  df <- read_csv(file, show_col_types = FALSE)
  return(df)
}) %>% bind_rows()

head(EB_wood_data)


##### Moorland Data #####

# provide file path to csv files
moorland_folder1 <- "F:/dissertation_data/easter_bavelaw/BirdNET_Output/moorland"

# list all csv files within woodland_folder1
moorland_files1 <- list.files(path = moorland_folder1, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)
# check list
moorland_files1

# exclude the parameters csv file
moorland_files1 <- moorland_files1[moorland_files1 != "F:/dissertation_data/easter_bavelaw/BirdNET_Output/BirdNET_analysis_params.csv"]
# check list
moorland_files1

# read and combine all csvs into one dataframe
EB_moor_data <- lapply(moorland_files1, function(file) {
  df <- read_csv(file, show_col_types = FALSE)
  return(df)
}) %>% bind_rows()

head(EB_moor_data)


##### Merging both habitats into one df #####

# add columns to denote field site
EB_wood_data <- EB_wood_data %>%
  mutate(site = "EB") # i.e. easter bavelaw
EB_moor_data <- EB_moor_data %>%
  mutate(site = "EB") # i.e. easter bavelaw
# add columns to denote habitat
EB_wood_data$habitat <- "woodland"
EB_moor_data$habitat <- "moorland"
# check data
head(EB_wood_data)
head(EB_moor_data)

EB_data <- bind_rows(EB_wood_data, EB_moor_data)
head(EB_data)


#### Baddinsgill Dataset ####


##### Woodland Data #####

# provide file path to csv files
woodland_folder2 <- "F:/dissertation_data/baddinsgill/BirdNET_Output/woodland"

# list all csv files within woodland_folder1
woodland_files2 <- list.files(path = woodland_folder2, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)
# check list
woodland_files2

# exclude the parameters csv file
woodland_files2 <- woodland_files2[woodland_files2 != "F:/dissertation_data/baddinsgill/BirdNET_Output/BirdNET_analysis_params.csv"]
# check list
woodland_files2

# read and combine all csvs into one dataframe
BD_wood_data <- lapply(woodland_files2, function(file) {
  df <- read_csv(file, show_col_types = FALSE)
  return(df)
}) %>% bind_rows()

head(BD_wood_data)


##### Moorland Data #####

# provide file path to csv files
moorland_folder2 <- "F:/dissertation_data/baddinsgill/BirdNET_Output/moorland"

# list all csv files within woodland_folder1
moorland_files2 <- list.files(path = moorland_folder2, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)
# check list
moorland_files2

# exclude the parameters csv file
moorland_files2 <- moorland_files2[moorland_files2 != "F:/dissertation_data/baddinsgill/BirdNET_Output/BirdNET_analysis_params.csv"]
# check list
moorland_files2

# read and combine all csvs into one dataframe
BD_moor_data <- lapply(moorland_files2, function(file) {
  df <- read_csv(file, show_col_types = FALSE)
  return(df)
}) %>% bind_rows()

head(BD_moor_data)


##### Merging both habitats into one df #####

# add columns to denote field site
BD_wood_data <- BD_wood_data %>%
  mutate(site = "BD") # i.e. baddinsgill
BD_moor_data <- BD_moor_data %>%
  mutate(site = "BD") # i.e. baddinsgill
# add columns to denote habitat
BD_wood_data$habitat <- "woodland"
BD_moor_data$habitat <- "moorland"
# check data
head(BD_wood_data)
head(BD_moor_data)

BD_data <- bind_rows(BD_wood_data, BD_moor_data)
head(BD_data)


#### Combining all sites data ####

phase2_data <- bind_rows(EB_data, BD_data)
head(phase2_data)


#### Data tidying ####

##### Extracting meta data from filepath #####

# check table structure
head(phase2_data)

# extract meta data from the file path stored in each row of the data frame
phase2_data <- phase2_data %>% 
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
head(phase2_data)

##### Improve headings for easier coding #####

# check current headings
colnames(phase2_data)

# rename columns using tidyverse package
phase2_data <- phase2_data %>% 
  rename(
    detect_start = `Start (s)`,
    detect_end = `End (s)`,
    scientific_n = `Scientific name`,
    common_n = `Common name`,
    conf = Confidence,
    file_n = File)

##### Calculate time of detection ######

# check structure of each required column
str(phase2_data$recording_time)
str(phase2_data$detect_start)

# insert colons into time data
phase2_data <- phase2_data %>% 
  mutate(
    # Insert colons to convert HHMMSS to HH:MM:SS
    recording_time_colon = gsub("^(\\d{2})(\\d{2})(\\d{2})", "\\1:\\2:\\3", recording_time),
    
    # convert recording start time from character to date-time
    recording_time_conv = as.POSIXct(recording_time_colon, format = "%H:%M:%S", tz = "UTC"),
    
    # calculate detection start time
    detect_start_time = recording_time_conv + detect_start,
    
    # calculate detection end time
    detect_end_time = recording_time_conv + detect_end,
    
    # reformat both
    detect_start_time = format(detect_start_time, "%H:%M:%S"),
    detect_end_time = format(detect_end_time, "%H:%M:%S")
    
  )


# remove obsolete columns
phase2_data <- phase2_data %>% 
  select(-detect_start, -detect_end, -recording_time_colon, - recording_time_conv)

head(phase2_data)


##### Additional Meta Data #####

# import meta data
metadata <- read_xlsx("./audiomoth_data/phase2_metadata.xlsx")
head(metadata)

# join the meta data to the raw datasheet
phase2_data <- phase2_data %>% 
  left_join(metadata, by = c("site","recording_date", "audiomoth_ID", "habitat"))

# check data
View(phase2_data)


###### Rearrange data frame ######

# rearrange columns
phase2_data <- phase2_data %>% 
  select(site, habitat, recording_date, audiomoth_ID, audiomoth_owner, SDcard_type, SDcard_size, lat_coord, lon_coord, recording_time, detect_start_time, detect_end_time, file_n, scientific_n, common_n, conf)
# check dataset
View(phase2_data)

# save to project directory
write_xlsx(phase2_data, "./audiomoth_data/phase2_BirdNETOutput.xlsx")


#### Removing Impossible Species ####
# use if removing more species than done when generating the species list
# i.e. hashed out species, which were only identified as impossible by one of two experts

# import data set
phase2_data <- read_xlsx("./audiomoth_data/phase2_BirdNETOutput.xlsx") # times preserved in xlsx format
head(phase2_data)

filtered_phase2_data <- phase2_data %>% filter(!(common_n == "Hooded Crow" |
                                                 common_n == "Crested Tit" |
                                                 common_n == "Atlantic Puffin" |
                                                 common_n == "Redwing" |
                                                 common_n == "Black Redstart" |
                                                 common_n == "Brant" |
                                                 common_n == "Common Loon" |
                                                 common_n == "Spotted Redshank" |
                                                 common_n == "Yellow-browed Warbler" |
                                                 common_n == "Jack Snipe" |
                                                 common_n == "Brambling" |
                                                 common_n == "Fieldfare" |
                                                 common_n == "Black-bellied Plover" |
                                                 common_n == "Rock Pipit" |
                                                 common_n == "Red-throated Loon" |
                                                 common_n == "Scottish Crossbill" |
                                                 common_n == "Bluethroat" |
                                                 common_n == "Snow Bunting" |
                                                 common_n == "Red-billed Chough" |
                                                 common_n == "Black Guillemot" |
                                                 common_n == "Arctic Loon" |
                                                 common_n == "Leach's Storm-Petrel" |
                                                 common_n == "Greater White-fronted Goose" |
                                                 common_n == "Long-tailed Duck" |
                                                 common_n == "Pink-footed Goose" |
                                                 common_n == "Bohemian Waxwing" |
                                                 common_n == "European Storm-Petrel" |
                                                 common_n == "Manx Shearwater" |
                                                 common_n == "Barnacle Goose" |
                                                 common_n == "Iceland Gull" |
                                                 common_n == "Sandwich Tern" |
                                                 common_n == "Bearded Reedling" |
                                                 common_n == "Greater Scaup" |
                                                 common_n == "Red-breasted Merganser" |
                                                 common_n == "Velvet Scoter" |
                                                 common_n == "Little Tern" |
                                                 common_n == "Long-tailed Jaeger" |
                                                 common_n == "Northern Gannet" |
                                                 common_n == "Ruddy Turnstone" |
                                                 common_n == "Common Eider" |
                                                 common_n == "Common Murre" |
                                                 common_n == "Common Scoter" |
                                                 common_n == "Common Shelduck" |
                                                 common_n == "Glossy Ibis" |
                                                 common_n == "Northern Shoveler" |
                                                 common_n == "Parasitic Jaeger" |
                                                 common_n == "Sanderling" 
))

# check rows were removed
count(unique(phase2_data))
count(unique(filtered_phase2_data))

# save filtered data
write_xlsx(filtered_phase2_data, "./audiomoth_data/phase2_BirdNETOutput.xlsx")
