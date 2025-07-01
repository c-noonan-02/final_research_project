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
write_xlsx(BD_pilot_data, "./audiomoth_data/BD2025_BirdNETOutput1.xlsx")



#### New Session ####

# import dataset again
BD_pilot_data <- read_xlsx("./audiomoth_data/BD2025_BirdNETOutput1.xlsx") # times preserved in xlsx format
head(BD_pilot_data)

##### Improve headings for easier coding #####

# check current headings
colnames(BD_pilot_data)

# rename columns using tidyverse package
BD_pilot_data <- BD_pilot_data %>% 
  rename(
    detect_start = `Start (s)`,
    detect_end = `End (s)`,
    scientific_n = `Scientific name`,
    common_n = `Common name`,
    conf = Confidence,
    file_n = File)

##### Calculate time of detection #####

# check structure of each required column
str(BD_pilot_data$recording_time)
str(BD_pilot_data$detect_start)

# insert colons into time data
BD_pilot_data <- BD_pilot_data %>% 
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
BD_pilot_data <- BD_pilot_data %>% 
  select(-detect_start, -detect_end, -recording_time_colon, - recording_time_conv)

head(BD_pilot_data)

# save updated dataframe to files
write_xlsx(BD_pilot_data, "./audiomoth_data/BD2025_BirdNETOutput2.xlsx")


##### Additional Meta Data #####

# software info
# SD card info
# department owned by
# habitat
# x and y coordinates

# import dataset again
BD_pilot_data <- read_xlsx("./audiomoth_data/BD2025_BirdNETOutput2.xlsx") # times preserved in xlsx format
head(BD_pilot_data)
metadata <- read_xlsx("./audiomoth_data/audiomoth_metadata.xlsx")
head(metadata)

# join the meta data to the raw datasheet
BD_pilot_data <- BD_pilot_data %>% 
  left_join(metadata, by = c("site","recording_date", "audiomoth_ID"))

# check data
View(BD_pilot_data)


##### Rearrange data frame #####

# rearrange columns
BD_pilot_data <- BD_pilot_data %>% 
  select(site, habitat, recording_date, audiomoth_ID, audiomoth_owner, SDcard_ID, lat_coord, lon_coord, recording_time, detect_start_time, detect_end_time, file_n, scientific_n, common_n, conf)
# check dataset
View(BD_pilot_data)

# save updated dataframe to files
write_xlsx(BD_pilot_data, "./audiomoth_data/BD2025_BirdNETOutput2.xlsx")


#### Removing Impossible Species ####

# remove species deemed impossible by experts
# would have been removed from the custom species list prior to analysis
# just removing them post-hoc here to save time rather than re-analyse

# import data set
BD_pilot_data <- read_xlsx("./audiomoth_data/BD2025_BirdNETOutput2.xlsx") # times preserved in xlsx format
head(BD_pilot_data)

filtered_pilot_data <- BD_pilot_data %>% filter(!(common_n == "Crested Tit" |
                                                    common_n == "Atlantic Puffin" |
                                                    #common_n == "Redwing" |
                                                    #common_n == "Black Redstart" |
                                                    common_n == "Brant" |
                                                    common_n == "Common Loon" |
                                                    common_n == "Spotted Redshank" |
                                                    common_n == "Jack Snipe" |
                                                    common_n == "Brambling" |
                                                    #common_n == "Fieldfare" |
                                                    common_n == "Black-bellied Plover" |
                                                    common_n == "Rock Pipit" |
                                                    common_n == "Red-throated Loon" |
                                                    #common_n == "Scottish Crossbill" |
                                                    common_n == "Bluethroat" |
                                                    #common_n == "Snow Bunting" |
                                                    common_n == "Red-billed Chough" |
                                                    common_n == "Black Guillemot" |
                                                    common_n == "Arctic Loon" |
                                                    common_n == "Leach's Storm-Petrel" |
                                                    common_n == "Greater White-fronted Goose" |
                                                    common_n == "Long-tailed Duck" |
                                                    #common_n == "Pink-footed Goose" |
                                                    common_n == "Bohemian Waxwing" |
                                                    common_n == "European Storm-Petrel" |
                                                    common_n == "Manx Shearwater" |
                                                    #common_n == "Barnacle Goose" |
                                                    common_n == "Iceland Gull" |
                                                    #common_n == "Sandwich Tern" |
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
                                                    #common_n == "Common Shelduck" |
                                                    common_n == "Glossy Ibis" |
                                                    #common_n == "Northern Shoveler" |
                                                    common_n == "Parasitic Jaeger" #|
                                                  #common_n == "Sanderling" 
))

# check rows were removed
count(unique(BD_pilot_data))
count(unique(filtered_pilot_data))

# save filtered data
write_xlsx(filtered_pilot_data, "./audiomoth_data/BD2025_BirdNETOutput3.xlsx")

# re-run if I decide to have a harsher filtering threshold