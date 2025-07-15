# build function to sort through the segments from both habitats
# randomly select segments from each habitat, up to a total of 100 total where possible (50 from each habitat)
# relocate selected segments to a new folder, called detections, with files from both habitats merged within the same folder to be reviewed collectively
# also relocate any segments from species unique to one habitat in their entirety to be reviewed as well
# so that 100 detections per species can be reviewed to generate species-specific confidence thresholds to be used across the whole dataset

#### Set Up ####

# clear environment
rm(list=ls())


#### Build Function ####
wav_file_selection <- function(path1, path2, dry_run = TRUE) {
  
  # specify if this is a dry run or not
  if (dry_run) {message("\n--- DRY RUN: No files will be modified. ---") 
  } else {message("\n--- NOT A DRY RUN: Files will be modified! ---")}
  
  # set- up the function
  base_dir <- dirname(path1) # select the base directory (the BirdNET_Review folder)
  # create a path for two new folders to store the sorted segments (selected and deleted ones)
  detections_dir <- file.path(base_dir, "detections") 
  deleted_dir <- file.path(base_dir, "deletions")
  # create these new folders
  dir.create(detections_dir, showWarnings = FALSE)
  dir.create(deleted_dir, showWarnings = FALSE)
  
  # list all immediate subdirectories (folder names, not file paths, and not into subfolders)
  subfolders1 <- list.dirs(path1, full.names =  FALSE, recursive = FALSE)
  subfolders2 <- list.dirs(path2, full.names =  FALSE, recursive = FALSE)
  # select subfolders common to both paths (species common to both habitats)
  common_subfolders <- intersect(subfolders1, subfolders2)
  # also select any subfolders unique to either path (species unique to one of the habitats)
  unique1 <- setdiff(subfolders1, subfolders2)
  unique2 <- setdiff(subfolders2, subfolders1)
  
  # extract the names of each focal folder
  name1 <- basename(path1)
  name2 <- basename(path2)
  
  
  # write a helper function to copy and rename the files
  copy_rename <- function(files, origin_name, target_root, subfolder, dry_run) {
    # go through all files provided
    for (file in files) {
      sub_rel <- gsub(paste0("^.*?", subfolder, "/?"), "", file)
      sub_dir <- dirname(sub_rel)
      orig_name <- basename(sub_rel)
      new_name <- paste0(origin_name, "_", orig_name) # create the new file name to denote which habitat the file was recorded in
      out_path <- file.path(target_root, subfolder, sub_dir, new_name) # write the filepath for the relocated file
      dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE) # create the new filepath
      
      # set-up so that a 'dry-run' can be run to test if the function is working
      if (dry_run) {
        # print message to explain the number of files that would be copied if run properly
        message("DRY RUN! Would copy: ", file, " -> ", out_path)
      } else { # If not move the files
        file.copy(file, out_path, overwrite = TRUE) # copy the selected files into the new directory
        file.remove(file) # cut the selected files from their original location
      }
    }
  }
  
  
  # process the matching subfolders (species found in both habitats)
  
  for (sub in common_subfolders) { # for each species found in both habitats
    message("\nProcessing matched subfolder: ", sub) # print a message to denote which folder is being processed
    
    # save the file paths for the subfolders (for the species currently being processed)
    subfolder1 <- file.path(path1, sub)
    subfolder2 <- file.path(path2, sub)
    
    # extract the files within each of the subfolders (i.e. all segments for the focal species)
    files1 <- list.files(subfolder1, pattern = "\\.wav$", full.names = TRUE, recursive = TRUE)
    files2 <- list.files(subfolder2, pattern = "\\.wav$", full.names = TRUE, recursive = TRUE)
    
    # count the number of .wav files in each subfolder
    n1 <- length(files1)
    n2 <- length(files2)
    # count the number of .wav files for each species in total
    total <- n1 + n2
    
    if (total <= 100) { # if there are 100 segments or less in total...
      retain1 <- n1
      retain2 <- n2
      # retain all of the segments
    } else { # if there are more than 100 segments for the species
      retain1 <- min(50, n1) # retain atleast 50 segments from folder1
      retain2 <- min(50, n2) # retain atleast 50 segments from folder2
      
      if ((n1 < 50 && n2 > 50) || 
          (n2 < 50 && n1 > 50)) {
        if (n1 < 50) { # if there are less than 50 segments in folder 1, but more than 50 in folder 2...
          retain1 <- n1 # keep all segments from folder 1
          retain2 <- min(100 - retain1, n2) # take up to 100 - retain1 from n2 (i.e. take enough to add up to 100 where possible, if not take all)
        } else { # if there are less than 50 segments in folder 2, but more than 50 in folder 1...
          retain2 <- n2 # keep all segments from folder 2
          retain1 <- min(100 - retain2, n1) # take up to 100 - retain2 from n1
        }
      }
    }
    
    set.seed(123) # keep the same randomised process each time - remove this to make the process completely random each time it is run
    
    # select the files to keep, if none have been selected by the above protocol then take the value character(0)
    files1_keep <- if (n1 > 0) sample(files1, retain1) else character(0)
    files2_keep <- if (n2 > 0) sample(files2, retain2) else character(0)
    
    # select any files in files1 that have not been selected in files1_keep to be 'deleted;
    files1_delete <- setdiff(files1, files1_keep)
    files2_delete <- setdiff(files2, files2_keep)
    
    # keep and move the selected segments to the new directory
    copy_rename(files1_keep, name1, detections_dir, sub, dry_run)
    copy_rename(files2_keep, name2, detections_dir, sub, dry_run)
    # remove the segments not selected to the new directory
    copy_rename(files1_delete, "", deleted_dir, sub, dry_run)
    copy_rename(files2_delete, "", deleted_dir, sub, dry_run)
    
    # print the number of segments retained and deleted
    message("   -> Retained ", length(c(files1_keep, files2_keep)), " files")
    message("   -> Deleted ", length(c(files1_delete, files2_delete)), " files")
  }
  
  # process the subfolders (species) unique to folder1
  for (sub in unique1) {
    message("\nProcessing unique folder from folder1: ", sub) # print a message to denote which folder is being processed
    # provide filepath to the subfolder being processed
    subfolder1 <- file.path(path1, sub)
    # extract the files within the subfolder (i.e. all segments for the focal species)
    files <- list.files(subfolder1, pattern = "\\.wav$", full.names = TRUE, recursive = TRUE)
    # keep and move the files to the new directory
    copy_rename(files, name1, detections_dir, sub, dry_run)
    # print message to explain the files moved
    message("   -> Retained ", length(files), " files to detections/")
  }
  
  # process the subfolders (species) unique to folder2
  for (sub in unique2) {
    message("\nProcessing unique folder from folder2: ", sub) # print a message to denote which folder is being processed
    # provide filepath to the subfolder being processed
    subfolder2 <- file.path(path2, sub)
    # extract the files within the subfolder (i.e. all segments for the focal species)
    files <- list.files(subfolder2, pattern = "\\.wav$", full.names = TRUE, recursive = TRUE)
    # keep and move the files to the new directory
    copy_rename(files, name2, detections_dir, sub, dry_run)
    # print message to explain the files moved
    message("   -> Retained ", length(files), " files to detections/")
  }

  # Function complete :)  
  if (dry_run) {message("\n--- DRY RUN COMPLETE: No files modified. ---")
  } else {message("\n--- PROCESS COMPLETE: File selection and transfer complete! ---")
    }
  }


#### Run Function ####
# set folder paths
folder1 <- "F:/dissertation_data/BirdNET_Review/woodland_species"
folder2 <- "F:/dissertation_data/BirdNET_Review/moorland_species"

# run the function
wav_file_selection(folder1, folder2, dry_run = FALSE)


#### Check for Additional Species ####

# need to check for species within Phase 2 dataset not within the Phase 1 dataset

# provide filepaths to phase2 data, and the review folder for phase 1 data
baddinsgill_folder <- "F:/dissertation_data/baddinsgill/BirdNET_Review"
easterbavelaw_folder <- "F:/dissertation_data/easter_bavelaw/BirdNET_Review"
review_folder <- "F:/dissertation_data/BirdNET_Review/detections"

# list all species in review data, and in phase 2 data
bad_subfolders <- list.dirs(baddinsgill_folder, full.names =  FALSE, recursive = FALSE)
eas_subfolders <- list.dirs(easterbavelaw_folder, full.names =  FALSE, recursive = FALSE)
rev_subfolders <- list.dirs(review_folder, full.names =  FALSE, recursive = FALSE)

# select any folders unique to the phase 2 datasets
bad_unique <- setdiff(bad_subfolders, rev_subfolders)
eas_unique <- setdiff(eas_subfolders, rev_subfolders)

# print any new species
bad_unique
eas_unique

# copy the segments for these into the folder for reviewing
# all had < 100 segments total, so no random selection necessary
# re-run above code after to ensure all have been copied over successfully