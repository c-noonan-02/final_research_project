#### Script Set-Up ####

##### Clear Environment #####

rm(list=ls())

##### Load Packages #####

library(readxl)
library(writexl)
library(dplyr)
library(ggplot2)
library(magick)
library(png)
library(grid)


#### Confidence Threshold Regression Plots ####

# import species summary
species_summary <- read_xlsx("./phase1_analysis/data/BD2025_species_summary.xlsx")
head(species_summary)

# convert .webp files to .png
webp_folder <- "F:/dissertation_data/BirdNET_Review/regressions"
png_folder <- "F:/dissertation_data/BirdNET_Review/regressions/PNGs"

# create output folder (if it doesn't exist)
if (!dir.exists(png_folder)) {
  dir.create(png_folder)
}

# get webp files
webp_files <- list.files(path = "F:/dissertation_data/BirdNET_Review/regressions", pattern = "\\.webp$", full.names = TRUE)

# loop over each file, convert and save as png
for (file in webp_files) {
  
  img <- image_read(file)
  
  # build output file name
  out_file <- file.path(png_folder, paste0(tools::file_path_sans_ext(basename(file)), ".png"))
  
  # save image as png
  image_write(img, path = out_file, format = "png")
}

# build function to reshape the images without legends
process_image <- function(image_path, output_path) {
  
  # read in the image
  img <- image_read(image_path)
  
  # resize width
  img_resized <- image_scale(img, "970x600!")
  
  # create a blank canvas
  canvas <- image_blank(width = 1200, height = 600, color = "white")
  
  # composite resized image on left
  img_padded <- image_composite(canvas, img_resized, offset = "+0+0")
  
  # save output
  image_write(img_padded, path = output_path, format = "png")
}

# specify which files need editing
png_edits <- c(
  "F:/dissertation_data/BirdNET_Review/regressions/PNGs/Arctic Tern.png",
  "F:/dissertation_data/BirdNET_Review/regressions/PNGs/Black Grouse.png",
  "F:/dissertation_data/BirdNET_Review/regressions/PNGs/Black-headed Gull.png",
  "F:/dissertation_data/BirdNET_Review/regressions/PNGs/Carrion Crow.png",
  "F:/dissertation_data/BirdNET_Review/regressions/PNGs/Common Chaffinch.png",
  "F:/dissertation_data/BirdNET_Review/regressions/PNGs/Common Cuckoo.png",
  "F:/dissertation_data/BirdNET_Review/regressions/PNGs/Common Goldeneye.png",
  "F:/dissertation_data/BirdNET_Review/regressions/PNGs/Common Merganser.png",
  "F:/dissertation_data/BirdNET_Review/regressions/PNGs/Corn Crake.png",
  "F:/dissertation_data/BirdNET_Review/regressions/PNGs/Eurasian Blackbird.png",
  "F:/dissertation_data/BirdNET_Review/regressions/PNGs/Eurasian Green Woodpecker.png",
  "F:/dissertation_data/BirdNET_Review/regressions/PNGs/Eurasian Jackdaw.png",
  "F:/dissertation_data/BirdNET_Review/regressions/PNGs/Eurasian Marsh-Harrier.png",
  "F:/dissertation_data/BirdNET_Review/regressions/PNGs/Eurasian Reed Warbler.png",
  "F:/dissertation_data/BirdNET_Review/regressions/PNGs/Eurasian Siskin.png",
  "F:/dissertation_data/BirdNET_Review/regressions/PNGs/Eurasian Sparrowhawk.png",
  "F:/dissertation_data/BirdNET_Review/regressions/PNGs/Eurasian Woodcock.png",
  "F:/dissertation_data/BirdNET_Review/regressions/PNGs/Eurasian Wren.png",
  "F:/dissertation_data/BirdNET_Review/regressions/PNGs/European Pied Flycatcher.png",
  "F:/dissertation_data/BirdNET_Review/regressions/PNGs/European Robin.png",
  "F:/dissertation_data/BirdNET_Review/regressions/PNGs/Garganey.png",
  "F:/dissertation_data/BirdNET_Review/regressions/PNGs/Gray Partridge.png",
  "F:/dissertation_data/BirdNET_Review/regressions/PNGs/Great Black-backed Gull.png",
  "F:/dissertation_data/BirdNET_Review/regressions/PNGs/Great Cormorant.png",
  "F:/dissertation_data/BirdNET_Review/regressions/PNGs/Great Crested Grebe.png",
  "F:/dissertation_data/BirdNET_Review/regressions/PNGs/Green Sandpiper.png",
  "F:/dissertation_data/BirdNET_Review/regressions/PNGs/Hen Harrier.png",
  "F:/dissertation_data/BirdNET_Review/regressions/PNGs/Herring Gull.png",
  "F:/dissertation_data/BirdNET_Review/regressions/PNGs/House Sparrow.png",
  "F:/dissertation_data/BirdNET_Review/regressions/PNGs/Lesser Whitethroat.png",
  "F:/dissertation_data/BirdNET_Review/regressions/PNGs/Little Egret.png",
  "F:/dissertation_data/BirdNET_Review/regressions/PNGs/Little Grebe.png",
  "F:/dissertation_data/BirdNET_Review/regressions/PNGs/Little Gull.png",
  "F:/dissertation_data/BirdNET_Review/regressions/PNGs/Long-eared Owl.png",
  "F:/dissertation_data/BirdNET_Review/regressions/PNGs/Northern Lapwing.png",
  "F:/dissertation_data/BirdNET_Review/regressions/PNGs/Northern Wheatear.png",
  "F:/dissertation_data/BirdNET_Review/regressions/PNGs/Red Kite.png",
  "F:/dissertation_data/BirdNET_Review/regressions/PNGs/Red-legged Partridge.png",
  "F:/dissertation_data/BirdNET_Review/regressions/PNGs/Ring-necked Pheasant.png",
  "F:/dissertation_data/BirdNET_Review/regressions/PNGs/Short-eared Owl.png",
  "F:/dissertation_data/BirdNET_Review/regressions/PNGs/Stock Dove.png",
  "F:/dissertation_data/BirdNET_Review/regressions/PNGs/Twite.png",
  "F:/dissertation_data/BirdNET_Review/regressions/PNGs/Whooper Swan.png",
  "F:/dissertation_data/BirdNET_Review/regressions/PNGs/Willow Warbler.png",
  "F:/dissertation_data/BirdNET_Review/regressions/PNGs/Wood Warbler.png"
)

# provide folder
png_folder <- "F:/dissertation_data/BirdNET_Review/regressions/PNGs"

# loop over the files, running the edit function
for (file in png_edits) {
  filename <- tools::file_path_sans_ext(basename(file))
  out_file <- file.path(png_folder, basename(file))
  process_image(file, out_file)
}

# get png files
png_files <- list.files(path = "F:/dissertation_data/BirdNET_Review/regressions/PNGs", pattern = "\\.png$", full.names = TRUE)

# read the images
images <- lapply(png_files, readPNG)

# define panel size
panel_rows <- 10
panel_cols <- 3
images_per <- (panel_rows/2) * panel_cols

# number of panels needed
num_panels <- ceiling(length(images) / images_per)

# function to define where to place each image
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)

# function to generate alphabet figure references
num_to_letters <- function(n) {
  out <- character(length(n))
  
  for (i in seq_along(n)) {
    num <- n[i]
    res <- ""
    while (num > 0) {
      rem <- (num - 1) %% 26
      res <- paste0(LETTERS[rem + 1], res)
      num <- (num - 1) %/% 26
    }
    out[i] <- res
  }
  return(out)
}

# plot
for (panel_idx in seq_len(num_panels)) {
  
  # set file name for this panel
  out_file <- sprintf("./phase1_analysis/plots/regression_plot_%02d.png", panel_idx)

  # open png device
  png(filename = out_file, width = 7000, height = 9000, res = 400)
  
  # determine the subset of images and files for this panel
  start_idx <- (panel_idx - 1) * images_per + 1
  end_idx <- min(panel_idx * images_per, length(images))
  
  # generate subset
  images_subset <- images[start_idx:end_idx]
  pngs_subset <- png_files[start_idx:end_idx]
  
  # open a new page for each panel
  grid.newpage()
  
  # set up viweport with custom layout sizes
  pushViewport(viewport(layout = grid.layout(
    nrow = panel_rows, ncol = panel_cols,
    )))
  
  for (i in seq_along(images_subset)) {
    
    # calculate positions: titles on odd rows, images on even rows
    row_img <- (ceiling(i / panel_cols) - 1) * 2 + 2
    col_img <- ((i - 1) %% panel_cols) + 1
    if (col_img == 0) col_img <- panel_cols
    
    # add cols and rows for title
    row_title <- row_img - 1
    col_title <- col_img
    
    # extract the common name from the file name
    common_name <- tools::file_path_sans_ext(basename(pngs_subset[i]))
    
    # look up the latin name
    latin_name <- species_summary$scientific_n[species_summary$common_n == common_name]
    
    # if there is no match set to unknown
    if (length(latin_name) == 0) {
      latin_name <- "UNKNOWN"
    }
    
    # get the absolute index for the image list
    global_i <- (panel_idx - 1) * images_per + i
    # get alphabet code for title
    letter_code <- num_to_letters(global_i)
    
    # generate figure reference
    figure_ref <- paste0(letter_code, ". ")
    
    # generate title text
    title_text <- bquote(bold(.(figure_ref)) ~ .(common_name) ~ "(" * italic(.(latin_name)) * ")")
    
    # draw title
    pushViewport(vplayout(row_title, col_title))
    grid.text(label = title_text,
              x = unit(0.05, "npc"), # far left
              y = unit(0.6, "npc"), # bottom
              just = c("left", "top"),
              gp = gpar(fontsize = 16))
    popViewport()
    
    # draw image below title
    pushViewport(vplayout(row_img, col_img))
    grid.raster(images_subset[[i]],
                width = unit(0.95, "npc"),
                height = unit(0.95, "npc"),
                just = c("centre", "bottom"))
    popViewport()
    
  }
  
  # close PNG device
  dev.off()
  
}



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
# set all to have the same y lim for easier comparison
# create cowplot or something similar with all histograms in panels for results section
# update once final species are finalised