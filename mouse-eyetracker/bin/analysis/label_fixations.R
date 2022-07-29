# LABEL FIXATIONS

# This scrip performs classification of the detected fixations, labeling 
# them according to the piece of information they fall into.

# # Written by Eduardo Rea 
# GMC Project
# NLP Lab
# July 2022  

# Load the required packages ----------------------------------------------
library(tidyverse)
library(spatialEco)
library(sp)

# Define base paths -------------------------------------------------------
path_root <- dirname(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)))
path_data <- file.path(path_root, "results", "datasets")

# Load the domain ranks and scenarios -------------------------------------

# Load the fixation data  -------------------------------------------------
column_class <- c("numeric", "numeric", "numeric", #subject, block and trial
                  "numeric", "numeric", "numeric", #start, end, duration
                  "numeric", "numeric") #x, y 

column_names <- c("id", "block", "trial", 
                  "start", "end", "duration", 
                  "x", "y")

raw_data <- read.csv(file = file.path(path_data, "preproc_fixations.csv"), 
                     colClasses = column_class, header = T) %>% 
  arrange(subject, block, trial, startT)

colnames(raw_data) <- column_names

# Prepare dataset for labeling --------------------------------------------

fixation_data <- raw_data %>%   
  group_by(id, block, trial) %>% 
  mutate(event = 1:n()) %>% 
  ungroup() %>% 
  mutate(position = NA)

# Get number subjects
participant_ids <- unique(fixation_data$id)
n_participants  <- length(participant_ids)

# Classify fixations according to location --------------------------------

# Define the AOIS polygons
# The position number corresponds to reading order (L-R, T-B):

#        o-------o   o-------o
#        |       |   |       |
#        |   1   |   |   2   |
#        |       |   |       |
#        o-------o   o-------o
#        
#        o-------o   o-------o
#        |       |   |       |
#        |   3   |   |   4   |
#        |       |   |       |
#        o-------o   o-------o

# Polygons are defined as follows:

# (x[4], y[4]) o--------o (x[3], y[3])
#              |        |
#              |        |
#              |        |
#              |        |
# (x[1], y[1]) o--------o (x[2], y[2)

# Regardless of the starting point chosen, the order of the vertex should be
# sequential with the shape you intend to sample from.
 
aoi_size   <- 140 # size of the AOI
aoi_offset <- aoi_size/2 # distance from the center to AOI's edge

x_offsets <- c(-1, 1, 1, -1) * aoi_offset #vertex offset from center aoi
y_offsets <- c(-1, -1, 1, 1) * -aoi_offset


## Get values to convert to top left origin (smi) from center origin ------
screen_res <- c(1920, 1080)
smi_center <- screen_res/2

#Y offsets are inverted (up is negative, down is positive)
top_left_center  <- c(-86.82, (-1 * 442.64)) + smi_center 
top_right_center <- c(86.82, (-1 * 442.64)) + smi_center

bot_left_center  <- c(-86.82, (-1 * 268.53)) + smi_center
bot_right_center <- c(86.82, (-1 * 268.53)) + smi_center
 
# Define AOI vertices -----------------------------------------------------
top_left_x <- x_offsets + top_left_center[1] # x coords from box vertices
top_left_y <- y_offsets + top_left_center[2] # y coords from box vertices

top_right_x <- x_offsets + top_right_center[1]
top_right_y <- y_offsets + top_right_center[2]

bot_left_x <- x_offsets + bot_left_center[1]
bot_left_y <- y_offsets + bot_left_center[2]

bot_right_x <- x_offsets + bot_right_center[1]
bot_right_y <- y_offsets + bot_right_center[2]

# Grouped sets of coordinates to make it easier to loop
aois_x <- list(top_left_x, top_right_x,
               bot_left_x, bot_right_x)

aois_y <- list(top_left_y, top_right_y,
               bot_left_y, bot_right_y)

# Define the number of regions to sample
n_AOIs <- length(aois_x)

# Detect fixations that fall within any of the AOIS
for (iAOI in 1:n_AOIs) {
  
  # Check if they fall within the left AOIS
  within_aoi <- as.logical(point.in.polygon(fixation_data$x, 
                                            fixation_data$y, 
                                            aois_x[[iAOI]], 
                                            aois_y[[iAOI]]))
  
  # Label the fixations according to the AOI where they fall
  fixation_data$position[within_aoi] <- iAOI
  
}

labelled_fixations <- fixation_data 


save(labelled_fixations, 
     file = file.path(path_data, "labelled_fixations.RData"))
