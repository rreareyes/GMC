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
path_root    <- dirname(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)))
path_results <- file.path(path_root, "results", "datasets")
path_keys    <- file.path(path_root, "results", "keys")

# Load the domain ranks and scenarios -------------------------------------

# Load the fixation data  -------------------------------------------------
column_class <- c("numeric", "numeric", "numeric", #subject, block and trial
                  "numeric", "numeric", "numeric", #start, end, duration
                  "numeric", "numeric") #x, y 

column_names <- c("id", "block", "trial", 
                  "start", "end", "duration", 
                  "x", "y")

raw_data <- read.csv(file = file.path(path_results, "preproc_fixations.csv"), 
                     colClasses = column_class, header = T) %>% 
  arrange(subject, block, trial, startT)

colnames(raw_data) <- column_names

# Prepare dataset for labeling --------------------------------------------
load(file = file.path(path_keys, "model_names.RData"))
load(file = file.path(path_keys, "task_conditions.RData"))
load(file = file.path(path_keys, "valid_responses_key.RData"))
load(file = file.path(path_keys, "trial_condition.RData"))

model_key <- read.csv(file = file.path(path_keys, 
                                       "model_classification.csv"), 
                      colClasses = c("numeric", "numeric")) %>% 
  mutate(model_name = factor(model, 
                             levels = 1:length(model_names), 
                             labels = model_names))

fixation_data <- raw_data %>%   
  group_by(id, block, trial) %>% 
  mutate(event = 1:n()) %>% 
  ungroup() %>% 
  mutate(position = NA) %>% 
  mutate(order = ((block-1) * (64)) + trial)

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

labelled_fixations <- fixation_data %>% 
  filter(!is.na(position))

# Generate remapped aois --------------------------------------------------
winner_info_key <- trial_condition_key %>% 
  mutate(win_p = delta_p > 0,
         win_r = delta_r > 0) %>% 
  select(id, order, win_p, win_r)

model_labelled_fixations <- left_join(model_key, 
                                      valid_responses_key,
                                      by = c("id")) %>% 
  left_join(task_condition_key) %>% 
  left_join(labelled_fixations) %>% 
  left_join(winner_info_key) %>% 
  filter(x < 1920 & x > 1 & y > 1 & y < 1080)

aoi_size   <- 140 #size of the square AOI in pixels
aoi_offset <- aoi_size/2 #AOI center edge

screen_res <- c(1920, 1080) #original resolution from stimulus pc
smi_center <- screen_res/2 #smi numbers the center according to the screen resolution

# Get remapped offsets ----------------------------------------------------
# Here we create the new coordinates for the remapped AOIs
# We create a simple vector with the positive or negative index
# from that represent the location of the new boxes (-1 = left/down +1 = up/right)
x_offsets <- c(-1, 1, 1, -1) * aoi_offset #vertex offset from center aoi
y_offsets <- c(-1, -1, 1, 1) * -aoi_offset

## Get SMI center coordinates from each AOI -------------------------------
## Here we map the coordinates from the PsychoPy stimulus presentation to the
## coordinate sysmet used by SMI. Since PsychoPy uses a center (0,0) approach,
## we need to apply the offset according to the SMI coordinates to match the 
## labelled fixations with these AOIs.
top_left_center  <- c(-86.82, (-1 * 442.64)) + smi_center 
top_right_center <- c(86.82, (-1 * 442.64)) + smi_center

bot_left_center  <- c(-86.82, (-1 * 268.53)) + smi_center
bot_right_center <- c(86.82, (-1 * 268.53)) + smi_center

## And now we apply vertex offset to the remapped center coordinates so we can
## get the bounding boxes for our AOIs
top_left_x <- x_offsets + top_left_center[1] # x coords from box vertices
top_left_y <- y_offsets + top_left_center[2] # y coords from box vertices

top_right_x <- x_offsets + top_right_center[1]
top_right_y <- y_offsets + top_right_center[2]

bot_left_x <- x_offsets + bot_left_center[1]
bot_left_y <- y_offsets + bot_left_center[2]

bot_right_x <- x_offsets + bot_right_center[1]
bot_right_y <- y_offsets + bot_right_center[2]

# AOI boxes
# Tibble that contains the actual coordinates we will use to draw the boxes in 
# plots later on.
aois <- tibble(
  position = c(1, 2, 3, 4),
  status = factor(c("r_winner", "r_loser", "p_winner", "p_loser"), 
                  levels = c("r_winner", "r_loser", "p_winner", "p_loser")),
  label = factor(c("Rw W", "Rw L", "Pr W", "Pr L"), 
                 levels = c("Rw W", "Rw L", "Pr W", "Pr L")),
  xlabel = c(min(top_left_x), max(top_right_x), min(bot_left_x), max(bot_right_x)),
  ylabel = c(min(top_left_y), min(top_right_y), max(bot_left_y), max(bot_right_y)),
  xmin = c(min(top_left_x), min(top_right_x), min(bot_left_x), min(bot_right_x)),
  xmax = c(max(top_left_x), max(top_right_x), max(bot_left_x), max(bot_right_x)),
  ymin = c(min(top_left_y), min(top_right_y), min(bot_left_y), min(bot_right_y)),
  ymax = c(max(top_left_y), max(top_right_y), max(bot_left_y), max(bot_right_y))
  )


aoi_fixations <- model_labelled_fixations %>% 
  filter(!is.na(position)) %>% 
  group_by(id, block, trial, order) %>% 
  mutate(fixation = 1:n()) %>% 
  ungroup() %>% 
  arrange(id, block, trial) %>% 
  mutate(
    fix_info = ifelse((p_top == 1 & position %in% c(1, 2)) | 
                      (p_top == 0 & position %in% c(3, 4)),
                      "p", "r"),
    
    fix_side = case_when(position  == 1 | position == 3 ~ "left",
                         position  == 2 | position == 4 ~ "right"),
    
    status = case_when(fix_info == "p" & win_p == T ~ "p_winner",
                       fix_info == "p" & win_p == F ~ "p_loser",
                       fix_info == "r" & win_r == T ~ "r_winner",
                       fix_info == "r" & win_r == F ~ "r_loser"),
    
    status = factor(status, levels = c("r_winner", "r_loser", "p_winner", "p_loser")),
    
    x_dev = case_when(position == 1 | position == 3 ~ x - top_left_center[1], 
                      position == 2 | position == 4 ~ x - top_right_center[1]),
    
    y_dev = case_when(position == 1 | position == 2 ~ y - top_left_center[2], 
                      position == 3 | position == 4 ~ y - bot_left_center[2]),
    
    x_mag = case_when(status == "r_winner" | status == "p_winner" ~ x_dev + top_left_center[1], 
                      status == "r_loser" | status ==  "p_loser"  ~ x_dev + top_right_center[1]),
    
    y_mag = case_when(status == "r_winner" | status == "r_loser"  ~ y_dev + top_left_center[2], 
                      status == "p_winner" | status ==  "p_loser" ~ y_dev + bot_left_center[2]),
    
    y_flip = case_when(p_top == 1 & (position  == 3 | position == 4) ~ y_dev + top_left_center[2],
                       p_top == 1 & (position  == 1 | position == 2) ~ y_dev + bot_left_center[2],
                       p_top == 0 & (position  == 1 | position == 2) ~ y_dev + top_left_center[2],
                       p_top == 0 & (position  == 3 | position == 4) ~ y_dev + bot_left_center[2])
    )

aoi_events_labelled <- aoi_fixations %>% 
  select(id, p_top, block, trial, order,
         fixation, start, end, duration, position, fix_info, fix_side, status, x_dev, y_dev)

save(labelled_fixations, 
     file = file.path(path_results, "labelled_fixations.RData"))

save(aois,
     file = file.path(path_keys, "aoi_boxes.RData"))

save(aoi_fixations,
     file = file.path(path_results, "aoi_fixations.RData"))

save(aoi_events_labelled,
     file = file.path(path_results, "aoi_events_labelled.RData"))

save(top_left_center, top_right_center,
     bot_left_center, bot_right_center,
     file = file.path(path_keys, "aoi_centers.RData"))
