library(tidyverse)

# Base directories --------------------------------------------------------
path_root    <- dirname(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)))
path_results <- file.path(path_root, "results", "datasets")

load(file = file.path(path_results, "normalized_trajectory_angles.RData"))

regression_data <- normalized_trajectory_angles %>% 
  select(id, model_name, trial, steps, relative_angle)

roster_participants <- unique(normalized_trajectory_angles$id)
n_participants <- length(roster_participants)

# Create loop for individual regressions ----------------------------------

for (iParticipant in 1:n_participants) {

  
    
}