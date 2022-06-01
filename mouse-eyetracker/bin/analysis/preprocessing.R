# Behavior preprocessing

# Load libraries and define base paths ------------------------------------
library(tidyverse)

path_root    <- dirname(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)))
path_data    <- file.path(path_root, "data")
path_mouse   <- file.path(path_data, "mouse")
path_choice  <- file.path(path_data, "behavior")
path_results <- file.path(path_root, "results")

files_choice <- list.files(path_choice) 
files_mouse  <- list.files(path_mouse) 

file_results <- "processed_behavior.RData"

# Load participant files --------------------------------------------------
n_choice = length(files_choice)
n_mouse  = length(files_mouse)

raw_choice <- list()
raw_mouse  <- list()

## Choices ----------------------------------------------------------------
for (iFile in 1:n_choice) {
  
  raw_choice[[iFile]] = read_csv(file.path(path_choice, 
                                           files_choice[iFile]), 
                                 col_types = cols()) %>% 
    mutate(file = files_choice[iFile])
  
}

## Mouse ------------------------------------------------------------------
for (iFile in 1:n_mouse) {
  
  raw_mouse[[iFile]] = read_csv(file.path(path_mouse, 
                                          files_mouse[iFile]), 
                                col_types = cols()) %>% 
    mutate(file = files_mouse[iFile])
  
}

# Merge participant's data ------------------------------------------------
cohort_choice <- bind_rows(raw_choice)
cohort_mouse  <- bind_rows(raw_mouse)

# Process choice data -----------------------------------------------------
processed_choice <- cohort_choice %>%
  separate(file, into = c(NA, # capture type
                          "id", 
                          NA, NA, NA, NA, NA, NA, # date/time
                          "p_top", "block", "color_condition", 
                          NA), # file extension
           sep = "-|.csv") %>% 
  mutate(duration = round(end_time - start_time, 3) * 1000,
         delta_r = r_left - r_right,
         delta_p = p_left - p_right,
         ratio_r = round(r_left/r_right, 2),
         ratio_p = round(p_left/p_right, 2),
         lg_rt_r = log(ratio_r),
         lg_rt_p = log(ratio_p),
         ratio_r_lvl = ntile(lg_rt_r, 9) - 5,
         ratio_p_lvl = ntile(lg_rt_p, 9) - 5) %>% 
  group_by(ratio_r_lvl) %>% 
  mutate(r_bin_label = paste(min(ratio_r), max(ratio_r), sep = "-")) %>% 
  ungroup() %>% 
  group_by(ratio_p_lvl) %>% 
  mutate(p_bin_label = paste(min(ratio_p), max(ratio_p), sep = "-")) %>% 
  ungroup()

# Process mouse data ------------------------------------------------------
processed_mouse <- cohort_mouse %>% 
  separate(file, into = c(NA, # capture type
                          "id", 
                          NA, NA, NA, NA, NA, NA, # date/time
                          "p_top", "block", "color_condition", 
                          NA), # file extension
           sep = "-|.csv") %>% 
  group_by(id, order) %>% 
  mutate(sample = 1 + frame - min(frame),
         step   = round(sample/max(sample), 4),
         bin    = ntile(frame, 101)) %>% 
  ungroup()

# Merge behavioral data ---------------------------------------------------
processed_behavior <- full_join(processed_choice, processed_mouse) %>% 
  mutate(trial = trial + 1, 
         order = order + 1,
         block = as.numeric(block) + 1) %>% 
  select(id, color_condition, p_top, rand_id, 
         trial_id, order, block, trial, 
         r_left, r_right, p_left, p_right,
         delta_r, delta_p, ratio_r, ratio_p, 
         ratio_r_lvl, ratio_p_lvl, r_bin_label, p_bin_label,
         sample, step, bin,
         response, duration, x, y)


# Export database ---------------------------------------------------------
save(processed_behavior,
     file = file.path(path_results, file_results))
