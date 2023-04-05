# Mousetracking trajectories extraction
library(tidyverse)
library(mousetrap)

# Base directories --------------------------------------------------------
path_root    <- dirname(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)))
path_results <- file.path(path_root, "results", "datasets")
path_keys    <- file.path(path_root, "results", "keys")

# Load base datasets ------------------------------------------------------
load(file.path(path_keys, "task_conditions.RData"))
load(file.path(path_keys, "trial_condition.RData"))

load(file = file.path(path_results, "preproc_mouse.RData"))
load(file = file.path(path_results, "preproc_choices.RData"))
load(file = file.path(path_keys, "model_names.RData"))

model_key <- read.csv(file = file.path(path_keys, "model_classification.csv"), 
                      colClasses = c("numeric", "numeric")) %>% 
  mutate(model_name = factor(model, 
                             levels = 1:length(model_names), 
                             labels = model_names))

preproc_choices <- select(preproc_choices, -trial) %>% 
  rename("trial" = order)

preproc_mouse <- select(preproc_mouse, -trial) %>% 
  rename("trial" = order)

rt_data <- group_by(preproc_choices, id) %>% 
  summarise(mean_rt = mean(duration), 
            sd      = sd(duration)) %>% 
  ungroup()

good_trials <- preproc_choices %>% 
  left_join(rt_data) %>% 
  group_by(id) %>% 
  filter(duration < mean_rt + 2 * sd) %>% 
  ungroup() %>% 
  distinct(id, trial)

trial_condition_key <- trial_condition_key %>% 
  select(id, "trial" = order, 
         delta_r, delta_p,
         ratio_r, ratio_p,
         bin_delta_r, bin_delta_p,
         bin_ratio_r, bin_ratio_p,
         bin_delta_ev, 
         bin_ratio_ev,
         bin_delta_sv) %>% 
  unique()

# Add trial labels to mousetracking data ----------------------------------
mouse_labelled_data <- left_join(preproc_mouse, trial_condition_key) %>% 
  left_join(model_key) %>% 
  filter(id %in% good_trials$id & trial %in% good_trials$trial) %>% 
  select(id, model, model_name, trial, 
         delta_r, delta_p,
         ratio_r, ratio_p,
         bin_delta_r, bin_delta_p,
         bin_ratio_r, bin_ratio_p,
         bin_delta_ev, 
         bin_ratio_ev,
         bin_delta_sv,
         sample, time, x, y) %>% 
  filter(y > -270)

# Load dataset into mousetrap for processing ------------------------------
prelim_trajectories <- mouse_labelled_data %>% 
  mt_import_long(xpos_label       = "x",
                 ypos_label       = "y", 
                 timestamps_label = "time", 
                 mt_seq_label     = "sample",
                 mt_id_label      = c("id", "model_name", "trial")) %>%
  mt_remap_symmetric(use = "trajectories") %>%
  mt_align_start_end(use = "trajectories") %>% 
  mt_export_long(use = "trajectories", 
                 use2_variables = c("id", "model_name", "trial")) 

# Identify and remove invalid trajectories --------------------------------
# Some trials skipped our previous quality controls and are flagged after 
# remapping or alignment, so we take them out before continuing with the
# time normalization.

invalid_trials <- prelim_trajectories %>% 
  filter(is.na(xpos) | is.na(ypos)) %>% 
  distinct(mt_id)

clean_mouse_data <- mouse_labelled_data %>% 
  unite("mt_id", id, model_name, trial, sep = "_", remove = F) %>% 
  filter(!(mt_id %in% invalid_trials$mt_id)) %>% 
  select(-mt_id)

clean_condition_mouse_data <- clean_mouse_data %>% 
  pivot_longer(cols = starts_with("bin_"),
               names_to = "metric",
               values_to = "level") %>%
  mutate(level  = factor(level),
         metric = factor(metric, 
                         levels = c("bin_delta_p", "bin_delta_r",
                                    "bin_ratio_p", "bin_ratio_r",
                                    "bin_delta_ev", "bin_ratio_ev",
                                    "bin_delta_sv"))) %>%
  select(id, model, model_name, trial, sample, metric, level, time, x, y) %>% 
  arrange(id, model, model_name, metric, level, trial, time)

clean_trial_mouse_data <- clean_mouse_data %>% 
  select(id, model, model_name, trial, sample, time, x, y) %>% 
  arrange(id, model, model_name, trial, time)

save(clean_trial_mouse_data, clean_condition_mouse_data,
     file = file.path(path_results, "mouse_trajectories.RData"))

# Temporal and spatial normalization --------------------------------------
# We use the default 101 bins to perform the time normalization. The spatial
# normalization implies alignment to a single quadrant and aligning start and
# end points from the trajectories.

tnorm_trajectories <- clean_trial_mouse_data %>% 
  mt_import_long(xpos_label       = "x",
                 ypos_label       = "y", 
                 timestamps_label = "time", 
                 mt_seq_label     = "sample",
                 mt_id_label      = c("id", "model_name", "trial")) %>%
  mt_remap_symmetric(use = "trajectories") %>%
  mt_align_start_end(use = "trajectories") %>% 
  mt_time_normalize(use = "trajectories") 

tnorm_condition_trajectories <- clean_condition_mouse_data %>% 
  mt_import_long(xpos_label       = "x",
                 ypos_label       = "y", 
                 timestamps_label = "time", 
                 mt_seq_label     = "sample",
                 mt_id_label      = c("id", "model_name", "trial",
                                      "metric", "level")) %>%
  mt_remap_symmetric(use = "trajectories") %>%
  mt_align_start_end(use = "trajectories") %>%
  mt_time_normalize(use = "trajectories") 

tnorm_aggregated_trajectories <- tnorm_condition_trajectories %>% 
  mt_aggregate(use            = "tn_trajectories",
               subject_id     = "mt_id", 
               use2_variables = c("model_name",
                                  "metric", "level"))

# Export individual and aggregated trajectories into tidy data sets -------
tnorm_tidy_trajectories <- tnorm_trajectories %>% 
  mt_export_long(use            = "tn_trajectories",
                 use2_variables = c("id", "model_name", "trial")) %>% 
  select(-mt_id)

# Perform angle normalization ---------------------------------------------
# Here we want any trajectories going directly to the target marked by an angle
# of 45 degrees. For that, we calculate the ideal angle towards the target at
# each sample, and then we get the relative angle in the trajectory based on 
# this ideal path.

tnorm_angles <- tnorm_tidy_trajectories %>% 
  mutate(x_target    = -1,
         y_target    = 1,
         x_next      = lead(xpos),
         y_next      = lead(ypos),
         obs_angle   = abs(atan2(x_next - xpos, y_next - ypos) * 180 / pi),
         ideal_angle = abs(atan2(x_target - xpos, y_target - ypos) * 180 / pi),
         norm_angle  = (obs_angle/ideal_angle) * 45) %>% 
  select(id, trial, 
         steps, xpos, ypos, x_next, y_next, 
         obs_angle, ideal_angle, norm_angle) %>%
  left_join(trial_condition_key) %>% 
  filter(steps < 101)

tnorm_aggregated_angles <- tnorm_aggregated_trajectories %>% 
  mutate(x_target    = -1,
         y_target    = 1,
         x_next      = lead(xpos),
         y_next      = lead(ypos),
         obs_angle   = abs(atan2(x_next - xpos, y_next - ypos) * 180 / pi),
         ideal_angle = abs(atan2(x_target - xpos, y_target - ypos) * 180 / pi),
         norm_angle  = (obs_angle/ideal_angle) * 45) %>% 
  select(model_name, metric, level,
         steps, xpos, ypos, x_next, y_next, 
         obs_angle, ideal_angle, norm_angle) %>%
  filter(steps < 101)

# # Export time normalized datasets -----------------------------------------
save(tnorm_tidy_trajectories,
     file = file.path(path_results, "tnorm_tidy_trajectories.RData"))

save(tnorm_angles,
     file = file.path(path_results, "tnorm_angles.RData"))

save(tnorm_aggregated_trajectories,
     file = file.path(path_results, "tnorm_aggregated_trajectories.RData"))

save(tnorm_aggregated_angles,
     file = file.path(path_results, "tnorm_aggregated_angles.RData"))
