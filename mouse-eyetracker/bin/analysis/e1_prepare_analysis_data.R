# Generate and export analysis datasets
library(tidyverse)

# Base directories --------------------------------------------------------
path_root    <- dirname(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)))
path_time    <- file.path(path_root, "data", "timing")
path_results <- file.path(path_root, "results", "datasets")
path_keys    <- file.path(path_root, "results", "keys")

# Load base datasets ------------------------------------------------------
load(file.path(path_keys, "task_conditions.RData"))
load(file.path(path_keys, "trial_condition.RData"))
load(file.path(path_keys, "model_names.RData"))
load(file.path(path_keys, "aoi_centers.RData"))
load(file.path(path_keys, "personality_scores.RData"))

load(file.path(path_results, "cleaned_responses.RData"))
load(file.path(path_results, "aoi_events_labelled.RData"))
load(file.path(path_results, "mouse_trajectories.RData"))
load(file.path(path_time, "tracker_timing.RData"))

model_key <- read.csv(file = file.path(path_keys, "model_classification.csv"), 
                      colClasses = c("numeric", "numeric")) |> 
  mutate(model_name = factor(model, 
                             levels = 1:length(model_names), 
                             labels = model_names)) 

euc.dist <- function(x1, x2) sqrt(sum((x1 - x2) ^ 2))

# Merge datasets ----------------------------------------------------------
response_data <- cleaned_responses |> 
  left_join(model_key) |> 
  filter(model %in% c(6, 18, 21, 22)) |> 
  left_join(trial_condition_key) |> 
  filter(response %in% c(0, 1)) |> 
  filter(ev_win %in% c(0, 1)) |> 
  mutate(correct = as.numeric(response == ev_win)) 

fixation_prelim_data <- left_join(aoi_events_labelled, model_key) |> 
  inner_join(cleaned_responses) |> 
  left_join(trial_condition_key) |> 
  filter(model %in% c(6, 18, 21, 22)) |> 
  droplevels() |> 
  mutate(
    fix_choice = case_when(response == 1 & fix_side == "left" ~ 1,
                           response == 0 & fix_side == "right" ~ 1,
                           response == 1 & fix_side == "right" ~ 0,
                           response == 0 & fix_side == "left" ~ 0))

transition_data <- fixation_prelim_data |> 
  select(id, model, model_name, block, trial, order, trial_id, fixation, position, fix_info, fix_side) |> 
  unite(fix_current, c(fix_info, fix_side), sep = "_", remove = F) |> 
  group_by(id, order) |> 
  mutate(fix_next = lead(fix_current)) |>
  filter(fix_current != fix_next) |> 
  mutate(transition = 1:n()) |> 
  ungroup() |> 
  unite(transition_label, c(fix_current, fix_next), sep = "-", remove = F) |> 
  mutate(
    transition_type = 
      case_when(str_detect(fix_current, "right") == str_detect(fix_next, "right") ~ "within option",
                
                str_detect(fix_current, "r_") == str_detect(fix_next, "r_") ~ "within feature",
                
                str_detect(fix_current, "r_") != str_detect(fix_next, "r_") &
                str_detect(fix_current, "right") != str_detect(fix_next, "right") ~ "between feature"
                )
    )

transition_extended_data <- fixation_prelim_data |> 
  select(id, model, model_name, block, trial, order, trial_id, fixation, position, fix_info, fix_side) |> 
  unite(fix_current, c(fix_info, fix_side), sep = "_", remove = F) |> 
  group_by(id, order) |> 
  mutate(fix_next = lead(fix_current)) |>
  filter(fix_current != fix_next) |> 
  mutate(transition = 1:n()) |> 
  ungroup() |> 
  unite(transition_label, c(fix_current, fix_next), sep = "-", remove = F) |> 
  mutate(
    transition_name = 
      case_when(
        (str_detect(fix_current, "r_right") ==  1 & str_detect(fix_next, "p_right") == 1) |
          (str_detect(fix_current, "r_left") == 1 & str_detect(fix_next, "p_left") == 1) ~ "wt_rw_pr",
                
        (str_detect(fix_current, "p_right") == 1 & str_detect(fix_next, "r_right") == 1) |
          (str_detect(fix_current, "p_left") == 1 & str_detect(fix_next, "r_left") == 1) ~ "wt_pr_rw",
        
        (str_detect(fix_current, "r_right") == 1 & str_detect(fix_next, "r_left") == 1) |
          (str_detect(fix_current, "r_left") == 1 & str_detect(fix_next, "r_right") == 1) ~ "bt_rw_rw",
        
        (str_detect(fix_current, "p_right") == 1 & str_detect(fix_next, "p_left") == 1) |
          (str_detect(fix_current, "p_left") == 1 & str_detect(fix_next, "p_right") == 1) ~ "bt_pr_pr",
        
        (str_detect(fix_current, "r_right") == 1 & str_detect(fix_next, "p_left") == 1) |
          (str_detect(fix_current, "r_left") == 1 & str_detect(fix_next, "p_right") == 1) ~ "bt_rw_pr",
        
        (str_detect(fix_current, "p_right") == 1 & str_detect(fix_next, "r_left") == 1) |
          (str_detect(fix_current, "p_left") == 1 & str_detect(fix_next, "r_right") == 1) ~ "bt_pr_rw"
      )
  )

# Define decision scenarios for each strategy (difficulty) ----------------
# We also relabel the fixation depending on the strategy specific interpretation
# such that always the winning option is in the left side (1, 3) and the losing
# option is in the right side (2, 4). Furthermore, we set the probabilities in 
# the top aois (1, 2) and the probability in the bottom (3, 4).

pr_ratio_data <- fixation_prelim_data |> 
  filter(model == 6) |> 
  mutate(aoi = case_when(ratio_p > 1 & fix_info == "p" & fix_side == "left"  ~ 3,
                         ratio_p < 1 & fix_info == "p" & fix_side == "right" ~ 3,
                         ratio_p < 1 & fix_info == "p" & fix_side == "left"  ~ 4,
                         ratio_p > 1 & fix_info == "p" & fix_side == "right" ~ 4,
                         
                         fix_info == "r" & fix_side == "left" & ratio_p > 1  ~ 1,
                         fix_info == "r" & fix_side == "right" & ratio_p > 1 ~ 2,
                         fix_info == "r" & fix_side == "left" & ratio_p < 1  ~ 2,
                         fix_info == "r" & fix_side == "right" & ratio_p < 1 ~ 1),
         
         x_corr = case_when(aoi == 1 | aoi == 3 ~ x_dev + top_left_center[1],
                            aoi == 2 | aoi == 4 ~ x_dev + top_right_center[1]),
         y_corr = case_when(aoi == 1 | aoi == 2 ~ y_dev + top_left_center[2],
                            aoi == 3 | aoi == 4 ~ y_dev + bot_left_center[2])) |> 
  select(id, model, model_name, block, trial, order, 
         response, rt, 
         fixation, start, end, duration, 
         x_corr, y_corr, aoi,
         "difficulty" = bin_ratio_p)


pr_tallying_data <- fixation_prelim_data |> 
  filter(model == 18) |> 
  mutate(aoi = case_when(ratio_p > 1 & fix_info == "p" & fix_side == "left"  ~ 3,
                         ratio_p < 1 & fix_info == "p" & fix_side == "right" ~ 3,
                         ratio_p < 1 & fix_info == "p" & fix_side == "left"  ~ 4,
                         ratio_p > 1 & fix_info == "p" & fix_side == "right" ~ 4,
                         
                         fix_info == "r" & fix_side == "left" & ratio_p > 1  ~ 1,
                         fix_info == "r" & fix_side == "right" & ratio_p > 1 ~ 2,
                         fix_info == "r" & fix_side == "left" & ratio_p < 1  ~ 2,
                         fix_info == "r" & fix_side == "right" & ratio_p < 1 ~ 1),
         
         x_corr = case_when(aoi == 1 | aoi == 3 ~ x_dev + top_left_center[1],
                            aoi == 2 | aoi == 4 ~ x_dev + top_right_center[1]),
         y_corr = case_when(aoi == 1 | aoi == 2 ~ y_dev + top_left_center[2],
                            aoi == 3 | aoi == 4 ~ y_dev + bot_left_center[2])) |> 
  select(id, model, model_name, block, trial, order, 
         response, rt, 
         fixation, start, end, duration, 
         x_corr, y_corr, aoi,
         "difficulty" = bin_ratio_p)

ev_ratio_data <- fixation_prelim_data |> 
  filter(model == 21) |> 
  filter(ratio_ev != 1) |> 
  mutate(aoi = case_when(ratio_ev > 1 & fix_info == "p" & fix_side == "left"  ~ 3,
                         ratio_ev > 1 & fix_info == "r" & fix_side == "left"  ~ 1,
                         ratio_ev > 1 & fix_info == "p" & fix_side == "right" ~ 4,
                         ratio_ev > 1 & fix_info == "r" & fix_side == "right" ~ 2,
                         
                         ratio_ev < 1 & fix_info == "p" & fix_side == "left"  ~ 4,
                         ratio_ev < 1 & fix_info == "r" & fix_side == "left"  ~ 2,
                         ratio_ev < 1 & fix_info == "p" & fix_side == "right" ~ 3,
                         ratio_ev < 1 & fix_info == "r" & fix_side == "right" ~ 1),
         
         x_corr = case_when(aoi == 1 | aoi == 3 ~ x_dev + top_left_center[1],
                            aoi == 2 | aoi == 4 ~ x_dev + top_right_center[1]),
         y_corr = case_when(aoi == 1 | aoi == 2 ~ y_dev + top_left_center[2],
                            aoi == 3 | aoi == 4 ~ y_dev + bot_left_center[2])) |> 
  select(id, model, model_name, block, trial, order, 
         response, rt, 
         fixation, start, end, duration, 
         x_corr, y_corr, aoi,
         "difficulty" = bin_ratio_ev)

sv_delta_data <- fixation_prelim_data |> 
  filter(model == 22) |> 
  filter(delta_sv != 0) |> 
  mutate(aoi = case_when(delta_sv > 0 & fix_info == "p" & fix_side == "left"  ~ 3,
                         delta_sv > 0 & fix_info == "r" & fix_side == "left"  ~ 1,
                         delta_sv > 0 & fix_info == "p" & fix_side == "right" ~ 4,
                         delta_sv > 0 & fix_info == "r" & fix_side == "right" ~ 2,
                         
                         delta_sv < 0 & fix_info == "p" & fix_side == "left"  ~ 4,
                         delta_sv < 0 & fix_info == "r" & fix_side == "left"  ~ 2,
                         delta_sv < 0 & fix_info == "p" & fix_side == "right" ~ 3,
                         delta_sv < 0 & fix_info == "r" & fix_side == "right" ~ 1),
         
         x_corr = case_when(aoi == 1 | aoi == 3 ~ x_dev + top_left_center[1],
                            aoi == 2 | aoi == 4 ~ x_dev + top_right_center[1]),
         y_corr = case_when(aoi == 1 | aoi == 2 ~ y_dev + top_left_center[2],
                            aoi == 3 | aoi == 4 ~ y_dev + bot_left_center[2])) |> 
  select(id, model, model_name, block, trial, order, 
         response, rt, 
         fixation, start, end, duration,
         x_corr, y_corr, aoi,
         "difficulty" = bin_delta_sv)

fixation_analysis_data <- full_join(pr_ratio_data, pr_tallying_data) |> 
  full_join(ev_ratio_data) |> 
  full_join(sv_delta_data) |> 
  mutate(status_label = case_when(aoi == 1 | aoi == 3 ~ "win",
                                  aoi == 2 | aoi == 4 ~ "lose"),
         
         feature_label = case_when(aoi == 1 | aoi == 2 ~ "rw",
                                   aoi == 3 | aoi == 4 ~ "pr"),
         
         feature_status_label = case_when(aoi == 1 ~ "wn_rw",
                                          aoi == 2 ~ "ls_rw",
                                          aoi == 3 ~ "wn_pr",
                                          aoi == 4 ~ "ls_pr")) |> 
  select(-c(trial)) |> 
  rename("trial" = order)

trial_timing <- cleaned_responses |> 
  select(id, "trial" = order, "trial_start" = start_time)

process_tracing_eye_data <- fixation_analysis_data |> 
  left_join(tracker_timing, by = c("id", "trial")) |> 
  mutate(fix_start_off = (start - t_start)/1000,
         fix_end_off   = (end - t_start)/1000) |> 
  inner_join(trial_timing, by = c("id", "trial")) |> 
  mutate(event_start = trial_start + fix_start_off,
         event_end   = trial_start + fix_end_off,
         time = event_start,
         event = sprintf("fixation_%02d", fixation)) |> 
  select(id, model, model_name, trial, x_corr, y_corr,
         difficulty, status_label, feature_label, feature_status_label, time,
         event, aoi, event_start, event_end)
  

process_tracing_mouse <- clean_trial_mouse_data |> 
  mutate(event = sprintf("mouse_%02d", sample)) |> 
  group_by(id, trial) |> 
  mutate(start_move = min(time)) |> 
  ungroup() |> 
  select(id, model, model_name, trial,
         event, time, start_move, x, y)

process_tracing_merged <- full_join(process_tracing_eye_data, process_tracing_mouse) |> 
  group_by(id, trial) |> 
  fill(start_move, .direction = "up") |> 
  filter(event_start >= start_move | is.na(event_end)) |> 
  arrange(id, trial, time) |> 
  mutate(section = str_replace_all(event, "mouse_..*", replacement = NA_character_)) |> 
  fill(section, .direction = "down") |> 
  filter(!is.na(section)) |> 
  group_by(id, trial, section) |> 
  mutate(guide = 1:n()) |> 
  filter(guide < 3 | guide == n()) |> 
  mutate(n_events = n()) |> 
  filter(n_events == 3) |> 
  ungroup()

mouse_travel <- filter(process_tracing_merged, str_detect(event, "mouse")) |> 
  select(id, model, model_name, trial, section, x, y, time) |> 
  group_by(id, model, model_name, trial, section) |> 
  mutate(moment = c("start", "end")) |> 
  pivot_wider(id_cols = c("id", "model", "model_name", "trial", "section"),
              values_from = c("x", "y", "time"), names_from = moment) |> 
  ungroup() |> 
  group_by(id, model, model_name, trial) |> 
  mutate(x_seq    = lead(x_end),
         y_seq    = lead(y_end),
         time_seq = lead(time_end)) |> 
  ungroup() |> 
  group_by(id, model, model_name, trial, section) |> 
  mutate(travel = euc.dist(cbind(x_start, y_start), 
                           cbind(x_end, y_end)),
         travel_seq = euc.dist(cbind(x_end, y_end),
                               cbind(x_seq, y_seq))) |> 
  mutate(duration = (time_end - time_start),
         duration_seq = (time_seq - time_end),
         speed = travel / duration,
         speed_seq = travel_seq/duration_seq) |> 
  ungroup() |> 
  arrange(id, trial, time_start) |> 
  select(id, model, model_name, trial, "event" = section,
         travel, duration, speed, 
         travel_seq, duration_seq, speed_seq)  
  

visual_sampling <- process_tracing_merged |> 
  select(id, model, model_name, trial, difficulty, event, aoi,
         status_label, feature_label, feature_status_label) |> 
  filter(str_detect(event, "fix"))

process_tracing_data <- inner_join(visual_sampling, mouse_travel)

### TO DO
# Get start and ending points for sim and lag models
# We can do this by filtering a moving window, depending on fixation anchoring


# Create exporting datasets -----------------------------------------------
choice_counts <- response_data |> 
  group_by(id, model, model_name) |> 
  summarise(total = n(), correct = sum(correct)) |> 
  ungroup()

choice_time <- fixation_analysis_data |> 
  select(id, trial, model, model_name, rt, difficulty) |> 
  distinct()
  
fixation_counts <- fixation_analysis_data |> 
  group_by(id, model, model_name, trial) |> 
  summarise(fix_count = n()) |> 
  ungroup()

fixation_counts_difficulty <- fixation_analysis_data |> 
  group_by(id, model, model_name, trial, difficulty) |> 
  summarise(fix_count = n()) |> 
  ungroup()

feature_fixation_counts <- fixation_analysis_data |> 
  group_by(id, model, model_name, feature_label) |> 
  summarise(fix_count = n()) |> 
  mutate(total = sum(fix_count)) |> 
  ungroup() |> 
  pivot_wider(names_from = feature_label, 
              values_from = fix_count, 
              values_fill = list(fix_count = 0))

feature_fixation_counts$y <- cbind(feature_fixation_counts$pr, 
                                   feature_fixation_counts$rw) 


feature_status_fixation_counts <- fixation_analysis_data |> 
  group_by(id, model, model_name, feature_status_label) |> 
  summarise(fix_count = n()) |> 
  mutate(total = sum(fix_count)) |> 
  ungroup() |> 
  pivot_wider(names_from  = feature_status_label, 
              values_from = fix_count, 
              values_fill = list(fix_count = 0))

feature_status_fixation_counts$y <- cbind(feature_status_fixation_counts$wn_rw, 
                                          feature_status_fixation_counts$ls_rw,
                                          feature_status_fixation_counts$wn_pr, 
                                          feature_status_fixation_counts$ls_pr) 


transition_counts <- transition_extended_data |> 
  group_by(id, model, model_name, transition_name) |> 
  summarise(trans_count = n()) |> 
  mutate(total = sum(trans_count)) |> 
  ungroup() |> 
  pivot_wider(names_from = transition_name, 
              values_from = trans_count, 
              values_fill = list(trans_count = 0))

transition_counts$y <- cbind(transition_counts$bt_pr_pr, 
                             transition_counts$bt_pr_rw, 
                             transition_counts$bt_rw_pr, 
                             transition_counts$bt_rw_rw,
                             transition_counts$wt_pr_rw,
                             transition_counts$wt_rw_pr) 

save(pr_ratio_data, pr_tallying_data,
     ev_ratio_data, sv_delta_data,

     choice_counts, choice_time,
     fixation_counts, fixation_counts_difficulty,
     feature_fixation_counts,
     feature_status_fixation_counts,
     transition_counts,
     process_tracing_data,
     
     file = file.path(path_results, "analysis_data.RData"))