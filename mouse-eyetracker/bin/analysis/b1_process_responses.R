# Behavior preprocessing

# Load libraries and define base paths ------------------------------------
library(tidyverse)

path_root      <- dirname(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)))
path_data      <- file.path(path_root, "data")
path_mouse     <- file.path(path_data, "mouse", "clean")
path_responses <- file.path(path_data, "behavior")
path_survey    <- file.path(path_data, "survey")
path_results   <- file.path(path_root, "results", "datasets")
path_keys      <- file.path(path_root, "results", "keys")

files_responses <- list.files(path_responses) 
files_mouse     <- list.files(path_mouse) 


# Define raw structures to import -----------------------------------------
n_responses <- length(files_responses)
n_mouse     <- length(files_mouse)

raw_responses <- list()
raw_mouse  <- list()

## Survey -----------------------------------------------------------------
### Define data types and names for import

n_drt    <- 30
n_drp    <- 30
n_deb    <- 30
n_bis    <- 30
n_bisbas <- 24

survey_names <- c("id", "age", "gender", "gender2", 
                  paste("drt", 1:n_drt, sep = "_"),
                  paste("drp", 1:n_drp, sep = "_"),
                  paste("deb", 1:n_deb, sep = "_"),
                  paste("bis", 1:n_bis, sep = "_"),
                  paste("bisbas", 1:n_bisbas, sep = "_")
                  )

import_types  <- c(
  # Unused columns from qualtrics
  rep("NULL", 17),
  # Demographics
  "integer", "integer", "character", "character",
  # Dospert Risk Taking
  rep("numeric", n_drt),
  # Dospert Risk Perceptions
  rep("numeric", n_drp),
  # Dospert Expected Benefit
  rep("numeric", n_deb),
  # BIS-11
  rep("numeric", n_bis),
  # BIS/BAS
  rep("numeric", n_bisbas)
  )

#IMPORTAT!
#make sure your export from qualtrics removes linebreaks, otherwise it is 
#a mess to import
preproc_survey <- read.csv(file = file.path(path_survey, 
                                            "survey_data.csv"),
                           header     = F, 
                           colClasses = import_types,
                           skip       = 3)

colnames(preproc_survey) <- survey_names

## Choices ----------------------------------------------------------------
for (iFile in 1:n_responses) {
  
  raw_responses[[iFile]] = read_csv(file.path(path_responses,
                                              files_responses[iFile]), 
                                    col_types = cols()) |> 
    mutate(file = files_responses[iFile])
  
}

## Mouse ------------------------------------------------------------------
for (iFile in 1:n_mouse) {
  
  raw_mouse[[iFile]] = read_csv(file.path(path_mouse, 
                                          files_mouse[iFile]), 
                                col_types = cols()) |> 
    mutate(file = files_mouse[iFile])
  
}

# Merge participant's data ------------------------------------------------
cohort_responses <- bind_rows(raw_responses) |>
  separate(file, into = c(NA, # capture type
                          "id", 
                          NA, NA, NA, NA, NA, NA, # date/time
                          "p_top", "block", "color_condition", 
                          NA), # file extension
           sep = "-|.csv") |> 
  mutate(id    = as.numeric(id),
         trial = trial + 1, 
         order = order + 1,
         block = as.numeric(block) + 1)

cohort_mouse <- bind_rows(raw_mouse) |> 
  separate(file, into = c(NA, # capture type
                          "id", 
                          NA, NA, NA, NA, NA, NA, # date/time
                          "p_top", "block", "color_condition", 
                          NA), # file extension
           sep = "-|.csv") |> 
  mutate(id    = as.numeric(id),
         trial = trial + 1, 
         order = order + 1,
         block = as.numeric(block) + 1)

# Process mouse data ------------------------------------------------------
preproc_mouse <- cohort_mouse |> 
  group_by(id, order) |> 
  mutate(sample = 1 + frame - min(frame),
         step   = round(sample/max(sample), 4),
         bin    = ntile(frame, 101)) |> 
  ungroup() 

# Generate valid mouse tracking trials key --------------------------------
valid_responses_key <- preproc_mouse |> 
  select(id, order) |> 
  distinct() |> 
  droplevels() |> 
  left_join(cohort_responses) |> 
  filter(round(end_time - start_time, 3) < 10) |> 
  select(id, order) |> 
  distinct()

# Define valid participants based in quality trials -----------------------
valid_participants <- valid_responses_key |> 
  group_by(id) |> 
  summarise(valid_trials = n()) |> 
  ungroup() |> 
  filter(valid_trials > 320 * 1/2)

# Process choice data -----------------------------------------------------
preproc_responses <- valid_participants |> 
  left_join(valid_responses_key) |> 
  left_join(cohort_responses) |> 
  filter(response != -1) |> 
  mutate(duration = round(end_time - start_time, 3) * 1000,
         delta_r = r_left - r_right,
         delta_p = p_left - p_right,
         ratio_r = round(r_left/r_right, 2),
         ratio_p = round(p_left/p_right, 2),
         lg_rt_r = log(ratio_r),
         lg_rt_p = log(ratio_p),
         bin_ratio_r = ntile(lg_rt_r, 9) - 5,
         bin_ratio_p = ntile(lg_rt_p, 9) - 5,
         # Expected value
         ev_left  = p_left/100 * r_left,
         ev_right = p_right/100 * r_right,
         delta_ev = round(ev_left - ev_right, 2),
         ratio_ev = round(ev_left/ev_right, 2),
         delta_sv = (p_left/100 * log(r_left)) - (p_right/100 * log(r_right)),
         # EV levels
         bin_delta_ev = ntile(delta_ev, 9) - 5,
         bin_ratio_ev = ntile(ratio_ev, 9) - 5,
         # SV levels
         bin_delta_sv = ntile(delta_sv, 9) - 5,
         ev_left  = p_left/100 * r_left,
         ev_right = p_right/100 * r_right,
         delta_ev = round(ev_left - ev_right, 2),
         ratio_ev = round(ev_left/ev_right, 2),
         delta_sv = (p_left/100 * log(r_left)) - (p_right/100 * log(r_right)),
         # Stimuli levels
         bin_delta_p = as.integer(delta_p/13),
         bin_delta_r = as.integer(delta_r/13),
         # Truncated levels
         tr_delta_p = round(p_left, -1) - round(p_right, -1),
         tr_delta_r = round(r_left, -1) - round(r_right, -1),
         bin_tr_p   = tr_delta_p/10,
         bin_tr_r   = tr_delta_r/10,
         # Tally
         tal_delta_p = sign(delta_p),
         tal_delta_r = sign(delta_r),
         sum_tal       = tal_delta_p + tal_delta_r) |> 
  group_by(bin_ratio_r) |> 
  mutate(r_bin_label = paste(min(ratio_r), max(ratio_r), sep = "-")) |> 
  ungroup() |> 
  group_by(bin_ratio_p) |> 
  mutate(p_bin_label = paste(min(ratio_p), max(ratio_p), sep = "-")) |> 
  ungroup() |> 
  mutate(ev_win = case_when(delta_ev > 0 ~ 1,
                            delta_ev < 0 ~ 0))


cleaned_responses <- preproc_responses |> 
  select(id, block, trial, order, trial_id, response, start_time, "rt" = duration, ev_win)

model_input <- preproc_responses |>  
  select(id, order, response, 
         delta_r, delta_p,
         ratio_r, ratio_p,
         bin_delta_r, bin_delta_p,
         bin_ratio_r, bin_ratio_p,
         bin_tr_r, bin_tr_p,tal_delta_r, tal_delta_p, sum_tal,
         delta_ev, ratio_ev,
         delta_sv
  )

model_names <- c(
  "RwDelta + PrDelta",
  "RwDelta",
  "PrDelta",
  
  "RwRatio + PrRatio",
  "RwRatio",
  "PrRatio",
  
  "RwDeltaLv + PrDeltaLv",
  "RwDeltaLv",
  "PrDeltaLv",
  
  "PrRatioLv + RwRatioLv",
  "RwRatioLv",
  "PrRatioLv",
  
  "PrDeltaTr + RwDeltaTr",
  "RwDeltaTr",
  "PrDeltaTr",
  
  "RwTl + PrTl",
  "RwTl",
  "PrTl",
  
  "TallySum",
  
  "EV Delta",
  "EV Ratio",
  "SV Delta"
)

# Merge behavioral data ---------------------------------------------------
preproc_behavior <- full_join(preproc_responses, preproc_mouse) |> 
  select(id, color_condition, p_top, rand_id, 
         trial_id, order, block, trial, 
         r_left, r_right, p_left, p_right,
         delta_r, delta_p, ratio_r, ratio_p, 
         bin_ratio_r, bin_ratio_p, r_bin_label, p_bin_label, ev_win,
         sample, step, bin,
         response, duration, x, y)


# Create reference key for conditions -------------------------------------
task_condition_key <- preproc_responses |> 
  mutate(win_r = delta_r > 0,
         win_p = delta_p > 0) |> 
  select(id, p_top, color_condition) |> 
  distinct()

# Generate trial conditions by level --------------------------------------
trial_condition_key <- preproc_responses |> 
  select(id, order, 
         delta_r, delta_p, 
         ratio_r, ratio_p,
         ratio_ev, delta_sv,
         bin_ratio_r, bin_ratio_p, 
         bin_delta_ev, bin_ratio_ev, 
         bin_delta_sv) |> 
  distinct() |> 
  mutate(bin_delta_p = abs(delta_p) /13,
         bin_delta_r = abs(delta_r) /13,
         bin_ratio_p = abs(bin_ratio_p),
         bin_ratio_r = abs(bin_ratio_r),
         bin_delta_ev = abs(bin_delta_ev),
         bin_ratio_ev = abs(bin_ratio_ev),
         bin_delta_sv = abs(bin_delta_sv))


# Prepare dataset for preliminary analysis --------------------------------
finalized_responses <- cohort_responses |> 
  select(id, order, response) |> 
  filter(response != -1)

prelim_responses <- preproc_responses |> 
  select(id, "trial" = order, 
         delta_r, bin_delta_r,
         delta_p, bin_delta_p,
         ratio_r, bin_ratio_r,
         ratio_p, bin_ratio_p,
         delta_ev, bin_delta_ev, 
         ratio_ev, bin_ratio_ev,
         response) |> 
  mutate(r_delta_choice = case_when(sign(delta_r) ==  1 & response == 1 ~ 1,
                                    sign(delta_r) == -1 & response == 0 ~ 1,
                                    TRUE ~ 0),
         p_delta_choice = case_when(sign(delta_p) ==  1 & response == 1 ~ 1,
                                    sign(delta_p) == -1 & response == 0 ~ 1,
                                    TRUE ~ 0),
         r_ratio_choice = case_when(ratio_r > 1 & response == 1 ~ 1,
                                    ratio_r < 1 & response == 0 ~ 1,
                                    TRUE ~ 0),
         p_ratio_choice = case_when(ratio_p > 1 & response == 1 ~ 1,
                                    ratio_p < 1 & response == 0 ~ 1,
                                    TRUE ~ 0),
         ev_delta_choice = case_when(sign(delta_ev) ==  1 & response == 1 ~ 1,
                                     sign(delta_ev) == -1 & response == 0 ~ 1,
                                     TRUE ~ 0),
         ev_ratio_choice = case_when(ratio_ev > 1 & response == 1 ~ 1,
                                     ratio_ev < 1 & response == 0 ~ 1,
                                     TRUE ~ 0)) |> 
  mutate(r_delta_choice_bin  = r_delta_choice * bin_delta_r,
         p_delta_choice_bin  = p_delta_choice * bin_delta_p,
         r_ratio_choice_bin  = r_ratio_choice * bin_ratio_r,
         p_ratio_choice_bin  = p_ratio_choice * bin_ratio_p,
         ev_delta_choice_bin = ev_delta_choice * bin_delta_ev,
         ev_ratio_choice_bin = ev_ratio_choice * bin_ratio_ev) 

euc.dist <- function(x1, x2) sqrt(sum((x1 - x2) ^ 2))

prelim_mouse <- finalized_responses |> 
  inner_join(preproc_mouse) |> 
  select(id, order, trial_id, sample, x, y, time) |> 
  filter(sample %% 3 == 0) |> 
  group_by(id, order) |> 
  mutate(rt = (max(time) - min(time)) * 1000) |> 
  mutate(x_n = lag(x),
         y_n = lag(y),
         time_n = lag(time)) |> 
  #filter(!is.na(x_n)) |> 
  group_by(id, order, sample) |> 
  mutate(point_travel = euc.dist(cbind(x, y), 
                         cbind(x_n, y_n)),
         point_time = (time - time_n) * 1000
         ) |> 
  ungroup()

mouse_vigor <- prelim_mouse |> 
  group_by(id, order) |> 
  mutate(total_travel = sum(point_travel, na.rm = T),
         avg_speed    = total_travel/rt) |> 
  ungroup() |> 
  group_by(id, order, sample) |> 
  mutate(point_speed = point_travel/point_time) |> 
  ungroup() |> 
  group_by(id, order) |> 
  mutate(point_speed_n = lag(point_speed)) |> 
  ungroup() |> 
  mutate(point_acceleration = (point_speed - point_speed_n)/(point_time))

mouse_vigor_summary <- mouse_vigor |>     
  group_by(id, order, rt, total_travel, avg_speed) |> 
  summarise(peak_speed = max(point_speed, na.rm = T),
            peak_acceleration = max(point_acceleration, na.rm = T)) |> 
  ungroup() |> 
  filter(total_travel != 0) |> 
  rename(trial = order)

behavior_summary <- inner_join(prelim_responses, mouse_vigor_summary)

# Export database ---------------------------------------------------------
save(task_condition_key,
     file = file.path(path_keys, "task_conditions.RData"))

save(trial_condition_key,
     file = file.path(path_keys, "trial_condition.RData"))

save(valid_responses_key,
     file = file.path(path_keys, "valid_responses_key.RData"))

save(preproc_behavior,
     file = file.path(path_results, "preproc_behavior.RData"))

save(preproc_mouse,
     file = file.path(path_results, "preproc_mouse.RData"))

save(preproc_responses, 
     file = file.path(path_results, "preproc_responses.RData"))

save(behavior_summary, 
     file = file.path(path_results, "behavior_summary.RData"))

save(cleaned_responses,
     file = file.path(path_results, "cleaned_responses.RData"))

save(preproc_survey,
     file = file.path(path_results, "preproc_survey.RData"))

save(model_names,
     file = file.path(path_keys, "model_names.RData"))

write_csv(model_input,
          file = file.path(path_results, "model_input.csv"),
          col_names = T)

