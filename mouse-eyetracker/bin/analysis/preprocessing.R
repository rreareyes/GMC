# Behavior preprocessing

# Load libraries and define base paths ------------------------------------
library(tidyverse)

path_root    <- dirname(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)))
path_data    <- file.path(path_root, "data")
path_mouse   <- file.path(path_data, "mouse", "clean")
path_choice  <- file.path(path_data, "behavior")
path_survey  <- file.path(path_data, "survey")
path_results <- file.path(path_root, "results", "datasets")
path_keys    <- file.path(path_root, "results", "keys")

files_choice <- list.files(path_choice) 
files_mouse  <- list.files(path_mouse) 


# Define raw structures to import -----------------------------------------
n_choice <- length(files_choice)
n_mouse  <- length(files_mouse)

raw_choice <- list()
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
                           header = F, 
                           colClasses = import_types,
                           skip = 3) 

colnames(preproc_survey) <- survey_names

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
preproc_choices <- cohort_choice %>%
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
         ) %>% 
  group_by(bin_ratio_r) %>% 
  mutate(r_bin_label = paste(min(ratio_r), max(ratio_r), sep = "-")) %>% 
  ungroup() %>% 
  group_by(bin_ratio_p) %>% 
  mutate(p_bin_label = paste(min(ratio_p), max(ratio_p), sep = "-")) %>% 
  ungroup() %>% 
  mutate(id = as.numeric(id),
         trial = trial + 1, 
         order = order + 1,
         block = as.numeric(block) + 1)

export_choices <- preproc_choices %>%  
  select(-c(start_time, end_time, trial, trial_id, block, 
            p_top, color_condition, rand_id, duration,
            r_bin_label, p_bin_label, 
            ev_left, ev_right, 
            delta_ev, ratio_ev, 
            delta_sv,
            bin_delta_ev, bin_ratio_ev, bin_delta_sv,
            lg_rt_r, lg_rt_p)) %>% 
  relocate(id, order, response) %>% 
  mutate(
    # Expected value
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
    s_tal       = tal_delta_p + tal_delta_r
    )

# Process mouse data ------------------------------------------------------
preproc_mouse <- cohort_mouse %>% 
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
  ungroup() %>% 
  mutate(id    = as.numeric(id),
         trial = trial + 1, 
         order = order + 1,
         block = as.numeric(block) + 1)

# Merge behavioral data ---------------------------------------------------
preproc_behavior <- full_join(preproc_choices, preproc_mouse) %>% 
  mutate(trial = trial + 1, 
         order = order + 1,
         block = as.numeric(block) + 1) %>% 
  select(id, color_condition, p_top, rand_id, 
         trial_id, order, block, trial, 
         r_left, r_right, p_left, p_right,
         delta_r, delta_p, ratio_r, ratio_p, 
         bin_ratio_r, bin_ratio_p, r_bin_label, p_bin_label,
         sample, step, bin,
         response, duration, x, y)


# Create reference key for conditions -------------------------------------
condition_key <- preproc_choices %>% 
  mutate(win_r = delta_r > 0,
         win_p = delta_p > 0) %>% 
  select(id, trial, block, order, p_top, color_condition, win_r, win_p)
  
# Export database ---------------------------------------------------------
save(condition_key,
     file = file.path(path_keys, "conditions.RData"))

save(preproc_behavior,
     file = file.path(path_results, "preproc_behavior.RData"))

save(preproc_mouse,
     file = file.path(path_results, "preproc_mouse.RData"))

save(preproc_choices,
     file = file.path(path_results, "preproc_choices.RData"))

save(preproc_survey,
     file = file.path(path_results, "preproc_survey.RData"))

write_csv(export_choices,
          file = file.path(path_results, "model_input.csv"),
          col_names = T)
