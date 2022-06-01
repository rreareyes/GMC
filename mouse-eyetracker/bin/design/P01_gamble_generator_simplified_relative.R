# Load required libraries -------------------------------------------------
library(tidyverse)
library(gtools)

# Define paths ------------------------------------------------------------
folder_root      <- dirname(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)))
folder_sequences <- file.path(folder_root, "data", "sequences")

time_stamp    <- format(Sys.time(), "%Y-%m-%d-%s")
sequence_file <- file.path(folder_sequences,
                           paste(time_stamp,
                                 "experiment-ratio-sequences.RData", sep = "-"))
sequence_export <- file.path(folder_sequences,
                             paste(time_stamp,
                                   "experiment-ratio-sequences.csv", sep = "-"))

# Define initial values ---------------------------------------------------
values <- seq(from = 11, to = 99, by = 1)

value_combinations <- data.frame(permutations(length(values), 2, values)) %>% 
  rename("left" = X1,
         "right" = X2) %>% 
  mutate(dif = round(left - right, 2),
         rel = round(left / right, 3),
         trans = log(rel)) 

# Constraint values to use ------------------------------------------------
rel_constrained <- value_combinations %>% 
  group_by(dif) %>% 
  mutate(dif_freq = n()) %>% 
  ungroup() %>% 
  filter(dif_freq > 19,
         !dif %% 13) %>% 
  group_by(dif) %>% 
  mutate(dif_level = dif/13) %>% 
  ungroup() %>% 
  filter(between(rel, 0.2, 1/0.2)) %>% 
  mutate(log_bins = cut(trans, breaks = 9, include.lowest = T),
         ratio_level = as.integer(log_bins) - 5) %>% 
  filter(ratio_level != 0) %>% 
  group_by(dif_level, ratio_level) %>% 
  mutate(cond = cur_group_id(),
         rel_dif_freq = n()) %>% 
  ungroup() %>% 
  mutate(ratio_bins = str_remove_all(as.character(log_bins), "[//(//)\\[\\]]")) %>% 
  separate(ratio_bins, into = c("low", "high"), sep = ",") %>% 
  mutate(low = exp(as.numeric(low)),
         high = exp(as.numeric(high)))

n_differences <- length(unique(rel_constrained$ratio_level))

rewards <- rel_constrained %>% 
  select(left, right, dif, rel, dif_level, ratio_level) %>% 
  group_by(ratio_level) %>% 
  sample_n(40, replace = T) %>% 
  ungroup() %>% 
  rename("rw_left"      = left,
         "rw_right"     = right,
         "rw_delta"     = dif,
         "rw_ratio"     = rel,
         "rw_delta_lvl" = dif_level,
         "rw_ratio_lvl" = ratio_level) %>% 
  group_by(rw_ratio_lvl) %>% 
  mutate(set = rep(sample(1:n_differences, n_differences), 5)) %>% 
  ungroup() %>% 
  arrange(set)

probabilities <- rel_constrained %>% 
  select(left, right, dif, rel, dif_level, ratio_level) %>% 
  group_by(ratio_level) %>% 
  sample_n(40, replace = T) %>% 
  ungroup() %>% 
  rename("p_left"      = left,
         "p_right"     = right,
         "p_delta"     = dif,
         "p_ratio"     = rel,
         "p_delta_lvl" = dif_level,
         "p_ratio_lvl" = ratio_level) %>% 
  group_by(p_ratio_lvl) %>% 
  ungroup() %>% 
  arrange(p_ratio_lvl)


controlled_combinations <- bind_cols(rewards, probabilities) %>% 
  select(-set) %>% 
  unite(col = "ratio_cond", rw_ratio_lvl, p_ratio_lvl) %>% 
  arrange(ratio_cond) %>% 
  mutate(trial_id = 1:n()) %>% 
  select(trial_id, rw_left, p_left, rw_right, p_right, rw_delta, p_delta, rw_ratio, p_ratio, ratio_cond)

save(controlled_combinations,
     file = file.path(folder_sequences, paste(time_stamp, "controlled_ratios.RData", sep = "-")))

# Create random sequences of trials ---------------------------------------
good_sequences <- list()
difference_plots <- list()

n_trials <- 320
n_blocks <- 5
n_sequences <- 10

for (iSequence in 1:n_sequences) {
  trial <- 1
  attempt <- 0
  good_sequences[[iSequence]] <- sample_n(controlled_combinations, 1)
  
  print(paste("Building sequence ", iSequence, "/", n_sequences, sep = ""))
  
  while (trial < n_trials) {
      attempt <- attempt + 1
      new_sample <- sample_n(controlled_combinations, 1)
      
      # Re-sample the trials if the sequence gets stuck
      if (attempt > 300) {
        good_sequences[[iSequence]] <- NULL
        good_sequences[[iSequence]] <- sample_n(controlled_combinations, 1)
        trial <- 1
        attempt <- 0
        
        } #end progress check
      
      # Avoid consecutive repetitions of stimuli and repeated items in the sequence
      if (new_sample$rw_left  != good_sequences[[iSequence]]$rw_left[trial] &
          new_sample$rw_right != good_sequences[[iSequence]]$rw_right[trial] &
          new_sample$p_left   != good_sequences[[iSequence]]$p_left[trial] &
          new_sample$p_right  != good_sequences[[iSequence]]$p_right[trial] &
          !new_sample$trial_id %in% good_sequences[[iSequence]]$trial_id
          ) 
        
        {
        # Add the successful sample to the sequence, reset attempt counter and move trial
        good_sequences[[iSequence]] <- bind_rows(good_sequences[[iSequence]], new_sample)
        trial <- trial + 1
        attempt <- 0
        } #end of repetition check
      
    } #end while loop
  
  good_sequences[[iSequence]] <- mutate(good_sequences[[iSequence]], 
                                        seq_id = iSequence,
                                        order  = 1:n_trials,
                                        block  = rep(1:n_blocks, each = 64),
                                        trial  = rep(1:64, n_blocks))

} #end sequence loop

# Create dataset with all the sequences merged
experiment_sequences <- bind_rows(good_sequences)

save(experiment_sequences,
     file = sequence_file)
write_csv(experiment_sequences,
          file = sequence_export)

