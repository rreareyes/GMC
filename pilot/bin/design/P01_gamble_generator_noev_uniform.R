# Load required libraries -------------------------------------------------
library(tidyverse)
library(gtools)

# Define paths ------------------------------------------------------------
folder_root      <- dirname(dirname(rstudioapi::getActiveDocumentContext()$path))
folder_scripts   <- file.path(folder_root, "Scripts")
folder_functions <- file.path(folder_scripts, "Functions")
folder_keys      <- file.path(folder_root, "Sequences")
folder_blocks    <- file.path(folder_keys, "blocks")

# Define vector of probabilities ------------------------------------------
p_above <- seq(from = 0.11, to = 0.9, by = 0.01)

# Define vector of rewards ------------------------------------------------
rewards <- seq(from = 11, to = 99, by = 3)

# Obtain permutations of rewards and probabilities ------------------------
mixed_data <- c(p_above, rewards) 

payout_combinations <- data.frame(permutations(length(mixed_data), 4, mixed_data)) %>% 
  filter(X1 > 1, X2 < 1, X3 > 1, X4 < 1) %>% 
  rename("rw_left"  = X1, 
         "p_left"   = X2, 
         "rw_right" = X3,
         "p_right"  = X4) %>% 
  mutate(trial_id = 1:n(),
         dif_rw   = abs(rw_left - rw_right),
         dif_p    = round(abs(p_left - p_right), 2)) %>% 
filter(dif_p  %in% c(0.15, 0.30, 0.45, 0.60),
       dif_rw %in% c(15, 30, 45, 60)) %>% 
  mutate(ev_right = rw_right * p_right,
         ev_left  = rw_left * p_left,
         dir_rw   = sign(rw_left - rw_right),
         dir_p    = sign(p_left - p_right),
         dif_ev   = round(abs(ev_left - ev_right), 3),
         supp_rw  = round(rw_left - rw_right),
         supp_p   = round(p_left - p_right, 2),
         supp_ev  = round(ev_left - ev_right, 3))

controlled_combinations <- payout_combinations %>% 
  group_by(supp_p, supp_rw) %>% 
  sample_n(5) %>% 
  ungroup()

save(payout_combinations,
     file = file.path(folder_keys, paste(time_stamp, "payout-combinations.RData", sep = "-")))

save(controlled_combinations, 
     file = file.path(folder_keys, paste(time_stamp, "controlled-combinations.RData", sep = "-")))

# Create a uniform distribution of differences across all levels ----------
conditions <- controlled_combinations %>% 
  distinct(supp_p, supp_rw)

n_conditions <- dim(conditions)[[1]]
n_trials_condition <- ceiling(300/n_conditions)

uniform_condition <- list()

for (iCondition in 1:n_conditions) {
  
  trials_condition <- filter(controlled_combinations, 
                             supp_rw == conditions$supp_rw[iCondition] & 
                             supp_p == conditions$supp_p[iCondition]) %>% 
    mutate(cond_id = iCondition)
  
  if (dim(trials_condition)[1] < 5) {
    
    uniform_condition[[iCondition]] <- sample_n(trials_condition, 5, T)
    
  } else if (dim(trials_condition)[1] >= 5){
    
    uniform_condition[[iCondition]] <- sample_n(trials_condition, 5, F)
    
  }
  
  
}

n_sequences    <- 10
n_blocks       <- 5
n_trials       <- dim(bind_rows(uniform_condition))[[1]]
n_trials_block <- n_trials/n_blocks

uniform_trials <- bind_rows(uniform_condition) %>% 
  mutate(uniform_id = 1:n()) %>% 
  group_by(cond_id) %>% 
  mutate(block = sample(1:n_blocks, 5)) %>% 
  ungroup()

save(uniform_trials,
     file = file.path(folder_keys, paste(time_stamp, "uniform_trials.RData", sep = "-")))

# Create random sequences of trials ---------------------------------------
good_sequences <- list()
difference_plots <- list()

start_time <- Sys.time()

for (iSequence in 1:n_sequences) {
  good_block <- list()
    
  for (iBlock in 1:n_blocks) {
    trial <- 1
    attempt <- 0
    good_block[[iBlock]] <- sample_n(filter(uniform_trials, 
                                            block == iBlock), 1)
    while (trial < n_trials_block) {
      attempt <- attempt + 1
      new_sample <- sample_n(filter(uniform_trials, 
                                    block == iBlock), 1)
      
      # Re-sample the trials if the sequence gets stuck
      if (attempt > 500) {
        good_block[[iBlock]] <- NULL
        good_block[[iBlock]] <- sample_n(filter(uniform_trials, 
                                                block == iBlock), 1)
        trial <- 1
        } #end progress check
      
      # Avoid consecutive repetitions of stimuli and repeated items in the sequence
      if (new_sample$rw_left  != good_block[[iBlock]]$rw_left[trial] &
          new_sample$rw_right != good_block[[iBlock]]$rw_right[trial] &
          new_sample$p_left   != good_block[[iBlock]]$p_left[trial] &
          new_sample$p_right  != good_block[[iBlock]]$p_right[trial] &
          !new_sample$uniform_id %in% good_block[[iBlock]]$uniform_id
          ) 
        
        {
        # Add the successful sample to the sequence, reset attempt counter and move trial
        good_block[[iBlock]] <- bind_rows(good_block[[iBlock]], new_sample)
        trial <- trial + 1
        attempt <- 0
        } #end of repetition check
      
      print(trial)
      
    } #end while loop
    
  } #end block loop
  
  good_sequences[[iSequence]] <- bind_rows(good_block) %>% 
    mutate(order    = 1:n(),
           sequence = iSequence,
           trial    = rep(1:n_trials_block, n_blocks))
  
} #end sequence loop

# Create dataset with all the sequences merged
experiment_sequences <- bind_rows(good_sequences)

save(experiment_sequences,
     file = file.path(folder_keys,
                      paste(time_stamp,
                            "experiment-sequences.RData", sep = "-")))
write_csv(experiment_sequences,
          file = file.path(folder_keys, 
                           paste(time_stamp, 
                                 "experiment-sequences.csv", sep = "-")))

# Create plots fo trials distributions
for (iSequence in 1:n_sequences) {
  
  difference_plots[[iSequence]] <- ggplot(good_sequences[[iSequence]]) +
  
    ggtitle(paste("Sequence", iSequence, sep = " "),
            subtitle = "The minimal distance is 0.15 and 15 for p and rw. \nThere are 64 conditions, with 5 trials per condition.") +
  
    geom_hline(aes(yintercept = 0)) +
  
    geom_vline(aes(xintercept = 0)) +
  
    geom_point(aes(x = supp_p,
                   y = supp_rw),
               alpha = 0.2,
               #width = 0.005,
               color = "darkblue",
               size  = 2) +
  
    scale_x_continuous(name = "Difference in reward") +
  
    scale_y_continuous(name = "Difference in probability") +
  
    theme_bw()

ggsave(filename = file.path(folder_blocks,
                            paste(time_stamp, "-", "sequence", iSequence, ".png",
                                  sep = "")),
       plot = difference_plots[[iSequence]],
       dpi = "retina")

}

# Save the plots for reference
save(difference_plots,
     file = file.path(folder_keys,
                      paste(time_stamp,
                            "difference_plots.RData", sep = "-")))


# Create independent files per block --------------------------------------
for (iSequence in 1:n_sequences) {

  for (iBlock in 1:n_blocks) {

    block_data <- filter(experiment_sequences,
                         sequence == iSequence & block == iBlock)

        write_csv(block_data,
                  file = file.path(folder_blocks,
                                   paste(time_stamp, "-", 
                                         "sequence", iSequence, "-",
                                         "block", iBlock, ".csv",
                                         sep = "")))

  }


}

