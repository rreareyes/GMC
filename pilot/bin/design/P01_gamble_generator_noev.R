
# Load required libraries -------------------------------------------------
library(tidyverse)
library(gtools)

# Define paths ------------------------------------------------------------
folder_root      <- dirname(dirname(rstudioapi::getActiveDocumentContext()$path))
folder_scripts   <- file.path(folder_root, "Scripts")
folder_functions <- file.path(folder_scripts, "Functions")
folder_keys      <- file.path(folder_root, "Sequences")
folder_blocks    <- file.path(folder_keys, "blocks")

source(file.path(folder_functions, "F01-sequence-generator.R"))

# Define vector of probabilities ------------------------------------------
p_above <- seq(from = 0.22, to = 0.82, by = 0.15)

# Define vector of rewards ------------------------------------------------
rewards <- seq(from = 23, to = 87, by = 15)

# Obtain permutations of rewards and probabilities ------------------------
mixed_data <- c(p_above, rewards)

payout_combinations <- data.frame(permutations(length(mixed_data), 4, mixed_data)) %>% 
  filter(X1 > 1, X2 < 1, X3 > 1, X4 < 1) %>% 
  rename("rw_left"  = X1, 
         "p_left"   = X2, 
         "rw_right" = X3,
         "p_right"  = X4) %>% 
  mutate(trial_id = 1:n(),
         ev_right = rw_right * p_right,
         ev_left  = rw_left * p_left,
         dir_rw   = sign(rw_left - rw_right),
         dir_p    = sign(p_left - p_right),
         dif_rw   = abs(rw_left - rw_right),
         dif_p    = round(abs(p_left - p_right), 2),
         dif_ev   = round(abs(ev_left - ev_right), 3),
         supp_rw  = round(rw_left - rw_right),
         supp_p   = round(p_left - p_right, 2),
         supp_ev  = round(ev_left - ev_right, 3)) 

time_stamp <- format(Sys.time(), "%Y-%m-%d-%s")

# save(payout_combinations,
#      file = file.path(folder_keys, paste(time_stamp, "payout-combinations.RData", sep = "-")))
#      

# Create random sequences of trials ---------------------------------------
n_sequences    <- 10
n_blocks       <- 5
n_trials       <- 300
n_trials_block <- 300/n_blocks

good_sequences <- list()
difference_plots <- list()

start_time <- Sys.time()

for (iSequence in 1:n_sequences) {
  
  trial <- 1
  
  good_sequences[[iSequence]] <- sample_n(payout_combinations, 1)

  while (trial < n_trials) {
    
    new_sample <- sample_n(payout_combinations, 1)
    
    if (!duplicated(new_sample$rw_left, good_sequences[[iSequence]]$rw_left[trial]) &
        !duplicated(new_sample$rw_right, good_sequences[[iSequence]]$rw_right[trial]) &
        !duplicated(new_sample$p_left, good_sequences[[iSequence]]$p_left[trial]) &
        !duplicated(new_sample$p_right, good_sequences[[iSequence]]$p_right[trial]) &
        !new_sample$trial_id %in% good_sequences[[iSequence]]$trial_id
        
        ) {
      
      good_sequences[[iSequence]] <- bind_rows(good_sequences[[iSequence]], new_sample)
      
      trial <- trial + 1
      
    }
    
  }
  
  good_sequences[[iSequence]] <- good_sequences[[iSequence]] %>% 
    mutate(order    = 1:n(),
           sequence = iSequence,
           trial    = rep(1:n_trials_block, n_blocks),
           block    = rep(1:n_blocks, each = n_trials_block))
    
  
  difference_plots[[iSequence]] <- ggplot(good_sequences[[iSequence]]) +
    
    ggtitle(paste("Sequence", iSequence, sep = " "), 
            subtitle = "Each sequece is different, the minimal distance is 0.7 and 7 for p and rw. Jitter added to facilitate view.") +
    
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
  
  # ggsave(filename = file.path(folder_blocks,
  #                             paste(time_stamp, "-", "sequence", iSequence, ".png",
  #                                   sep = "")), 
  #        plot = difference_plots[[iSequence]], 
  #        dpi = "retina")
  }

experiment_sequences <- bind_rows(good_sequences)

# save(experiment_sequences, 
#      file = file.path(folder_keys, 
#                       paste(time_stamp, 
#                             "experiment-sequences.RData", sep = "-")))
# 
# save(difference_plots, 
#      file = file.path(folder_keys, 
#                       paste(time_stamp, 
#                             "difference_plots.RData", sep = "-")))
# 
# write_csv(experiment_sequences,
#           file = file.path(folder_keys, paste(time_stamp, "experiment-sequences.csv", sep = "-")))
 
# Create independent files per block --------------------------------------
for (iSequence in 1:n_sequences) {

  for (iBlock in 1:n_blocks) {

    block_data <- filter(experiment_sequences,
                         sequence == iSequence & block == iBlock)
# 
#     write_csv(block_data,
#               file = file.path(folder_blocks,
#                                paste(time_stamp, "-", "sequence", iSequence, "-",
#                                      "block", iBlock, ".csv",
#                                      sep = "")))

  }


}



