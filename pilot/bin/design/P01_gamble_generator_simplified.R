# Load required libraries -------------------------------------------------
library(tidyverse)
library(gtools)

# Define paths ------------------------------------------------------------
folder_root      <- dirname(dirname(rstudioapi::getActiveDocumentContext()$path))
folder_scripts   <- file.path(folder_root, "Scripts")
folder_functions <- file.path(folder_scripts, "Functions")
folder_keys      <- file.path(folder_root, "Sequences")
folder_blocks    <- file.path(folder_keys, "blocks")

time_stamp <- format(Sys.time(), "%Y-%m-%d-%s")

# Define initial values ---------------------------------------------------
values <- seq(from = 11, to = 99, by = 1)

value_combinations <- data.frame(permutations(length(values), 2, values)) %>% 
  rename("left" = X1,
         "right" = X2) %>% 
  mutate(dif  = round(left - right, 2)) 

# Constraint values to use ------------------------------------------------
values_constrained <- value_combinations %>% 
  group_by(dif) %>% 
  mutate(freq = n()) %>% 
  ungroup() %>% 
  filter(freq > 19) %>% 
  filter(dif %in% seq(-85, 85, 17)) %>% 
  select(-freq) %>% 
  mutate(condition = as.factor(dif),
         condition = recode(condition, 
                            "-68" = "a",
                            "-51" = "b",
                            "-34" = "c",
                            "-17" = "d",
                            "68" = "h",
                            "51" = "g",
                            "34" = "f",
                            "17" = "e"))

n_differences <- length(unique(values_constrained$dif))

rewards <- values_constrained %>% 
  group_by(dif) %>% 
  sample_n(40, replace = T) %>% 
  ungroup() %>% 
  rename("r_left"  = left,
         "r_right" = right,
         "r_dif"   = dif,
         "r_cond"  = condition) %>% 
  group_by(r_cond) %>% 
  mutate(set = rep(sample(1:n_differences, n_differences), 5)) %>% 
  ungroup() %>% 
  arrange(set)

probabilities <- values_constrained %>% 
  group_by(dif) %>% 
  sample_n(40, replace = T) %>% 
  ungroup() %>% 
  mutate("p_left"  = left/100,
         "p_right" = right/100,
         "p_dif"   = dif/100,
         "p_cond" =  condition,
         .keep = "none") %>% 
  group_by(p_cond) %>% 
  ungroup() %>% 
  arrange(p_cond)

controlled_combinations <- bind_cols(rewards, probabilities) %>% 
  select(-set) %>% 
  unite(col = "condition", r_cond, p_cond) %>% 
  arrange(condition) %>% 
  mutate(trial_id = 1:n()) %>% 
  select(r_left, p_left, r_right, p_right, condition, trial_id, r_dif, p_dif)
# 
# save(controlled_combinations, 
#      file = file.path(folder_keys, paste(time_stamp, "controlled-combinations.RData", sep = "-")))

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
      if (new_sample$r_left  != good_sequences[[iSequence]]$r_left[trial] &
          new_sample$r_right != good_sequences[[iSequence]]$r_right[trial] &
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

# save(experiment_sequences,
#      file = file.path(folder_keys,
#                       paste(time_stamp,
#                             "experiment-sequences.RData", sep = "-")))
# write_csv(experiment_sequences,
#           file = file.path(folder_keys, 
#                            paste(time_stamp, 
#                                  "experiment-sequences.csv", sep = "-")))
# 
# # Create plots fo trials distributions
# for (iSequence in 1:n_sequences) {
#   
#   difference_plots[[iSequence]] <- ggplot(good_sequences[[iSequence]]) +
#   
#     ggtitle(paste("Sequence", iSequence, sep = " "),
#             subtitle = "The minimal distance is 0.17 and 17 for p and rw. \nThere are 64 conditions, with 5 trials per condition.") +
#   
#     geom_hline(aes(yintercept = 0)) +
#   
#     geom_vline(aes(xintercept = 0)) +
#   
#     geom_point(aes(x = p_dif,
#                    y = r_dif),
#                alpha = 0.2,
#                #width = 0.005,
#                color = "darkblue",
#                size  = 2) +
#   
#     scale_x_continuous(name = "Difference in probability") +
#   
#     scale_y_continuous(name = "Difference in reward") +
#   
#     theme_bw()
# 
# ggsave(filename = file.path(folder_blocks,
#                             paste(time_stamp, "-", "sequence", iSequence, ".png",
#                                   sep = "")),
#        plot = difference_plots[[iSequence]],
#        dpi = "retina")
# 
# }
# 
# # Save the plots for reference
# save(difference_plots,
#      file = file.path(folder_keys,
#                       paste(time_stamp,
#                             "difference_plots.RData", sep = "-")))
# 
# 
# # Create independent files per block --------------------------------------
# for (iSequence in 1:n_sequences) {
# 
#   for (iBlock in 1:n_blocks) {
# 
#     block_data <- filter(experiment_sequences,
#                          seq_id == iSequence & block == iBlock)
# 
#         write_csv(block_data,
#                   file = file.path(folder_blocks,
#                                    paste(time_stamp, "-", 
#                                          "sequence", iSequence, "-",
#                                          "block", iBlock, ".csv",
#                                          sep = "")))
# 
#   }
# 
# 
# }

