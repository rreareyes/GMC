
# Load required libraries -------------------------------------------------
library(tidyverse)
library(gtools)

# Define paths ------------------------------------------------------------
folder_root      <- dirname(dirname(rstudioapi::getActiveDocumentContext()$path))
folder_scripts   <- file.path(folder_root, "Scripts")
folder_functions <- file.path(folder_scripts, "Functions")
folder_keys      <- file.path(folder_root, "Keys")
folder_blocks    <- file.path(folder_keys, "Blocks-simple")

source(file.path(folder_functions, "F01-sequence-generator.R"))

# Define vector of probabilities ------------------------------------------
p_above <- seq(from = 0.22, to = 0.82, by = 0.07)

# Define vector of rewards ------------------------------------------------
rewards <- seq(from = 230, to = 870, by = 70)

# Obtain permutations of rewards and probabilities ------------------------
mixed_data <- c(p_above, rewards)

payout_combinations <- data.frame(permutations(length(mixed_data), 4, mixed_data)) %>% 
  filter(X1 > 1, X2 < 1, X3 > 1, X4 < 1) %>% 
  rename("rw_left"  = X1, 
         "p_left"   = X2, 
         "rw_right" = X3,
         "p_right"  = X4) %>% 
  mutate(ev_right = rw_right * p_right,
         ev_left  = rw_left * p_left,
         dir_rw   = sign(rw_left - rw_right),
         dir_p    = sign(p_left - p_right),
         dif_rw   = abs(rw_left - rw_right),
         dif_p    = abs(p_left - p_right),
         dif_ev   = round(abs(ev_left - ev_right), 3),
         supp_rw  = rw_left - rw_right,
         supp_p   = p_left - p_right,
         supp_ev  = round(ev_left - ev_right, 3)) 

plot(payout_combinations$supp_p, payout_combinations$supp_rw)

save(payout_combinations, 
     file = file.path(folder_keys, "payout-combinations.RData"))


# Remove too similar pairs, easy options and low freq EVs -----------------
clean_combinations <- payout_combinations %>% 
  filter(dir_rw != dir_p) %>% 
  filter(dif_p > 0.2 & dif_rw > 15) %>%
  group_by(dif_ev) %>%
  mutate(members = n()) %>%
  ungroup() %>%
  filter(members > 20 & dif_ev > 1)

# Create uniform distribution of EVs --------------------------------------
uniform_combinations <- clean_combinations %>% 
  group_by(dif_ev) %>% 
  sample_n(10) %>% 
  ungroup()

# Create random sequences of trials ---------------------------------------
good_seq <- 0

n_sequences    <- 10
n_blocks       <- 5
n_trials       <- 300
n_trials_block <- 300/n_blocks

trial_sequences <- list()

start_time <- Sys.time()


random_seq <- trial_seq(combinations = uniform_combinations, 
                        n_trials     = n_trials, 
                        n_sequences  = 1) 

while (good_seq < n_sequences) {
  
  # Create provisional sequence randomly drawn from the uniform values
  
  prov_seq <- sample_n(random_seq, n_trials) %>% 
    mutate(order    = 1:n(),
           sequence = good_seq + 1,
           block    = rep(c(1:n_blocks), each = n_trials_block))
  
  prov_left_rw  <- c(prov_seq$rw_left)
  prov_right_rw <- c(prov_seq$rw_right)
  prov_left_p   <- c(prov_seq$p_left)
  prov_right_p  <- c(prov_seq$p_right)
  
  u_lenght <- length(prov_left_rw)
  
  # Check the number of repeating values in each of the stimulus
  u_left_rw  <- length(rle(prov_left_rw)[[1]])
  u_right_rw <- length(rle(prov_right_rw)[[1]])
  u_left_p   <- length(rle(prov_left_p)[[1]])
  u_right_p  <- length(rle(prov_right_p)[[1]])
  
  t1 <- u_lenght - u_left_rw
  t2 <- u_lenght - u_right_rw
  t3 <- u_lenght - u_left_p
  t4 <- u_lenght - u_right_p
  
  # Threshold for allowed number of repetitions
  if (t1 < 20 & t2 < 20 & t3 < 20 & t4 < 20) {
    
    trial_sequences[[good_seq + 1]] <- prov_seq
    #write.csv(select(prov_seq, 1:4), 
             # file = file.path(folder_keys, 
                 #              paste("random-sequence-", 
               #                      good_seq, ".csv", sep = "")), 
              #row.names = F)
    good_seq <- good_seq + 1
    
  }

}


# Merge sequences and export database -------------------------------------
experiment_sequences <- tibble(bind_rows(trial_sequences)) %>% 
  # Identify consecutive duplicates
  mutate(t1 = lead(rw_left, default = 0),
         t2 = lead(rw_right, default = 0),
         t3 = lead(p_left, default = 0),
         t4 = lead(p_right, default = 0)) %>% 
  # Add a small number to duplicates to make them unique
  mutate(rw_left = if_else(rw_left == t1, 
                           rw_left + sample(c(-5, -4, -3, -2, 2, 3, 4, 5), 1), 
                           rw_left),
         rw_right = if_else(rw_right == t2, 
                            rw_right + sample(c(-5, -4, -3, -2, 2, 3, 4, 5), 1), 
                            rw_right),
         p_left = if_else(p_left == t3, 
                          p_left + sample(c(-0.05, -0.04, -0.03, -0.02, 0.02, 0.03, 0.04, 0.05), 1), 
                          p_left),
         p_right = if_else(p_right == t4, 
                           p_right + sample(c(-0.05, -0.04, -0.03, -0.02, 0.02, 0.03, 0.04, 0.05), 1),
                           p_right)) %>% 
  # Recalculate evs and differences
  mutate(ev_right = rw_right * p_right,
         ev_left  = rw_left * p_left,
         dir_rw   = sign(rw_left - rw_right),
         dir_p    = sign(p_left - p_right),
         dir_ev    = sign(ev_left - ev_right),
         dif_rw   = abs(rw_left - rw_right),
         dif_p    = abs(p_left - p_right),
         dif_ev   = round(abs(ev_left - ev_right), 3)) %>% 
  # Select variables to export to the final database
  select(sequence, block, "trial_id" = trial, order, 
         rw_left, p_left, ev_left, 
         rw_right, p_right, ev_right, 
         dir_rw, dir_p, dir_ev, 
         dif_rw, dif_p, dif_ev)

# Quality metrics
bias <- experiment_sequences %>% 
  group_by(sequence) %>% 
  summarise(p = sum(dir_p), 
            rw = sum(dir_rw), 
            ev = sum(dir_ev))

end_time <- Sys.time()

print(end_time - start_time)
print(bias)

save(experiment_sequences, bias,
     file = file.path(folder_keys, paste(format(Sys.time(), "%Y-%m-%d-%s"), "experiment-sequences.RData", sep = "-")))

write_csv(experiment_sequences, 
          file = file.path(folder_keys, paste(format(Sys.time(), "%Y-%m-%d-%s"), "experiment-sequences.csv", sep = "-")))

# Create independent files per block --------------------------------------
for (iSequence in 1:n_sequences) {
  
  for (iBlock in 1:n_blocks) {
    
    block_data <- filter(experiment_sequences, 
                         sequence == iSequence & block == iBlock)
    
    write_csv(block_data, 
              file = file.path(folder_blocks, 
                               paste("sequence", iSequence, "-",
                                     "block", iBlock, ".csv", 
                                     sep = ""))
              
              )
      
  }
  
  
}



