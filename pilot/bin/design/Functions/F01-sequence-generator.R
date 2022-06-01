trial_seq <- function(combinations, n_trials = 300, n_sequences = 10) {
  
  trial_sequences <- list()
  
  for (iSequence in 1:n_sequences) {
    
    indiv_sequence = combinations %>% 
      sample_n(n_trials) %>% 
      mutate(trial = 1:n())
    
    trial_sequences[[iSequence]] <- indiv_sequence
    
  }
  
  trial_dataset <- bind_rows(trial_sequences)
  
  return(trial_dataset)
  
}

