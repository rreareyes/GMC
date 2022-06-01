# GMC-S01
# Import responses
# This script imports and preprocess the data from the GMC experiment. 
# It uses the set of .csv files from the PsychoPy task

# Written by Eduardo Rea
# GMC project
# NLP Lab
# October 2021
 
# Load libraries and set base directories and variables -------------------
library(tidyverse)
library(janitor)

folder_root    <- dirname(dirname(rstudioapi::getActiveDocumentContext()$path))
folder_results <- file.path(folder_root, "Results", "Datasets")
folder_data    <- file.path(folder_root, "Data", "Responses")
folder_keys    <- file.path(folder_root, "Data", "Keys")

files_data <- list.files(path = folder_data)
nFiles     <- length(files_data)


# Import conditions key from the log --------------------------------------

raw_log <- read.csv(file.path(folder_keys, "gmc-log.csv"))
  
# Import behavioral data --------------------------------------------------

raw_subject_data <- list()

for (iFile in 1:nFiles) {
  
  subject_file <- file.path(folder_data, files_data[iFile])
  
  raw_variables <- read.csv(file   = subject_file, 
                            header = T, 
                            nrows  = 0) %>% 
    clean_names() 
  
  ## Define the columns we want to keep -----------------------------------
  relevant_variables <- raw_variables %>% 
    select(condition:jitter, choice_key_keys, choice_key_rt) %>% 
    colnames()
  
  # Create index of data types to ignore irrelevant variables -------------
  # The read.csv function can ignore specific columns if we provide a vector
  # of characters of the same length of the whole set of variables, and
  # we set to NULL the ones we want to ignore. We can ask the function to
  # guess the appropriate data type for the variables we want to keep by 
  # setting them as NA in our vector of data types.
  
  relevant_index <- colnames(raw_variables) %in% relevant_variables
  relevant_guide <- rep("NULL", length(colnames(raw_variables))) 
  
  relevant_guide[relevant_index] <- NA
  
  ## Import and clean data missing values ---------------------------------
  raw_subject_data[[iFile]] <- read.csv(file       = subject_file, 
                                        colClasses = relevant_guide, 
                                        na.strings = c("", " ", "NA")) %>% 
    clean_names() %>% 
    drop_na() %>% 
    mutate(id              = as.numeric(strsplit(basename(subject_file), "_")[[1]][1]),
           choice_key_keys = choice_key_keys == "s") %>% 
    relocate(id) %>% 
    rename("choice" = choice_key_keys,
           "rt"     = choice_key_rt)
  
}

# Merge all the data into a single dataframe ------------------------------
gamble_data <- bind_rows(raw_subject_data) %>% 
  full_join(raw_log) %>% 
  filter(!is.na(condition)) %>% 
  mutate(ev_left    = p_left * r_left,
         ev_right   = p_right * r_right,
         ev_dif     = round(ev_left - ev_right, 2),
         p_dif      = p_dif * 100,
         p_left     = p_left * 100,
         p_right    = p_right * 100,
         p_rel      = p_left/p_right,
         r_rel      = r_left/r_right,
         choice     = as.numeric(choice),
         p_dif_tr   = round(p_left, -1) - round(p_right, -1),
         r_dif_tr   = round(r_left, -1) - round(r_right, -1),
         p_level    = p_dif/17,
         r_level    = r_dif/17,
         p_level_tr = p_dif_tr/10,
         r_level_tr = r_dif_tr/10,
         p_dif_tal  = sign(p_dif),
         r_dif_tal  = sign(r_dif),
         s_tal      = p_dif_tal + r_dif_tal,
         # ttb prob priority
         #magnitude
         p_pttb_mg_A = ifelse(abs(p_dif) > 18, p_dif, 0),
         r_pttb_mg_A = ifelse(abs(p_dif) > 18, 0, r_dif),
         p_pttb_mg_B = ifelse(abs(p_dif) > 35, p_dif, 0),
         r_pttb_mg_B = ifelse(abs(p_dif) > 35, 0, r_dif),
         p_pttb_mg_C = ifelse(abs(p_dif) > 52, p_dif, 0),
         r_pttb_mg_C = ifelse(abs(p_dif) > 52, 0, r_dif),
         #levels
         p_pttb_lv_A = ifelse(abs(p_dif) > 18, p_level, 0),
         r_pttb_lv_A = ifelse(abs(p_dif) > 18, 0, r_level),
         p_pttb_lv_B = ifelse(abs(p_dif) > 35, p_level, 0),
         r_pttb_lv_B = ifelse(abs(p_dif) > 35, 0, r_level),
         p_pttb_lv_C = ifelse(abs(p_dif) > 52, p_level, 0),
         r_pttb_lv_C = ifelse(abs(p_dif) > 52, 0, r_level),
         #equal
         p_pttb_tl_A = ifelse(abs(p_dif) > 18, p_dif_tal, 0),
         r_pttb_tl_A = ifelse(abs(p_dif) > 18, 0, r_dif_tal),
         p_pttb_tl_B = ifelse(abs(p_dif) > 35, p_dif_tal, 0),
         r_pttb_tl_B = ifelse(abs(p_dif) > 35, 0, r_dif_tal),
         p_pttb_tl_C = ifelse(abs(p_dif) > 52, p_dif_tal, 0),
         r_pttb_tl_C = ifelse(abs(p_dif) > 52, 0, r_dif_tal),
         # ttb rw priority
         # magnitude
         r_rttb_mg_A = ifelse(abs(r_dif) > 18, r_dif, 0),
         p_rttb_mg_A = ifelse(abs(r_dif) > 18, 0, p_dif),
         r_rttb_mg_B = ifelse(abs(r_dif) > 35, r_dif, 0),
         p_rttb_mg_B = ifelse(abs(r_dif) > 35, 0, p_dif),
         r_rttb_mg_C = ifelse(abs(r_dif) > 52, r_dif, 0),
         p_rttb_mg_C = ifelse(abs(r_dif) > 52, 0, p_dif),
         #levels
         r_rttb_lv_A = ifelse(abs(r_dif) > 18, r_level, 0),
         p_rttb_lv_A = ifelse(abs(r_dif) > 18, 0, p_level),
         r_rttb_lv_B = ifelse(abs(r_dif) > 35, r_level, 0),
         p_rttb_lv_B = ifelse(abs(r_dif) > 35, 0, p_level),
         r_rttb_lv_C = ifelse(abs(r_dif) > 52, r_level, 0),
         p_rttb_lv_C = ifelse(abs(r_dif) > 52, 0, p_level),
         #equal
         r_rttb_tl_A = ifelse(abs(r_dif) > 18, r_dif_tal, 0),
         p_rttb_tl_A = ifelse(abs(r_dif) > 18, 0, p_dif_tal),
         r_rttb_tl_B = ifelse(abs(r_dif) > 35, r_dif_tal, 0),
         p_rttb_tl_B = ifelse(abs(r_dif) > 35, 0, p_dif_tal),
         r_rttb_tl_C = ifelse(abs(r_dif) > 52, r_dif_tal, 0),
         p_rttb_tl_C = ifelse(abs(r_dif) > 52, 0, p_dif_tal))

# Prepare data for model selection ----------------------------------------
model_input <- gamble_data %>% 
  mutate(id = as.numeric(id)) %>% 
  select(id, stage, rw_top, choice,
         p_dif, r_dif, 
         ev_dif, 
         p_rel, r_rel,
         p_dif_tr:p_rttb_tl_C)

write_csv(model_input,
          file = file.path(folder_results, "model_data.csv"),
          col_names = T)


# Export full dataset -----------------------------------------------------
write_csv(gamble_data,
          file = file.path(folder_results, "response_data.csv"),
          col_names = T)

save(gamble_data,
     file = file.path(folder_results, "response_data.RData"))


