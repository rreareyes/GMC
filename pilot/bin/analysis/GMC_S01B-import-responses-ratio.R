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

folder_root    <- dirname(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)))
folder_results <- file.path(folder_root, "results", "datasets")
folder_data    <- file.path(folder_root, "data", "responses", "ratio")
folder_keys    <- file.path(folder_root, "data", "keys")
folder_logs    <- file.path(folder_root, "data", "logs")

files_data <- list.files(path = folder_data, pattern = )
nFiles     <- length(files_data)

dir.create(folder_results, showWarnings = T)

# Import conditions key from the log --------------------------------------

raw_log <- read.csv(file.path(folder_logs, "gmc-log.csv"))
  
# Import behavioral data --------------------------------------------------

raw_subject_data <- list()

for (iFile in 1:nFiles) {
  
  subject_file <- file.path(folder_data, files_data[iFile])
  
  raw_variables <- read.csv(file   = subject_file, 
                            header = T, 
                            nrows  = 0) %>% 
    clean_names() %>%
    rename(condition = starts_with("condition"))
  
  ## Define the columns we want to keep -----------------------------------
  relevant_variables <- raw_variables %>% 
    select(condition:trial, choice_key_keys, choice_key_rt) %>% 
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
    rename(condition = starts_with("condition")) %>% 
    
    mutate(id              = as.numeric(strsplit(basename(subject_file), "_")[[1]][1]),
           choice_key_keys = choice_key_keys == "s") %>% 
    relocate(id) %>% 
    rename("choice" = choice_key_keys,
           "rt"     = choice_key_rt)
  
}

cohort_data <- bind_rows(raw_subject_data)

# Merge all the data into a single dataframe ------------------------------
gamble_data <- cohort_data %>%
  full_join(raw_log) %>%
  filter(!is.na(condition)) %>%
  mutate(
    # Relabel choices
    choice = as.numeric(choice),
    # Expected value
    ev_left  = p_left/100 * rw_left,
    ev_right = p_right/100 * rw_right,
    ev_delta = round(ev_left - ev_right, 2),
    ev_ratio = round(ev_left/ev_right, 2),
    # Stimuli levels
    p_delta_lvl  = as.integer(p_delta/13),
    rw_delta_lvl = as.integer(rw_delta/13),
    # Truncated levels
    p_delta_tr  = round(p_left, -1) - round(p_right, -1),
    rw_delta_tr = round(rw_left, -1) - round(rw_right, -1),
    p_lvl_tr    = p_delta_tr/10,
    rw_lvl_tr   = rw_delta_tr/10,
    # Ratio levels
    log_p_ratio    = log(p_ratio),
    log_rw_ratio   = log(rw_ratio),
    log_p_bins     = cut(log_p_ratio, breaks = 9, include.lowest = T),
    log_rw_bins    = cut(log_rw_ratio, breaks = 9, include.lowest = T),
    p_ratio_level  = as.integer(log_p_bins) - 5,
    rw_ratio_level = as.integer(log_rw_bins) - 5,
    # Tally
    p_delta_tal  = sign(p_delta),
    rw_delta_tal = sign(rw_delta),
    s_tal        = p_delta_tal + rw_delta_tal,
    # THIS LOOKS AWFUL, WE CAN MAYBE REPLACE IT WITH LESS COLUMNS AND DO PIVOT WIDER
    # TTB prob priority
    #magnitude
    p_pttb_mg_A  = ifelse(abs(p_delta) > 14, p_delta, 0),
    rw_pttb_mg_A = ifelse(abs(p_delta) > 14, 0, rw_delta),
    p_pttb_mg_B  = ifelse(abs(p_delta) > 27, p_delta, 0),
    rw_pttb_mg_B = ifelse(abs(p_delta) > 27, 0, rw_delta),
    p_pttb_mg_C  = ifelse(abs(p_delta) > 40, p_delta, 0),
    rw_pttb_mg_C = ifelse(abs(p_delta) > 40, 0, rw_delta),
    p_pttb_mg_D  = ifelse(abs(p_delta) > 53, p_delta, 0),
    rw_pttb_mg_D = ifelse(abs(p_delta) > 53, 0, rw_delta),
    p_pttb_mg_E  = ifelse(abs(p_delta) > 66, p_delta, 0),
    rw_pttb_mg_E = ifelse(abs(p_delta) > 66, 0, rw_delta),
    #levels
    p_pttb_lv_A  = ifelse(abs(p_delta) > 14, p_delta_lvl, 0),
    rw_pttb_lv_A = ifelse(abs(p_delta) > 14, 0, rw_delta_lvl),
    p_pttb_lv_B  = ifelse(abs(p_delta) > 27, p_delta_lvl, 0),
    rw_pttb_lv_B = ifelse(abs(p_delta) > 27, 0, rw_delta_lvl),
    p_pttb_lv_C  = ifelse(abs(p_delta) > 40, p_delta_lvl, 0),
    rw_pttb_lv_C = ifelse(abs(p_delta) > 40, 0, rw_delta_lvl),
    p_pttb_lv_D  = ifelse(abs(p_delta) > 53, p_delta_lvl, 0),
    rw_pttb_lv_D = ifelse(abs(p_delta) > 53, 0, rw_delta_lvl),
    p_pttb_lv_E  = ifelse(abs(p_delta) > 66, p_delta_lvl, 0),
    rw_pttb_lv_E = ifelse(abs(p_delta) > 66, 0, rw_delta_lvl),
    #equal
    p_pttb_tl_A  = ifelse(abs(p_delta) > 14, p_delta_tal, 0),
    rw_pttb_tl_A = ifelse(abs(p_delta) > 14, 0, rw_delta_tal),
    p_pttb_tl_B  = ifelse(abs(p_delta) > 27, p_delta_tal, 0),
    rw_pttb_tl_B = ifelse(abs(p_delta) > 27, 0, rw_delta_tal),
    p_pttb_tl_C  = ifelse(abs(p_delta) > 40, p_delta_tal, 0),
    rw_pttb_tl_C = ifelse(abs(p_delta) > 40, 0, rw_delta_tal),
    p_pttb_tl_D  = ifelse(abs(p_delta) > 53, p_delta_tal, 0),
    rw_pttb_tl_D = ifelse(abs(p_delta) > 53, 0, rw_delta_tal),
    p_pttb_tl_E  = ifelse(abs(p_delta) > 66, p_delta_tal, 0),
    rw_pttb_tl_E = ifelse(abs(p_delta) > 66, 0, rw_delta_tal),
    #ratio
    p_rt_pttb_mg_A  = ifelse(abs(p_ratio) > 0, p_ratio, 0),
    rw_rt_pttb_mg_A = ifelse(abs(rw_ratio) > 0, 0, rw_ratio),
    p_rt_pttb_mg_B  = ifelse(abs(p_ratio) > 1, p_ratio, 0),
    rw_rt_pttb_mg_B = ifelse(abs(rw_ratio) > 1, 0, rw_ratio),
    p_rt_pttb_mg_C  = ifelse(abs(p_ratio) > 2, p_ratio, 0),
    rw_rt_pttb_mg_C = ifelse(abs(rw_ratio) > 2, 0, rw_ratio),
    p_rt_pttb_mg_D  = ifelse(abs(p_ratio) > 3, p_ratio, 0),
    rw_rt_pttb_mg_D = ifelse(abs(rw_ratio) > 3, 0, rw_ratio),
    #
    p_rt_pttb_lv_A  = ifelse(abs(p_ratio_level) > 0, p_ratio_level, 0),
    rw_rt_pttb_lv_A = ifelse(abs(rw_ratio_level) > 0, 0, rw_ratio_level),
    p_rt_pttb_lv_B  = ifelse(abs(p_ratio_level) > 1, p_ratio_level, 0),
    rw_rt_pttb_lv_B = ifelse(abs(rw_ratio_level) > 1, 0, rw_ratio_level),
    p_rt_pttb_lv_C  = ifelse(abs(p_ratio_level) > 2, p_ratio_level, 0),
    rw_rt_pttb_lv_C = ifelse(abs(rw_ratio_level) > 2, 0, rw_ratio_level),
    p_rt_pttb_lv_D  = ifelse(abs(p_ratio_level) > 3, p_ratio_level, 0),
    rw_rt_pttb_lv_D = ifelse(abs(rw_ratio_level) > 3, 0, rw_ratio_level),
    
    # TTB rw priority
    # magnitude
    p_rttb_mg_A  = ifelse(abs(rw_delta) > 14, rw_delta, 0),
    rw_rttb_mg_A = ifelse(abs(rw_delta) > 14, 0, p_delta),
    p_rttb_mg_B  = ifelse(abs(rw_delta) > 27, rw_delta, 0),
    rw_rttb_mg_B = ifelse(abs(rw_delta) > 27, 0, p_delta),
    p_rttb_mg_C  = ifelse(abs(rw_delta) > 40, rw_delta, 0),
    rw_rttb_mg_C = ifelse(abs(rw_delta) > 40, 0, p_delta),
    p_rttb_mg_D  = ifelse(abs(rw_delta) > 53, rw_delta, 0),
    rw_rttb_mg_D = ifelse(abs(rw_delta) > 53, 0, p_delta),
    p_rttb_mg_E  = ifelse(abs(rw_delta) > 66, rw_delta, 0),
    rw_rttb_mg_E = ifelse(abs(rw_delta) > 66, 0, p_delta),
    #levels
    p_rttb_lv_A  = ifelse(abs(rw_delta) > 14, rw_delta_lvl, 0),
    rw_rttb_lv_A = ifelse(abs(rw_delta) > 14, 0, p_delta_lvl),
    p_rttb_lv_B  = ifelse(abs(rw_delta) > 27, rw_delta_lvl, 0),
    rw_rttb_lv_B = ifelse(abs(rw_delta) > 27, 0, p_delta_lvl),
    p_rttb_lv_C  = ifelse(abs(rw_delta) > 40, rw_delta_lvl, 0),
    rw_rttb_lv_C = ifelse(abs(rw_delta) > 40, 0, p_delta_lvl),
    p_rttb_lv_D  = ifelse(abs(rw_delta) > 53, rw_delta_lvl, 0),
    rw_rttb_lv_D = ifelse(abs(rw_delta) > 53, 0, p_delta_lvl),
    p_rttb_lv_E  = ifelse(abs(rw_delta) > 66, rw_delta_lvl, 0),
    rw_rttb_lv_E = ifelse(abs(rw_delta) > 66, 0, p_delta_lvl),
    #equal
    p_rttb_tl_A  = ifelse(abs(rw_delta) > 14, rw_delta_tal, 0),
    rw_rttb_tl_A = ifelse(abs(rw_delta) > 14, 0, p_delta_tal),
    p_rttb_tl_B  = ifelse(abs(rw_delta) > 27, rw_delta_tal, 0),
    rw_rttb_tl_B = ifelse(abs(rw_delta) > 27, 0, p_delta_tal),
    p_rttb_tl_C  = ifelse(abs(rw_delta) > 40, rw_delta_tal, 0),
    rw_rttb_tl_C = ifelse(abs(rw_delta) > 40, 0, p_delta_tal),
    p_rttb_tl_D  = ifelse(abs(rw_delta) > 53, rw_delta_tal, 0),
    rw_rttb_tl_D = ifelse(abs(rw_delta) > 53, 0, p_delta_tal),
    p_rttb_tl_E  = ifelse(abs(rw_delta) > 66, rw_delta_tal, 0),
    rw_rttb_tl_E = ifelse(abs(rw_delta) > 66, 0, p_delta_tal),
    #ratio
    p_rt_rttb_mg_A  = ifelse(abs(rw_ratio) > 0, 0, p_ratio),
    rw_rt_rttb_mg_A = ifelse(abs(rw_ratio) > 0, rw_ratio, 0),
    p_rt_rttb_mg_B  = ifelse(abs(rw_ratio) > 1, 0, p_ratio),
    rw_rt_rttb_mg_B = ifelse(abs(rw_ratio) > 1, rw_ratio, 0),
    p_rt_rttb_mg_C  = ifelse(abs(rw_ratio) > 2, 0, p_ratio),
    rw_rt_rttb_mg_C = ifelse(abs(rw_ratio) > 2, rw_ratio, 0),
    p_rt_rttb_mg_D  = ifelse(abs(rw_ratio) > 3, 0, p_ratio),
    rw_rt_rttb_mg_D = ifelse(abs(rw_ratio) > 3, rw_ratio, 0),
    #
    p_rt_rttb_lv_A  = ifelse(abs(rw_ratio_level) > 0, 0, p_ratio_level),
    rw_rt_rttb_lv_A = ifelse(abs(rw_ratio_level) > 0, rw_ratio_level, 0),
    p_rt_rttb_lv_B  = ifelse(abs(rw_ratio_level) > 1, 0, p_ratio_level),
    rw_rt_rttb_lv_B = ifelse(abs(rw_ratio_level) > 1, rw_ratio_level, 0),
    p_rt_rttb_lv_C  = ifelse(abs(rw_ratio_level) > 2, 0, p_ratio_level),
    rw_rt_rttb_lv_C = ifelse(abs(rw_ratio_level) > 2, rw_ratio_level, 0),
    p_rt_rttb_lv_D  = ifelse(abs(rw_ratio_level) > 3, 0, p_ratio_level),
    rw_rt_rttb_lv_D = ifelse(abs(rw_ratio_level) > 3, rw_ratio_level, 0),
    # Subjective Value
    sv_dif = (p_left/100 * log(rw_left)) - (p_right/100 * log(rw_right))
    ) #%>% 
  #unite(ratio_cond, p_ratio_level, rw_ratio_level, remove = F) %>% 
  #unite(delta_cond, p_delta_lvl, rw_delta_lvl, remove = F) 


# Prepare data for model selection ----------------------------------------
model_input <- gamble_data %>%
  mutate(id = as.numeric(id)) %>%
  select(id, stage, rw_top, 
         choice,
         p_delta, rw_delta,
         ev_delta,
         p_ratio, rw_ratio,
         p_delta_lvl:rw_lvl_tr,
         p_delta_tal:s_tal,
         p_pttb_mg_A:rw_pttb_tl_E,
         p_rttb_mg_A:rw_rttb_tl_E,
         p_ratio_level:rw_ratio_level,
         ev_ratio, 
         p_rt_pttb_mg_A:rw_rt_pttb_lv_D,
         p_rt_rttb_mg_A:rw_rt_rttb_lv_D,
         sv_dif
         )

write_csv(model_input,
          file = file.path(folder_results, "model_data_ratio.csv"),
          col_names = T)

# Export full dataset -----------------------------------------------------
write_csv(gamble_data,
          file = file.path(folder_results, "response_data_ratio.csv"),
          col_names = T)

save(gamble_data,
     file = file.path(folder_results, "response_data_ratio.RData"))


