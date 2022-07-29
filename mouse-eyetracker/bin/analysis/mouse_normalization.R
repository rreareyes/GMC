# Mousetracking time normalization
library(tidyverse)
library(mousetrap)

# Base directories --------------------------------------------------------
path_root    <- dirname(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)))
path_results <- file.path(path_root, "results", "datasets")
path_keys    <- file.path(path_root, "results", "keys")

# Decision models
model_names <- c(
  "PrDelta + RwDelta",
  "RwDelta",
  "PrDelta",
  "PrRatio + RwRatio",
  "RwRatio",
  "PrRatio",
  "PrRatioLv + RwRatioLv",
  "RwRatioLv",
  "PrRatioLv",
  "EV",
  "EV Ratio",
  "SV",
  "PrDeltaLv + RwDeltaLv",
  "PrDeltaLv",
  "RwDeltaLv",
  "PrDeltaTr + RwDeltaTr",
  "PrDeltaTr",
  "RwDeltaTr",
  "PrTrLv + RwTrLv",
  "PrTrLv",
  "RwTrLv",
  "PrTl + RwTl",
  "PrTl",
  "RwTl",
  "TallySum"
)


# Load base datasets ------------------------------------------------------
load(file = file.path(path_keys, "conditions.RData"))
load(file = file.path(path_results,  "preproc_mouse.RData"))
load(file = file.path(path_results,  "preproc_choices.RData"))

model_key <- read.csv(file = file.path(path_keys, "model_classification.csv"), 
                      colClasses = c("numeric", "numeric")) %>% 
  mutate(model_name = factor(model, 
                             levels = 1:length(model_names), 
                             labels = model_names))


# Generate trial conditions by level --------------------------------------
trial_key <- preproc_choices %>% 
  select(id, order, delta_r, delta_p, bin_ratio_r, bin_ratio_p, bin_delta_ev, bin_ratio_ev, bin_delta_sv) %>% 
  distinct() %>% 
  mutate(bin_delta_p = abs(delta_p) /13,
         bin_delta_r = abs(delta_r) /13,
         bin_ratio_p = abs(bin_ratio_p),
         bin_ratio_r = abs(bin_ratio_r),
         bin_delta_ev = abs(bin_delta_ev),
         bin_ratio_ev = abs(bin_ratio_ev),
         bin_delta_sv = abs(bin_delta_sv))


# Add trial labels to mousetracking data ----------------------------------
mouse_labelled_data <- left_join(preproc_mouse, trial_key) %>% 
  left_join(model_key) %>% 
  pivot_longer(cols = starts_with("bin_"), names_to = "metric", values_to = "level") %>% 
  mutate(level = factor(level),
         metric = factor(metric, levels = c("bin_delta_p", "bin_delta_r", 
                                            "bin_ratio_p", "bin_ratio_r",
                                            "bin_delta_ev", "bin_ratio_ev",
                                            "bin_delta_sv"))) %>% 
  select(id, model, model_name, trial = order, sample, metric, level, time, x, y)


# Load dataset into mousetrap for processing ------------------------------
tnorm_mouse_data <- mouse_labelled_data %>% 
  mt_import_long(xpos_label = "x",
                 ypos_label = "y", 
                 timestamps_label = "time", 
                 mt_seq_label = "sample",
                 mt_id_label = c("id", "model_name", "trial",
                                 "metric", "level")) %>% 
  mt_remap_symmetric(use = "trajectories") %>%
  mt_time_normalize(use = "trajectories") 

# Aggregate data by condition and export tidy in format --------------------
aggregated_tnorm_mouse_data <- tnorm_mouse_data %>% 
  mt_aggregate(use = "tn_trajectories", 
               subject_id = "mt_id", 
               use2_variables = c("model_name", 
                                  "metric", "level")) 

# Export time normalized datasets -----------------------------------------
save(tnorm_mouse_data, 
     file = file.path(path_results, "tnorm_mouse.RData"))

save(aggregated_tnorm_mouse_data, 
     file = file.path(path_results, "aggregated_tnorm_mouse.RData"))

