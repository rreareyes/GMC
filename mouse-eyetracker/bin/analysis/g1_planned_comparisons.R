# PERFORM PLANNED COMPARISONS AND CREATE SUMMARIES

# This script takes the posterior samples from the Bayesian models and 
# performs planned paired comparisons between groups and levels. It uses
# 3 supplementary functions to avoid repetitions

# Load libraries ----------------------------------------------------------
library(rstan)
library(brms) 
library(tidybayes)
library(tidyverse)
library(scales)
library(emmeans)

# Base directories --------------------------------------------------------
path_root      <- dirname(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)))
path_results   <- file.path(path_root, "results", "datasets")
path_fits      <- file.path(path_root, "results", "fits")
path_summaries <- file.path(path_root, "results", "summaries")
path_helpers   <- file.path(path_root, "bin", "analysis", "helpers")

# Load the models to extract draws
choice_fit <- readRDS(file.path(path_fits, "choice1.0.rds"))
feature_status_fixations_fit <- readRDS(file.path(path_fits, "feature_status_fixations1.0.rds"))
transition_fit <- readRDS(file.path(path_fits, "transitions1.0.rds"))

# Load functions for extracting and cleaning posterior --------------------
source(file.path(path_helpers, "h_tidy_posterior.R"))
source(file.path(path_helpers, "h_rope_summary.R"))
source(file.path(path_helpers, "h_hdi_intervals.R"))
source(file.path(path_helpers, "h_compare_strategies.R"))

# Tibbles for group effects -----------------------------------------------
effect_choice <- choice_fit$data %>% 
  select(model_name) %>% 
  unique() %>% 
  mutate(total = 320)

effect_transition <- transition_fit$data %>% 
  select(model_name) %>% 
  unique() %>% 
  mutate(total = 100)

effect_feature_status_fixation <- feature_status_fixations_fit$data %>% 
  select(model_name) %>% 
  unique() %>% 
  mutate(total = 100)

# Tibbles to extract individual medians -----------------------------------
effect_ind_choice <- choice_fit$data %>% 
  select(model_name, id) %>% 
  unique() %>% 
  mutate(total = 320)

effect_ind_feature_status_fixation <- feature_status_fixations_fit$data %>% 
  select(model_name, id) %>% 
  unique() %>% 
  mutate(total = 100)

effect_ind_transition <- transition_fit$data %>% 
  select(model_name, id) %>% 
  unique() %>% 
  mutate(total = 100)

# Formulas for group effects ----------------------------------------------
formula_choice <- brmsformula(correct | trials(total) ~ 1 + model_name)

formula_feature_status_fixation <- brmsformula(y | trials(total) ~ 1 + model_name)

formula_transition <- brmsformula(y | trials(total) ~ 1 + model_name)

# Formulas for individual effects -----------------------------------------
formula_ind_choice <- brmsformula(correct | trials(total) ~ 1 + model_name + (1 + model_name | id))

formula_ind_feature_status_fixation <- brmsformula(y | trials(total) ~ 1 + model_name + (1 | id))

formula_ind_transition <- brmsformula(y | trials(total) ~ 1 + model_name + (1 | id))

# Choices -----------------------------------------------------------------
# Group Effects
samples_choice <- tidy_posterior(posterior = choice_fit, 
                                 effects   = effect_choice, 
                                 formula   = formula_choice)

# Individual estimates
individual_choice <- tidy_posterior(posterior = choice_fit, 
                                    effects   = effect_ind_choice,
                                    formula   = formula_ind_choice) %>% 
  
  group_by(id, model_name) %>% 
  summarise(percent  = median(percent),
            estimate = median(estimate),
            standard = median(standard)) %>% 
  ungroup() 

comp_choice_group <- samples_choice %>% 
  mutate(estimate = estimate * (sqrt(3)/pi)) %>% 
  select(-percent) %>% 
  pivot_wider(id_cols = c(draw), names_from = model_name, values_from = estimate) %>% 
  compare_strategies() %>% 
  pivot_longer(cols = -c(draw), names_to = "comparison", values_to = "difference")

rope_choice_group_diff <- rope_summary(comp_choice_group, c("comparison")) 

hdi_choice            <- hdi_intervals(samples_choice, c("model_name"), "percent")
hdi_choice_group_diff <- hdi_intervals(comp_choice_group, c("comparison"))

# Fixations ---------------------------------------------------------------
# Group Effects

samples_feature_status_fixation <- tidy_posterior(
  posterior = feature_status_fixations_fit, 
  effects   = effect_feature_status_fixation, 
  formula   = formula_feature_status_fixation) %>% 
  rename("label" = `.category`) %>% 
  mutate(label = case_when(label == 1 ~ "winning_reward",
                           label == 2 ~ "losing_reward",
                           label == 3 ~ "winning_probability",
                           label == 4 ~ "losing_probability"))

# Individual estimates
individual_feature_status_fixation <- tidy_posterior(
  posterior = feature_status_fixations_fit, 
  effects   = effect_ind_feature_status_fixation,
  formula   = formula_ind_feature_status_fixation) %>% 
  rename("label" = `.category`) %>% 
  mutate(label = case_when(label == 1 ~ "winning_reward",
                           label == 2 ~ "losing_reward",
                           label == 3 ~ "winning_probability",
                           label == 4 ~ "losing_probability")) %>% 
  
  group_by(id, model_name, label) %>% 
  summarise(percent  = median(percent),
            estimate = median(estimate),
            standard = median(standard)) %>% 
  ungroup() 

comp_feature_status_fixation <- samples_feature_status_fixation %>% 
  mutate(estimate = estimate * (sqrt(3)/pi)) %>% 
  select(-percent) %>%
  pivot_wider(id_cols = c(draw, model_name), names_from = label, values_from = estimate) %>% 
  mutate(wr_wp = `winning_reward` - `winning_probability`,
         wr_lr = `winning_reward` - `losing_reward`,
         wr_lp = `winning_reward` - `losing_probability`,
         wp_lr = `winning_probability` - `losing_reward`,
         wp_lp = `winning_probability` - `losing_probability`,
         lr_lp = `losing_reward`  - `losing_probability`) %>% 
  select(-c(`winning_reward`, `winning_probability`, `losing_reward`, `losing_probability`)) %>% 
  pivot_longer(cols = -c(draw, model_name), names_to = "comparison", values_to = "difference")

comp_feature_status_fixation_group <- samples_feature_status_fixation %>% 
  mutate(estimate = estimate * (sqrt(3)/pi)) %>% 
  select(-percent) %>% 
  pivot_wider(id_cols = c(draw, label), names_from = model_name, values_from = estimate) %>% 
  compare_strategies() %>% 
  pivot_longer(cols = -c(draw, label), names_to = "comparison", values_to = "difference")

rope_feature_status_fixation_diff <- rope_summary(
  comp_feature_status_fixation, c("model_name", "comparison")) 

rope_feature_status_fixation_group_diff <- rope_summary(
  comp_feature_status_fixation_group, c("comparison", "label")) 

hdi_feature_status_fixation <- hdi_intervals(
  samples_feature_status_fixation, c("model_name", "label"), "percent")

hdi_feature_status_fixation_diff <- hdi_intervals(
  comp_feature_status_fixation, c("model_name", "comparison"))

hdi_feature_status_fixation_group_diff <- hdi_intervals(
  comp_feature_status_fixation_group, c("comparison", "label"))

# Transitions -------------------------------------------------------------
# Group Effects

samples_transitions <- tidy_posterior(
  posterior = transition_fit, 
  effects   = effect_transition,
  formula   = formula_transition) %>% 
  rename("label" = `.category`) %>% 
  mutate(label = case_when(label == 1 ~ "between_pr_pr",
                           label == 2 ~ "between_pr_rw",
                           label == 3 ~ "between_rw_pr",
                           label == 4 ~ "between_rw_rw",
                           label == 5 ~ "within_pr_rw",
                           label == 6 ~ "within_rw_pr"))

# Individual estimates
individual_transitions <- tidy_posterior(
  posterior = transition_fit, 
  effects   = effect_ind_transition,
  formula   = formula_ind_transition) %>% 
  rename("label" = `.category`) %>% 
  mutate(label = case_when(label == 1 ~ "between_pr_pr",
                           label == 2 ~ "between_pr_rw",
                           label == 3 ~ "between_rw_pr",
                           label == 4 ~ "between_rw_rw",
                           label == 5 ~ "within_pr_rw",
                           label == 6 ~ "within_rw_pr")) %>% 
  
  group_by(id, model_name, label) %>% 
  summarise(percent  = median(percent),
            estimate = median(estimate),
            standard = median(standard)) %>% 
  ungroup() 

comp_transitions <- samples_transitions %>% 
  mutate(estimate = estimate * (sqrt(3)/pi)) %>% 
  select(-percent) %>% 
  pivot_wider(id_cols = c(draw, model_name), names_from = label, values_from = estimate) %>% 
  mutate(btprpr_btprrw = `between_pr_pr` - `between_pr_rw`,
         btprpr_btrwpr = `between_pr_pr` - `between_rw_pr`,
         btprpr_btrwrw = `between_pr_pr` - `between_rw_rw`,
         btprpr_wtprrw = `between_pr_pr` - `within_pr_rw`,
         btprpr_wtrwpr = `between_pr_pr` - `within_rw_pr`,
         btprrw_btrwpr = `between_pr_rw` - `between_rw_pr`,
         btprrw_btrwrw = `between_pr_rw` - `between_rw_rw`,
         btprrw_wtprrw = `between_pr_rw` - `within_pr_rw`,
         btprrw_wtrwpr = `between_pr_rw` - `within_rw_pr`,
         btrwpr_btrwrw = `between_rw_pr` - `between_rw_rw`,
         btrwpr_wtprrw = `between_rw_pr` - `within_pr_rw`,
         btrwpr_wtrwpr = `between_rw_pr` - `within_rw_pr`,
         btrwrw_wtprrw = `between_rw_rw` - `within_pr_rw`,
         btrwrw_wtrwpr = `between_rw_rw` - `within_rw_pr`,
         wtprrw_wtrwpr = `within_pr_rw` - `within_rw_pr`) %>% 
  select(-c(`between_pr_pr`, `between_pr_rw`, `between_rw_pr`, `between_rw_rw`, `within_pr_rw`, `within_rw_pr`)) %>% 
  pivot_longer(cols = -c(draw, model_name), names_to = "comparison", values_to = "difference")

comp_transitions_group <- samples_transitions %>% 
  mutate(estimate = estimate * (sqrt(3)/pi)) %>% 
  select(-percent) %>% 
  pivot_wider(id_cols = c(draw, label), names_from = model_name, values_from = estimate) %>% 
  compare_strategies() %>% 
  pivot_longer(cols = -c(draw, label), names_to = "comparison", values_to = "difference")


rope_transitions_diff <- rope_summary(
  comp_transitions, c("model_name", "comparison"))

rope_transitions_group_diff <- rope_summary(
  comp_transitions_group, c("comparison", "label")) 

hdi_transitions <- hdi_intervals(
  samples_transitions, c("model_name", "label"), "percent")

hdi_transitions_diff <- hdi_intervals(
  comp_transitions, c("model_name", "comparison"))

hdi_transitions_group_diff <- hdi_intervals(
  comp_transitions_group, c("comparison", "label"))


# Save data ---------------------------------------------------------------
save(individual_choice, 
     individual_feature_status_fixation,
     individual_transitions,
     
     file = file.path(path_summaries, "bayes_individual_medians.RData"))


save(comp_choice_group, 
     comp_feature_status_fixation_group, 
     comp_transitions_group,
     
     file = file.path(path_summaries, "bayes_comparisons.RData"))


save(rope_feature_status_fixation_diff,
     rope_transitions_diff,
  
     rope_choice_group_diff, 
     rope_feature_status_fixation_group_diff, 
     rope_transitions_group_diff,
     
     file = file.path(path_summaries, "bayes_rope.RData"))


save(hdi_choice,
     hdi_feature_status_fixation,
     hdi_transitions,
     
     hdi_feature_status_fixation_diff,
     hdi_transitions_diff,
     
     hdi_choice_group_diff,
     hdi_feature_status_fixation_group_diff,
     hdi_transitions_group_diff,
     
     file = file.path(path_summaries, "bayes_hdi.RData"))
