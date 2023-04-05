library(tidyverse)
library(broom)

# Base directories --------------------------------------------------------
path_root    <- dirname(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)))
path_results <- file.path(path_root, "results", "datasets")
path_keys    <- file.path(path_root, "results", "keys")

load(file = file.path(path_results, "preproc_survey.RData"))
load(file.path(path_keys, "model_names.RData"))

model_key <- read.csv(file = file.path(path_keys, "model_classification.csv"), 
                      colClasses = c("numeric", "numeric")) %>% 
  mutate(model_name = factor(model, 
                             levels = 1:length(model_names), 
                             labels = model_names)) 

preproc_survey <- left_join(model_key, preproc_survey)

# DOSPERT -----------------------------------------------------------------
## DOSPER Subscales -------------------------------------------------------
dospert_ethical      <- c(6, 9, 10, 16, 29, 30)
dospert_health       <- c(5, 15, 17, 20, 23, 26)
dospert_recreational <- c(2, 11, 13, 19, 24, 25)
dospert_social       <- c(1, 7, 21, 22, 27, 28)

dospert_financial           <- c(12, 4, 18, 3, 14, 8)
dospert_financial_gamble    <- c(12, 4, 18)
dospert_financial_investing <- c(3, 14, 8)

## DOSPERT Scoring --------------------------------------------------------
dospert_data <- mutate(preproc_survey, id = as.integer(id)) %>% 
  filter(id > 1005 & id < 1200) %>% 
  select(c(id, gender, age, model, starts_with("d"))) %>% 
  pivot_longer(cols = starts_with("d"), 
               names_to = c("scale", "question"), 
               values_to = "rating", names_sep = "_") %>% 
  mutate(subscale = case_when(question %in% dospert_ethical      ~ "Ethical",
                              question %in% dospert_financial    ~ "Financial",
                              question %in% dospert_health       ~ "Health",
                              question %in% dospert_recreational ~ "Recreational",
                              question %in% dospert_social       ~ "Social")) %>% 
  
  mutate(special_items = case_when(question %in% dospert_financial_gamble    ~ "Gambling",
                                   question %in% dospert_financial_investing ~ "Investing")) %>% 
  filter(!is.na(rating)) %>% 
  group_by(id) %>% 
  mutate(count = n()) %>% 
  ungroup() %>% 
  filter(between(count, 80, 90)) %>%  #to remove incomplete questionnaires
  mutate(survey = "dospert")
  
dospert_subscale_scores <- dospert_data %>% 
  group_by(id, gender, age, model, survey, subscale) %>% 
  summarise(score      = sum(rating), 
            mean_score = mean(rating, na.rm = T)) %>% 
  ungroup() 

dospert_scale_scores <- dospert_data %>% 
  select(-c(subscale, special_items)) %>% 
  group_by(id, gender, age, model, survey, scale) %>% 
  summarise(score      = sum(rating),
            mean_score = mean(rating)) %>% 
  ungroup()

risk_attitude_data <- dospert_data %>% 
  pivot_wider(id_cols     = c("id", "gender", "age", "model", "question", "subscale"), 
              names_from  = "scale", 
              values_from = rating) %>% 
  group_by(id, gender, age, model) %>% 
  do(model = tidy(lm(drt ~ deb + drp, data = ., na.action = "na.omit"))) %>% 
  unnest(model) %>% 
  ungroup() %>% 
  mutate(term = str_remove_all(string = term, pattern = "\\(|\\)")) %>% 
  select(-c(std.error, statistic)) %>% 
  rename("scale" = term) %>% 
  mutate(coefficient = case_when(scale == "deb" ~ "a",
                                 scale == "drp" ~ "b",
                                 scale == "Intercept" ~ "c")) %>% 
  left_join(dospert_scale_scores) 

dospert_risk_attitude_groups <- risk_attitude_data %>% 
  filter(coefficient == "b") %>% 
  select(id, gender, age, model, survey, coefficient, estimate) %>% 
  mutate(behavior = case_when(sign(estimate) ==  1 ~ "Risk Seeking",
                              sign(estimate) == -1 ~ "Risk Averse")) 

dospert_risk_attitude_scores <- risk_attitude_data %>% 
  select(id, gender, age, model, coefficient, estimate, mean_score) %>% 
  pivot_wider(id_cols     = c(id, gender, age, model), 
              names_from  = coefficient, 
              values_from = c(estimate, mean_score)) %>% 
  mutate(risk_attitude = estimate_c + (estimate_a * mean_score_a) + (estimate_b * mean_score_b)) %>% 
  select(id, gender, age, model, score = risk_attitude) %>% 
  mutate(survey = "dospert")
  
# BIS/BAS -----------------------------------------------------------------
## BIS/BAS Subscales ------------------------------------------------------
bas_drv <- c(3, 9, 12, 21) #BAS Drive
bas_fun <- c(5, 10, 15, 20) #BAS Fun seeking
bas_rwd <- c(4, 7, 14, 18, 23) #BAS reward responsiveness
bis     <- c(2, 8, 13, 16, 19, 22, 24) #motivation to avoid aversive outcome

# BIS/BAS Scoring ---------------------------------------------------------
bisbas_reverse <- c(2, 22)

bisbas_data <- mutate(preproc_survey, id = as.integer(id)) %>% 
  filter(id > 1005 & id < 1200) %>% 
  select(c(id, gender, age, model, starts_with("bisbas"))) %>% 
  pivot_longer(cols      = starts_with("bisbas"), 
               names_to  = c("question"), 
               values_to = "rating", names_prefix = "bisbas_") %>% 
  mutate(subscale = case_when(question %in% bas_drv ~ "Drive",
                              question %in% bas_fun ~ "Fun seeking",
                              question %in% bas_rwd ~ "Reward responsiveness",
                              question %in% bis ~ "BIS"),
         survey = "bisbas") %>% 
  filter(!is.na(rating) & !is.na(question)) %>% 
  mutate(rating = ifelse(question %in% bisbas_reverse, #reverse scoring
                         (max(rating, na.rm = T) + 1) - rating, 
                         rating)) %>% 
  group_by(id) %>% 
  mutate(count = n()) %>% 
  ungroup() %>% 
  filter(between(count, 20, 24))


bisbas_scores <- bisbas_data %>% 
  group_by(id, gender, age, model, survey, subscale) %>% 
  summarise(score      = sum(rating), 
            mean_score = mean(rating)) %>% 
  ungroup()

# Barrat Impulsiveness (BIS-11) -------------------------------------------
## First Order Factors ----------------------------------------------------
bis11_attention  <- c(5, 9, 11, 20, 28)
bis11_cog_instab <- c(6, 24, 26)

bis11_motor    <- c(2, 3, 4, 17, 19, 22, 25)
bis11_persever <- c(16, 21, 23, 30)

bis11_self_ctrl <- c(1, 7, 8, 12, 13, 14)
bis11_cog_cmplx <- c(10, 15, 18, 27, 29)

## 2nd Order Factors ------------------------------------------------------
bis11_attentional  <- c(bis11_attention, bis11_cog_instab)
bis11_motor_grl    <- c(bis11_motor, bis11_persever)
bis11_non_planning <- c(bis11_self_ctrl, bis11_cog_cmplx)

## BIS-11 Scoring ---------------------------------------------------------
bis11_reverse <- c(9, 20, 30, 1, 7, 8, 12, 13, 10, 15, 29)

bis11_data <- mutate(preproc_survey, id = as.integer(id)) %>% 
  filter(id > 1005 & id < 1200) %>% 
  select(c(id, gender, age, model, starts_with("bis_"))) %>% 
  pivot_longer(cols         = starts_with("bis_"), 
               names_to     = c("question"), 
               values_to    = "rating", 
               names_prefix = "bis_") %>% 
  mutate(first_factor = 
           case_when(question %in% bis11_attention  ~ "Attention",
                     question %in% bis11_cog_instab ~ "Cognitive instability",
                     question %in% bis11_motor      ~ "Motor",
                     question %in% bis11_persever   ~ "Perseverance",
                     question %in% bis11_self_ctrl  ~ "Self-control",
                     question %in% bis11_cog_cmplx  ~ "Cognitive complexity")) %>% 
  mutate(second_factor = 
           case_when(question %in% bis11_attentional  ~ "Attention",
                     question %in% bis11_motor_grl    ~ "Motor",
                     question %in% bis11_non_planning ~ "Nonplanning")) %>% 
  filter(!is.na(rating)) %>% 
  mutate(rating = ifelse(question %in% bis11_reverse, #reverse scoring
                         (max(rating, na.rm = T) + 1) - rating, 
                         rating),
         survey = "bis11") %>% 
  group_by(id) %>% 
  mutate(count = n()) %>% 
  ungroup() %>% 
  filter(between(count, 25, 30))

bis11_1st_factor_scores <- bis11_data %>%
  group_by(id, gender, age, model, survey, first_factor) %>% 
  summarise(score      = sum(rating),
            mean_score = mean(rating)) %>% 
  ungroup()

bis11_2nd_factor_scores <- bis11_data %>%
  group_by(id, gender, age, model, survey, second_factor) %>% 
  summarise(score      = sum(rating),
            mean_score = mean(rating)) %>% 
  ungroup()

# Save scores -------------------------------------------------------------
save(dospert_subscale_scores,
     dospert_scale_scores,
     dospert_risk_attitude_groups,
     dospert_risk_attitude_scores,
     bisbas_scores,
     bis11_1st_factor_scores,
     bis11_2nd_factor_scores,
     file = file.path(path_keys, "personality_scores.RData"))
