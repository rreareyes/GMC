# STATISTICAL MODELS
 
# All models estimated random effects for each participant, nested within 
# groups. Each model was fitted using 4 chains with enough iterations to 
# reach 10,000 ESS in the group level random parameters, in order to have 
# reliable estimation of the standard deviation of the posterior. 


# Load libraries ----------------------------------------------------------
library(rstan)
library(brms) 
library(scales)

# Base directories --------------------------------------------------------
path_root    <- dirname(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)))
path_results <- file.path(path_root, "results", "datasets")
path_fits    <- file.path(path_root, "results", "fits")

# Load base datasets ------------------------------------------------------
load(file.path(path_results, "analysis_data.RData"))

# Setup -------------------------------------------------------------------
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

# Define priors -----------------------------------------------------------
priors_binomial <- c(
  
  prior(normal(0, 2), class = b),
  
  prior(normal(0, 2), class = Intercept),
  
  prior(lkj(2), class = cor),
  
  prior(normal(0, 2), class = sd)
  
)

prior_time <- c(
  
  prior(normal(0, 1), class = Intercept),
  
  prior(normal(0, 1), class = b),
  
  prior(normal(0, 1), class = sd),
  
  prior(normal(-2, 0.75), class = Intercept, dpar = ndt),
  
  prior(normal(0, 0.5), class = sd, dpar = ndt),
  
  prior(normal(-2, 0.75), class = Intercept, dpar = sigma),
  
  prior(normal(0, 0.5), class = sd, dpar = sigma)
  
)

priors_transitions <- c(
  
  prior(normal(0, 2), class = b, dpar = mu2),
  prior(normal(0, 2), class = b, dpar = mu3),
  prior(normal(0, 2), class = b, dpar = mu4),
  prior(normal(0, 2), class = b, dpar = mu5),
  prior(normal(0, 2), class = b, dpar = mu6),
  
  prior(normal(0, 2), class = Intercept, dpar = mu2),
  prior(normal(0, 2), class = Intercept, dpar = mu3),
  prior(normal(0, 2), class = Intercept, dpar = mu4),
  prior(normal(0, 2), class = Intercept, dpar = mu5),
  prior(normal(0, 2), class = Intercept, dpar = mu6),
  
  prior(normal(0, 2), class = sd, dpar = mu2),
  prior(normal(0, 2), class = sd, dpar = mu3),
  prior(normal(0, 2), class = sd, dpar = mu4),
  prior(normal(0, 2), class = sd, dpar = mu5),
  prior(normal(0, 2), class = sd, dpar = mu6)
  
)

priors_feature_status_fixations <- c(
  
  prior(normal(0, 2), class = b, dpar = mu2),
  prior(normal(0, 2), class = b, dpar = mu3),
  prior(normal(0, 2), class = b, dpar = mu4),
  
  prior(normal(0, 2), class = Intercept, dpar = mu2),
  prior(normal(0, 2), class = Intercept, dpar = mu3),
  prior(normal(0, 2), class = Intercept, dpar = mu4),
  
  prior(normal(0, 2), class = sd, dpar = mu2),
  prior(normal(0, 2), class = sd, dpar = mu3),
  prior(normal(0, 2), class = sd, dpar = mu4)
  
)


# Choices -----------------------------------------------------------------
choice1.0 <- brm(
  data    = choice_counts,
  family  = binomial,
  formula = correct | trials(total) ~ 1 + model_name + (1 + model_name | id),
  prior   = priors_binomial,
  iter    = 10000,
  warmup  = 5000,
  chains  = 4,
  cores   = 4,
  control = list(adapt_delta = 0.99),
  file    = file.path(path_fits, "choice1.0")
)


# Response time -----------------------------------------------------------
# init_ndt <- list(list(Intercept_ndt = -5),
#                  list(Intercept_ndt = -5),
#                  list(Intercept_ndt = -5),
#                  list(Intercept_ndt = -5))
# 
# time1.0 <- brm(
#   data    = choice_time,
#   family  = shifted_lognormal,
#   formula = bf(rt ~ 1 + model_name + (1 | id), 
#                sigma ~ (1 | model_name),
#                ndt ~ (1 | model_name)), 
#   prior   = prior_time, 
#   iter    = 10000,
#   warmup  = 5000,
#   init    = init_ndt,
#   chains  = 4, 
#   cores   = 4,  
#   control = list(adapt_delta = 0.95, max_treedepth = 11), 
#   file    = file.path(path_fits, "time1.0")
# )


# Transitions -------------------------------------------------------------

transitions1.0 <- brm(
  data    = transition_counts,
  family  = multinomial(refcat = 1, 
                        link   = logit),
  formula = y | trials(total) ~ 1 + model_name + (1 | id), 
  prior   = priors_transitions,
  iter    = 10000,
  warmup  = 5000,
  chains  = 4, 
  cores   = 4,  
  #control = list(adapt_delta = 0.95), 
  file    = file.path(path_fits, "transitions1.0") 
)


# Fixations ---------------------------------------------------------------
feature_status_fixations1.0 <- brm(
  data    = feature_status_fixation_counts,
  family  = multinomial(refcat = 1, 
                        link   = logit),
  formula = y | trials(total) ~ 1 + model_name + (1 | id), 
  prior   = priors_feature_status_fixations,
  iter    = 10000,
  warmup  = 5000,
  chains  = 4, 
  cores   = 4,  
  #control = list(adapt_delta = 0.95), 
  file    = file.path(path_fits, "feature_status_fixations1.0") 
)


process_tracing1.0 <- brm(
  data = mouse_eye_tracing,
  family = shifted_lognormal,
  formula = bf(speed ~ 1 + model_name * status_label * feature_label + (1 | id),
               sigma ~ (1 | model_name * status_label * feature_label),
               ndt ~ (1 | model_name)),
  
  
)