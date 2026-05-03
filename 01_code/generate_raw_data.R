# load packages 
############## Packages ################
source(here::here('01_code/packages.R'))

# source functions for data generation
invisible(sapply(list.files(here::here('01_code/functions/data_generation'),
                            full.names = TRUE), source))

wrapper_data_generation(
  '00_sim_data', # subfolder to store the data
  n = 2000, # Number of obserations
  p_vec = c(10, 50, 100), # Vector with number of covariates
  covariates = 'cont-cov',
  uncorrelated = TRUE, # TRUE/FALSE for uncorrleated 
  effect_size_vec = seq(0, 2, .2), # vector for effect sizes
  baseline_effect = TRUE, # baseline effect
  compliance = 0.75, # compliance rate 
  confounded = FALSE) # confounded structure

