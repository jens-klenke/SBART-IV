# load packages 
############## Packages ################
source(here::here('01_code/packages.R'))

# source all files in the functions folder
invisible(
  sapply(
    list.files(
      here::here('01_code/functions'),
      full.names = TRUE, 
      recursive = TRUE),
    source))


# running summarize function
MCMC_summarise("02_sim_results", "corr", "co.0.5")
