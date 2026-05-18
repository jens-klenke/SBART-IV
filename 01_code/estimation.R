# load packages 
############## Packages ################
source(here::here('01_code/packages.R'))

# source all files in the functions folder
invisible(sapply(
    list.files(
      here::here('01_code/functions/estimation'),
      full.names = TRUE),
    source))

# estimation -----
estimation_fun(compliance = 0.75, corr_str = 'uncorr', n_workers = 25)
