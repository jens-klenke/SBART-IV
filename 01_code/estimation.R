# load packages 
############## Packages ################
source(here::here('01_code/packages.R'))

# source all files in the functions folder
invisible(
  sapply(
    list.files(
      here::here('01_code/functions/estimation'),
      full.names = TRUE),
    source))

# parallel plan
options(future.globals.maxSize = 2147483648) # 2GB
future::plan(multisession, workers = floor(parallel::detectCores()* 0.5))

# 
# path to folder of simulated data
sim_data_path <- "00_sim_data" # use

# identification function to filter sim_data?
# avoiding saving?

# preparing  ----
data <- tibble::tibble(
    # path for loading data
    path_in = list.files(sim_data_path, recursive = TRUE, full.names = TRUE)
  ) %>%
    # filter for desired setting
    dplyr::filter(str_detect(path_in, 'ef.0.4_co.0.75_baseline.ef_uncorrelated__')) %>%
    dplyr::mutate(ncov = readr::parse_number(stringr::str_extract(
      path_in,
      pattern = 'ncov.[0-9]*'),
      locale = readr::locale(decimal_mark = ","))) %>%
  dplyr::mutate(row_num =
                  glue::glue("{dplyr::row_number(.)} of {max(dplyr::row_number(.))}"))

# tries
data %<>%
  dplyr::slice_sample(n = 4)

#### Estimation ----
sim_results <- data %>%
  dplyr::mutate(furrr::future_pmap_dfr(., wrapper_function, 
                                       .progress = TRUE,
                                       .options = furrr_options(seed = TRUE)))

# save(sim_results, file = "03_output\\ef.0.4_co.0.75_corr.RData")

# clean up ---
unlink(tempdir(), recursive = TRUE, force = TRUE)
base::unlink(here::here('temp_results/*'))
unlink(dirname(tempdir()), recursive = TRUE, force = TRUE)

