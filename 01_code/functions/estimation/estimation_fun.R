# estimation function for the server
estimation_fun <- function(effects = c('0', '0.2', '0.4','0.6', '0.8', '1',
                                       '1.2', '1.4', '1.6', '1.8', '2'),
                           compliance = 0.75, corr_str = 'uncorr',
                           conf_str = 'unconf', N = 1e3, n_workers = 25, ... ){
  
  # parallel plan
  future::plan(multisession, workers = n_workers)
  
  # get define variables
  compliance_rate <- compliance
  corr <- ifelse(corr_str == 'corr', '_corr', '_uncorr')
  corr_f <- ifelse(corr_str == 'corr', '_correlated', '_uncorrelated')
  conf <- ifelse(conf_str == 'conf', '_conf', '')
  conf_f <- ifelse(conf_str == 'conf', '_confounded', '__')
  effects <- effects
  n_number <- glue::glue('n_{N}')
  
  # all necessary files
  files <- paste0('ef.', effects, '_co.', compliance_rate, '_baseline.ef', corr_f, conf_f)
  cases <- glue::glue('n.{N}_ef.{effects}_co.{compliance_rate}{corr}{conf}.RData')
  
  # save path
  save_path <- glue::glue("02_sim_results\\{n_number}")
  
  # 
  if(!dir.exists(save_path)){
    dir.create(save_path, recursive = TRUE)
  }

  # loop for
  for (i in seq_along(files)) {
    file <- files[i]
    case <- cases[i]
    
    # reading data
    data <- tibble::tibble(
      # path for loading data
      path_in = list.files(glue("00_sim_data\\{n_number}"), recursive = TRUE,
                           full.names = TRUE)) %>%
      dplyr::filter(str_detect(path_in, file)) %>%
      dplyr::mutate(ncov = readr::parse_number(stringr::str_extract(path_in, pattern = 'ncov.[0-9]*'), 
                                               locale =  readr::locale(decimal_mark = ",")))
    
    # estimation
    sim_results <- data %>%
      dplyr::mutate(furrr::future_pmap_dfr(., wrapper_function, 
                                           .progress = FALSE,
                                           .options = furrr_options(seed = TRUE)))
    
    
    # saving 
    save(sim_results, file = glue("{save_path}\\{case}"))
  }
  # ending parllel
  future::plan(sequential)
}
