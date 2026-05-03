#'
#'
wrapper_data_generation <- function(
    path, H = 500, n = 1000, p_vec, covariates, uncorrelated, effect_size_vec,
    baseline_effect, compliance = 0.75, confounded, ..) {
  
  # function head 
  baseline <- ifelse(baseline_effect, 'baseline.ef', 'no.baseline.ef')
  uncorrelated_data <- ifelse(uncorrelated, 'uncorrelated', 'correlated')
  conf  <- ifelse(confounded, 'confounded', '')
  
  
  for (j in seq_along(effect_size_vec)) {
    
    # loop over effect sizes
    effect_size <- effect_size_vec[j]
    
    # start finished data for effect size
    cat(glue::glue("Starting data simulation for effect size {effect_size}. \n"))
  
    for (i in seq_along(p_vec)) {
      # get number of covvariates  
      p <- p_vec[i]
      
      # folder path 
      folder_path <- paste0(path,
                            '\\effect_', effect_size,
                            '\\compliance.', compliance, '\\',
                            uncorrelated_data, '\\',
                            baseline, conf, '\\',
                            'ncov.', p)
      
      # name of the dataset
      data_name <- glue::glue("ef.{effect_size}_co.{compliance}_{baseline}_{uncorrelated_data}_{conf}_ncov.{p}")
      
      if(!dir.exists(folder_path)){
        dir.create(folder_path, recursive = TRUE)
      } 
      
      ### generate data
      for (j in 1:H){
        generate_dataset(n = n, p = p, covariates = covariates, base_line_effect = baseline_effect, uncorrelated = uncorrelated,
                         effect_size = effect_size, confounded = confounded) %>%
          saveRDS(file = paste0(folder_path, '/', data_name, '_', j, '.rds'))
        # printing
        if(j %% 250 == 0)
          cat(glue::glue("\n {j} of {H} Dataset with {p} covariates finished. \n"))
      }
    }
    # finished data for effect size
    cat(glue::glue("\n Finished dataset for effect size {effect_size}. \n
                   ------------------------------------------------ \n\n\n"))
  }
}
