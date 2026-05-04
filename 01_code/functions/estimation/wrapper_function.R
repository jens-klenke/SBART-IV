wrapper_function <- function(path_in, row_num, ...){
  data <- readRDS(path_in)
  # renaming?
  base::return(run_mcmc(data$y, data$w, data$z, data$X, data$tau_true, data$w1, data$w0))
}
