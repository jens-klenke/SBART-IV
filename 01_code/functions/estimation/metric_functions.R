### EVALUATION FUNCTIONS
bias_fun <- function(tau_true, tau_pred, ...) mean(tau_true - tau_pred)
abs_bias_fun <- function(tau_true, tau_pred, ...) mean(abs(tau_true - tau_pred))
PEHE_fun <- function(tau_true, tau_pred, ...) mean( (tau_true - tau_pred)^2)
MC_se_fun <- function(x, B) qt(0.975, B-1)*sd(x)/sqrt(B)

# confidence interal (width) and coverage (against compiler effect)
coverage_95_fun <- function(obj, cace_ef, ...){
  
  # estimation 
  est <- obj$coefficients[2, 'Estimate']
  # std error 
  std.error <- obj$coefficients[2, 'Std. Error']
  # degree of fredoom (n - p)
  df_iv <- obj$df[2]
  
  # confidence interval and distance
  low_ci <- est - std.error * qt(0.975, df_iv) 
  upper_ci <- est + std.error * qt(0.975, df_iv)
  width_ci <- upper_ci - low_ci
  
  # checking coverage
  cove <- low_ci < cace_ef & cace_ef < upper_ci
  
  # return_df
  df <- tibble::tibble(
    "coverage" = cove,
    "low_ci"   = low_ci,
    "upper_ci" = upper_ci,
    "width_ci" = width_ci)
  
  return(df)
}

check_fun <- function(results, ...){
  est_problem <- results %>%
    dplyr::select(est_problems) %>%
    dplyr::summarize(any_yes = any(est_problems == "yes", na.rm = TRUE)) %>%
    pull()
  return(est_problem)
}

# function
analysis_fun <- function(results, ...){
  
  # df
  df <- results %>%
    tibble::as_tibble() %>%
    dplyr::select(bcf_results, s_bcf_results) %>%
    tidyr::pivot_longer(cols = c('bcf_results', 's_bcf_results'), names_to = 'detect_model', values_to = 'results') %>%
    dplyr::mutate(methods = names(results)) %>% 
    dplyr::mutate(est_problems = purrr::pmap_lgl(., check_fun),
                  leaves_df = purrr::pmap(., leaf_df_fun)
    ) %>%
    dplyr::arrange(detect_model)
  
  df %<>%
    dplyr::mutate(purrr::pmap_df(., metric_fun_df))
  
  return(df)
  
}




# not in estimation ----
metric_fun_df <- function(leaves_df, ...) {
  leaves_df %>%
    dplyr::mutate(difference = tau_true - tau_pred,
                  sq_difference = (tau_true - tau_pred)^2) %>%
    dplyr::summarise(bias  = mean(difference),
                     bias_rm.na = mean(difference, na.rm = TRUE),
                     abs_bias  = mean(abs(difference)),
                     abs_bias_rm.na = mean(abs(difference), na.rm = TRUE),
                     PEHE  = mean(sq_difference),
                     PEHE_rm.na = mean(sq_difference, na.rm = TRUE),
                     coverage = mean(coverage)
    )
}

# collect all pred and true taus in leaves
leaf_df_fun <- function(results, ...){
  df <- results %>%
    dplyr::filter(leaves == 1) %>%
    dplyr::select(pred) %>%
    tidyr::unnest(pred)
  
  return(df)
}

# check if we FALSE -> estimation problems!
