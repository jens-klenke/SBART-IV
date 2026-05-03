# wrapper_post_processing <- function(path_in, bcf_iv, sbcf_iv, ...){
#   
#   # getting the data path on current machine
#   data <- readRDS(paste0(sim_data_path(verbose = FALSE), "\\n_1000\\", 
#                          sub(".*effect_", "effect_", path_in)))
#   
#   # getting the IV Data for step 3
#   iv.data <- as.data.frame(cbind(data$y, data$w, data$z, data$X))
#   names(iv.data) <- c("y", "w", "z", paste0('x', 1:ncol(data$X)))
#   
#   # re estimating step 3 for bcf_iv
#   bcf_iv <- post_processing(iv.data, bcf_iv)
#   
#   # re estimating step 3 for sbcf_iv
#   sbcf_iv <- post_processing(iv.data, sbcf_iv)
#   
#   df <- tibble::tibble(
#     bcf_iv = list(bcf_iv),
#     sbcf_iv = list(sbcf_iv)
#   )
#   
#   return(df)
# }

post_processing <- function(path_in, results, ...){
  # getting the data
  data <- readRDS(paste0(sim_data_path(verbose = FALSE), "\\n_1000\\", 
                         sub(".*effect_", "effect_", {{path_in}})))
  # IV Data
  iv.data <- as.data.frame(cbind(data$y, data$w, data$z, data$X))
  names(iv.data) <- c("y", "w", "z", paste0('x', 1:ncol(data$X)))
  
  # getting iv results
  ivResults <- results %>%
    dplyr::select(ivResults) %>%
    tidyr::unnest(ivResults) %>%
    dplyr::mutate(iv.data = list(iv.data)) %>%
    dplyr::select(-c(TP, FN, FP, TN))
  
  # IV Estimation for each subgroup
  ivResults %<>%
    # getting new 
    dplyr::mutate(purrr::pmap_df(., iv.estimation_fun_post_processing)) %>%
    # deleting data
    dplyr::select(-iv.data) %>%
    # changing node groups
    dplyr::mutate(node_subgroup = dplyr::case_when(
      node_subgroup == 'neg' ~ 'l2',
      node_subgroup == 'pos' ~ 'l1', 
      .default = node_subgroup)
      ) %>%
    # determining subgroup detection
    dplyr::mutate(
      # Detection and False Detection on subgroup level
      rule_det = ifelse(node_subgroup %in% c('l2', 'l1') & leaves, 1, 0),
      rule_det_l1 = ifelse(node_subgroup %in% c('l1') & leaves, 1, 0),
      rule_det_l2 = ifelse(node_subgroup %in% c('l2') & leaves, 1, 0),
      rule_false_det = ifelse(!node_subgroup %in% c('l2', 'l1') & significant & leaves,  1, 0)) %>%
    tidyr::replace_na(list(rule_false_det = 0))

  # clean ivResults
  # rules results ----
  rule_results <- ivResults %>%
    # filter for leaves
    dplyr::filter(leaves == TRUE) %>%
    dplyr::summarise(
      # number of subgroups
      n_leave = sum(leaves),
      # Detected subgroups
      rule_det = sum(rule_det),
      # Detected subgroup l1
      rule_det_l1 = sum(rule_det_l1),
      # Detected subgroup l2
      rule_det_l2 = sum(rule_det_l2),
      # Indicator function if a false subgroup has a significant effect
      rule_false_det = sum(rule_false_det) > 0
    )
  
  # Individual results in leaves ----
  individual_results <- ivResults %>%
    # filter for leaves
    dplyr::filter(leaves == TRUE) %>%
    dplyr::select(-c(node_abs_bias, rule_det, rule_det_l1, rule_det_l2, rule_false_det)) %>%
    tidyr::unnest(pred)
  
  # determine TP, FN, FP, TN
  individual_results %<>%
    dplyr::mutate(
      # True Positive but TP is determined based on x1 and x2, not the rule itself
      TP = dplyr::case_when(
        # negatively affected cases
        x1 == 1 & x2 == 1 & node_subgroup %in% c('l1', 'l2') & significant ~ 1,
        # positive affected cases
        x1 == 0 & x2 == 0 & node_subgroup %in% c('l1', 'l2') & significant ~ 1,
        .default = 0
      ),
      # False Negative but
      FN = dplyr::case_when(
        # Does that cover all? What if the Effect is in the other direction? 
        # negatively affected cases
        x1 == 1 & x2 == 1 & !significant ~ 1,
        # positive affected cases
        x1 == 0 & x2 == 0 & !significant ~ 1,
        # negatively affected cases
        x1 == 1 & x2 == 1 & !node_subgroup %in% c('l1', 'l2') & significant ~ 1,
        # positive affected cases
        x1 == 0 & x2 == 0 & !node_subgroup %in% c('l1', 'l2') & significant ~ 1,
        # estimation Problem
        c(x1 == 1 & x2 == 1 | x1 == 0 & x2 == 0) & is.na(CCACE) ~ 1, 
        .default = 0
      ),
      # False Positve but FP is determined based on x1 and x2, not the rule itself
      FP = dplyr::case_when(
        !c(x1 == 1 & x2 == 1 | x1 == 0 & x2 == 0) & significant ~ 1,
        .default = 0
      ),
      # TRUE Negative but TN is determined based on x1 and x2, not the rule itself
      TN = dplyr::case_when(
        !c(x1 == 1 & x2 == 1 | x1 == 0 & x2 == 0) & !significant ~ 1,
        !c(x1 == 1 & x2 == 1 | x1 == 0 & x2 == 0) & is.na(CCACE) ~ 1,
        .default = 0
      )
    )

    # collecting results
  results <- tibble::tibble(
    'ivResults' = list(ivResults),
    'rule_results' = list(rule_results),
    'individual_results' = list(individual_results)
  )
  
  return(results)
}

iv.estimation_fun_post_processing <- function(CCACE, pred, iv.data, ...){
  
  if(!is.na(CCACE)){
  
  pred %<>%
    dplyr::mutate(real_subgroup = 
                    dplyr::case_when(
                      real_subgroup == 'neg' ~ 'l2',
                      real_subgroup == 'pos' ~ 'l1', 
                      .default = real_subgroup)
                  )
  
  # index | subgroup
  index <- pred %>%
    dplyr::pull(index)
  
  # subset
  subset <- iv.data[index, ]
  
  # estimating IV model
  iv.reg <- ivreg(y ~ w | z, data = subset)
  
  # IV estimation summary
  iv.summary <- summary(iv.reg, diagnostics = TRUE)
  
  df <- coverage_95_fun_post_processing(iv.summary, pred)
  
  }
  
  if(is.na(CCACE)){
    
    df <- tibble::tibble(
      'std_error' = NA,
      'conf_low' = NA,
      'conf_upper' = NA,
      'conf_width' = NA,
      'pred' = list(pred),
      'node_coverage' = NA
    )
  }
  
  return(df)
}

coverage_95_fun_post_processing <- function(obj, pred, ...){
  
  # estimate
  est <- obj$coefficients[2, 'Estimate']
  # std error
  std.error <- obj$coefficients[2, 'Std. Error']
  # degree of fredoom
  df_iv <- obj$df[2]
  
  pred %<>%
    dplyr::mutate(
      # confidence interval
      low_ci = est - std.error * qt(0.975, df_iv),
      upper_ci = est + std.error * qt(0.975, df_iv),
      width_ci = upper_ci - (low_ci)) %>%
    dplyr::mutate(
      # checking coverage
      coverage = low_ci < cace_ef & cace_ef < upper_ci)
  
  cove <- pred %>%
    dplyr::summarise(coverage = mean(coverage),
                     low_ci = mean(low_ci),
                     upper_ci = mean(upper_ci),
                     width_ci = mean(width_ci))
  
  summary_vec <- tibble::tibble(
    'std_error' = std.error,
    'conf_low' = cove$low_ci,
    'conf_upper' = cove$upper_ci,
    'conf_width' = cove$width_ci,
    'pred' = list(pred),
    'node_coverage' = cove$coverage
  )
  return(summary_vec)
}
