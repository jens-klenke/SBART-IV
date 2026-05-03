iv_summary_func <- function(obj, subset, inference, sub_pop, pred_df, ...){
  
  proportion <- nrow(subset)/nrow(inference)
  compliers <- length(which(subset$z==subset$w))/nrow(inference)

  summary <- summary(obj, diagnostics = TRUE)
  iv.effect <-  summary$coef[2,1]
  p.value <- summary$coef[2,4]
  std.error <- summary$coefficients[2, 'Std. Error']
  p.value.weak.iv <- summary$diagnostics[1,4]
  itt <- iv.effect*compliers
  
  # Adding prediction & confidence interval metrics
  pred_df %<>%
    dplyr::mutate(tau_pred = iv.effect,
                  coverage_95_fun(summary, .$cace_ef))
  
  # node coverage 
  cove <- pred_df %>%
    dplyr::summarise(coverage = mean(coverage),
                     low_ci = mean(low_ci),
                     upper_ci = mean(upper_ci),
                     width_ci = mean(width_ci))
  
  # Store Results
  summary_vec <- tibble::tibble(
    'node' = as.character(sub_pop), 
    'est_problems' = 'no',
    'CCACE' = iv.effect,
    'std_error' = std.error,
    'conf_low' = cove$low_ci,
    'conf_upper' = cove$upper_ci,
    'conf_width' = cove$width_ci,
    'pvalue' = p.value,
    'Weak_IV_test' = p.value.weak.iv,
    'Pi_obs' = proportion, 
    'ITT' = itt,
    'Pi_compliers' = compliers, 
    'pred_df' = list(pred_df),
    'node_coverage' = cove$coverage
  )
  
  # compute metrics
  summary_vec$node_pehe <- PEHE_fun(pred_df$tau_pred, pred_df$cace_ef)
  summary_vec$node_bias <- bias_fun(pred_df$tau_pred, pred_df$cace_ef)
  summary_vec$node_abs_bias <- abs_bias_fun(pred_df$tau_pred, pred_df$cace_ef)
  
  # return summary vector
  return(summary_vec)
  
}
