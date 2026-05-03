# used insite BCF_IV-own estimation
heterogeneous_treatment_estimation <- function(
    fit.tree, inference, adj_method, pred_df, ...){
  
  # rules end terminal nodes
  rules <- as.numeric(row.names(fit.tree$frame[fit.tree$numresp]))
  
  # Initialize Outputs # NEW
  # Initialize Outputs # NEW
  bcfivMat <- tibble::tibble(
    "node" = rep(NA_character_, length(rules)),
    "est_problems" = rep(NA_character_, length(rules)),
    "CCACE" = rep(NA_real_, length(rules)),
    "std_error" = rep(NA_real_, length(rules)),
    "conf_low" = rep(NA_real_, length(rules)),
    "conf_upper" = rep(NA_real_, length(rules)),
    "conf_width" = rep(NA_real_, length(rules)),
    "pvalue" = rep(NA_real_, length(rules)),
    "Weak_IV_test" = rep(NA_real_, length(rules)),
    "Pi_obs" = rep(NA_real_, length(rules)),
    "ITT" = rep(NA_real_, length(rules)),
    "Pi_compliers" = rep(NA_real_, length(rules)),
    "pred" = rep(NA, length(rules)),
    "node_coverage" = rep(NA, length(rules)),
    "node_pehe" = rep(NA, length(rules)),
    "node_bias" = rep(NA, length(rules)),
    "node_abs_bias" = rep(NA, length(rules))
  )
  
  # Generate Leaves (end notes) Indicator
  lvs <- leaves <- numeric(length(rules)) 
  lvs[unique(fit.tree$where)] <- 1
  leaves[rules[lvs==1]] <- 1
  
  ####  Step 4: Run an IV Regression on each Node   ####
  
  # Run an IV Regression on the Root
  iv.root <- ivreg(y ~ w | z,  
                   data = inference) # inference dataset
  
  # Store Results for Root
  bcfivMat[1, ] <- iv_summary_func(iv.root, inference, inference, 
                                   sub_pop = 'root', pred_df)

  # delete root estimations 
  rm(iv.root)
  
  # Initialize New Data
  names(inference) <- paste(names(inference), sep="")
  
  # Run a loop to get the rules (sub-populations)
  for (i in rules[-1]){
    # Create a Vector to Store all the Dimensions of a Rule
    sub <- as.data.frame(matrix(NA, nrow = 1,
                                ncol = nrow(as.data.frame(
                                  path.rpart(fit.tree, node = i, print.it = FALSE)
                                  )
                                  )-1)
                         )
    
    quiet(capture.output(for (j in 1:ncol(sub)){
      # Store each Rule as a Sub-population
      sub[,j] <- as.character(
        print(
          as.data.frame(
            path.rpart(fit.tree, node = i, print.it = FALSE))[j+1,1]
          )
        )
      # combine rule to one path 
      sub_pop <- noquote(paste(sub , collapse = " & "))
    }))
    
    # get subset 
    subset <- with(inference, inference[which(eval(parse(text = sub_pop))),])
    
    pred_subset <- pred_df %>%
      dplyr::filter(index %in% as.numeric(row.names(subset))) # get the right taus
    
    # Run the IV Regression
    if (length(unique(subset$w))!= 1 & length(unique(subset$z))!= 1 & nrow(subset) >2){
      
      # freq
      iv.reg <- ivreg(y ~ w | z, data = subset)
      
      #### Step 5: Output the Values of each CCACE ####
      bcfivMat[i,] <- iv_summary_func(iv.reg, subset, inference, sub_pop, pred_subset)
    }
    
    if (!(length(unique(subset$w))!= 1 & length(unique(subset$z))!= 1 & nrow(subset) >2)){
      print('estimation problem')
      bcfivMat[i,] <- tibble::tibble(
        "node" = as.character(sub_pop),
        "est_problems" = 'yes',
        "CCACE" = NA_real_,
        "std_error" = NA_real_,
        "conf_low" = NA_real_,
        "conf_upper" = NA_real_,
        "conf_width" = NA_real_,
        "pvalue" = NA_real_,
        "Weak_IV_test" = NA_real_,
        "Pi_obs" = NA_real_,
        "ITT" = NA_real_,
        "Pi_compliers" = NA_real_,
        "pred" =  list(pred_subset),
        "node_coverage" = NA_real_,
        "node_pehe" = NA_real_,
        "node_bias" = NA_real_,
        "node_abs_bias" = NA_real_)
      }
    
    # detect and delete data and models
    rm(list = ls()[ls() %in% c('subset', 'iv.reg')])
  }
  
  # Adjust P.values ----
  bcfiv_correction <- bcfivMat %>%
    dplyr::mutate(leaves = leaves)
  
  adj <-stats::p.adjust(as.numeric(bcfiv_correction$pvalue[which(bcfiv_correction$leaves==1)]),
                        paste(adj_method))
  
  Adj_pvalue <- rep(NA, length(rules))
  Adj_pvalue[which(bcfiv_correction$leaves==1)] <- adj
  
  # Store Results
  ivResults <- bcfivMat %>%
    dplyr::mutate('leaves' = leaves,
                  'adj_pvalue' = Adj_pvalue)

  ## Add overall metrics (only leaves) ----
  ivResults %<>%
    dplyr::mutate(node_subgroup = case_when(
      node == 'x1>=0.5 & x2>=0.5' | node == 'x2>=0.5 & x1>=0.5' | node == 'x2> 0.5 & x1> 0.5' | node == 'x1> 0.5 & x2> 0.5' ~ 'l2',
      node == 'x2< 0.5 & x1< 0.5' | node == 'x1< 0.5 & x2< 0.5' | node == 'x2<=0.5 & x1<=0.5' | node == 'x1<=0.5 & x2<=0.5' ~ 'l1')
    ) %>%
    dplyr::mutate(significant = adj_pvalue < 0.05) %>%
#    dplyr::select(-c(node_coverage, node_pehe, node_bias, Weak_IV_test, est_problems)) %>%
    dplyr::mutate(
      # Detection and False Detection on subgroup level
      rule_det = ifelse(node_subgroup %in% c('l2', 'l1') & leaves, 1, 0),
      rule_det_l1 = ifelse(node_subgroup %in% c('l1') & leaves, 1, 0),
      rule_det_l2 = ifelse(node_subgroup %in% c('l2') & leaves, 1, 0),
      rule_false_det = ifelse(!node_subgroup %in% c('l2', 'l1') & significant & leaves,  1, 0))
  
  # Rules results ----
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
      # Indicator function if a false subgroup has a positive coefficient
      rule_false_det = sum(rule_false_det) > 0
    )
  
  # Individual results in leaves ----
  individual_results <- ivResults %>%
    # filter for leaves
    dplyr::filter(leaves == TRUE) %>%
    dplyr::select(-c(node_abs_bias, rule_det, rule_false_det)) %>%
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
  
  # summaries
  individual_results %<>%
    dplyr::summarise(
      TP = sum(TP),
      FN = sum(FN),
      FP = sum(FP),
      TN = sum(TN),
      TPR = sum(TP) / sum(c(TP, FN)),
      FNR = sum(FN) / sum(c(TP, FN)),
      FPR = sum(FP) / sum(c(FP, TN)),
      TNR = sum(TN) / sum(c(FP, TN)),
      Recall = TP / (TP + TN),
      Precision = TP / (TP + FP),
      # harmnoic mean of precision and recall
      F_score = TP / (TP + 0.5 * (FP + FN)),
      PEHE = mean((tau_true - cace_ef)^2),
      bias = mean((tau_true - cace_ef)),
      abs_bias = mean(abs(tau_true - cace_ef)),
      PEHE_rm = mean((tau_true - cace_ef)^2, na.rm = TRUE),
      bias_rm = mean((tau_true - cace_ef), na.rm = TRUE),
      abs_bias_rm = mean(abs(tau_true - cace_ef), na.rm = TRUE)
    )
 
  #### Return Results ####
  return(
    tibble::tibble(
      'ivResults' = list(ivResults), 
      'rule_results' = list(rule_results),
      'individual_results' = list(individual_results)
    )
  )
}

