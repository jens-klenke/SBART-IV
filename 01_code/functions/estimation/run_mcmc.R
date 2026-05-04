#'
#'
#'
#'
run_mcmc <- function(y, w, z, x, tau_true, w1, w0, binary = FALSE, n_burn = 3000, n_sim = 7000,
                       inference_ratio = 0.5, max_depth = 2, cp = 0.01,
                       minsplit = 30, adj_method = "holm", seed = 42, ...){
  
  ######################################################
  ####         Step 0: Initialize the Data          ####
  ######################################################
  
  # Split data into Discovery and Inference
  set.seed(seed)
  index <- sample(nrow(x), nrow(x)*inference_ratio, replace=FALSE)
  
  # Initialize total dataset
  iv.data <- as.data.frame(cbind(y, w, z, x))
  names(iv.data) <- c("y", "w", "z", paste0('x', 1:ncol(x))) # names for covariates
  
  # Discovery and Inference Samples
  discovery <- iv.data[-index,]
  # 'names not fitting' # binary tree starts with V2 and not V4 in covariates
  inference <- iv.data[index,] 
  
  # saving to a dataframe? 
  pred_df <- tibble::tibble(
    index = index,
    tau_true = tau_true[index],
    y = y[index],
    z = z[index],
    w1 = w1[index],
    x1 = x[index, 1],
    x2 = x[index, 2],
    tau_pred = NA_real_) %>%
    dplyr::mutate(
      # compute effect if they would compli with the treatment
      cace_ef = dplyr::case_when(
        x1 == 0 & x2 == 0 ~ max(tau_true),
        x1 == 1 & x2 == 1 ~ max(tau_true)*-1,
        .default = 0),
      real_subgroup = dplyr::case_when(
        x1 == 0 & x2 == 0 ~ 'l2',
        x1 == 1 & x2 == 1 ~ 'l1',
        .default = NA_character_)
    )
  
  # print('Step 0 completed')
  
  ######################################################
  ####  Step 1: Compute the Bayesian Causal Forest  ####
  ######################################################
  
  # Compute the Propensity Score though a Logistic Regression
  p.score <- glm(z ~ x[-index,],
                 family = binomial,
                 data = discovery)
  
  # changed to get the probabilities
  pihat <- predict(p.score, as.data.frame(x[-index,]), type = 'response')
  
  # Perform the Bayesian Causal Forest  to calculate the Proportion of Compliers (pic)
  pic_bcf_tree <- quiet(bartCause::bartc(w[-index], z[-index], x[-index,],
                                         n.samples = n_sim, n.burn = n_burn, 
                                         n.chains = 2L))
  
  tau_bcf_pic <- bartCause::extract(pic_bcf_tree, type = "ite")
  pic_bcf <- apply(tau_bcf_pic, 2, mean)
  
  # non-overlap
  pic_bcf_org <- pic_bcf == 0
  pic_bcf[pic_bcf == 0] <- 1e-6
  
  ## new PIC 
  # prepare dataset
  dat_test <- data.frame(w=factor(w[-index]), z=z[-index], x[-index,])
  
  # fit BART to the observed data (W given Z and X) 
  soft_test <- SoftBart::softbart_probit(w ~. , data=dat_test, test_data = dat_test,
                                         opts = Opts(num_burn = 2500, num_save = 1250), verbose = FALSE)
  
  # for E[W (1) | X]
  val_covs_1 <- data.frame(w=factor(w[-index]), z=ifelse(dat_test$z==1, 1, 1), x[-index,])
  soft_test_w1 <- predict(soft_test, newdata=val_covs_1)
  
  # and E[W (0) | X]
  val_covs_0 <- data.frame(w=factor(w[-index]), z=ifelse(dat_test$z==0, 0, 0), x[-index,])
  soft_test_w0 <- predict(soft_test, newdata=val_covs_0)
  # implying we can also obtain draws from E[W (1)−W (0) | X] for each person.
  pic_sbart <- soft_test_w1$p_mean - soft_test_w0$p_mean
  
  ######################################################
  ####     Continuous and Discrete Outcomes         ####
  ######################################################
  
  # Perform the Bayesian Causal Forest for the ITT
  bcf_itt.tree <- quiet(
    SparseBCF::SparseBCF(y[-index], z[-index], x[-index,], pihat = pihat,
                         nsim = n_sim, nburn = n_burn, sparse = F))
  
  bcf_tau_itt <- bcf_itt.tree$tau
  bcf_itt <- colMeans(bcf_tau_itt)
  
  # Get posterior of treatment effects
  bcf_tauhat <- bcf_itt/pic_bcf
  bcf_exp <- as.data.frame(cbind(bcf_tauhat, x[-index,]))
  
  # repair names for binary tree
  names(bcf_exp)[2:length(bcf_exp)] <- names(inference)[-(1:3)]

  
  ## SBART
  s_bcf_itt.tree <- quiet(
    SparseBCF::SparseBCF(y[-index], z[-index], x[-index,], pihat = pihat,
                         nsim = n_sim, nburn = n_burn)
  )
  
  s_bcf_tau_itt <- s_bcf_itt.tree$tau
  s_bcf_itt <- colMeans(s_bcf_tau_itt)
  
  # posterior splitting probabilities
  s_bcf_post_split_probs <- colMeans(s_bcf_itt.tree$varprb_tau)
  
  # Get posterior of treatment effects
  s_bcf_tauhat <- s_bcf_itt/pic_sbart
  s_bcf_exp <- as.data.frame(cbind(s_bcf_tauhat, x[-index,]))
  
  # repair names
  names(s_bcf_exp)[2:length(s_bcf_exp)] <- names(inference)[-(1:3)]
  
  # print('Step 1 completed')
  ######################################################
  ####  Step 2: Build a CART on the Unit Level CITT ####
  ######################################################
  
  # without cost function
    bcf_fit.tree <- rpart(bcf_tauhat ~ .,
                          data = bcf_exp,
                          maxdepth = max_depth,
                          cp = cp,
                          minsplit = minsplit)
    
    # binary tree for sparse trees
    s_bcf_fit.tree <- rpart(s_bcf_tauhat ~ .,
                            data = s_bcf_exp,
                            maxdepth = max_depth,
                            cp = cp,
                            minsplit = minsplit,
                            cost = (max(s_bcf_post_split_probs)/s_bcf_post_split_probs)
                            )
    
  # print('Step 2 completed')
  ######################################################
  ####    Step 3: Extract Rules and IV Estimation   ####
  ######################################################

  bcf_ivResults <- heterogeneous_treatment_estimation(bcf_fit.tree, inference = inference,
                                                      adj_method = adj_method, pred_df = pred_df)
  
  s_bcf_ivResults <- heterogeneous_treatment_estimation(s_bcf_fit.tree, inference = inference,
                                                        adj_method = adj_method, pred_df = pred_df)
  
  ######################################################
  ####             Step 4: Return results           ####
  ######################################################
  #  print('before returning')
  
  return(
    tibble::tibble(
      'bcf_iv' = list(bcf_ivResults), 
      'sbcf_iv' = list(s_bcf_ivResults),
      'pic_sbart' = list(pic_sbart),
      'pic_bcf' = list(pic_bcf),
      'pic_bcf_org' = list(pic_bcf_org)
      )
  )
}