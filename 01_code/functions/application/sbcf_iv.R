sbcf_iv <- function(y, w, z, x, 
                    #tau_true, w1, w0, # just for sim study evaluation needed. 
                    binary = FALSE, n_burn = 3000, n_sim = 7000,
                    inference_ratio = 0.5, max_depth = 2, cp = 0.01,
                    minsplit = 30, adj_method = "holm", seed = 42, cost = TRUE, ...) {
  
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
  
  
  
  ######################################################
  ####  Step 1: Compute the Shrinkage Bayesian Causal Forest  ####
  ######################################################
  
  # Compute the Propensity Score though a Logistic Regression
  p.score <- glm(z ~ x[-index,],
                 family = binomial,
                 data = discovery)
  pihat <- predict(p.score, as.data.frame(x[-index,]), type="response")
  
  # Perform the SoftBART  to calculate the Proportion of Compliers (pic)
  # prepare dataset
  dat_test <- data.frame(w=factor(w[-index]), z=z[-index], x[-index,])
  
  # fit SoftBART probit to the observed data (W given Z and X) 
  soft_test <- SoftBart::softbart_probit(w ~. , data=dat_test, test_data = dat_test, verbose = TRUE)
  
  # for E[W (1) | X]
  val_covs_1 <- data.frame(w=factor(w[-index]), z=ifelse(dat_test$z==1, 1, 1), x[-index,])
  soft_test_w1 <- predict(soft_test, newdata=val_covs_1)
  
  # and E[W (0) | X]
  val_covs_0 <- data.frame(w=factor(w[-index]), z=ifelse(dat_test$z==0, 0, 0), x[-index,])
  soft_test_w0 <- predict(soft_test, newdata=val_covs_0)
  # implying we can also obtain draws from E[W (1)−W (0) | X] for each person.
  pic <- soft_test_w1$p_mean - soft_test_w0$p_mean
  
  print("Proportion of Compliers readily computed.")
  
  
  
  ######################################################
  ####     Continuous Outcomes                  ####
  ######################################################
  
  if (binary == FALSE){
    
    
    # Perform the Shrinkage Bayesian Causal Forest for the ITT
    s_bcf_itt.tree <- SparseBCF::SparseBCF(discovery$y, discovery$z, x[-index,], pihat = pihat,
                                           nsim = n_sim, nburn = n_burn)
    
    tau_itt <- s_bcf_itt.tree$tau
    itt <- colMeans(tau_itt)
    
    # posterior splitting probabilities
    post_split_probs <- colMeans(s_bcf_itt.tree$varprb_tau)
    
    
    
    # Get posterior of treatment effects with new pic
    tauhat <- itt/pic
    exp <- as.data.frame(cbind(tauhat, x[-index,]))
    
    # repair names 
    names(exp)[2:length(exp)] <- names(inference)[-(1:3)]
    
    
    ######################################################
    ####  Step 2: Build a CART on the Unit Level CITT ####
    ######################################################
    
    fit.tree <- rpart(tauhat ~ .,
                      data = exp,
                      maxdepth = max_depth,
                      cp=cp,
                      minsplit=minsplit,
                      cost = (max(post_split_probs)/post_split_probs))
    
    ######################################################
    ####  Step 3: Extract the Causal Rules (Nodes)    ####
    ######################################################
    
    rules <- as.numeric(row.names(fit.tree$frame[fit.tree$numresp]))
    
    # Initialize Outputs
    bcfivMat <- as.data.frame(matrix(NA, nrow = length(rules), ncol=7))
    names(bcfivMat) <- c("node", "CCACE", "pvalue", "Weak_IV_test", "Pi_obs", "ITT", "Pi_compliers")
    
    # Generate Leaves Indicator
    lvs <- leaves <- numeric(length(rules)) 
    lvs[unique(fit.tree$where)] <- 1
    leaves[rules[lvs==1]] <- 1
    
    ######################################################
    ####  Step 4: Run an IV Regression on each Node   ####
    ######################################################
    
    # Run an IV Regression on the Root
    iv.root <- ivreg(y ~ w | z,  
                     data = inference)
    summary <- summary(iv.root, diagnostics = TRUE)
    iv.effect.root <-  summary$coef[2,1]
    p.value.root <- summary$coef[2,4]
    p.value.weak.iv.root <- summary$diagnostics[1,4]
    proportion.root <- 1
    compliers.root <- length(which(as.vector(inference$z)==as.vector(inference$w)))/nrow(inference)
    itt.root <- iv.effect.root*compliers.root
    
    # Store Results for Root
    bcfivMat[1,] <- c( NA , round(iv.effect.root, 4), round(p.value.root, 4), round(p.value.weak.iv.root, 4), round(proportion.root, 4), round(itt.root, 4), round(compliers.root, 4))
    
    # Initialize New Data
    names(inference) <- paste(names(inference), sep="")
    
    # Run a loop to get the rules (sub-populations)
    for (i in rules[-1]){
      # Create a Vector to Store all the Dimensions of a Rule
      sub <- as.data.frame(matrix(NA, nrow = 1,
                                  ncol = nrow(as.data.frame(path.rpart(fit.tree, node=i, print.it = FALSE)))-1))
      SimDesign::quiet(capture.output(for (j in 1:ncol(sub)){
        # Store each Rule as a Sub-population
        sub[,j] <- as.character(print(as.data.frame(path.rpart(fit.tree,node=i,print.it=FALSE))[j+1,1]))
        sub_pop <- noquote(paste(sub , collapse = " & "))
      }))
      
      subset <- with(inference, inference[which(eval(parse(text=sub_pop))),])
      
      # Run the IV Regression
      if (length(unique(subset$w))!= 1 | length(unique(subset$z))!= 1){
        iv.reg <- ivreg(y ~ w | z,  
                        data = subset)
        summary <- summary(iv.reg, diagnostics = TRUE)
        iv.effect <-  summary$coef[2,1]
        p.value <- summary$coef[2,4]
        p.value.weak.iv <- summary$diagnostics[1,4]
        compliers <- length(which(as.vector(subset$z)==as.vector(subset$w)))/nrow(subset)
        itt <- iv.effect*compliers
        
        # Proportion of observations in the node
        proportion.node <- nrow(subset)/nrow(inference)
        
        ######################################################
        ####   Step 5: Output the Values of each CCACE   ####
        ######################################################
        
        bcfivMat[i,] <- c(sub_pop, round(iv.effect, 4), round(p.value, 4), round(p.value.weak.iv, 4), round(proportion.node, 4), round(itt, 4), round(compliers, 4))
      }
      
      # Delete data
      rm(subset)
    }
    
    # Adjust P.values 
    bcfiv_correction <- cbind(as.data.frame(bcfivMat), leaves)
    adj <- round(p.adjust( as.numeric(bcfiv_correction$pvalue[which(bcfiv_correction$leaves==1)]) ,  paste(adj_method)), 5)
    Adj_pvalue <- rep(NA, length(rules)) 
    Adj_pvalue[which(bcfiv_correction$leaves==1)] <- adj
    
    # Store Results
    sbcfivResults <- cbind(as.data.frame(bcfivMat), Adj_pvalue)
  }
  
  
  # Return Results
  return(list(fit.tree_discovery=fit.tree,
              sbcfivResults=sbcfivResults) 
  )
}





