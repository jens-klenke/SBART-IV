# load packages 
############## Packages ################
source(here::here('01_code/packages.R'))

# source all files in the functions folder
invisible(
  sapply(
    list.files(
      here::here('01_code/functions'),
      full.names = TRUE, 
      recursive = TRUE),
    source))


# =========================================================================
# Empirical application: 401(k) eligibility and net financial assets
# Data: 1991 SIPP, via DoubleML R package.
# IV design follows Poterba, Venti & Wise (1994, 1995); benchmark LATE
# specification follows Chernozhukov et al. (2018, Sec. 6.3).
#   Y: net_tfa   net financial assets
#   W: p401      401(k) participation (endogenous)
#   Z: e401      employer eligibility (instrument)
# ========================================================================


out_dir <- here::here("04_emp_appl_401k")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

set.seed(123)

# ---- Base sample (9 raw covariates) and propensity-score trimming -------
# Trim at 0.1 to enforce overlap; participation propensity is fit by logit on X.
data_base  <- fetch_401k(return_type = "data.table", instrument = TRUE)
ps_base    <- glm(p401 ~ ., family = binomial,
                  data = data_base %>% dplyr::select(-c(net_tfa, e401)))
pihat_base <- predict(ps_base, type = "response")
data_base  <- data_base[pihat_base > 0.1 & pihat_base < 0.9, ]

# ---- (1) DoubleML LATE benchmark on the trimmed sample ------------------
# Abadie (2003) kappa-weighted LATE; cross-fitted IIVM of Chernozhukov et al. (2018).
# Provides a level reference (lit. range ~ $9k-$13k) against which to read
# the SBCF-IV subgroup estimates.
dml_data <- DoubleMLData$new(
  data = data_base, y_col = "net_tfa", d_cols = "p401", z_cols = "e401",
  x_cols = setdiff(names(data_base), c("net_tfa", "p401", "e401")))
dml_late <- DoubleMLIIVM$new(dml_data,
                             ml_g = lrn("regr.ranger"),
                             ml_m = lrn("classif.ranger"),
                             ml_r = lrn("classif.ranger"),
                             n_folds = 5)
dml_late$fit()
print(dml_late)

# ---- (2) SBCF-IV: base specification (9 covariates) ---------------------
X_base <- data_base %>% dplyr::select(-c(net_tfa, e401, p401)) %>% as.matrix()
sbcf_iv_out <- sbcf_iv(y = data_base$net_tfa,
                       w = data_base$p401,
                       z = data_base$e401,
                       x = X_base,
                       max_depth = 4)


# -----------------------------------------------------------------
##### 1. Align sbcfivResults to inference.tree$frame via rpart node IDs ####
# -----------------------------------------------------------------

inference.tree <- sbcf_iv_out$fit.tree_discovery
sbcfiv_res     <- sbcf_iv_out$sbcfivResults

node_ids <- as.integer(rownames(inference.tree$frame))

ccace_by_node   <- as.numeric(sbcfiv_res$CCACE)[node_ids]
pvalue_by_node  <- as.numeric(sbcfiv_res$pvalue)[node_ids]
adjp_by_node    <- as.numeric(sbcfiv_res$Adj_pvalue)[node_ids]
weakiv_by_node  <- as.numeric(sbcfiv_res$Weak_IV_test)[node_ids]
picom_by_node   <- as.numeric(sbcfiv_res$Pi_compliers)[node_ids]
piobs_by_node   <- as.numeric(sbcfiv_res$Pi_obs)[node_ids]

stopifnot(length(ccace_by_node) == nrow(inference.tree$frame))

# Assign CCACE to yval at every node (internal + leaf)
inference.tree$frame$yval <- ccace_by_node


# -----------------------------------------------------------------
##### 3. Build node labels #####
#
# -----------------------------------------------------------------



node_labels <- sprintf(
  "cCACE = %.2f\npi_obs = %.2f\npi_com = %.2f",
  ccace_by_node,
  piobs_by_node,
  picom_by_node
)

# flag stat. sig. leaf-node CACE estimates
leaf_rows <- inference.tree$frame$var == "<leaf>"
adjp_by_node_threshold <- 0.1

flag_statsign <- leaf_rows & !is.na(adjp_by_node) & adjp_by_node < adjp_by_node_threshold

node_labels[flag_statsign] <- sprintf(
  "cCACE = %.2f *\npi_obs = %.2f\npi_com = %.2f",
  ccace_by_node[flag_statsign],
  piobs_by_node[flag_statsign],
  picom_by_node[flag_statsign]
)

# -----------------------------------------------------------------
##### 4. Plot inference tree. #####
# -----------------------------------------------------------------

# Per-node box colors: light blue for internal nodes, darker for leaves

# Two shades of blue — adjust to taste
col_internal <- "#D6EAF8"  # light blue
col_leaf     <- "#5DADE2"  # medium-dark blue

# Vector of length nrow(frame), one color per node
box_cols <- ifelse(leaf_rows, col_leaf, col_internal)

# Plot

pdf(file.path(out_dir, "inference_tree_base.pdf"))
rpart.plot(
  inference.tree,
  type        = 2,
  extra       = 0,
  roundint    = FALSE,
  box.palette = 0,              # disable the default gradient
  box.col     = box_cols,       # per-node color vector
  node.fun    = function(x, labs, digits, varlen) node_labels
)
dev.off()

# -----------------------------------------------------------------
##### 5. Leaf-level diagnostic table with per-criterion flags #####
# -----------------------------------------------------------------

leaf_n    <- inference.tree$frame$n

leaf_diag <- data.frame(
  node_id       = node_ids[leaf_rows],
  n             = leaf_n[leaf_rows],
  pi_obs        = piobs_by_node[leaf_rows],
  pi_compliers  = picom_by_node[leaf_rows],
  weakiv_p      = weakiv_by_node[leaf_rows],
  CACE          = ccace_by_node[leaf_rows],
  pvalue        = pvalue_by_node[leaf_rows],
  adj_pvalue    = adjp_by_node[leaf_rows]
)

# Largest-first for readability
leaf_diag <- leaf_diag[order(-leaf_diag$n), ]
rownames(leaf_diag) <- NULL
print(leaf_diag, digits = 3)

# Export tables
write.csv(
  leaf_diag,
  file = here::here("04_emp_appl_401k",  "leaf_diagnostics.csv"),
  row.names = FALSE
)

write.csv(
  sbcfiv_res,
  file = here::here("04_emp_appl_401k", "inference_tree_results.csv"),
  row.names = FALSE
)

# Match Splitting vars to xvars
names_covariates <- colnames(X_base)
labels_df <- data.frame(
  node   = as.integer(rownames(inference.tree$frame)),
  var    = as.character(inference.tree$frame$var),
  n      = inference.tree$frame$n,
  yval   = inference.tree$frame$yval
)
labels_df <- labels_df[labels_df$var != "<leaf>", ]
labels_df$original_name <- names_covariates[
  as.integer(gsub("\\D", "", labels_df$var))
]
print(labels_df)




