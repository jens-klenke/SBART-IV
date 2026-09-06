# load packages 
############## Packages ################
source(here::here(file.path('01_code', 'packages.R')))

# source all files in the functions folder
invisible(
  sapply(
    list.files(
      here::here(file.path('01_code', 'functions')),
      full.names = TRUE, 
      recursive = TRUE),
    source))


# =========================================================================
# Empirical application: Oregon Health Insurance Experiment
# Data: can be loaded by making an account on the website https://nber.org/
#   and then downloading the public data from https://www.nber.org/oregon/4.data.html
#   the R function read_dta() can be used to read in the data
# IV design and replication follows Johnson et al. (2019) (https://arxiv.org/abs/1908.03652)
#   Y: notbadday_tot                     number of days (out of past 30) when poor health not impaired regular activities
#   D: ohp_all_ever_firstn_30sep2009     take-up of Medicaid insurance (endogenous)
#   Z: treatment                         lottery Medicaid insurance (instrument)
# ========================================================================


#### Load and clean data ####

### load data
# data can be loaded by making an account on the website https://nber.org/
# and then downloading the public data from https://www.nber.org/oregon/4.data.html
# the R function read_dta() can be used to read in the data

# Descriptive Variables: 
# This dataset contains demographic characteristics that were recorded when individuals signed up for
# the lottery and lottery selection. Some of these variables are necessary to replicate Finkelstein et al
# 2012, Baicker et al (2013), and Taubman et al. (2014).
descriptive <- haven::read_dta(here::here(file.path('05_emp_appl_OHIE', 'raw_data', 'oregonhie_descriptive_vars.dta')))

# State Program Variables:
# This dataset contains information from the state of Oregon on individuals’ participation in the
# following state programs: Medicaid, the Supplemental Nutrition Assistance Program (SNAP), and
# Temporary Assistance to Needy Families (TANF). This dataset includes the insurance variables
# necessary to replicate Finkelstein et al 2012, Baicker et al (2013), and Taubman et al. (2014).
stateprograms<- haven::read_dta(here::here(file.path('05_emp_appl_OHIE', 'raw_data', 'oregonhie_stateprograms_vars.dta')))

# Twelve Month Mail Survey:
# This dataset contains variables from a mail survey which began in July 2009 with intensive follow up
# continuing until March 2010. This survey is referred to as the “Twelve Month Survey.” The survey
# contained questions about health insurance as well as health care needs, experiences and costs. This
# dataset includes variables necessary to replicate parts of Finkelstein et al (2012).
survey12m <- haven::read_dta(here::here(file.path('05_emp_appl_OHIE', 'raw_data', 'oregonhie_survey12m_vars.dta')))


### clean data 
# helper function change row name based on person id and order ascending
dt_trans <- function(x){
  x <- data.frame(x,row.names = x$person_id)
  x <- x[order(x$person_id),]
  return(x)
}

# merge data set
dt_full <- plyr::join_all(list(descriptive,survey12m, stateprograms), by = 'person_id' ,type="full")
dt_full <- dt_trans(dt_full)


#transfer variables
dt_full <- dt_full %>% 
  mutate(age_old = if_else(birthyear_list >= 1945 & birthyear_list <= 1958,1,0),
         age_young = if_else(birthyear_list >= 1959 & birthyear_list <= 1989,1,0),
         edu_1 = if_else(edu_12m == 1,1,0),edu_2 = if_else(edu_12m == 2,1,0),
         edu_3 = if_else(edu_12m == 3,1,0),edu_4 = if_else(edu_12m == 4,1,0),
         employ_1 = if_else(employ_hrs_12m == 1,1,0),employ_2 = if_else(employ_hrs_12m == 2,1,0),
         employ_3 = if_else(employ_hrs_12m == 3,1,0),employ_4 = if_else(employ_hrs_12m == 4,1,0),
         flp_1 = if_else(hhinc_pctfpl_12m < 50,1,0),
         flp_2 = if_else(hhinc_pctfpl_12m >= 50 & hhinc_pctfpl_12m < 75,1,0),
         flp_3 = if_else(hhinc_pctfpl_12m >= 75 & hhinc_pctfpl_12m < 100,1,0),
         flp_4 = if_else(hhinc_pctfpl_12m >= 100&hhinc_pctfpl_12m < 150,1,0),
         flp_5 = if_else(hhinc_pctfpl_12m >= 150,1,0),
         ddddraw_sur_1 = if_else(wave_survey12m==1,1,0),  
         ddddraw_sur_2 = if_else(wave_survey12m==2,1,0),
         ddddraw_sur_3 = if_else(wave_survey12m==3,1,0),
         ddddraw_sur_4 = if_else(wave_survey12m==4,1,0),
         ddddraw_sur_5 = if_else(wave_survey12m==5,1,0),
         ddddraw_sur_6 = if_else(wave_survey12m==6,1,0),
         ddddraw_sur_7 = if_else(wave_survey12m==7,1,0),
         dddnumhh_li_1 = if_else(numhh_list==1,1,0),  
         dddnumhh_li_2 = if_else(numhh_list==2,1,0),
         dddnumhh_li_3 = if_else(numhh_list==3,1,0),
         ddddraXnum_1_1 = if_else(wave_survey12m==1 & numhh_list==1,1,0),  
         ddddraXnum_1_2 = if_else(wave_survey12m==1 & numhh_list==2,1,0),  
         ddddraXnum_1_3 = if_else(wave_survey12m==1 & numhh_list==3,1,0),  
         ddddraXnum_2_1 = if_else(wave_survey12m==2 & numhh_list==1,1,0),  
         ddddraXnum_2_2 = if_else(wave_survey12m==2 & numhh_list==2,1,0),
         ddddraXnum_2_3 = if_else(wave_survey12m==2 & numhh_list==3,1,0),
         ddddraXnum_3_1 = if_else(wave_survey12m==3 & numhh_list==1,1,0),  
         ddddraXnum_3_2 = if_else(wave_survey12m==3 & numhh_list==2,1,0),
         ddddraXnum_3_3 = if_else(wave_survey12m==3 & numhh_list==3,1,0),
         ddddraXnum_4_1 = if_else(wave_survey12m==4 & numhh_list==1,1,0),  
         ddddraXnum_4_2 = if_else(wave_survey12m==4 & numhh_list==2,1,0),
         ddddraXnum_4_3 = if_else(wave_survey12m==4 & numhh_list==3,1,0),
         ddddraXnum_5_1 = if_else(wave_survey12m==5 & numhh_list==1,1,0),  
         ddddraXnum_5_2 = if_else(wave_survey12m==5 & numhh_list==2,1,0),
         ddddraXnum_5_3 = if_else(wave_survey12m==5 & numhh_list==3,1,0),
         ddddraXnum_6_1 = if_else(wave_survey12m==6 & numhh_list==1,1,0),  
         ddddraXnum_6_2 = if_else(wave_survey12m==6 & numhh_list==2,1,0),
         ddddraXnum_6_3 = if_else(wave_survey12m==6 & numhh_list==3,1,0),
         ddddraXnum_7_1 = if_else(wave_survey12m==7 & numhh_list==1,1,0),  
         ddddraXnum_7_2 = if_else(wave_survey12m==7 & numhh_list==2,1,0),
         ddddraXnum_7_3 = if_else(wave_survey12m==7 & numhh_list==3,1,0),
         chl_chk_bin_12m = if_else(chl_chk_12m == 3,0,1 ),
         dia_chk_bin_12m = if_else(dia_chk_12m  == 3,0,1 ),
         mam_chk_bin_12m = case_when(
           mam_chk_12m == 1 & birthyear_list<=1968 ~ 1,
           mam_chk_12m == 2 & birthyear_list<=1968 ~ 0, 
           mam_chk_12m == 3 & birthyear_list<=1968 ~ 0,
           birthyear_list > 1968 |female_list == 0 ~ NA_real_,  
         ),
         pap_chk_bin_12m = case_when(
           pap_chk_12m == 1 & birthyear_list<=1968 ~ 1,
           pap_chk_12m == 2 & birthyear_list<=1968 ~ 0, 
           pap_chk_12m == 3 & birthyear_list<=1968 ~ 0,
           birthyear_list > 1968 |female_list == 0 ~ NA_real_,  
         ),
         pap_bin = if_else(pap_chk_12m == 1,1,0),
         health_good_12m = if_else(health_gen_bin_12m == 1,0,1),
         health_not_poor_12m = if_else(health_gen_12m != 1,1,0),
         health_chg_12m = if_else(health_chg_bin_12m == 1,0,1),
         notbadday_phys = 30-baddays_phys_12m,
         notbadday_tot = 30-baddays_tot_12m, # Number of days (out of past 30) when poor health not impaired regular activities
         notbadday_ment = 30-baddays_ment_12m,
         not_dep_screen_12m = if_else((dep_interest_12m + dep_sad_12m)>=5,0,1),
         not_er_noner_12m = if_else(er_noner_12m == 1,0,1),
         poshappy_12m = if_else(happiness_12m == 3,0,1),
         llldraw_lot_1 = if_else( draw_lottery==1,1,0),  
         llldraw_lot_2 = if_else( draw_lottery==2,1,0),
         llldraw_lot_3 = if_else( draw_lottery==3,1,0),
         llldraw_lot_4 = if_else( draw_lottery==4,1,0),
         llldraw_lot_5 = if_else( draw_lottery==5,1,0),
         llldraw_lot_6 = if_else( draw_lottery==6,1,0),
         llldraw_lot_7 = if_else( draw_lottery==7,1,0),
         llldraw_lot_8 = if_else( draw_lottery==8,1,0)
  )

dt_resp <- dt_full %>%
  filter(sample_12m_resp == 1) %>% mutate(age = 2008 - birthyear_list)

with(dt_resp, prop.table(table(treatment, useNA = "ifany")))
with(dt_full, prop.table(table(treatment, sample_12m_resp, useNA = "ifany")))

# female_list  
#dt_resp$female_list[8870];
#dt_resp$female_12m[8870];
# Known data issue: person_id X reports female_12m == 1 but female_list == NA/0.
problem_id <- rownames(dt_resp)[8870]
stopifnot(is.na(dt_resp[problem_id, "female_list"]) || dt_resp[problem_id, "female_list"] == 0)
dt_resp[problem_id, "female_list"] <- 1


# create education variable to group data 
# indicator for high school edu or less vs more than highschool edu (0 vs 1)
edu <- if_else(dt_resp$edu_12m >= 3,1,0)
dt_resp$edu <- edu



# --------------------------------------------------------------------
# 0. Preliminaries 
# --------------------------------------------------------------------

set.seed(123)

# define vars
y <- dt_resp[["notbadday_tot"]]              # outcome as a vector
D <- dt_resp$ohp_all_ever_firstn_30sep2009   # treatment
Z <- dt_resp$treatment # IV

# load covariate set from Johnson et al. 
load(here::here(file.path('05_emp_appl_OHIE', 'raw_data', 'pairohieBMcontOut.RData')))
Xbm <- pairohieBMcontOut[, !(names(pairohieBMcontOut) %in% c("Z", "pairMatched"))]


# --------------------------------------------------------------------
# 2. Drop rows with NA in y, D, Z, or X, consistently across all
# --------------------------------------------------------------------

dat_model <- tibble(y = y, D = D, Z = Z, Xbm) %>%
  tidyr::drop_na()

X_mat <- dat_model %>% dplyr::select(-y, -D, -Z) %>% as.matrix()

cat("Analysis sample size:", nrow(dat_model), "\n")
cat("Number of covariates:", ncol(X_mat), "\n")


# model estimation
sbcf_iv_out <- sbcf_iv(
  y = dat_model$y,
  w = dat_model$D,
  z = dat_model$Z,
  x = X_mat,
  max_depth = 4        # as in Johnson et al. (2019)
)



# -----------------------------------------------------------------
# 1. Align sbcfivResults to inference.tree$frame via rpart node IDs
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

leaf_rows <- inference.tree$frame$var == "<leaf>"
leaf_n    <- inference.tree$frame$n


# Assign CCACE to yval at every node (internal + leaf)
inference.tree$frame$yval <- ccace_by_node

# -----------------------------------------------------------------
# 3. Build node labels
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
# 4. Plot inference tree.
# -----------------------------------------------------------------

# Per-node box colors: light blue for internal nodes, darker for leaves

# Two shades of blue — adjust to taste
col_internal <- "#D6EAF8"  # light blue
col_leaf     <- "#5DADE2"  # medium-dark blue

# Vector of length nrow(frame), one color per node
box_cols <- ifelse(leaf_rows, col_leaf, col_internal)

# Plot

pdf(file=here::here(file.path("05_emp_appl_OHIE", "results", "inference_tree.pdf")))
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
# 5. Leaf-level diagnostic table with per-criterion flags
# -----------------------------------------------------------------

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
  file = here::here("05_emp_appl_OHIE", "results", "leaf_diagnostics.csv"),
  row.names = FALSE
)

write.csv(
  sbcfiv_res,
  file = here::here("05_emp_appl_OHIE", "results", "inference_tree_results.csv"),
  row.names = FALSE
)


# Match Splitting vars to xvars
names_covariates <- colnames(X_mat)
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
