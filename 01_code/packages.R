# packages
if (!require("pak", quietly = TRUE)) install.packages("pak")
if (!require("pacman", quietly = TRUE)) install.packages("pacman")

# Github packages
if (!require("bcf")) pak::pak("jaredsmurray/bcf")
if (!require("SparseBCF")) pak::pak("albicaron/SparseBCF")
if (!require("BayesIV")) pak::pak("fbargaglistoffi/BCF-IV")

# BiocManager
if (!require("BiocManager", quietly = TRUE)) install.packages("BiocManager")
if (BiocManager::version() != "3.20") BiocManager::install("BiocVersion")

# Pacman also installes CRAN packages if needed
pacman::p_load(
  tibble,
  ggplot2,
  here,
  readr,
  furrr,
  Hmisc,
  magrittr,
  MASS,
  stats,
  invgamma,
  AER,
  splines,
  MCMCpack,
  BayesTree,
  dbarts,
  bcf,
  crayon,
  future,
  rpart,
  bartCause,
  dplyr,
  tidyr,
  stringr,
  rpart.plot,
  devtools,
  SoftBart,
  brms,
  cowplot,
  glue,
  magick,
  DoubleML, 
  mlr3, 
  mlr3learners,
  ranger,
  haven,
  plyr,
  # Algorithms 
  BayesIV,
  SparseBCF
)

