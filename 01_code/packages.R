# packages
if (!require("pak")) install.packages("pak")
if (!require("pacman")) install.packages("pacman")

# Github packages 
if (!require("SparseBCF")) pak::pak("albicaron/SparseBCF")
if (!require("BayesIV")) pak::pak("fbargaglistoffi/BCF-IV")

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
  # Algorithms 
  BayesIV,
  SparseBCF
)

