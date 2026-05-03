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
  splines,
  MCMCpack,
  BayesTree,
  dbarts,
  bcf,
  crayon,
  future,
  rpart,
  AER,
  bartCause,
  dplyr,
  tidyr,
  stringr,
  kableExtra,
  rpart.plot,
  devtools,
  SoftBart,
  brms,
  cowplot,
  glue,
  patchwork,
  magick,
  # Algorithms 
  BayesIV,
  SparseBCF
)

