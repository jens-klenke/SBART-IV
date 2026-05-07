# SBART-IV

R Project to replicate the MCMC results for our paper.

---

## Requirements

- **R** (version 4.0.0 or higher) → [Download R](https://cran.r-project.org/)
- **RStudio** (recommended) → [Download RStudio](https://posit.co/download/rstudio-desktop/)

---

## Getting Started

### 1. Open the R Project
Open the file **`ProjectName.Rproj`** in RStudio.  
> It is important to open the **`.Rproj` file** and not just the R scripts directly, as this ensures the project environment is loaded correctly.


### 2. Install Required Packages
Once the project is open, run the setup script in RStudio:

```r
source("setup.R")
```
> If the installation fails for any reason, the script `packages.R` inside the folder `01_code` can be used to manually install the packages. 

### 3. Run the Analysis
Inside the folder `01_code` there are three main scripts to replicate simulations:

- `generate_raw_data.R` simulates data 
- `estimation.R` runs the MCMC on the simulated data, which estimates the BCF-IV and SBCF-IV models
- `MCMC_evaluation.R` performs the evaluation of the MCMC runs

Moreover, the folder `01_code` holds a script for each empirical application: 

- `emp_appl_OHIE.R` for the application based on the Oregon Health Insurance Experiment dataset (Johnson et al. 2022)
- `emp_appl_401k.R` for the application based on the 401(k) dataset (Chernozhukov et al. 2018)

Note that for the empirical application based on the Oregon Health Insurance Experiment dataset, you need to download the respective datasets beforehand as specified in the script.
The following datasets need to be downloaded from the [OHIE website](https://www.nber.org/research/data/oregon-health-insurance-experiment-data) and loaded into the folder `05_emp_appl_OHIE/raw_data`: 

- `oregonhie_descriptive_vars.dta`
- `oregonhie_stateprograms_vars.dta`
- `oregonhie_survey12m_vars.dta`

For the application based on the 401(k) dataset, data is fetched from the `DoubleML` package.


