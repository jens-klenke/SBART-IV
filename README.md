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
Inside the folder `01_code` there are three main scripts:

- `generate_raw_data.R` simulates data 
- `estimation.R` runs the MCMC on the simulated data, which estimates the BCF-IV and SBCF-IV models
- `MCMC_evaluation.R` performs the evaluation of the MCMC runs

