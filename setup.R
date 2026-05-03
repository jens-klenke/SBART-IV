# setup.R
# =============================================
# Run this script first to install all required
# packages for this project
# if that fails the script packages.R in folder 01_code contains all used packages
# =============================================

# Step 1: Install renv if not already installed
if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv")
}

# Step 2: Restore all packages from the lockfile
renv::restore()

# Step 3: Confirm everything is set up
cat("Setup complete! All packages have been installed.\n")
cat("You can now run the main project scripts.\n")