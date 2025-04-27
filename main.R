# Main Script
# This script runs all the analysis steps in sequence

# Load required libraries
library(here)

# Set working directory to project root
setwd(here())

# Source all R scripts
source("R/utils.R")
source("R/01_data_cleaning.R")
source("R/02_analysis.R")
source("R/03_models.R")
source("R/04_visualization.R")

# Print completion message
cat("Analysis completed successfully!\n")
cat("Check the output/ directory for results.\n")
