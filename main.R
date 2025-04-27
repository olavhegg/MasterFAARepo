# Main Script
# This script runs all the analysis steps in sequence

# Load required libraries
library(readxl)
library(dplyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(xts)
library(zoo)
library(tseries)
library(vars)
library(gridExtra)  # Added for visualization

# Source utility functions
source("R/utils.R")

# Define base period for analysis
base_period_start <- as.Date("2001-01-01")
base_period_end <- as.Date("2024-12-31")

# Initialize logging
initialize_log()

log_message("Starting analysis pipeline...\n")

# Step 1: Data Cleaning and Preparation
log_message("\n==========================================\n")
log_message("STEP 1: Data Cleaning and Preparation\n")
log_message("==========================================\n")

# Source and run data cleaning script
source("R/01_data_cleaning.R")

# Step 2: Analysis
log_message("\n==========================================\n")
log_message("STEP 2: Analysis\n")
log_message("==========================================\n")

# Source and run analysis script
source("R/02_analysis.R")

# Step 3: Modeling
log_message("\n==========================================\n")
log_message("STEP 3: Modeling\n")
log_message("==========================================\n")

# Source and run modeling script
source("R/03_models.R")

# Step 4: Visualization
log_message("\n==========================================\n")
log_message("STEP 4: Visualization\n")
log_message("==========================================\n")

# Source and run visualization script
source("R/04_visualization.R")

log_message("\nAnalysis pipeline completed successfully!\n")

# Save session information
log_message("\n==========================================\n")
log_message("Saving session information\n")
log_message("==========================================\n")
tryCatch({
  # Create session info
  session_info <- sessionInfo()
  
  # Save to file
  sink("results/session_info.txt")
  print(session_info)
  sink()
  
  log_message("✓ Session info saved to results/session_info.txt\n")
}, error = function(e) {
  log_message(sprintf("\nError in Session info saving: %s\n", e$message))
  stop("Session info saving failed")
})

# Final success message
log_message("\n==========================================\n")
log_message("ANALYSIS COMPLETED SUCCESSFULLY!\n")
log_message(sprintf("Time completed: %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
log_message("==========================================\n\n")

# Output locations
log_message("Output locations:\n")
log_message("- Processed data: data/processed/\n")
log_message("- Analysis results: results/\n")
log_message("- Figures: output/figures/\n")
log_message("- Tables: output/tables/\n")
log_message("- Log file: results/analysis_log.txt\n")
log_message("- Session info: results/session_info.txt\n")

# Close the log file
sink()
log_message("Analysis log saved to results/analysis_log.txt\n")
