# Main Script
# This script runs all the analysis steps in sequence and saves processed data

# Initialize renv
if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv")
}
renv::restore()

# Load utility functions first (needed for logging)
source("R/utils.R")

# Start logging
initialize_log()

# ==========================================
# INITIALIZATION
# ==========================================
log_message("Initializing analysis environment...\n\n")

# Set working directory to project root
log_message("Setting working directory...\n")
setwd(here::here())
log_message("Working directory set to:", getwd(), "\n\n")

# Create project directory structure
create_project_dirs()

# ==========================================
# ANALYSIS STEPS
# ==========================================
log_message("Starting analysis pipeline...\n\n")

log_message("==========================================\n")
log_message("STEP 1: Data Cleaning and Preparation\n")
log_message("==========================================\n")
stop_on_error({
  source("R/01_data_cleaning.R")
}, "Data cleaning")

log_message("\n==========================================\n")
log_message("STEP 2: Data Analysis\n")
log_message("==========================================\n")
stop_on_error({
  source("R/02_analysis.R")
}, "Analysis")

log_message("\n==========================================\n")
log_message("STEP 3: Model Estimation\n")
log_message("==========================================\n")
stop_on_error({
  source("R/03_models.R")
}, "Modeling")

log_message("\n==========================================\n")
log_message("STEP 4: Visualization\n")
log_message("==========================================\n")
stop_on_error({
  source("R/04_visualization.R")
}, "Visualization")

# Save session info for reproducibility
log_message("\n==========================================\n")
log_message("Saving session information\n")
log_message("==========================================\n")
stop_on_error({
  writeLines(capture.output(sessionInfo()), "results/session_info.txt")
  log_message("✓ Session info saved to results/session_info.txt\n")
}, "Session info saving")

# Print completion message
log_message("\n==========================================\n")
log_message("ANALYSIS COMPLETED SUCCESSFULLY!\n")
log_message("Time completed:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
log_message("==========================================\n")
log_message("\nOutput locations:\n")
log_message("- Processed data: data/processed/\n")
log_message("- Analysis results: results/\n")
log_message("- Figures: output/figures/\n")
log_message("- Tables: output/tables/\n")
log_message("- Log file: results/analysis_log.txt\n")
log_message("- Session info: results/session_info.txt\n")

# Close the log file
sink()
log_message("Analysis log saved to results/analysis_log.txt\n")
