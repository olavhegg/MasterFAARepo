# Setup Script
# This script handles first-time setup and package management

#' Initialize the project environment
#' @return TRUE if successful, FALSE otherwise
initialize_project <- function() {
  message("Initializing MasterFAA project environment...")
  
  # Check R version
  if (getRversion() < "4.5.0") {
    stop("This project requires R version 4.5.0 or higher. Please update R.")
  }
  
  # Initialize renv if needed
  if (!requireNamespace("renv", quietly = TRUE)) {
    install.packages("renv")
  }
  
  # Restore project dependencies
  message("Restoring project dependencies...")
  renv::restore()
  
  # Load required packages
  required_packages <- c(
    "readxl",      # For reading Excel files
    "dplyr",       # For data manipulation
    "tidyr",       # For data tidying
    "zoo",         # For time series operations
    "xts",         # For extended time series
    "tseries",     # For time series analysis
    "vars",        # For VAR modeling
    "lubridate",   # For date operations
    "stringr",     # For string operations
    "ggplot2",     # For plotting
    "here",        # For project paths
    "knitr",       # For report generation
    "rmarkdown",   # For R Markdown
    "jsonlite",    # For JSON handling
    "tools",       # For various tools
    "methods"      # For S4 classes
  )
  
  # Load each package with error handling
  load_package <- function(package_name) {
    tryCatch({
      if (!requireNamespace(package_name, quietly = TRUE)) {
        message("Installing package: ", package_name)
        renv::install(package_name)
      }
      library(package_name, character.only = TRUE)
      message("✓ Successfully loaded: ", package_name)
      TRUE
    }, error = function(e) {
      message("Error with package ", package_name, ": ", e$message)
      FALSE
    })
  }
  
  # Load all packages
  message("\nLoading required packages...")
  package_status <- sapply(required_packages, load_package)
  
  if (!all(package_status)) {
    warning("Some packages failed to load. Check the messages above.")
  }
  
  # Create project directory structure
  create_project_structure()
  
  message("\nProject initialization completed!")
  return(all(package_status))
}

#' Create project directory structure
create_project_structure <- function() {
  message("\nCreating project directory structure...")
  
  # Define required directories
  dirs <- c(
    "data/raw",
    "data/processed/cleaned",
    "data/processed/analyzed",
    "output/models",
    "output/figures",
    "output/tables",
    "results"
  )
  
  # Create directories
  for (dir in dirs) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    message("✓ Created directory: ", dir)
  }
}

#' Check required data files
#' @return TRUE if all files exist, FALSE otherwise
check_required_files <- function() {
  message("\nChecking required data files...")
  
  required_files <- c(
    "data/raw/vgnettsøk.xlsx",
    "data/raw/vgaltannet.xlsx",
    "data/raw/dagbladetsøk.xlsx",
    "data/raw/dagbladetaltannet.xlsx",
    "data/raw/nrksøk.xlsx",
    "data/raw/nrkaltannet.xlsx",
    "data/raw/aftenpostensøk.xlsx",
    "data/raw/aftenpostenaltannet.xlsx",
    "data/raw/dnsøk.xlsx",
    "data/raw/dnaltannet.xlsx",
    "data/raw/vgpapirsøk.xlsx",
    "data/raw/vgaltannetpapir.xlsx",
    "data/raw/EPUEurope.xlsx",
    "data/raw/VIX.xlsx",
    "data/raw/BrentOil.xlsx",
    "data/raw/EXR.xlsx",
    "data/raw/ECBDFR.xlsx",
    "data/raw/IR.xlsx"
  )
  
  missing_files <- required_files[!file.exists(required_files)]
  
  if (length(missing_files) > 0) {
    message("\nMissing required files:")
    for (file in missing_files) {
      message("- ", file)
    }
    return(FALSE)
  }
  
  message("✓ All required data files are present")
  return(TRUE)
} 