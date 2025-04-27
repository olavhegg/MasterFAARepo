# Check R version
if (getRversion() < "4.5.0") {
  stop("This project requires R version 4.5.0 or higher. Please update R.")
}

# Platform-specific settings
if (Sys.info()["sysname"] == "Windows") {
  # Windows specific settings
  options(renv.config.install.verbose = TRUE)
  if (!file.exists("C:/rtools43/usr/bin")) {
    warning("Rtools not found. Some packages might fail to install.")
  }
} else if (Sys.info()["sysname"] == "Darwin") {
  # macOS specific settings
  if (Sys.info()["machine"] == "arm64") {
    # ARM Mac specific settings
    options(renv.config.install.verbose = TRUE)
    if (!file.exists("/Library/Apple/usr/bin/rosetta")) {
      warning("Rosetta 2 not found. Some packages might fail to install.")
    }
  }
}

# Project specific settings
options(
  renv.config.auto.snapshot = TRUE,
  renv.config.cache.enabled = TRUE,
  renv.config.install.verbose = TRUE
)

# Load renv
source("renv/activate.R")
