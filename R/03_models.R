# Modeling Script
# This script performs the VAR analysis and other statistical tests

# Load required libraries
library(vars)
library(tseries)
library(xts)
library(urca)
library(dplyr)      # For data manipulation
library(tidyr)      # For data tidying
library(lubridate)  # For date operations

#' Run VAR model analysis
#' @return List containing VAR model results
run_var_model <- function() {
  log_message("Loading analyzed data for modeling...\n")
  
  tryCatch({
    # Load processed data
    relative_epu <- readRDS("data/processed/analyzed/relative_epu.rds")
    vix_returns <- readRDS("data/processed/analyzed/vix_returns.rds")
    oil_monthly_vol <- readRDS("data/processed/analyzed/oil_volatility.rds")
    exch_returns <- readRDS("data/processed/analyzed/exchange_returns.rds")
    diff_rates_change <- readRDS("data/processed/analyzed/interest_rate_diff.rds")
    rel_inflation_vol <- readRDS("data/processed/analyzed/relative_inflation_vol.rds")
    
    log_message("Preparing data for VAR model...\n")
    # Merge all variables into one dataset
    monthly_data <- merge(
      relative_epu,
      vix_returns,
      oil_monthly_vol,
      diff_rates_change,
      rel_inflation_vol,
      exch_returns,
      join = "inner"
    )
    
    # Rename remaining columns
    colnames(monthly_data)[2:6] <- c(
      "VIX_Returns",
      "Oil_Volatility",
      "IR_Diff_Change",
      "Rel_Inflation_Vol",
      "Exchange_Returns"
    )
    
    # Exclude January 2001 and handle NAs
    monthly_data <- monthly_data["2001-02-01/"]
    
    # Check for and report any remaining NAs
    na_summary <- colSums(is.na(monthly_data))
    if (any(na_summary > 0)) {
      log_message("Warning: Found NAs in the following variables:\n")
      for (col in names(na_summary)) {
        if (na_summary[col] > 0) {
          log_message(sprintf("%s: %d NAs\n", col, na_summary[col]))
        }
      }
    }
    
    # Remove any rows with NAs
    monthly_data <- na.omit(monthly_data)
    log_message(sprintf("Final dataset dimensions after NA removal: %d rows x %d columns\n", 
                       nrow(monthly_data), ncol(monthly_data)))
    
    log_message("Performing stationarity tests...\n")
    # Perform ADF tests for stationarity
    adf_results <- list()
    for (col in colnames(monthly_data)) {
      tryCatch({
        adf_results[[col]] <- adf.test(monthly_data[,col], alternative = "stationary")
        log_message(sprintf("ADF test completed for %s\n", col))
      }, error = function(e) {
        log_message(sprintf("Error in ADF test for %s: %s\n", col, e$message))
      })
    }
    
    # Save ADF test results
    saveRDS(adf_results, "output/tables/adf_test_results.rds")
    
    log_message("Selecting optimal lag length...\n")
    # Convert xts to matrix for VAR analysis
    var_data <- as.matrix(monthly_data)
    
    # Find optimal lag length
    lag_select <- VARselect(var_data, lag.max = 12, type = "const")
    optimal_lag_aic <- lag_select$selection["AIC(n)"]
    optimal_lag_bic <- lag_select$selection["SC(n)"]
    
    # Save lag selection results
    saveRDS(lag_select, "output/tables/lag_selection.rds")
    
    log_message("Estimating VAR model...\n")
    # Run VAR model with optimal lag (using AIC)
    var_model <- VAR(var_data, p = optimal_lag_aic, type = "const")
    
    # Save VAR model
    saveRDS(var_model, "output/models/var_model.rds")
    
    log_message("Performing model diagnostics...\n")
    # Check for autocorrelation in residuals
    serial_test <- serial.test(var_model, lags.pt = 12, type = "PT.asymptotic")
    saveRDS(serial_test, "output/tables/serial_test.rds")
    
    # Test for stability
    stability_test <- stability(var_model)
    saveRDS(stability_test, "output/tables/stability_test.rds")
    
    log_message("Generating impulse responses...\n")
    # Generate impulse response functions
    irf_epu <- irf(var_model, impulse = "Relative_EPU", response = c("Exchange_Returns", "VIX_Returns", "Oil_Volatility"),
                   n.ahead = 24, boot = TRUE)
    irf_exch <- irf(var_model, impulse = "Exchange_Returns", response = c("Relative_EPU", "VIX_Returns", "Oil_Volatility"),
                    n.ahead = 24, boot = TRUE)
    
    # Save impulse response results
    saveRDS(irf_epu, "output/models/irf_epu.rds")
    saveRDS(irf_exch, "output/models/irf_exch.rds")
    
    log_message("Calculating variance decomposition...\n")
    # Generate forecast error variance decomposition
    fevd_epu <- fevd(var_model, n.ahead = 24)
    saveRDS(fevd_epu, "output/models/fevd_results.rds")
    
    log_message("✓ Model estimation completed successfully\n")
    
    return(list(
      model = var_model,
      adf_results = adf_results,
      lag_selection = lag_select,
      irf_epu = irf_epu,
      irf_exch = irf_exch,
      fevd_results = fevd_epu,
      serial_test = serial_test,
      stability = stability_test
    ))
    
  }, error = function(e) {
    log_message(sprintf("Error in VAR analysis: %s\n", e$message))
    return(NULL)
  })
}

# Main modeling function
run_modeling <- function() {
  log_message("Starting modeling process...\n")
  
  # Run VAR analysis
  var_results <- run_var_model()
  if (is.null(var_results)) {
    stop("Failed to run VAR analysis")
  }
  
  log_message("✓ Modeling completed successfully\n")
  return(var_results)
}

# Run the modeling process
run_modeling()
