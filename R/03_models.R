# Modeling Script
# This script performs the VAR analysis and other statistical tests

# Load required libraries
library(vars)
library(tseries)
library(xts)

# Load processed data
relative_epu <- readRDS("data/processed/relative_epu.rds")
vix_returns <- readRDS("data/processed/vix_returns.rds")
oil_monthly_vol <- readRDS("data/processed/oil_volatility.rds")
exch_returns <- readRDS("data/processed/exchange_returns.rds")
diff_rates_change <- readRDS("data/processed/interest_rate_diff.rds")
rel_inflation_vol <- readRDS("data/processed/relative_inflation_vol.rds")

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

# Rename columns
colnames(monthly_data) <- c(
  "Relative_EPU",
  "VIX_Returns",
  "Oil_Volatility",
  "IR_Diff_Change",
  "Rel_Inflation_Vol",
  "Exchange_Returns"
)

# Exclude January 2001
monthly_data <- monthly_data["2001-02-01/"]

# Perform ADF tests for stationarity
adf_results <- list(
  EPU = adf.test(monthly_data$Relative_EPU, alternative = "stationary"),
  VIX = adf.test(monthly_data$VIX_Returns, alternative = "stationary"),
  Oil = adf.test(monthly_data$Oil_Volatility, alternative = "stationary"),
  IR = adf.test(monthly_data$IR_Diff_Change, alternative = "stationary"),
  Inflation = adf.test(monthly_data$Rel_Inflation_Vol, alternative = "stationary"),
  Exchange = adf.test(monthly_data$Exchange_Returns, alternative = "stationary")
)

# Save ADF test results
saveRDS(adf_results, "output/tables/adf_test_results.rds")

# Find optimal lag length
lag_select <- VARselect(monthly_data, lag.max = 12, type = "const")
optimal_lag_aic <- lag_select$selection["AIC(n)"]
optimal_lag_bic <- lag_select$selection["SC(n)"]

# Save lag selection results
saveRDS(lag_select, "output/tables/lag_selection.rds")

# Run VAR model with optimal lag (using AIC)
var_model <- VAR(monthly_data, p = optimal_lag_aic, type = "const")

# Save VAR model
saveRDS(var_model, "output/models/var_model.rds")

# Check for autocorrelation in residuals
serial_test <- serial.test(var_model, lags.pt = 12, type = "PT.asymptotic")
saveRDS(serial_test, "output/tables/serial_test.rds")

# Test for stability
stability_test <- stability(var_model)
saveRDS(stability_test, "output/tables/stability_test.rds")

# Generate impulse response functions
irf_epu <- irf(var_model, impulse = "Relative_EPU", response = c("Exchange_Returns", "VIX_Returns", "Oil_Volatility"),
               n.ahead = 24, boot = TRUE)
irf_exch <- irf(var_model, impulse = "Exchange_Returns", response = c("Relative_EPU", "VIX_Returns", "Oil_Volatility"),
                n.ahead = 24, boot = TRUE)

# Save impulse response results
saveRDS(irf_epu, "output/models/irf_epu.rds")
saveRDS(irf_exch, "output/models/irf_exch.rds")

# Generate forecast error variance decomposition
fevd_epu <- fevd(var_model, n.ahead = 24)
saveRDS(fevd_epu, "output/models/fevd_results.rds")
