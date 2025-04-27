# Visualization Script
# This script generates plots and figures from the analysis results

# Load required libraries
library(ggplot2)
library(xts)
library(vars)
library(dplyr)
library(tidyr)

# Create theme for consistent plotting
theme_custom <- function() {
  theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10),
      legend.title = element_text(face = "bold"),
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      strip.background = element_blank(),
      strip.text = element_text(face = "bold")
    )
}

log_message("Loading data and model results...\n")
# Load analyzed data
tryCatch({
  # Load analyzed data
  relative_epu <- readRDS("data/processed/analyzed/relative_epu.rds")
  vix_returns <- readRDS("data/processed/analyzed/vix_returns.rds")
  oil_monthly_vol <- readRDS("data/processed/analyzed/oil_volatility.rds")
  exch_returns <- readRDS("data/processed/analyzed/exchange_returns.rds")
  diff_rates_change <- readRDS("data/processed/analyzed/interest_rate_diff.rds")
  rel_inflation_vol <- readRDS("data/processed/analyzed/relative_inflation_vol.rds")
  
  # Load model results
  var_model <- readRDS("output/models/var_model.rds")
  irf_epu <- readRDS("output/models/irf_epu.rds")
  irf_exch <- readRDS("output/models/irf_exch.rds")
  fevd_epu <- readRDS("output/models/fevd_results.rds")
  
  log_message("✓ Data loaded successfully\n")
}, error = function(e) {
  log_message(sprintf("Error loading data: %s\n", e$message))
  stop("Failed to load data")
})

log_message("Generating relative EPU plot...\n")
tryCatch({
  # Convert xts to data frame for plotting
  relative_epu_df <- data.frame(
    Date = index(relative_epu),
    Relative_EPU = coredata(relative_epu)[,1]
  )
  
  # Plot 2: Relative EPU
  plot_relative_epu <- ggplot(relative_epu_df, aes(x = Date, y = Relative_EPU)) +
    geom_line(color = "#1D3557", size = 1) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    labs(
      title = "Relative EPU (Norway - Eurozone)",
      subtitle = "Difference between Norwegian and Eurozone EPU indices",
      x = "Date",
      y = "Relative EPU"
    ) +
    theme_custom()
  
  # Save plot
  ggsave("output/figures/relative_epu.png", plot_relative_epu, width = 10, height = 6, dpi = 300)
  log_message("✓ Relative EPU plot saved\n")
}, error = function(e) {
  log_message(sprintf("Error generating relative EPU plot: %s\n", e$message))
})

log_message("Generating impulse response plots...\n")
tryCatch({
  # Convert IRF results to data frames for plotting
  irf_epu_df <- data.frame(
    Horizon = 1:length(irf_epu$irf$Relative_EPU),
    Exchange = irf_epu$irf$Relative_EPU[, "Exchange_Returns"],
    VIX = irf_epu$irf$Relative_EPU[, "VIX_Returns"],
    Oil = irf_epu$irf$Relative_EPU[, "Oil_Volatility"]
  )
  
  # Plot IRF for EPU shocks
  plot_irf_epu <- ggplot(irf_epu_df, aes(x = Horizon)) +
    geom_line(aes(y = Exchange, color = "Exchange Rate"), size = 1) +
    geom_line(aes(y = VIX, color = "VIX"), size = 1) +
    geom_line(aes(y = Oil, color = "Oil Volatility"), size = 1) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    labs(
      title = "Impulse Response to EPU Shock",
      subtitle = "Response of key variables to a one-standard-deviation shock in EPU",
      x = "Horizon (months)",
      y = "Response",
      color = "Variable"
    ) +
    scale_color_manual(values = c("Exchange Rate" = "#1D3557", "VIX" = "#E63946", "Oil Volatility" = "#6A4C93")) +
    theme_custom()
  
  # Save plot
  ggsave("output/figures/irf_epu.png", plot_irf_epu, width = 10, height = 6, dpi = 300)
  log_message("✓ Impulse response plot saved\n")
}, error = function(e) {
  log_message(sprintf("Error generating impulse response plot: %s\n", e$message))
})

log_message("Generating variance decomposition plot...\n")
tryCatch({
  # Convert FEVD results to data frame for plotting
  fevd_df <- data.frame(
    Horizon = 1:length(fevd_epu$Relative_EPU),
    EPU = as.numeric(fevd_epu$Relative_EPU),
    Exchange = as.numeric(fevd_epu$Exchange_Returns),
    VIX = as.numeric(fevd_epu$VIX_Returns),
    Oil = as.numeric(fevd_epu$Oil_Volatility)
  )
  
  # Plot FEVD
  plot_fevd <- ggplot(fevd_df, aes(x = Horizon)) +
    geom_line(aes(y = EPU, color = "EPU"), size = 1) +
    geom_line(aes(y = Exchange, color = "Exchange Rate"), size = 1) +
    geom_line(aes(y = VIX, color = "VIX"), size = 1) +
    geom_line(aes(y = Oil, color = "Oil Volatility"), size = 1) +
    labs(
      title = "Forecast Error Variance Decomposition",
      subtitle = "Proportion of forecast error variance explained by each variable",
      x = "Horizon (months)",
      y = "Proportion of Variance",
      color = "Variable"
    ) +
    scale_color_manual(values = c(
      "EPU" = "#1D3557", 
      "Exchange Rate" = "#E63946", 
      "VIX" = "#6A4C93", 
      "Oil Volatility" = "#457B9D"
    )) +
    theme_custom()
  
  # Save plot
  ggsave("output/figures/fevd.png", plot_fevd, width = 10, height = 6, dpi = 300)
  log_message("✓ Variance decomposition plot saved\n")
}, error = function(e) {
  log_message(sprintf("Error generating variance decomposition plot: %s\n", e$message))
})

log_message("Creating summary statistics...\n")
tryCatch({
  # Convert xts objects to numeric vectors for summary statistics
  relative_epu_vec <- as.numeric(relative_epu)
  vix_returns_vec <- as.numeric(vix_returns)
  oil_monthly_vol_vec <- as.numeric(oil_monthly_vol)
  diff_rates_change_vec <- as.numeric(diff_rates_change)
  rel_inflation_vol_vec <- as.numeric(rel_inflation_vol)
  exch_returns_vec <- as.numeric(exch_returns)
  
  # Create a summary table of key statistics
  summary_stats <- data.frame(
    Variable = c("Relative EPU", "VIX Returns", "Oil Volatility", 
                 "Interest Rate Diff", "Inflation Vol", "Exchange Returns"),
    Mean = c(mean(relative_epu_vec, na.rm = TRUE),
             mean(vix_returns_vec, na.rm = TRUE),
             mean(oil_monthly_vol_vec, na.rm = TRUE),
             mean(diff_rates_change_vec, na.rm = TRUE),
             mean(rel_inflation_vol_vec, na.rm = TRUE),
             mean(exch_returns_vec, na.rm = TRUE)),
    SD = c(sd(relative_epu_vec, na.rm = TRUE),
           sd(vix_returns_vec, na.rm = TRUE),
           sd(oil_monthly_vol_vec, na.rm = TRUE),
           sd(diff_rates_change_vec, na.rm = TRUE),
           sd(rel_inflation_vol_vec, na.rm = TRUE),
           sd(exch_returns_vec, na.rm = TRUE))
  )
  
  # Save summary statistics
  write.csv(summary_stats, "output/tables/summary_statistics.csv", row.names = FALSE)
  log_message("✓ Summary statistics saved\n")
}, error = function(e) {
  log_message(sprintf("Error generating summary statistics: %s\n", e$message))
})

log_message("✓ Visualization completed successfully\n")
