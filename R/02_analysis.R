# Analysis Script
# This script processes the cleaned data and prepares it for modeling

# Load required libraries
library(dplyr)
library(xts)
library(zoo)
library(tseries)
library(vars)

log_message("Loading cleaned data...\n")
# Load cleaned data from step 1
epu_monthly <- readRDS("data/processed/cleaned/norwegian_epu.rds")
epu_eu <- readRDS("data/processed/cleaned/eurozone_epu.rds")
gdp_eu <- readRDS("data/processed/cleaned/eurozone_gdp.rds")
vix_data <- readRDS("data/processed/cleaned/vix_data.rds")
oil_data <- readRDS("data/processed/cleaned/oil_data.rds")
exch_data <- readRDS("data/processed/cleaned/exchange_rate.rds")
ecb_data <- readRDS("data/processed/cleaned/ecb_rates.rds")
nb_data <- readRDS("data/processed/cleaned/nb_rates.rds")
nor_inflation <- readRDS("data/processed/cleaned/norwegian_inflation.rds")
eu_inflation <- readRDS("data/processed/cleaned/eurozone_inflation.rds")

log_message("Calculating relative EPU...\n")
# Process Eurozone EPU data
# Reshape the data to long format
epu_long <- epu_eu %>% 
  pivot_longer(-c(Date, Year), names_to = "Country", values_to = "EPU")
gdp_long <- gdp_eu %>% 
  pivot_longer(-Year, names_to = "Country", values_to = "GDP")

# Merge the EPU and GDP by Year and Country
merged_data <- left_join(epu_long, gdp_long, by = c("Year", "Country"))

# Re-base each country's EPU index
rebased_data <- merged_data %>% 
  group_by(Country) %>% 
  mutate(Base_Mean = mean(EPU[Date >= as.Date("2001-01-01") & Date <= as.Date("2024-12-31")],
                          na.rm = TRUE),
         Rebased_EPU = (EPU / Base_Mean) * 100) %>% 
  ungroup()

# Calculate GDP-weighted Eurozone EPU index
weighted_data <- rebased_data %>% 
  filter(!is.na(Rebased_EPU) & !is.na(GDP)) %>% 
  group_by(Date) %>% 
  mutate(total_GDP = sum(GDP, na.rm = TRUE),
         weight = GDP / total_GDP) %>% 
  ungroup()

epu_weighted <- weighted_data %>% 
  group_by(Date) %>% 
  summarise(Weighted_EPU = sum(Rebased_EPU * weight, na.rm = TRUE)) %>% 
  ungroup()

# Normalize the weighted index
base_period_eu <- epu_weighted %>% 
  filter(Date >= as.Date("2001-01-01") & Date <= as.Date("2024-12-31"))
base_mean_eu <- mean(base_period_eu$Weighted_EPU, na.rm = TRUE)
normalization_factor <- 100 / base_mean_eu
epu_weighted <- epu_weighted %>% 
  mutate(Weighted_EPU_normalized = Weighted_EPU * normalization_factor)

# Calculate relative EPU
relative_epu <- xts(
  epu_monthly$EPU_index - epu_weighted$Weighted_EPU_normalized,
  order.by = epu_monthly$Date
)
colnames(relative_epu) <- "Relative_EPU"

log_message("Processing financial market data...\n")
# Process VIX and Oil data
vix_xts <- xts(vix_data$VIX, order.by = vix_data$Date)
oil_xts <- xts(oil_data$OilPrice, order.by = oil_data$Date)

# Calculate returns and volatility
vix_returns <- diff(log(vix_xts))
oil_returns <- diff(log(oil_xts))
oil_monthly_vol <- apply.monthly(oil_returns, sd, na.rm = TRUE)
index(oil_monthly_vol) <- as.Date(format(index(oil_monthly_vol), "%Y-%m-01"))

# Process Exchange Rate data
exch_xts <- xts(exch_data$EURNOK, order.by = exch_data$Date)
exch_returns <- diff(log(exch_xts))

# Process Interest Rate data
ecb_xts <- xts(ecb_data$ECBRATE, order.by = ecb_data$Date)
nb_xts <- xts(nb_data$Rate, order.by = nb_data$Date)

# Calculate interest rate differential
merged_rates <- merge(ecb_xts, nb_xts, join = "inner")
colnames(merged_rates) <- c("ECB_Rate", "NB_Rate")
diff_rates <- merged_rates$NB_Rate - merged_rates$ECB_Rate
colnames(diff_rates) <- "Rate difference"
diff_rates_change <- diff(diff_rates)

# Process Inflation data
nor_inflation <- nor_inflation %>%
  mutate(InflationRate = YoY,
         InflationVol = rollapply(InflationRate, 
                                  width = 12, 
                                  FUN = sd, 
                                  align = "right", 
                                  fill = NA, 
                                  na.rm = TRUE))

eu_inflation <- eu_inflation %>%
  mutate(InflationRate = YoY,
         InflationVol = rollapply(InflationRate,
                                  width = 12,
                                  FUN = sd,
                                  align = "right",
                                  fill = NA,
                                  na.rm = TRUE))

# Calculate relative inflation volatility
nor_inflation_xts <- xts(nor_inflation$InflationVol, order.by = nor_inflation$Date)
eu_inflation_xts <- xts(eu_inflation$InflationVol, order.by = eu_inflation$Date)
inflation_vol_combined <- merge(nor_inflation_xts, eu_inflation_xts, join = "inner")
colnames(inflation_vol_combined) <- c("Norway_InflationVol", "Euro_InflationVol")
rel_inflation_vol <- inflation_vol_combined[, "Norway_InflationVol"] - inflation_vol_combined[, "Euro_InflationVol"]

log_message("Saving analysis results...\n")
# Save processed data for modeling
saveRDS(relative_epu, "data/processed/analyzed/relative_epu.rds")
saveRDS(vix_returns, "data/processed/analyzed/vix_returns.rds")
saveRDS(oil_monthly_vol, "data/processed/analyzed/oil_volatility.rds")
saveRDS(exch_returns, "data/processed/analyzed/exchange_returns.rds")
saveRDS(diff_rates_change, "data/processed/analyzed/interest_rate_diff.rds")
saveRDS(rel_inflation_vol, "data/processed/analyzed/relative_inflation_vol.rds")

log_message("✓ Analysis completed successfully\n")
