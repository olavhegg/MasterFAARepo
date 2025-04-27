# Data Cleaning Script
# This script handles the import and initial processing of all data sources

# Load required libraries
library(readxl)
library(dplyr)
library(stringr)
library(lubridate)
library(xts)
library(zoo)
library(tseries)
library(vars)

# Source utility functions
source("R/utils.R")

# Set base period for analysis
base_period_start <- as.Date("2001-01-01")
base_period_end   <- as.Date("2024-12-31")

# Import and process Norwegian EPU data
# Read and process all newspaper datasets
vgnettsøk_df         <- process_newspaper("data/raw/vgnettsøk.xlsx",         "data/raw/vgaltannet.xlsx")
dagbladetsøk_df      <- process_newspaper("data/raw/dagbladetsøk.xlsx",      "data/raw/dagbladetaltannet.xlsx")
nrksøk_df            <- process_newspaper("data/raw/nrksøk.xlsx",            "data/raw/nrkaltannet.xlsx")
aftenpostensøk_df    <- process_newspaper("data/raw/aftenpostensøk.xlsx",    "data/raw/aftenpostenaltannet.xlsx")
dnsøk_df             <- process_newspaper("data/raw/dnsøk.xlsx",             "data/raw/dnaltannet.xlsx")
vgpapirsøk_df        <- process_newspaper("data/raw/vgpapirsøk.xlsx",        "data/raw/vgaltannetpapir.xlsx")

# Combine and aggregate the standardized series
df_all <- bind_rows(vgnettsøk_df, dagbladetsøk_df, nrksøk_df, 
                    aftenpostensøk_df, dnsøk_df, vgpapirsøk_df)

epu_monthly <- df_all %>% 
  group_by(Date) %>% 
  summarise(Avg_Std_Fraction = mean(Std_Fraction, na.rm = TRUE)) %>% 
  ungroup()

# Complete the monthly sequence
all_months <- data.frame(Date = seq.Date(from = base_period_start,
                                         to   = base_period_end,
                                         by   = "month"))
epu_monthly <- merge(all_months, epu_monthly, by = "Date", all.x = TRUE)

# Normalize the aggregated series to a base mean of 100
base_period_data <- epu_monthly %>% 
  filter(Date >= base_period_start & Date <= base_period_end)
M <- mean(base_period_data$Avg_Std_Fraction, na.rm = TRUE)
epu_monthly <- epu_monthly %>% 
  mutate(EPU_index = Avg_Std_Fraction * (100 / M))

# Save processed data
saveRDS(epu_monthly, "data/processed/norwegian_epu.rds")

# Import and process Eurozone EPU data
epu_eu <- read_excel("data/raw/EPUEurope.xlsx", sheet = "EPU") %>%
  mutate(Date = my(Date),  
         Year = year(Date))

gdp_eu <- read_excel("data/raw/EPUEurope.xlsx", sheet = "GDP") %>%
  mutate(Year = as.integer(Year))

# Process and save Eurozone EPU data
saveRDS(epu_eu, "data/processed/eurozone_epu.rds")
saveRDS(gdp_eu, "data/processed/eurozone_gdp.rds")

# Import and process VIX data
vix_data <- read_excel("data/raw/VIX.xlsx")
vix_data$Date <- as.Date(vix_data$Date, format="%Y-%m-%d")
saveRDS(vix_data, "data/processed/vix_data.rds")

# Import and process Oil Price data
oil_data <- read_excel("data/raw/BrentOil.xlsx")
oil_data$Date <- as.Date(oil_data$Date, format="%Y-%m-%d")
saveRDS(oil_data, "data/processed/oil_data.rds")

# Import and process Exchange Rate data
exch_data <- read_excel("data/raw/EXR.xlsx")
exch_data$Date <- as.Date(exch_data$Date, format = "%Y-%m-%d")
saveRDS(exch_data, "data/processed/exchange_rate.rds")

# Import and process Interest Rate data
ecb_data <- read_excel("data/raw/ECBDFR.xlsx")
ecb_data$Date <- as.Date(ecb_data$Date, format = "%Y-%m-%d")
saveRDS(ecb_data, "data/processed/ecb_rates.rds")

nb_data <- read_excel("data/raw/IR.xlsx")
nb_data$Date <- as.Date(nb_data$Date, format = "%Y-%m-%d")
saveRDS(nb_data, "data/processed/nb_rates.rds")

# Import and process Inflation data
nor_inflation <- read_excel("data/raw/03013_20250408-140901.xlsx")
nor_inflation <- nor_inflation %>%
  mutate(Date = as.Date(as.yearmon(Date, "%YM%m"), frac = 0)) %>% 
  arrange(Date)
saveRDS(nor_inflation, "data/processed/norwegian_inflation.rds")

eu_inflation <- read_excel("data/raw/prc_hicp_manr__custom_16171528_page_spreadsheet.xlsx")
eu_inflation <- na.omit(eu_inflation)
eu_inflation <- eu_inflation %>%
  mutate(Date = as.Date(as.yearmon(Date, "%Y-%m"), frac = 0)) %>%
  arrange(Date)
saveRDS(eu_inflation, "data/processed/eurozone_inflation.rds")
