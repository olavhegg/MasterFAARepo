# Data Cleaning Script
# This script cleans and processes all raw data files

# Suppress "New names" messages and other unnecessary output
options(readr.show_col_types = FALSE)
options(readxl.show_col_types = FALSE)
options(readxl.show_progress = FALSE)
options(readxl.show_names = FALSE)  # This will suppress the "New names" messages

# Create directories if they don't exist
dir.create("data/processed/cleaned", recursive = TRUE, showWarnings = FALSE)
dir.create("data/processed/analyzed", recursive = TRUE, showWarnings = FALSE)

# Process Norwegian EPU data
log_message("Processing Norwegian EPU data...\n")
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
saveRDS(epu_monthly, "data/processed/cleaned/norwegian_epu.rds")

# Process Eurozone EPU data
log_message("Processing Eurozone EPU data...\n")
epu_eu <- suppressMessages(read_excel("data/raw/EPUEurope.xlsx", sheet = "EPU", col_names = TRUE)) %>%
  mutate(Date = my(Date),  
         Year = year(Date))

gdp_eu <- suppressMessages(read_excel("data/raw/EPUEurope.xlsx", sheet = "GDP", col_names = TRUE)) %>%
  mutate(Year = as.integer(Year))

# Process and save Eurozone EPU data
saveRDS(epu_eu, "data/processed/cleaned/eurozone_epu.rds")
saveRDS(gdp_eu, "data/processed/cleaned/eurozone_gdp.rds")

# Process VIX data
log_message("Processing VIX data...\n")
vix_data <- suppressMessages(read_excel("data/raw/VIX.xlsx", col_names = TRUE))
vix_data$Date <- as.Date(vix_data$Date, format="%Y-%m-%d")
saveRDS(vix_data, "data/processed/cleaned/vix_data.rds")

# Process Oil Price data
log_message("Processing Oil price data...\n")
oil_data <- suppressMessages(read_excel("data/raw/BrentOil.xlsx", col_names = TRUE))
oil_data$Date <- as.Date(oil_data$Date, format="%Y-%m-%d")
saveRDS(oil_data, "data/processed/cleaned/oil_data.rds")

# Process Exchange Rate data
log_message("Processing Exchange rate data...\n")
exch_data <- suppressMessages(read_excel("data/raw/EXR.xlsx", col_names = TRUE))
exch_data$Date <- as.Date(exch_data$Date, format = "%Y-%m-%d")
saveRDS(exch_data, "data/processed/cleaned/exchange_rate.rds")

# Process ECB interest rates
log_message("Processing ECB interest rates...\n")
ecb_data <- suppressMessages(read_excel("data/raw/ECBDFR.xlsx", col_names = TRUE))
ecb_data$Date <- as.Date(ecb_data$Date, format = "%Y-%m-%d")
saveRDS(ecb_data, "data/processed/cleaned/ecb_rates.rds")

# Process Norges Bank interest rate data
log_message("Processing Norges Bank interest rates...\n")
nb_data <- suppressMessages(read_excel("data/raw/IR.xlsx", skip = 6, col_names = c("Date", "Rate")))

# Convert date and rate columns
nb_data$Date <- as.yearmon(nb_data$Date, "%Y-%m")
nb_data$Date <- as.Date(nb_data$Date)
nb_data$Rate <- as.numeric(nb_data$Rate)

# Remove any NA values
nb_data_clean <- na.omit(nb_data)
saveRDS(nb_data_clean, "data/processed/cleaned/nb_rates.rds")

# Process Inflation data
log_message("Processing inflation data...\n")
nor_inflation <- read_excel("data/raw/03013_20250408-140901.xlsx", col_names = TRUE)
nor_inflation <- nor_inflation %>%
  mutate(Date = as.Date(as.yearmon(Date, "%YM%m"), frac = 0)) %>% 
  arrange(Date)
saveRDS(nor_inflation, "data/processed/cleaned/norwegian_inflation.rds")

eu_inflation <- read_excel("data/raw/prc_hicp_manr__custom_16171528_page_spreadsheet.xlsx", col_names = TRUE)
eu_inflation <- na.omit(eu_inflation)
eu_inflation <- eu_inflation %>%
  mutate(Date = as.Date(as.yearmon(Date, "%Y-%m"), frac = 0)) %>%
  arrange(Date)
saveRDS(eu_inflation, "data/processed/cleaned/eurozone_inflation.rds")

log_message("✓ All data cleaned and saved to data/processed/cleaned/\n")
