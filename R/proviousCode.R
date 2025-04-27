#KODE ####################################################################
###############################################################################
# LOAD LIBRARIES  
#########################################################
# LOAD LIBRARIES
###############################################################################
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




###############################################################################
# HELPER FUNCTION FOR EPU DATA HANDLING: Convert Norwegian month-year strings to Date
###############################################################################
convert_nor_month_year <- function(x) {
  x <- trimws(x)
  # Dictionary mapping Norwegian month abbreviations/names to numeric month strings.
  month_dict <- c(
    "Jan." = "01", "Feb." = "02", "Mars" = "03", "Mar." = "03",
    "Apr." = "04", "Mai"  = "05", "Juni" = "06", "Juli" = "07",
    "Aug." = "08", "Sep." = "09", "Sept." = "09", "Okt." = "10",
    "Nov." = "11", "Des." = "12"
  )
  
  pattern <- "^(\\S+)\\s+(\\d{4})$"
  month_part <- str_replace(x, pattern, "\\1")
  year_part  <- str_replace(x, pattern, "\\2")
  numeric_month <- unname(month_dict[month_part])
  date_string <- ifelse(!is.na(numeric_month),
                        paste0(year_part, "-", numeric_month, "-01"),
                        NA)
  as.Date(date_string)
}

###############################################################################
# SET BASE PERIOD FOR ANALYSIS (2001-01-01 to 2024-12-31)
###############################################################################
base_period_start <- as.Date("2001-01-01")
base_period_end   <- as.Date("2024-12-31")

###############################################################################
# Norwegian EPU INDEX
###############################################################################
# FUNCTION TO READ, PROCESS, AND STANDARDIZE EACH NEWSPAPER SERIES
process_newspaper <- function(file_path_EPU, file_path_All, skip_val = 1) {
  # Read Excel files for EPU counts and total article counts
  df_EPU <- read_excel(file_path_EPU, col_names = FALSE, skip = skip_val)
  df_All <- read_excel(file_path_All, col_names = FALSE, skip = skip_val)
  
  # Rename columns
  names(df_EPU) <- c("Date", "EPU_Count")
  names(df_All) <- c("Date", "All_Count")
  
  # Convert the Date column
  df_EPU$Date <- convert_nor_month_year(df_EPU$Date)
  df_All$Date <- convert_nor_month_year(df_All$Date)
  
  # Compute the fraction of EPU articles for each month
  df <- df_EPU %>%
    mutate(All_Count = df_All$All_Count,
           Fraction = EPU_Count / All_Count)
  
  # Compute the standard deviation over the base period for this newspaper
  sigma_i <- sd(df$Fraction[df$Date >= base_period_start & df$Date <= base_period_end],
                na.rm = TRUE)
  
  # Create a standardized series: divide each fraction by the base period's sigma
  df <- df %>% mutate(Std_Fraction = Fraction / sigma_i)
  
  # Return only the Date and standardized fraction columns
  return(dplyr::select(df, Date, Std_Fraction))
}

# READ AND PROCESS ALL NEWSPAPER DATASETS
vgnettsøk_df         <- process_newspaper("~/Downloads/vgnettsøk.xlsx",         "~/Downloads/vgaltannet.xlsx")
dagbladetsøk_df      <- process_newspaper("~/Downloads/dagbladetsøk.xlsx",      "~/Downloads/dagbladetaltannet.xlsx")
nrksøk_df            <- process_newspaper("~/Downloads/nrksøk.xlsx",            "~/Downloads/nrkaltannet.xlsx")
aftenpostensøk_df    <- process_newspaper("~/Downloads/aftenpostensøk.xlsx",    "~/Downloads/aftenpostenaltannet.xlsx")
dnsøk_df             <- process_newspaper("~/Downloads/dnsøk.xlsx",             "~/Downloads/dnaltannet.xlsx")
vgpapirsøk_df        <- process_newspaper("~/Downloads/vgpapirsøk.xlsx",        "~/Downloads/vgaltannetpapir.xlsx")

# COMBINE AND AGGREGATE THE STANDARDIZED SERIES
df_all <- bind_rows(vgnettsøk_df, dagbladetsøk_df, nrksøk_df, 
                    aftenpostensøk_df, dnsøk_df, vgpapirsøk_df)

epu_monthly <- df_all %>% 
  group_by(Date) %>% 
  summarise(Avg_Std_Fraction = mean(Std_Fraction, na.rm = TRUE)) %>% 
  ungroup()

# COMPLETE THE MONTHLY SEQUENCE
all_months <- data.frame(Date = seq.Date(from = base_period_start,
                                         to   = base_period_end,
                                         by   = "month"))
epu_monthly <- merge(all_months, epu_monthly, by = "Date", all.x = TRUE)

# NORMALIZE THE AGGREGATED SERIES TO A BASE MEAN OF 100
base_period_data <- epu_monthly %>% 
  filter(Date >= base_period_start & Date <= base_period_end)
M <- mean(base_period_data$Avg_Std_Fraction, na.rm = TRUE)
epu_monthly <- epu_monthly %>% 
  mutate(EPU_index = Avg_Std_Fraction * (100 / M))


###############################################################################
# Eurozone GDP-WEIGHTED EPU INDEX (Country-level)
###############################################################################
# Read the EPU sheet for Europe
epu_eu <- read_excel("~/Downloads/EPUEurope.xlsx", sheet = "EPU") %>%
  mutate(Date = my(Date),  
         Year = year(Date))

# Read the GDP sheet (contains annual GDP data from 2001 to 2024)
gdp_eu <- read_excel("~/Downloads/EPUEurope.xlsx", sheet = "GDP") %>%
  mutate(Year = as.integer(Year))

# Reshape the data to long format
epu_long <- epu_eu %>% 
  pivot_longer(-c(Date, Year), names_to = "Country", values_to = "EPU")
gdp_long <- gdp_eu %>% 
  pivot_longer(-Year, names_to = "Country", values_to = "GDP")

# Merge the EPU and GDP by Year and Country
merged_data <- left_join(epu_long, gdp_long, by = c("Year", "Country"))

# Re-base each country's EPU index over 2001-2024 so that its mean is 100, in order
# to make it comparable with the Norwegian EPU index
rebased_data <- merged_data %>% 
  group_by(Country) %>% 
  mutate(Base_Mean = mean(EPU[Date >= as.Date("2001-01-01") & Date <= as.Date("2024-12-31")],
                          na.rm = TRUE),
         Rebased_EPU = (EPU / Base_Mean) * 100) %>% 
  ungroup()


# For each month, compute GDP weights (using only countries with available data) 
# and calculate the weighted Eurozone EPU index
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

# Normalize the weighted index to have a base period mean of 100
base_period_eu <- epu_weighted %>% 
  filter(Date >= as.Date("2001-01-01") & Date <= as.Date("2024-12-31"))
base_mean_eu <- mean(base_period_eu$Weighted_EPU, na.rm = TRUE)
normalization_factor <- 100 / base_mean_eu
epu_weighted <- epu_weighted %>% 
  mutate(Weighted_EPU_normalized = Weighted_EPU * normalization_factor)


# --- Merge the two indices for combined plotting & correlation ---
combined <- merge(
  epu_monthly[, c("Date", "EPU_index")],
  epu_weighted[, c("Date", "Weighted_EPU_normalized")],
  by = "Date"
)

# Calculate the correlation between the two indices
index_correlation <- cor(combined$EPU_index, combined$Weighted_EPU_normalized, use = "complete.obs")
print(paste("Correlation between Norwegian and Eurozone indices:", round(index_correlation, 4)))


# Norwegian EPU Index Plot
NorwayEPU <- ggplot(epu_monthly, aes(x = Date, y = EPU_index)) +
  geom_line(color = "#1D3557", linewidth = 1.3, alpha = 0.9) +
  labs(
    title = "Norwegian Economic Policy Uncertainty (EPU) Index",
    subtitle = "Monthly data (Base Mean = 100)",
    x = "Date", y = "EPU Index"
  ) +
  scale_x_date(date_breaks = "3 years", date_labels = "%Y") +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 17, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 13),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_text(size = 13),
    legend.position = "none"
  )

# Eurozone EPU Index Plot
EUROEPU <- ggplot(epu_weighted, aes(x = Date, y = Weighted_EPU_normalized)) +
  geom_line(color = "#E63946", linewidth = 1.3, alpha = 0.9) +
  labs(
    title = "Eurozone GDP-Weighted Economic Policy Uncertainty (EPU) Index",
    subtitle = "Monthly data (Base Mean = 100)",
    x = "Date", y = "EPU Index"
  ) +
  scale_x_date(date_breaks = "3 years", date_labels = "%Y") +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 17, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 13),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_text(size = 13),
    legend.position = "none"
  )

# Combined EPU Index Plot
EPU_combined <- ggplot(combined, aes(x = Date)) +
  geom_line(aes(y = EPU_index, color = "Norwegian EPU"), linewidth = 1.2) +
  geom_line(aes(y = Weighted_EPU_normalized, color = "Eurozone EPU"), linewidth = 1.2) +
  labs(
    title = "Comparison of Norwegian and Eurozone EPU Indices",
    subtitle = "Normalized monthly indices (Base Mean = 100)",
    x = "Date", y = "Index", color = "Index"
  ) +
  scale_color_manual(values = c("Norwegian EPU" = "#1D3557", "Eurozone EPU" = "#E63946")) +
  scale_x_date(date_breaks = "3 years", date_labels = "%Y") +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 17, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 13),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_text(size = 13),
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 12)
  )

# Display all three plots
print(NorwayEPU)
print(EUROEPU)
print(EPU_combined)


# --- Convert the European EPU data frame into an xts object ---
epu_weighted_xts <- xts(epu_weighted$Weighted_EPU_normalized, order.by = epu_weighted$Date)
colnames(epu_weighted_xts) <- "Weighted_EPU_Normalized"

epu_monthly_xts <- xts(epu_monthly$EPU_index, order.by = epu_monthly$Date)


# --- Rename the column of the Norwegian EPU xts for clarity ---
colnames(epu_monthly_xts) <- "Norwegian_EPU"

# --- Merge the two xts objects on common dates ---
# Use an inner join so that only dates available in both series are kept.
merged_epu <- merge(epu_monthly_xts, epu_weighted_xts, join = "inner")

# --- Calculate Relative EPU ---
# Relative_EPU is defined as Norwegian_EPU minus Weighted_EPU_Normalized.
relative_epu <- merged_epu[, "Norwegian_EPU"] - merged_epu[, "Weighted_EPU_Normalized"]
colnames(relative_epu) <- "Relative_EPU"

# --- Preview the resulting relative EPU series ---
head(relative_epu)





###############################################################################
# IMPORT CONTROL VARIABLES TO BE USED IN ANALYSIS
###############################################################################



###############################################################################
# IMPORT VIX DATA
###############################################################################
vix_data <- read_excel("~/Downloads/VIX.xlsx")
# Convert the Date column to Date type
vix_data$Date <- as.Date(vix_data$Date, format="%Y-%m-%d")
# Check the data structure
str(vix_data)

# Assuming the VIX values are in a column named "VIX"
# Convert the data frame to an xts object
vix_xts <- xts(vix_data$VIX, order.by = vix_data$Date)
head(vix_xts)

###############################################################################
# IMPORT OIL PRICE DATA
###############################################################################
oil_data <- read_excel("~/Downloads/BrentOil.xlsx")
# Convert the Date column to Date type
oil_data$Date <- as.Date(oil_data$Date, format="%Y-%m-%d")
# Check the data structure
str(oil_data)

oil_xts <- xts(oil_data$OilPrice, order.by = oil_data$Date)
head(oil_xts)

# Since VIX and oil price data are only available on trading days,
# the xts objects naturally only include those dates
head(index(vix_xts))
head(index(oil_xts))

###############################################################################
# CALCULATING DAILY RETURNS
###############################################################################
vix_returns <- diff(log(vix_xts))
oil_returns <- diff(log(oil_xts))


###############################################################################
# IMPORT EXCHANGE RATE DATA
###############################################################################
# Import exchange rate data
exch_data <- read_excel("~/Downloads/EXR.xlsx")

# Convert the Date column to Date type
exch_data$Date <- as.Date(exch_data$Date, format = "%Y-%m-%d")


# Convert the data frame to an xts object
exch_xts <- xts(exch_data$EURNOK, order.by = exch_data$Date)


# Merge with VIX and Oil
combined_data <- merge(exch_xts, vix_xts, oil_xts, join = "inner")
colnames(combined_data) <- c("EURNOK", "VIX", "OilPrice")

combined_clean <- na.omit(combined_data)

###############################################################################
# CREATE PLOTS FOR EXR, VIX, AND OIL
###############################################################################
# Convert the xts objects to data frames for plotting
exch_df <- data.frame(Date = index(exch_xts), EURNOK = coredata(exch_xts))
vix_df   <- data.frame(Date = index(vix_xts), VIX = coredata(vix_xts))
oil_df   <- data.frame(Date = index(oil_xts), OilPrice = coredata(oil_xts))


# --- Exchange Rate Chart (EUR/NOK) ---
plot_exch <- ggplot(exch_df, aes(x = Date, y = EURNOK)) +
  geom_line(color = "#1D3557", linewidth = 1.2, alpha = 0.9) +
  labs(
    title = "Exchange Rate: EUR/NOK",
    subtitle = "Daily spot exchange rate",
    x = "Date", y = "EUR per NOK"
  ) +
  scale_x_date(date_breaks = "3 years", date_labels = "%Y") +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold", size = 17, hjust = 0.5),
    plot.subtitle = element_text(size = 13, hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# --- VIX Chart ---
plot_vix <- ggplot(vix_df, aes(x = Date, y = VIX)) +
  geom_line(color = "#E63946", linewidth = 1.2, alpha = 0.9) +
  labs(
    title = "VIX Index (Market Volatility)",
    subtitle = "Daily closing values",
    x = "Date", y = "VIX Level"
  ) +
  scale_x_date(date_breaks = "3 years", date_labels = "%Y") +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold", size = 17, hjust = 0.5),
    plot.subtitle = element_text(size = 13, hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# --- Oil Price Chart ---
oil_df_omittedna <- na.omit(oil_df)

plot_oil <- ggplot(oil_df_omittedna, aes(x = Date, y = OilPrice)) +
  geom_line(color = "#6A4C93", linewidth = 1.2, alpha = 0.9) +
  labs(
    title = "Crude Oil Prices",
    subtitle = "Daily spot prices (USD per barrel)",
    x = "Date", y = "Price (USD)"
  ) +
  scale_x_date(date_breaks = "3 years", date_labels = "%Y") +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold", size = 17, hjust = 0.5),
    plot.subtitle = element_text(size = 13, hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# --- Show plots ---
print(plot_exch)
print(plot_vix)
print(plot_oil)



###############################################################################
# IMPORT THE ECB RATE DATA
###############################################################################
ecb_data <- read_excel("~/Downloads/ECBDFR.xlsx")
str(ecb_data)

# Convert the Date column to Date type
ecb_data$Date <- as.Date(ecb_data$Date, format = "%Y-%m-%d")

# Convert the data frame to an xts object:
ecb_xts <- xts(ecb_data$ECBRATE, order.by = ecb_data$Date)
head(ecb_xts)

###############################################################################
# IMPORT NORGES BANK RATE DATA
###############################################################################
nb_data <- read_excel("~/Downloads/IR (1).xlsx")
str(nb_data)

# Convert the Date column to Date type
nb_data$Date <- as.Date(nb_data$Date, format = "%Y-%m-%d")

# Convert the NB data to an xts object:
nb_xts <- xts(nb_data$NBRate, order.by = nb_data$Date)
head(nb_xts)

###############################################################################
# MERGE THE TWO SERIES ON COMMON DATES
###############################################################################
merged_rates <- merge(ecb_xts, nb_xts, join = "inner")
colnames(merged_rates) <- c("ECB_Rate", "NB_Rate")

###############################################################################
# CALCULATE THE INTEREST RATE DIFFERENTIAL
###############################################################################
# Interest rate differentiel is defined as NB Rate minus ECB Rate
diff_rates <- merged_rates$NB_Rate - merged_rates$ECB_Rate
colnames(diff_rates) <- "Rate difference"


# Convert the merged xts object to a data frame for plotting
rates_df <- data.frame(
  Date = index(merged_rates),
  ECB_Rate = coredata(merged_rates$ECB_Rate),
  NB_Rate = coredata(merged_rates$NB_Rate)
)

ggplot(rates_df, aes(x = Date)) +
  geom_line(aes(y = ECB_Rate, color = "ECB Deposit Rate"), linewidth = 1.2, alpha = 0.9) +
  geom_line(aes(y = NB_Rate, color = "Norges Bank Rate"), linewidth = 1.2, alpha = 0.9) +
  labs(
    title = "Monetary Policy Rates: Norges Bank vs. ECB",
    subtitle = "Daily rates (2001–2024)",
    x = "Date", y = "Interest Rate (%)",
    color = "Policy Rate"
  ) +
  scale_color_manual(
    values = c("ECB Deposit Rate" = "#E63946", "Norges Bank Rate" = "#1D3557")
  ) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5, margin = margin(b = 10)),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_text(size = 13),
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 12)
  )


###############################################################################
# IMPORT NORWEGIAN INFLATION DATA (2000-2024), 1 year prior to analysis period to calculate 12m sd
###############################################################################
nor_inflation <- read_excel("~/Downloads/03013_20250408-140901.xlsx")

# Convert the Date column from format "2000M01" (or "2001M01") to a Date object (first day of month)
nor_inflation <- nor_inflation %>%
  mutate(Date = as.Date(as.yearmon(Date, "%YM%m"), frac = 0)) %>% 
  arrange(Date)

###############################################################################
# CALCULATE THE MONTHLY INFLATION RATE & ROLLING VOLATILITY
###############################################################################
# Since the YoY column already represents the inflation rate, we set InflationRate = YoY
nor_inflation <- nor_inflation %>%
  mutate(InflationRate = YoY,
         InflationVol = rollapply(InflationRate, 
                                  width = 12, 
                                  FUN = sd, 
                                  align = "right", 
                                  fill = NA, 
                                  na.rm = TRUE))

# Filter out data before 2001
nor_inflation <- nor_inflation %>%
  filter(Date >= as.Date("2001-01-01"))




###############################################################################
# IMPORT EUROZONE INFLATION DATA (2000-2024), 1 year prior to analysis period
###############################################################################
eu_inflation <- read_excel("~/Downloads/prc_hicp_manr__custom_16171528_page_spreadsheet.xlsx")
eu_inflation <- na.omit(eu_inflation)

# Convert the Date column from format "2000-01" to a Date object (first day of month)
eu_inflation <- eu_inflation %>%
  mutate(Date = as.Date(as.yearmon(Date, "%Y-%m"), frac = 0)) %>%
  arrange(Date)

###############################################################################
# CALCULATE THE MONTHLY INFLATION RATE & ROLLING VOLATILITY
###############################################################################
# Since the YoY column already represents the inflation rate, we set InflationRate = YoY
eu_inflation <- eu_inflation %>%
  mutate(InflationRate = YoY,
         InflationVol = rollapply(InflationRate,
                                  width = 12,
                                  FUN = sd,
                                  align = "right",
                                  fill = NA,
                                  na.rm = TRUE))

# Filter out data before 2001
eu_inflation <- eu_inflation %>%
  filter(Date >= as.Date("2001-01-01"))


# Merge on Date, add suffixes to distinguish them
inflation_vol_combined <- full_join(
  nor_inflation %>% dplyr::select(Date, NorwayVol = InflationVol),
  eu_inflation %>% dplyr::select(Date, EuroVol = InflationVol),
  by = "Date"
)










###############################################################################
# HERE WE BEGIN TO ADJUST OUR VARIABLES TO FIT IN THE VAR
###############################################################################

###############################################################################
# AGGREGATE DAILY VIX DATA TO MONTHLY AVERAGE
###############################################################################
# Assuming you have an xts object 'vix_xts' of daily VIX data
vix_monthly <- apply.monthly(vix_xts, mean, na.rm = TRUE)
# Reset the index to the first day of the month rather than the end
index(vix_monthly) <- as.Date(format(index(vix_monthly), "%Y-%m-01"))

###############################################################################
# COMPUTE DAILY OIL PRICE LOG RETURNS AND THEN MONTHLY VOLATILITY
###############################################################################
oil_returns <- diff(log(oil_xts))
# Compute monthly volatility as the standard deviation of daily oil returns
oil_monthly_vol <- apply.monthly(oil_returns, sd, na.rm = TRUE)
# Reset the index to the first day of the month
index(oil_monthly_vol) <- as.Date(format(index(oil_monthly_vol), "%Y-%m-01"))

###############################################################################
# FINAL CHECK: ALIGN ALL MONTHLY VARIABLES
###############################################################################


vix_monthly <- xts(coredata(vix_monthly), order.by = index(vix_monthly))
colnames(vix_monthly) <- "VIX_avg"

oil_monthly_vol <- xts(coredata(oil_monthly_vol), order.by = index(oil_monthly_vol))
colnames(oil_monthly_vol) <- "Oil_Vol"

diff_rates <- xts(coredata(diff_rates), order.by = index(diff_rates))
colnames(diff_rates) <- "IR_Diff"

# using the last trading day, then adjust the index to the first day of the month.
exch_monthly <- apply.monthly(exch_xts, last)
index(exch_monthly) <- as.Date(format(index(exch_monthly), "%Y-%m-01"))
colnames(exch_monthly) <- "ExchangeRate"

# Convert daily diff_rates to monthly by taking the last observation
diff_rates_monthly <- apply.monthly(diff_rates, last)
index(diff_rates_monthly) <- as.Date(format(index(diff_rates_monthly), "%Y-%m-01"))
colnames(diff_rates_monthly) <- "IR_Diff"



###############################################################################
# (f) PROCESS INFLATION VOLATILITY: Create a relative inflation volatility measure
###############################################################################


# Convert them to xts objects
nor_inflation_xts <- xts(nor_inflation$InflationVol, order.by = nor_inflation$Date)
colnames(nor_inflation_xts) <- "Norway_InflationVol"

eu_inflation_xts <- xts(eu_inflation$InflationVol, order.by = eu_inflation$Date)
colnames(eu_inflation_xts) <- "Euro_InflationVol"

# Merge the two inflation volatility series on Date (inner join for common dates)
inflation_vol_combined <- merge(nor_inflation_xts, eu_inflation_xts, join = "inner")
# Compute the relative inflation volatility as the difference (Norway minus Eurozone)
Rel_Inflation_Vol <- inflation_vol_combined[, "Norway_InflationVol"] - inflation_vol_combined[, "Euro_InflationVol"]
# Bind this relative measure as an additional column
inflation_vol_rel <- cbind(inflation_vol_combined, Rel_Inflation_Vol)
colnames(inflation_vol_rel)[ncol(inflation_vol_rel)] <- "Rel_Inflation_Vol"
# For the VAR, you may choose to use only the relative measure.
# For example, we can extract the relative series:
inflation_vol_xts_final <- inflation_vol_rel[, "Rel_Inflation_Vol"]

###############################################################################
# MERGE ALL MONTHLY VARIABLES INTO ONE DATASET
###############################################################################
# Now we have the following xts objects (all with the same date index formatted as the first day of the month):
# - relative_epu: Relative EPU_Index (Norway - Eurozone)
# - vix_monthly: VIX_avg
# - oil_monthly_vol: Oil_Vol
# - diff_rates: IR_Diff
# - inflation_vol_xts_final: Rel_Inflation_Vol
# - exch_monthly: ExchangeRate

# We merge these using an inner join:
monthly_data <- do.call(merge, list(relative_epu, vix_monthly,
                                    oil_monthly_vol, diff_rates_monthly,
                                    inflation_vol_xts_final, exch_monthly))
# Rename the columns
colnames(monthly_data) <- c("Relative_EPU", "VIX_avg", "Oil_Vol", "IR_Diff", "Rel_Inflation_Vol", "ExchangeRate")

# Preview the final merged monthly data
head(monthly_data)

# Exclude January 2001: keep data starting from February 1, 2001
monthly_data <- monthly_data["2001-02-01/"]




###############################################################################
# PERFORM ADF TESTS
###############################################################################


var_data_matrix <- coredata(monthly_data)

adf_EPU <- adf.test(var_data_matrix[, "Relative_EPU"], alternative = "stationary")
print(adf_EPU)

adf_VIX <- adf.test(var_data_matrix[, "VIX_avg"], alternative = "stationary")
print(adf_VIX)

adf_Oil <- adf.test(var_data_matrix[, "Oil_Vol"], alternative = "stationary")
print(adf_Oil)

adf_IR <- adf.test(var_data_matrix[, "IR_Diff"], alternative = "stationary")
print(adf_IR)

adf_Inflation <- adf.test(var_data_matrix[, "Rel_Inflation_Vol"], alternative = "stationary")
print(adf_Inflation)

adf_Exchange <- adf.test(var_data_matrix[, "ExchangeRate"], alternative = "stationary")
print(adf_Exchange)




# --- Transforming the Exchange Rate Series ---
# Since exch_monthly contains the level of the exchange rate (EUR/NOK),
# we compute the log returns to induce stationarity.
exch_returns <- diff(log(exch_monthly))
colnames(exch_returns) <- "Exchange_Returns"
exch_returns <- na.omit(exch_returns)



# Check stationarity of the exchange rate returns using the ADF test
adf_exch_returns <- adf.test(coredata(exch_returns), alternative = "stationary")
print(adf_exch_returns)

# --- Transforming the Interest Rate Differential ---
# Your original monthly IR differential is in diff_rates_monthly (NB_Rate - ECB_Rate)
# The ADF test on diff_rates_monthly suggested non-stationarity.
# Taking the first difference typically helps:
ir_diff_change <- diff(diff_rates_monthly)
colnames(ir_diff_change) <- "IR_Diff_Change"
ir_diff_change <- na.omit(ir_diff_change)


# Check stationarity of the differenced interest rate differential using the ADF test
adf_ir_diff_change <- adf.test(coredata(ir_diff_change), alternative = "stationary")
print(adf_ir_diff_change)




# --- Merge the Stationary Variables ---
# Because the diff() operations drop the first observation, an inner join is used to 
# ensure that all series are aligned to the same dates.
var_data_stationary <- merge(
  relative_epu,            # Relative EPU
  vix_monthly,             # VIX_avg
  oil_monthly_vol,         # Oil_Vol
  ir_diff_change,          # IR_Diff_Change (stationary version of IR differential)
  inflation_vol_xts_final, # Rel_Inflation_Vol
  exch_returns,            # Exchange_Returns (stationary version of exchange rate)
  join = "inner"
)

# --- Rename the columns for clarity ---
colnames(var_data_stationary) <- c(
  "Relative_EPU", "VIX_avg", "Oil_Vol", 
  "IR_Diff_Change", "Rel_Inflation_Vol", "Exchange_Returns"
)


# Exclude January 2001: keep data starting from February 1, 2001
var_data_stationary <- var_data_stationary["2001-02-01/"]


# Preview the final merged monthly data
head(var_data_stationary)


###############################################################################
# FIND OPTIMAL LAG LENGTH
###############################################################################


# Run the lag selection on your stationary VAR dataset.
lag_select <- VARselect(var_data_stationary, lag.max = 12, type = "const")

# Extract the optimal lag orders for AIC and BIC (also referred to as SC)
optimal_lag_aic <- lag_select$selection["AIC(n)"]
optimal_lag_bic <- lag_select$selection["SC(n)"]

cat("Optimal lag order according to AIC:", optimal_lag_aic, "\n")
cat("Optimal lag order according to BIC:", optimal_lag_bic, "\n")



###############################################################################
# RUN VAR
###############################################################################

# VAR model using optimal lag (2)
var_model_aic <- VAR(var_data_stationary, p = 6, type = "const")


# Print summary
cat("\nSummary for VAR model selected by AIC:\n")
print(summary(var_model_aic))

# Check for autocorrelation in residuals
serial_test <- serial.test(var_model_aic, lags.pt = 12, type = "PT.asymptotic")
print(serial_test)

# Test for stability
stability_plot <- stability(var_model_aic)
plot(stability_plot)

