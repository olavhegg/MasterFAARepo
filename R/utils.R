# Helper Functions for Data Processing

# Function to convert Norwegian month-year strings to Date
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

# Function to process newspaper EPU data
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
