# Helper Functions for Data Processing

# Logging functions
initialize_log <- function(log_file = "results/analysis_log.txt") {
  # Ensure the results directory exists
  dir.create(dirname(log_file), showWarnings = FALSE, recursive = TRUE)
  
  # Create a nicely formatted start message
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  start_msg <- paste0(
    "╔══════════════════════════════════════════════════════════════════════════════╗\n",
    "║                     MasterFAA Analysis Log                                    ║\n",
    "╠══════════════════════════════════════════════════════════════════════════════╣\n",
    "║ Started: ", timestamp, "                                                  ║\n",
    "╚══════════════════════════════════════════════════════════════════════════════╝\n\n"
  )
  
  # Console output
  cat(start_msg)
  
  # File output - create new file with explicit encoding
  con <- file(log_file, "w", encoding = "UTF-8")
  tryCatch({
    writeLines(iconv(start_msg, to = "UTF-8"), con = con, sep = "", useBytes = TRUE)
  }, error = function(e) {
    warning("Could not write to log file: ", e$message)
  }, finally = {
    close(con)
  })
}

log_message <- function(..., log_file = "results/analysis_log.txt") {
  # Create message and ensure proper encoding
  msg <- paste0(...)
  msg_encoded <- iconv(msg, to = "UTF-8")
  
  # Console output
  cat(msg)
  
  # File output - append with explicit encoding
  con <- file(log_file, "a", encoding = "UTF-8")
  tryCatch({
    writeLines(msg_encoded, con = con, sep = "", useBytes = TRUE)
  }, error = function(e) {
    warning("Could not write to log file: ", e$message)
  }, finally = {
    close(con)
  })
}

#' Handle errors with logging
#' @param expr Expression to evaluate
#' @param operation_name Name of the operation for logging
#' @return The result of the expression if successful, NULL if failed
handle_error_with_logging <- function(expr, operation_name) {
  tryCatch({
    result <- expr()
    if (is.null(result)) {
      log_message(sprintf("\nOperation '%s' returned NULL, indicating failure\n", operation_name))
      return(NULL)
    }
    return(result)
  }, error = function(e) {
    log_message(sprintf("\nError in %s: %s\n", operation_name, e$message))
    return(NULL)
  })
}

#' Stop on error with logging
#' @param expr Expression to evaluate
#' @param step_name Name of the step for logging
#' @return TRUE if successful, FALSE otherwise
stop_on_error <- function(expr, step_name) {
  success <- handle_error_with_logging(expr, step_name)
  if (!success) {
    log_message(sprintf("\nStep '%s' failed\n", step_name))
    return(FALSE)
  }
  log_message(sprintf("✓ %s completed successfully\n", step_name))
  return(TRUE)
}

# Function to create project directory structure
create_project_dirs <- function() {
  log_message("Creating directory structure...\n")
  dirs_to_create <- c("data/processed", "results", "output/figures", "output/tables")
  for (dir in dirs_to_create) {
    if (!dir.exists(dir)) {
      dir.create(dir, recursive = TRUE)
      log_message("Created directory:", dir, "\n")
    } else {
      log_message("Directory exists:", dir, "\n")
    }
  }
  log_message("\n")
}

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
  result <- handle_error_with_logging(function() {
    # Log the files being processed
    log_message(sprintf("Processing files: %s and %s\n", basename(file_path_EPU), basename(file_path_All)))
    
    # Read Excel files for EPU counts and total article counts
    tryCatch({
      df_EPU <- suppressMessages(read_excel(file_path_EPU, col_names = FALSE, skip = skip_val))
      df_All <- suppressMessages(read_excel(file_path_All, col_names = FALSE, skip = skip_val))
      
      # Validate data frames
      if (!is.data.frame(df_EPU) || !is.data.frame(df_All)) {
        stop("Input files must contain valid data frames")
      }
      
      if (ncol(df_EPU) < 2 || ncol(df_All) < 2) {
        stop("Input files must have at least 2 columns")
      }
      
      # Rename columns
      names(df_EPU) <- c("Date", "EPU_Count")
      names(df_All) <- c("Date", "All_Count")
      
      # Convert the Date column
      df_EPU$Date <- convert_nor_month_year(df_EPU$Date)
      df_All$Date <- convert_nor_month_year(df_All$Date)
      
      # Validate date conversion
      if (any(is.na(df_EPU$Date)) || any(is.na(df_All$Date))) {
        stop("Date conversion failed - check date format in input files")
      }
      
      # Ensure numeric columns
      df_EPU$EPU_Count <- as.numeric(df_EPU$EPU_Count)
      df_All$All_Count <- as.numeric(df_All$All_Count)
      
      # Log any NA values in the counts
      na_epu <- sum(is.na(df_EPU$EPU_Count))
      na_all <- sum(is.na(df_All$All_Count))
      if (na_epu > 0 || na_all > 0) {
        log_message(sprintf("Warning: Found %d NA values in EPU counts and %d NA values in All counts\n", 
                          na_epu, na_all))
      }
      
      # Compute the fraction of EPU articles for each month
      df <- df_EPU %>%
        mutate(All_Count = df_All$All_Count,
               Fraction = EPU_Count / All_Count)
      
      # Compute the standard deviation over the base period for this newspaper
      sigma_i <- sd(df$Fraction[df$Date >= base_period_start & df$Date <= base_period_end],
                    na.rm = TRUE)
      
      if (is.na(sigma_i) || sigma_i == 0) {
        stop("Standard deviation calculation failed - check data values")
      }
      
      # Create a standardized series: divide each fraction by the base period's sigma
      df <- df %>% mutate(Std_Fraction = Fraction / sigma_i)
      
      # Return only the Date and standardized fraction columns
      result <- dplyr::select(df, Date, Std_Fraction)
      
      # Log success
      log_message(sprintf("Successfully processed %s\n", basename(file_path_EPU)))
      
      return(result)
    }, error = function(e) {
      log_message(sprintf("Error processing files: %s\n", e$message))
      stop(e)
    })
  }, sprintf("Processing newspaper data: %s", basename(file_path_EPU)))
  
  if (is.null(result)) {
    stop("Failed to process newspaper data")
  }
  
  return(result)
}
