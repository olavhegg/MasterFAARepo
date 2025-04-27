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

# Enhanced error handling and logging function
handle_error_with_logging <- function(expr, operation_name) {
  tryCatch({
    expr()
  }, error = function(e) {
    # Get the call stack
    calls <- sys.calls()
    call_stack <- paste(sapply(calls, function(call) paste(deparse(call), collapse="\n")), collapse="\n")
    
    # Log detailed error information with better formatting
    log_message("\n╔══════════════════════════════════════════════════════════════════════════════╗\n")
    log_message("║ ❌ ERROR in ", operation_name, "\n")
    log_message("╠══════════════════════════════════════════════════════════════════════════════╣\n")
    log_message("║ Error message: ", conditionMessage(e), "\n")
    log_message("║ Error location: ", as.character(e$call), "\n")
    
    # If there's data involved in the error, log its structure
    error_env <- parent.frame()
    if (exists("data", error_env)) {
      data_obj <- get("data", error_env)
      log_message("║ Data structure involved:\n")
      con <- textConnection("output", "w", local = TRUE)
      tryCatch({
        str(data_obj, file = con)
        log_message("║ ", paste(output, collapse = "\n║ "), "\n")
      }, finally = {
        close(con)
      })
    }
    
    # Log the call stack
    log_message("║ Call stack:\n")
    for (i in seq_along(calls)) {
      log_message("║   ", i, ": ", deparse(calls[[i]]), "\n")
    }
    log_message("╚══════════════════════════════════════════════════════════════════════════════╝\n")
    
    # Re-throw the error to maintain the error state
    stop(sprintf("Operation failed: %s - %s", operation_name, e$message))
  })
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
  handle_error_with_logging(function() {
    # Read Excel files for EPU counts and total article counts
    df_EPU <- suppressMessages(read_excel(file_path_EPU, col_names = FALSE, skip = skip_val))
    df_All <- suppressMessages(read_excel(file_path_All, col_names = FALSE, skip = skip_val))
    
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
  }, sprintf("Processing newspaper data: %s", basename(file_path_EPU)))
}

# Function to handle errors and stop execution
stop_on_error <- function(expr, step_name) {
  handle_error_with_logging(
    function() {
      expr
      log_message("✓ ", step_name, " completed successfully\n")
      return(TRUE)
    },
    step_name
  )
}
