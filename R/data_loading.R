# NYC SQF Analysis

# ---- Load packages ----
library(tidyverse)
library(lubridate)

# ---- Define data-loading functions ---
load_sqf_year <- function(year, data_dir = "data/raw") {
  # Input validation
  stopifnot(
    "Year must be numeric" = is.numeric(year),
    "Year must be a single value" = length(year) == 1,
    "Year must be between 2006 and 2012" = year >= 2006 & year <= 2012,
    "Data directory must be a character string" = is.character(data_dir)
  )
  
  # Construct file path using file.path() for cross-platform compatibility
  file_name <- paste0(year, ".csv")
  file_path <- file.path(data_dir, file_name)
  
  # Check if file exists before attempting to read
  if (!file.exists(file_path)) {
    stop(sprintf("File not found: %s\nPlease ensure the file exists in %s", 
                 file_name, data_dir))
  }
  
  # Inform user of progress
  message(sprintf("Loading data for year %d...", year))
  
  # Load CSV with all columns as character to avoid type guessing issues
  # This is important because the same column may have different types across years
  data <- read_csv(
    file_path,
    col_types = cols(.default = col_character()),
    show_col_types = FALSE  # Suppress column type messages
  )
  
  # Add year column for identification after combining
  data <- data %>%
    mutate(data_year = as.integer(year))
  
  # Return the loaded data
  return(data)
}

# --- Load and combine SQF data for multiple years ---
#'
#' Loads SQF data for multiple years and combines them into a single data frame.
#' Uses load_sqf_year() internally for each year.
#'
#' @param years Integer vector, years to load (each must be 2006-2012)
#' @param data_dir Character, path to directory containing CSV files
#' @return Tibble with combined SQF data from all specified years
#'
#' @examples
#' # Load all years
#' sqf_all <- load_sqf_all()
#'
#' # Load specific years
#' sqf_subset <- load_sqf_all(years = c(2010, 2011, 2012))


load_sqf_all <- function(years = 2006:2012, data_dir = "data/raw") {
  # Validate inputs
  stopifnot(
    "Years must be numeric" = is.numeric(years),
    "Years must be between 2006 and 2012" = all(years >= 2006 & years <= 2012)
  )
  
  message(sprintf("Loading %d years of SQF data...", length(years)))
  
  # Load each year and combine
  # Using map_dfr() from purrr: applies function to each year, combines rows
  sqf_data <- map_dfr(years, load_sqf_year, data_dir = data_dir)
  
  message(sprintf("Successfully loaded %s rows from %d years", 
                  format(nrow(sqf_data), big.mark = ","),
                  length(years)))
  
  return(sqf_data)
}

