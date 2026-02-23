# ============================================================================
# R/data_recoding.R
# Helper functions to recode raw SQF data to standardized format
# ============================================================================

library(tidyverse)
library(lubridate)

# ---- (1.1) RECODE RACE ----

#' Recode raw race codes to standardized categories
#'
#' Converts NYPD's single-letter race codes to full category names.
#' Based on NYPD Stop, Question and Frisk Database codebook.
#'
#' @param race_raw Character vector of raw race codes (W, B, P, Q, A, I, Z)
#' @return Ordered factor with levels: White, Black, Hispanic, Asian, Other
#'
#' @details
#' Mapping (from NYPD codebook):
#' - W = White
#' - B = Black
#' - P, Q = Hispanic
#' - A = Asian
#' - I, Z = Other
#' - Invalid/missing codes = NA
#'
#' @examples
#' recode_race(c("W", "B", "P", "A"))
#' # [1] White Black Hispanic Asian
#' # Levels: White < Black < Hispanic < Asian < Other
#'
#' recode_race(c("Q", "X", "Z"))  # X becomes NA
#' # [1] Hispanic <NA>    Other
#'
recode_race <- function(race_raw) {
  race_clean <- dplyr::recode(
    race_raw,
    "W" = "White",
    "B" = "Black",
    "P" = "Hispanic",
    "Q" = "Hispanic",
    "A" = "Asian",
    "I" = "Other",
    "Z" = "Other",
    .default = NA_character_
  )

  factor(
    race_clean,
    levels = c("White", "Black", "Hispanic", "Asian", "Other"),
    ordered = TRUE
  )
}

# ---- (1.2) PARSE DATE AND TIME ----

#' Parse SQF date and time fields into datetime
#'
#' Combines separate date and time fields into a single POSIXct datetime.
#' Handles multiple date formats used across different years in SQF dataset.
#'
#' @param datestop Character vector of dates (various formats per year)
#' @param timestop Character/numeric vector of times (24-hour, 0-padded)
#' @param date_format Character, format hint: "ymd" (2006) or "mdy" (2007+)
#'
#' @return POSIXct datetime vector. Invalid dates become NA.
#'
#' @details
#' Date Formats by Year:
#' - 2006: "YYYY-MM-DD" → parsed as "ymd HM"
#' - 2007+: "MMDDYYYY" → padded to 8 digits, parsed as "mdyHM"
#'
#' Note: 2009-2012 data has inconsistent formatting with missing leading zeros
#' on months (e.g., "1122009" instead of "01122009"). This function pads all
#' 2007+ dates to 8 digits before parsing.
#'
#' Handles sentinel values that indicate missing data:
#' - datestop: "1900-12-31", "12311900", ""
#' - timestop: leading zeros are added (e.g., "105" → "0105")
#'
#' @examples
#' # 2006 format (YYYY-MM-DD)
#' parse_sqf_datetime("2006-01-15", "1430", "ymd")
#'
#' # 2007+ format (MMDDYYYY, variable length)
#' parse_sqf_datetime("01022007", "1530", "mdy")
#' parse_sqf_datetime("1122009", "0800", "mdy")  # Missing leading zero
#'
#' # Sentinel values
#' parse_sqf_datetime("1900-12-31", "1430", "ymd")  # Returns NA
#'
parse_sqf_datetime <- function(datestop, timestop, date_format = "ymd") {
  # Convert to character and trim whitespace
  datestop <- as.character(datestop) %>% stringr::str_trim()
  timestop <- as.character(timestop)

  # Remove sentinel date values (missing data markers)
  datestop <- dplyr::na_if(datestop, "1900-12-31")
  datestop <- dplyr::na_if(datestop, "12311900")
  datestop <- dplyr::na_if(datestop, "")

  # Pad time to 4 digits (e.g., "830" → "0830")
  timestop <- stringr::str_pad(timestop, width = 4, side = "left", pad = "0")

  # NEW FIX: For 2007+ dates, pad to 8 digits
  # Handles inconsistent formatting in 2009-2012 data
  # Example: "1122009" (7 chars) → "01122009" (8 chars)
  if (date_format == "mdy") {
    datestop <- stringr::str_pad(datestop, width = 8, side = "left", pad = "0")
  }

  # Combine date and time strings
  # IMPORTANT: Use if_else to preserve NA dates (paste(NA, x) creates "NA" string!)
  datetime_str <- dplyr::if_else(
    is.na(datestop),
    NA_character_,
    paste(datestop, timestop)  # e.g., "2006-01-15 1430" or "01022007 1530"
  )

  # Determine parsing format
  orders <- if (date_format == "ymd") {
    "ymd HM"   # 2006: "2006-01-15 1430" (with dashes, space before time)
  } else if (date_format == "mdy") {
    "mdyHM"    # 2007+: "01022007 1530" (no dashes, space before time)
  } else {
    stop("date_format must be 'ymd' or 'mdy'")
  }

  # Parse to POSIXct datetime
  lubridate::parse_date_time(datetime_str, orders = orders, quiet = FALSE)
}

# ---- (1.3) CLEAN AGE ----

#' Clean age variable
#'
#' Converts age to integer and replaces invalid sentinel values with NA.
#' NYPD used various codes for missing/invalid ages in the SQF data.
#'
#' @param age_raw Character or numeric vector of raw ages
#' @return Integer vector with sentinel values replaced by NA
#'
#' @details
#' Sentinel values replaced with NA:
#' - 99 (missing/not entered)
#' - 377 (invalid data)
#' - 999 (unknown/unreliable)
#'
#' These codes are domain knowledge from NYPD data documentation.
#'
#' @examples
#' clean_age(c("25", "30", "99", "377"))
#' # [1]  25  30  NA  NA
#'
#' clean_age(c("034", "099", "025"))
#' # [1]  34  NA  25
#'
clean_age <- function(age_raw) {
  # Convert character strings to integer
  age_clean <- as.integer(age_raw)

  # Replace sentinel values with NA
  age_clean[age_clean %in% c(99, 377, 999)] <- NA

  age_clean
}

# ---- (2.1) MAIN RECODING FUNCTION ----

#' Recode raw SQF data to standardized format
#'
#' Transforms raw SQF data with inconsistent formatting into a clean,
#' standardized format suitable for analysis. Uses helper functions to
#' ensure consistent recoding rules are applied across all years.
#'
#' This function:
#' - Generates unique IDs combining year and row number
#' - Parses and validates date/time fields
#' - Recodes categorical variables (race, gender)
#' - Cleans numeric variables (age, coordinates, precinct)
#' - Derives new variables (police_force from multiple input columns)
#'
#' @param data_raw Tibble, raw SQF data from `load_sqf_year()`
#' @param year Integer, year of the data (2006-2012)
#'   Used for: ID generation and determining date format
#'
#' @return Tibble with 11 standardized columns:
#'   - `id` (character): Unique identifier "YYYY-###"
#'   - `date` (Date): Date of stop
#'   - `time` (POSIXct): Exact datetime of stop
#'   - `year` (integer): Year of stop (2006-2012)
#'   - `race` (ordered factor): White, Black, Hispanic, Asian, Other
#'   - `female` (logical): TRUE if female, FALSE if male, NA if unknown
#'   - `age` (integer): Age in years (sentinel values replaced with NA)
#'   - `police_force` (logical): TRUE if any physical force used
#'   - `precinct` (integer): NYPD precinct (1-123)
#'   - `xcoord` (numeric): X coordinate (NY State Plane Coordinates)
#'   - `ycoord` (numeric): Y coordinate (NY State Plane Coordinates)
#'
#' @details
#' Date Formats by Year:
#' - 2006: "YYYY-MM-DD" (format = "ymd")
#' - 2007-2012: "MMDDYYYY" (format = "mdy")
#'
#' Police Force Columns Combined:
#' - pf_hands, pf_wall, pf_grnd, pf_drwep, pf_ptwep
#' - pf_baton, pf_hcuff, pf_pepsp, pf_other
#'
#' @examples
#' \dontrun{
#' # Load and recode one year
#' sqf_2006_raw <- load_sqf_year(2006)
#' sqf_2006_clean <- recode_sqf_year(sqf_2006_raw, 2006)
#'
#' # Check output
#' str(sqf_2006_clean)
#' head(sqf_2006_clean)
#' }
#'
#' @export
#'
recode_sqf_year <- function(data_raw, year) {
  # Input validation
  stopifnot(
    "data_raw must be a data frame" = is.data.frame(data_raw),
    "year must be numeric" = is.numeric(year),
    "year must be between 2006 and 2012" = year >= 2006 & year <= 2012
  )

  # Determine date format by year (2006 is different)
  date_format <- if (year == 2006) "ymd" else "mdy"

  # Transform and standardize the data
  data_raw %>%
    dplyr::transmute(
      # ---- Identifiers ----

      # Generate unique ID: "YYYY-row_number"
      # Example: "2006-1", "2006-2", ..., "2006-500000"
      id = stringr::str_c(year, "-", 1:dplyr::n()),

      # ---- Date and Time ----

      # Parse datetime ONCE (more efficient)
      # This will be used for both date and time columns
      .datetime = parse_sqf_datetime(datestop, timestop, date_format = date_format),

      # Extract date component (Date class, no time)
      # Example: 2006-01-15
      date = lubridate::as_date(.datetime),

      # Keep full datetime (POSIXct class with time component)
      # Example: 2006-01-15 14:30:00
      time = .datetime,

      # ---- Year ----

      # Store year as integer for consistency and filtering
      year = as.integer(year),

      # ---- Categorical Variables ----

      # Recode race codes (W, B, P, Q, A, I, Z) to full names
      # Returns ordered factor with levels: White < Black < Hispanic < Asian < Other
      race = recode_race(race),

      # Convert sex (M/F) to logical female indicator
      # TRUE = Female, FALSE = Male, NA = Unknown
      female = dplyr::case_when(
        sex == "F" ~ TRUE,
        sex == "M" ~ FALSE,
        TRUE ~ NA
      ),

      # ---- Numeric Variables ----

      # Clean age: convert to integer and remove sentinel values (99, 377, 999)
      age = clean_age(age),

      # ---- Derived Variables ----

      # Police force: TRUE if ANY of the 9 force indicators is "Y"
      # Combines: hands, wall, ground, weapon_drawn, weapon_pointed,
      #           baton, handcuffs, pepper_spray, other
      # If all are N: FALSE. If any is Y: TRUE. If all NA: NA.
      police_force = (pf_hands == "Y" | pf_wall == "Y" | pf_grnd == "Y" |
                      pf_drwep == "Y" | pf_ptwep == "Y" | pf_baton == "Y" |
                      pf_hcuff == "Y" | pf_pepsp == "Y" | pf_other == "Y"),

      # ---- Location Variables ----

      # Precinct: convert from character to integer
      # Raw column "pct" contains precinct code (1-123)
      precinct = as.integer(pct),

      # X coordinate: New York State Plane Coordinate system
      xcoord = as.numeric(xcoord),

      # Y coordinate: New York State Plane Coordinate system
      ycoord = as.numeric(ycoord),

      # Drop temporary datetime column (no longer needed)
      .datetime = NULL
    )
}
