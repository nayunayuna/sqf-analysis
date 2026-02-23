# Extract Helper Functions

# ---(1.1) Recode raw race codes to standardized categories---
# Converts NYPD's single-letter race codes to full category names.
#
#' Mapping: (Factors with levels)
#' - W = White
#' - B = Black  
#' - P, Q = Hispanic
#' - A = Asian
#' - I, Z = Other
#' - Invalid codes = NA
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





# ---(1.2) Parse SQF date and time fields---

parse_sqf_datetime <- function(datestop, timestop, date_format = "ymd") {
  # Turn inputs into character and clean whitespace
  datestop <- as.character(datestop) %>% stringr::str_trim()
  timestop <- as.character(timestop)

  # Remove sentinel dates
  datestop <- dplyr::na_if(datestop, "1900-12-31")
  datestop <- dplyr::na_if(datestop, "12311900")
  datestop <- dplyr::na_if(datestop, "")

  # Pad time to 4 digits
  timestop <- stringr::str_pad(timestop, width = 4, side = "left", pad = "0")

  # Combine and parse
  datetime_str <- paste(datestop, timestop)
  
  orders <- if (date_format == "ymd") {
    "ymd HM"
  } else if (date_format == "mdy") {
    "mdy HM"
  } else {
    stop("date_format must be 'ymd' or 'mdy'")
  }

  lubridate::parse_date_time(datetime_str, orders = orders, quiet = FALSE)
}




# ---(1.3) Clean age fields---
clean_age <- function(age_raw) {
  age_clean <- as.integer(age_raw)
  age_clean[age_clean %in% c(99, 377, 999)] <- NA
  age_clean
}


# ---(2.1) write `recode_sqf_year()` function ---
recode_sqf_year <- function(data_raw, year) {    
  # Determine date format by year
  date_format <- if (year == 2006) "ymd" else "mdy"

  data_raw %>%
    dplyr::transmute(
      # Create unique ID with year and row number
      id = stringr::str_c(year, "-", 1:dplyr::n()),

      # Parse date (remove sentinel values and convert to Date)
      date = parse_sqf_datetime(datestop, timestop, date_format = date_format) %>%
        lubridate::as_date(),

      # Parse datetime
      time = parse_sqf_datetime(datestop, timestop, date_format = date_format),

      # Year as integer
      year = as.integer(year),

      # Recode race to standardized categories
      race = recode_race(race),

      # Convert gender M/F to logical female indicator
      female = dplyr::case_when(
        sex == "F" ~ TRUE,
        sex == "M" ~ FALSE,
        TRUE ~ NA
      ),

      # Clean age (remove sentinels)
      age = clean_age(age),

      # Police force: TRUE if ANY force column is "Y"
      # Explicitly check all known pf_* columns
      police_force = (pf_hands == "Y" | pf_wall == "Y" | pf_grnd == "Y" | 
                      pf_drwep == "Y" | pf_ptwep == "Y" | pf_baton == "Y" | 
                      pf_hcuff == "Y" | pf_pepsp == "Y" | pf_other == "Y"),

      # Precinct as integer
      pct = as.integer(pct),

      # Coordinates as numeric
      xcoord = as.numeric(xcoord),
      ycoord = as.numeric(ycoord)
    )
}
