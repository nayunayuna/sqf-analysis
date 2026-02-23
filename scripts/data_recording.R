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