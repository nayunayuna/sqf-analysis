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

