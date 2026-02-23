# ============================================================================
# R/data_validation.R
#' Performs comprehensive checks on cleaned SQF data to ensure 
#' quality and catch potential data issues early.
# ============================================================================
#' Validate cleaned SQF data
#'
#' Performs comprehensive checks on cleaned SQF data to ensure
#' quality and catch potential data issues early.
#'
#' @param data Tibble, cleaned SQF data
#' @return List with validation results and any issues found
#'
#' @examples
#' sqf_clean <- read_rds("data/sqf_clean.rds")
#' validation <- validate_sqf_data(sqf_clean)
#' print(validation)
#'
validate_sqf_data <- function(data) {
  issues <- list()

  # ---- Check 1: Required columns exist ----
  required_cols <- c(
    "id", "date", "time", "year", "race", "female",
    "age", "police_force", "precinct", "xcoord", "ycoord"
  )
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    issues$missing_columns <- missing_cols
  }

  # ---- Check 2: Year in valid range (2006-2012) ----
  invalid_years <- data %>%
    dplyr::filter(year < 2006 | year > 2012) %>%
    nrow()
  if (invalid_years > 0) {
    issues$invalid_years <- sprintf(
      "%d rows with year outside 2006-2012",
      invalid_years
    )
  }

  # ---- Check 3: Age in plausible range (0-120, allowing NA) ----
  invalid_ages <- data %>%
    dplyr::filter(!is.na(age) & (age < 0 | age > 120)) %>%
    nrow()
  if (invalid_ages > 0) {
    issues$invalid_ages <- sprintf(
      "%d rows with age < 0 or > 120",
      invalid_ages
    )
  }

  # ---- Check 4: Race categories are valid ----
  # For ordered factor, check all non-NA values are valid levels
  valid_races <- c("White", "Black", "Hispanic", "Asian", "Other")
  invalid_races <- data %>%
    dplyr::filter(!is.na(race) & !race %in% valid_races) %>%
    nrow()
  if (invalid_races > 0) {
    issues$invalid_races <- sprintf(
      "%d rows with invalid race category",
      invalid_races
    )
  }

  # ---- Check 5: Female is logical (TRUE/FALSE/NA only) ----
  # Check: Column should already be logical type (from recoding)
  if (!is.logical(data$female)) {
    issues$female_type <- "female column is not logical class"
  }

  # ---- Check 6: Police force is logical ----
  # Check: Column should already be logical type (from recoding)
  if (!is.logical(data$police_force)) {
    issues$police_force_type <- "police_force column is not logical class"
  }

  # ---- Check 7: Coordinates present (not too many missing) ----
  coord_missing <- data %>%
    dplyr::filter(is.na(xcoord) | is.na(ycoord)) %>%
    nrow()
  pct_missing <- (coord_missing / nrow(data)) * 100
  if (pct_missing > 10) {
    issues$coordinates_missing <- sprintf(
      "%.2f%% of rows missing coordinates",
      pct_missing
    )
  }

  # ---- Check 8: IDs are unique ----
  duplicate_ids <- data %>%
    dplyr::group_by(id) %>%
    dplyr::filter(dplyr::n() > 1) %>%
    nrow()
  if (duplicate_ids > 0) {
    issues$duplicate_ids <- sprintf(
      "%d duplicate IDs found",
      duplicate_ids
    )
  }

  # ---- Check 9: Precinct in valid range (1-123) ----
  # FIXED: Changed from 77 to 123 (actual NYPD precinct count)
  invalid_precinct <- data %>%
    dplyr::filter(!is.na(precinct) & (precinct < 1 | precinct > 123)) %>%
    nrow()
  if (invalid_precinct > 0) {
    issues$invalid_precinct <- sprintf(
      "%d rows with precinct outside 1-123",
      invalid_precinct
    )
  }

  # ---- Check 10: Date and time are correct class ----
  if (!inherits(data$date, "Date")) {
    issues$date_type <- "date column is not Date class"
  }
  if (!inherits(data$time, "POSIXct")) {
    issues$time_type <- "time column is not POSIXct class"
  }

  # ---- Check 11: ID format is correct ----
  # IDs should match format "YYYY-#####"
  invalid_ids <- data %>%
    dplyr::filter(!stringr::str_detect(id, "^\\d{4}-\\d+$")) %>%
    nrow()
  if (invalid_ids > 0) {
    issues$invalid_id_format <- sprintf(
      "%d rows with invalid ID format",
      invalid_ids
    )
  }

  # ---- Return results ----
  list(
    passed = length(issues) == 0,
    n_issues = length(issues),
    issues = issues,
    n_rows = nrow(data),
    n_cols = ncol(data),
    year_range = paste(
      min(data$year, na.rm = TRUE),
      "-",
      max(data$year, na.rm = TRUE)
    ),
    timestamp = Sys.time()
  )
}