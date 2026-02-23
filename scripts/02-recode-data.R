# ============================================================================
# scripts/02-recode-data.R
# Recode raw SQF data to standardized format
# ============================================================================

library(tidyverse)
library(lubridate)

# ---- Source helper functions ----
source("R/data_recording.R")  # Contains recode_sqf_year() and helpers

# ---- Load raw data ----
# SKIP sourcing 01-load-data.R (it already ran once to create sqf_raw.rds)
# Just load the saved file directly:
message("Loading raw SQF data...")
sqf_raw <- read_rds("data/sqf_raw.rds")
message(sprintf("Loaded: %s rows", format(nrow(sqf_raw), big.mark = ",")))

# ---- Recode each year ----
# Split by year, apply recoding function, combine results
message("\nRecoding SQF data by year...")

sqf_clean <- sqf_raw %>%
  dplyr::group_split(data_year) %>%
  purrr::map_dfr(
    ~ recode_sqf_year(.x, year = .x$data_year[1]),
    .id = NULL
  )

# ---- Summary statistics ----
message("\n=== Recoding Complete ===")
message(sprintf("Total observations: %s", format(nrow(sqf_clean), big.mark = ",")))
message(sprintf("Total columns: %d", ncol(sqf_clean)))

# Show observations per year
year_summary <- sqf_clean %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(
    n_stops = dplyr::n(),
    .groups = "drop"
  ) %>%
  dplyr::arrange(year)

print(year_summary)

# ---- Save cleaned data ----
message("\nSaving cleaned data...")
write_rds(sqf_clean, "data/sqf_clean.rds", compress = "gz")
message("✓ Saved to: data/sqf_clean.rds")

# ---- Quick data quality check ----
message("\n=== Quick Data Quality Check ===")
message(sprintf("Race distribution:\n"))
print(table(sqf_clean$race, useNA = "ifany"))

message(sprintf("\nPolice force used: %.2f%%", 
                100 * mean(sqf_clean$police_force, na.rm = TRUE)))

message(sprintf("Missing dates: %d (%.2f%%)", 
                sum(is.na(sqf_clean$date)),
                100 * mean(is.na(sqf_clean$date))))